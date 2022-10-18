use std::collections::BTreeSet;
use std::io::Write;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand};
use config::Config;
use model::UpdateParams;
use relative_path::RelativePath;
use url::Url;

use crate::actions::{Actions, ActionsCheck};
use crate::badges::Badges;
use crate::file::{File, LineColumn};
use crate::model::{work_cargo_toml, Ci, CrateParams, Readme, Validation};
use crate::urls::{UrlError, Urls};

mod actions;
mod badges;
mod cargo;
mod config;
mod file;
mod gitmodules;
mod model;
mod templates;
mod urls;
mod workspace;

const README_MD: &str = "README.md";
const PROJECTS_TOML: &str = "Projects.toml";

#[tokio::main]
async fn main() -> Result<()> {
    entry().await
}

async fn entry() -> Result<()> {
    let root = find_root()?;

    let template = templates::Templating::new()?;

    let config = {
        let config_path = root.join(PROJECTS_TOML);
        config::load(&config_path, &template)
            .with_context(|| config_path.to_string_lossy().into_owned())?
    };

    let opts = Opts::try_parse()?;

    let mut badges = Badges::new();

    for badge in &config.badges {
        badges.push(badge);
    }

    let mut actions = Actions::default();
    actions.latest("actions/checkout", "v3");
    actions.check("actions-rs/toolchain", &ActionsRsToolchainActionsCheck);
    actions.deny("actions-rs/cargo", "using `run` is less verbose and faster");

    let default_workflow = config.default_workflow.render(&config.global_render())?;

    let cx = Ctxt {
        root: &root,
        config: &config,
        badges: &badges,
        actions: &actions,
        default_workflow,
    };

    match &opts.action {
        Action::Run(run) => {
            let gitmodules_path = root.join(".gitmodules");
            let gitmodules_bytes;
            let mut modules = Vec::new();

            if gitmodules_path.is_file() {
                gitmodules_bytes = std::fs::read(root.join(".gitmodules"))?;

                modules.extend(
                    parse_git_modules(&gitmodules_bytes)
                        .with_context(|| anyhow!("{}", gitmodules_path.display()))?,
                );
            } else {
                modules.push(module_from_git(&root)?);
            }

            let mut validation = Vec::new();
            let mut urls = Urls::default();

            for module in &modules {
                run_module(&cx, run, module, &mut validation, &mut urls)?;
            }

            for validation in &validation {
                validate(&cx, run, validation)?;
            }

            let o = std::io::stdout();
            let mut o = o.lock();

            for (url, test) in urls.bad_urls() {
                let path = &test.path;
                let (line, column, string) =
                    temporary_line_fix(&test.file, test.range.start, test.line_offset)?;

                if let Some(error) = &test.error {
                    writeln!(o, "{path}:{line}:{column}: bad url: `{url}`: {error}")?;
                } else {
                    writeln!(o, "{path}:{line}:{column}: bad url: `{url}`")?;
                }

                writeln!(o, "{string}")?;
            }

            if run.url_checks {
                url_checks(&mut o, urls).await?;
            }
        }
    }

    Ok(())
}

#[derive(Default, Parser)]
struct Run {
    #[arg(long)]
    fix: bool,
    #[arg(long)]
    url_checks: bool,
    filters: Vec<String>,
}

#[derive(Subcommand)]
enum Action {
    Run(Run),
}

#[derive(Default, Parser)]
struct Opts {
    #[command(subcommand)]
    action: Action,
}

impl Default for Action {
    fn default() -> Self {
        Self::Run(Run::default())
    }
}

struct Ctxt<'a> {
    root: &'a Path,
    config: &'a Config,
    badges: &'a Badges<'a>,
    actions: &'a Actions<'a>,
    default_workflow: String,
}

/// Run a single module.
fn run_module(
    cx: &Ctxt<'_>,
    run: &Run,
    module: &Module<'_>,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    if !run.filters.is_empty()
        && !run
            .filters
            .iter()
            .all(|filter| module.name.contains(filter))
    {
        return Ok(());
    }

    let (module_path, module_url) = if let (Some(path), Some(url)) = (module.path, &module.url) {
        (path, url)
    } else {
        println!(
            ".gitmodules: {name}: missing `path` and/or `url`",
            name = module.name
        );
        return Ok(());
    };

    let repo = String::from(module_url.path().trim_matches('/'));

    let cargo_toml = match module.cargo_toml {
        Some(cargo_toml) => module_path.join(cargo_toml),
        None => module_path.join(workspace::CARGO_TOML),
    };

    let workspace = workspace::open(cx.root, &cargo_toml)?;

    let primary_crate = module.krate.or(repo.split('/').last());

    let primary_crate = match workspace.primary_crate(primary_crate)? {
        Some(primary_crate) => primary_crate,
        None => return Err(anyhow!("{module_path}: cannot determine primary crate",)),
    };

    let params = cx.config.per_crate_render(CrateParams {
        repo: &repo,
        name: primary_crate.manifest.crate_name()?,
    });

    let documentation = cx.config.documentation.render(&params)?;
    let url_string = module_url.to_string();

    let update_params = UpdateParams {
        license: &cx.config.license,
        readme: README_MD,
        repository: &url_string,
        homepage: &url_string,
        documentation: &documentation,
        authors: &cx.config.authors,
    };

    for package in workspace.packages() {
        if package.manifest.is_publish()? {
            work_cargo_toml(package, validation, &update_params)?;
        }
    }

    if module.is_enabled("ci") {
        let path = module_path.join(".github").join("workflows");
        let ci = Ci::new(
            &path,
            &cx.config.job_name,
            cx.actions,
            &primary_crate.manifest,
            !workspace.is_single_crate(),
        );
        ci.validate(cx.root, validation)
            .with_context(|| anyhow!("ci validation: {}", cx.config.job_name))?;
    }

    if module.is_enabled("readme") {
        build_readme(
            &cx,
            module_path,
            &primary_crate.manifest_dir,
            params.crate_params,
            validation,
            urls,
        )?;

        for package in workspace.packages() {
            if package.manifest_dir != module_path {
                if package.manifest.is_publish()? {
                    let crate_params = CrateParams {
                        repo: &repo,
                        name: package.manifest.crate_name()?,
                    };

                    build_readme(
                        cx,
                        &package.manifest_dir,
                        &package.manifest_dir,
                        crate_params,
                        validation,
                        urls,
                    )?;
                }
            }
        }
    }

    Ok(())
}

/// Find root path to use.
fn find_root() -> Result<PathBuf> {
    let mut current = std::env::current_dir()?;

    loop {
        if current.join(PROJECTS_TOML).is_file() {
            return Ok(current);
        }

        if !current.pop() {
            return Err(anyhow!("missing projects directory"));
        }
    }
}

/// Process module information from a git repository.
fn module_from_git(_: &Path) -> Result<Module<'static>> {
    Err(anyhow!("cannot get a module from .git repo"))
}

/// Report and apply a asingle validation.
fn validate(cx: &Ctxt<'_>, run: &Run, error: &Validation) -> Result<()> {
    Ok(match error {
        Validation::MissingWorkflows { path } => {
            println!("{path}: Missing workflows directory");
        }
        Validation::MissingWorkflow { path, candidates } => {
            println!("{path}: Missing workflow");

            for candidate in candidates.iter() {
                println!("  Candidate: {candidate}");
            }

            if run.fix {
                if let [from] = candidates.as_ref() {
                    println!("{path}: Rename from {from}",);
                    std::fs::rename(from.to_path(cx.root), path.to_path(cx.root))?;
                } else {
                    std::fs::write(path.to_path(cx.root), &cx.default_workflow)?;
                }
            }
        }
        Validation::DeprecatedWorkflow { path } => {
            println!("{path}: Reprecated Workflow");
        }
        Validation::WrongWorkflowName {
            path,
            actual,
            expected,
        } => {
            println!("{path}: Wrong workflow name: {actual} (actual) != {expected} (expected)");
        }
        Validation::OutdatedAction {
            path,
            name,
            actual,
            expected,
        } => {
            println!(
                "{path}: Outdated action `{name}`: {actual} (actual) != {expected} (expected)"
            );
        }
        Validation::DeniedAction { path, name, reason } => {
            println!("{path}: Denied action `{name}`: {reason}");
        }
        Validation::CustomActionsCheck { path, name, reason } => {
            println!("{path}: Action validation failed `{name}`: {reason}");
        }
        Validation::MissingReadme { path } => {
            println!("{path}: Missing README");
        }
        Validation::MismatchedLibRs { path, new_file } => {
            if run.fix {
                println!("{path}: Fixing lib.rs");
                std::fs::write(path.to_path(cx.root), new_file.as_bytes())?;
            } else {
                println!("{path}: Mismatched lib.rs");
            }
        }
        Validation::BadReadme { path, new_file } => {
            if run.fix {
                println!("{path}: Fixing README.md");
                std::fs::write(path.to_path(cx.root), new_file.as_bytes())?;
            } else {
                println!("{path}: Bad README.md");
            }
        }
        Validation::ToplevelHeadings {
            path,
            file,
            range,
            line_offset,
        } => {
            let (line, column, string) = temporary_line_fix(&file, range.start, *line_offset)?;
            println!("{path}:{line}:{column}: doc comment has toplevel headings");
            println!("{string}");
        }
        Validation::MissingPreceedingBr {
            path,
            file,
            range,
            line_offset,
        } => {
            let (line, column, string) = temporary_line_fix(&file, range.start, *line_offset)?;
            println!("{path}:{line}:{column}: missing preceeding <br>");
            println!("{string}");
        }
        Validation::MissingFeature { path, feature } => {
            println!("{path}: missing features `{feature}`");
        }
        Validation::NoFeatures { path } => {
            println!("{path}: trying featured build (--all-features, --no-default-features), but no features present");
        }
        Validation::MissingEmptyFeatures { path } => {
            println!("{path}: missing empty features build");
        }
        Validation::MissingAllFeatures { path } => {
            println!("{path}: missing all features build");
        }
        Validation::CargoTomlIssues {
            path,
            cargo: modified_cargo,
            issues,
        } => {
            println!("{path}:");

            for issue in issues {
                println!("  {issue}");
            }

            if run.fix {
                if let Some(modified_cargo) = modified_cargo {
                    modified_cargo.save_to(path.to_path(cx.root))?;
                }
            }
        }
    })
}

/// Perform url checks.
async fn url_checks<O>(o: &mut O, urls: Urls) -> Result<()>
where
    O: Write,
{
    let (tx, mut rx) = tokio::sync::mpsc::channel(1);

    let total = urls.check_urls();
    let checks = urls.check_urls_task(3, tx);
    tokio::pin!(checks);
    let mut count = 1;
    let mut completed = false;

    loop {
        tokio::select! {
            result = checks.as_mut(), if !completed => {
                result?;
                completed = true;
            }
            result = rx.recv() => {
                let result = match result {
                    Some(result) => result,
                    None => break,
                };

                match result {
                    Ok(_) => {}
                    Err(UrlError { url, status, tests }) => {
                        writeln!(o, "{count:>3}/{total} {url}: {status}")?;

                        for test in tests {
                            let path = &test.path;
                            let (line, column, string) = temporary_line_fix(&test.file, test.range.start, test.line_offset)?;
                            writeln!(o, "  {path}:{line}:{column}: {string}")?;
                        }
                    }
                }

                count += 1;
            }
        }
    }

    Ok(())
}

/// Temporary line comment fix which adjusts the line and column.
fn temporary_line_fix(file: &File, pos: usize, line_offset: usize) -> Result<(usize, usize, &str)> {
    let (LineColumn { line, column }, string) = file.line_column(pos)?;
    let line = line_offset + line;
    let column = column + 4;
    Ok((line, column, string))
}

/// Perform readme validation.
fn build_readme(
    cx: &Ctxt<'_>,
    readme_path: &RelativePath,
    crate_path: &RelativePath,
    params: CrateParams<'_>,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    let readme_path = readme_path.join(README_MD);
    let lib_rs = crate_path.join("src").join("lib.rs");

    let readme = Readme::new(&readme_path, &lib_rs, cx.badges, params, cx.config);

    readme
        .validate(cx.root, validation, urls)
        .with_context(|| anyhow!("{readme_path}: readme validation"))?;

    Ok(())
}

/// A git module.
#[derive(Debug, Clone)]
pub(crate) struct Module<'a> {
    name: &'a str,
    path: Option<&'a RelativePath>,
    url: Option<Url>,
    cargo_toml: Option<&'a RelativePath>,
    krate: Option<&'a str>,
    disabled: BTreeSet<&'a str>,
}

impl Module<'_> {
    /// Test if a feature is disabled.
    fn is_enabled(&self, feature: &str) -> bool {
        !self.disabled.contains(feature)
    }
}

/// Parse gitmodules from the given input.
pub(crate) fn parse_git_modules(input: &[u8]) -> Result<Vec<Module<'_>>> {
    let mut parser = gitmodules::Parser::new(input);

    let mut modules = Vec::new();

    while let Some(module) = parse_git_module(&mut parser)? {
        modules.push(module);
    }

    return Ok(modules);

    /// Parse a git module.
    pub(crate) fn parse_git_module<'a>(
        parser: &mut gitmodules::Parser<'a>,
    ) -> Result<Option<Module<'a>>> {
        let mut path = None;
        let mut url = None;
        let mut disabled = BTreeSet::new();
        let mut cargo_toml = None;
        let mut krate = None;

        let mut section = match parser.parse_section()? {
            Some(section) => section,
            None => return Ok(None),
        };

        while let Some((key, value)) = section.next_section()? {
            match key {
                "path" => {
                    let string = std::str::from_utf8(value)?;
                    path = Some(RelativePath::new(string));
                }
                "url" => {
                    let string = std::str::from_utf8(value)?;
                    url = Some(str::parse::<Url>(string)?);
                }
                "cargo-toml" => {
                    let string = std::str::from_utf8(value)?;
                    cargo_toml = Some(RelativePath::new(string));
                }
                "crate" => {
                    krate = Some(std::str::from_utf8(value)?);
                }
                "disabled" => {
                    disabled = std::str::from_utf8(value)?
                        .split(',')
                        .map(str::trim)
                        .collect();
                }
                _ => {}
            }
        }

        Ok(Some(Module {
            name: section.name(),
            path,
            url,
            cargo_toml,
            krate,
            disabled,
        }))
    }
}

struct ActionsRsToolchainActionsCheck;

impl ActionsCheck for ActionsRsToolchainActionsCheck {
    fn check(&self, mapping: &serde_yaml::Mapping) -> Result<(), String> {
        let with = match mapping.get("with").and_then(|v| v.as_mapping()) {
            Some(with) => with,
            None => return Err(String::from("missing with")),
        };

        if with.get("toolchain").and_then(|v| v.as_str()).is_none() {
            return Err(String::from("missing toolchain"));
        }

        Ok(())
    }
}
