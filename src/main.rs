use std::collections::BTreeSet;
use std::io::Write;
use std::path::{Path, PathBuf};

use anyhow::{anyhow, Context, Result};
use model::UpdateParams;
use relative_path::RelativePath;
use url::Url;

use crate::actions::{Actions, ActionsCheck};
use crate::badges::{Badge, Badges};
use crate::config::ConfigBadge;
use crate::file::{File, LineColumn};
use crate::model::{work_cargo_toml, Ci, Readme, ReadmeParams, Validation};
use crate::urls::{UrlError, Urls};

mod actions;
mod badges;
mod cargo;
mod config;
mod file;
mod gitmodules;
mod model;
mod urls;
mod workspace;

const DOCS_RS: &str = "https://docs.rs";
const GITHUB_COM: &str = "https://github.com";
const README_MD: &str = "README.md";

argwerk::define! {
    #[derive(Default)]
    #[usage = "tool [-h]"]
    struct Opts {
        help: bool,
        fix: bool,
        url_checks: bool,
        filters: Vec<String>,
    }
    /// Show this help.
    ["-h" | "--help"] => {
        println!("{}", Opts::help());
        help = true;
    }
    /// Fix any issues with projects.
    ["--fix"] => {
        fix = true;
    }
    /// Perform URL checks.
    ["--url-checks"] => {
        url_checks = true;
    }
    /// Filter which project to apply changes to.
    [filter] => {
        filters.push(filter);
    }
}

#[tokio::main]
async fn main() -> Result<()> {
    entry().await
}

async fn entry() -> Result<()> {
    let root = PathBuf::from(
        std::env::var_os("CARGO_MANIFEST_DIR")
            .ok_or_else(|| anyhow!("missing CARGO_MANIFEST_DIR"))?,
    );

    let config_path = root.join("config.toml");
    let config =
        config::load(&config_path).with_context(|| config_path.to_string_lossy().into_owned())?;

    let opts = Opts::args()?;

    if opts.help {
        return Ok(());
    }

    let mut badges = Badges::new();

    for badge in &config.badges {
        match badge {
            ConfigBadge::Github => {
                badges.push(&GithubBadge);
            }
            ConfigBadge::CratesIo => {
                badges.push(&CratesIoBadge);
            }
            ConfigBadge::DocsRs => {
                badges.push(&DocsRsBadge);
            }
            ConfigBadge::GithubActions => {
                badges.push(&GithubActionsBadge);
            }
        }
    }

    let mut uses = Actions::default();
    uses.latest("actions/checkout", "v3");
    uses.check("actions-rs/toolchain", &ActionsRsToolchainActionsCheck);
    uses.deny("actions-rs/cargo", "using `run` is less verbose and faster");

    let mut validation = Vec::new();

    let gitmodules_bytes = std::fs::read(root.join(".gitmodules"))?;
    let modules = parse_git_modules(&gitmodules_bytes).context(".gitmodules")?;

    let mut urls = Urls::default();

    for module in &modules {
        if !opts.filters.is_empty()
            && !opts
                .filters
                .iter()
                .all(|filter| module.name.contains(filter))
        {
            continue;
        }

        let (module_path, module_url) = if let (Some(path), Some(url)) = (module.path, &module.url)
        {
            (path, url)
        } else {
            println!(
                ".gitmodules: {name}: missing `path` and/or `url`",
                name = module.name
            );
            continue;
        };

        let repo = String::from(module_url.path().trim_matches('/'));

        let cargo_toml = match module.cargo_toml {
            Some(cargo_toml) => module_path.join(cargo_toml),
            None => module_path.join(workspace::CARGO_TOML),
        };

        let workspace = workspace::open(&root, &cargo_toml)?;

        let primary_crate = module.krate.or(repo.split('/').last());

        let primary_crate = match workspace.primary_crate(primary_crate)? {
            Some(primary_crate) => primary_crate,
            None => return Err(anyhow!("{module_path}: cannot determine primary crate",)),
        };

        let crate_name = primary_crate
            .manifest
            .crate_name()
            .with_context(|| anyhow!("{cargo_toml}: failed to read"))?;

        let documentation = format!("{DOCS_RS}/{crate_name}");

        let url_string = module_url.to_string();

        let update_params = UpdateParams {
            license: &config.license,
            readme: README_MD,
            repository: &url_string,
            homepage: &url_string,
            documentation: &documentation,
            authors: &config.authors,
        };

        for package in workspace.packages() {
            if package.manifest.is_publish()? {
                work_cargo_toml(package, &mut validation, &update_params)?;
            }
        }

        if module.is_enabled("ci") {
            let path = module_path.join(".github").join("workflows").to_path(&root);
            let ci = Ci::new(
                &path,
                &config.job_name,
                &uses,
                &primary_crate.manifest,
                !workspace.is_single_crate(),
            );
            ci.validate(&mut validation)
                .with_context(|| anyhow!("ci validation: {}", config.job_name))?;
        }

        if module.is_enabled("readme") {
            build_readme(
                &root,
                module_path,
                &primary_crate.manifest_dir,
                &repo,
                crate_name,
                &badges,
                &mut validation,
                &mut urls,
            )?;

            for package in workspace.packages() {
                if package.manifest_dir != module_path {
                    if package.manifest.is_publish()? {
                        let crate_name = package.manifest.crate_name()?;
                        build_readme(
                            &root,
                            &package.manifest_dir,
                            &package.manifest_dir,
                            &repo,
                            crate_name,
                            &badges,
                            &mut validation,
                            &mut urls,
                        )?;
                    }
                }
            }
        }
    }

    for error in &validation {
        match error {
            Validation::MissingWorkflows { path } => {
                println!("{path}: Missing workflows directory", path = path.display());
            }
            Validation::MissingWorkflow { path, candidates } => {
                println!("{path}: Missing workflow", path = path.display());

                for candidate in candidates.iter() {
                    println! {
                        "  Candidate: {candidate}",
                        candidate = candidate.display()
                    };
                }

                if opts.fix {
                    if let [from] = candidates.as_ref() {
                        println!(
                            "{path}: Rename from {from}",
                            path = path.display(),
                            from = from.display()
                        );
                        std::fs::rename(from, path)?;
                    } else {
                        std::fs::write(path, &config.default_workflow)?;
                    }
                }
            }
            Validation::DeprecatedWorkflow { path } => {
                println!("{path}: Reprecated Workflow", path = path.display());
            }
            Validation::WrongWorkflowName {
                path,
                actual,
                expected,
            } => {
                println! {
                    "{path}: Wrong workflow name: {actual} (actual) != {expected} (expected)",
                    path = path.display()
                };
            }
            Validation::OutdatedAction {
                path,
                name,
                actual,
                expected,
            } => {
                println! {
                    "{path}: Outdated action `{name}`: {actual} (actual) != {expected} (expected)",
                    path = path.display()
                };
            }
            Validation::DeniedAction { path, name, reason } => {
                println! {
                    "{path}: Denied action `{name}`: {reason}",
                    path = path.display()
                };
            }
            Validation::CustomActionsCheck { path, name, reason } => {
                println! {
                    "{path}: Action validation failed `{name}`: {reason}",
                    path = path.display()
                };
            }
            Validation::MissingReadme { path } => {
                println!("{path}: Missing README");
            }
            Validation::MismatchedLibRs { path, new_file } => {
                if opts.fix {
                    println!("{path}: Fixing lib.rs");
                    std::fs::write(path.to_path(&root), new_file.as_bytes())?;
                } else {
                    println!("{path}: Mismatched lib.rs");
                }
            }
            Validation::BadReadme { path, new_file } => {
                if opts.fix {
                    println!("{path}: Fixing README.md");
                    std::fs::write(path.to_path(&root), new_file.as_bytes())?;
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
                println! {
                    "{path}: missing features `{feature}`", path = path.display()
                };
            }
            Validation::NoFeatures { path } => {
                println! {
                    "{path}: trying featured build (--all-features, --no-default-features), but no features present", path = path.display()
                };
            }
            Validation::MissingEmptyFeatures { path } => {
                println! {
                    "{path}: missing empty features build", path = path.display()
                };
            }
            Validation::MissingAllFeatures { path } => {
                println! {
                    "{path}: missing all features build", path = path.display()
                };
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

                if opts.fix {
                    if let Some(modified_cargo) = modified_cargo {
                        modified_cargo.save_to(path.to_path(&root))?;
                    }
                }
            }
        }
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

    if opts.url_checks {
        url_checks(&mut o, urls).await?;
    }

    Ok(())
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
    root: &Path,
    readme_path: &RelativePath,
    crate_path: &RelativePath,
    repo: &str,
    crate_name: &str,
    badges: &Badges,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    let params = ReadmeParams { repo, crate_name };
    let readme_path = readme_path.join(README_MD);
    let lib_rs = crate_path.join("src").join("lib.rs");

    let readme = Readme::new(&readme_path, &lib_rs, badges, &params);

    readme
        .validate(root, validation, urls)
        .with_context(|| anyhow!("{readme_path}: readme validation"))?;

    Ok(())
}

struct GithubBadge;

impl Badge for GithubBadge {
    fn build(&self, params: &ReadmeParams<'_>) -> Result<String> {
        let ReadmeParams { repo, .. } = params;
        let badge_repo = repo.replace('-', "--");
        Ok(format!("[<img alt=\"github\" src=\"https://img.shields.io/badge/github-{badge_repo}-8da0cb?style=for-the-badge&logo=github\" height=\"20\">]({GITHUB_COM}/{repo})"))
    }
}

struct CratesIoBadge;

impl Badge for CratesIoBadge {
    fn build(&self, params: &ReadmeParams<'_>) -> Result<String> {
        let ReadmeParams { crate_name, .. } = params;
        Ok(format!("[<img alt=\"crates.io\" src=\"https://img.shields.io/crates/v/{crate_name}.svg?style=for-the-badge&color=fc8d62&logo=rust\" height=\"20\">](https://crates.io/crates/{crate_name})"))
    }
}

struct DocsRsBadge;

impl Badge for DocsRsBadge {
    fn build(&self, params: &ReadmeParams<'_>) -> Result<String> {
        const BADGE_IMAGE: &str = "data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K";
        let ReadmeParams { crate_name, .. } = params;
        let badge_crate_name = crate_name.replace('-', "--");
        Ok(format!("[<img alt=\"docs.rs\" src=\"https://img.shields.io/badge/docs.rs-{badge_crate_name}-66c2a5?style=for-the-badge&logoColor=white&logo={BADGE_IMAGE}\" height=\"20\">]({DOCS_RS}/{crate_name})"))
    }
}

struct GithubActionsBadge;

impl Badge for GithubActionsBadge {
    fn build(&self, params: &ReadmeParams<'_>) -> Result<String> {
        let ReadmeParams { repo, .. } = params;
        Ok(format!("[<img alt=\"build status\" src=\"https://img.shields.io/github/workflow/status/{repo}/CI/main?style=for-the-badge\" height=\"20\">]({GITHUB_COM}/{repo}/actions?query=branch%3Amain)"))
    }
}

/// A git module.
#[derive(Debug, Clone)]
pub(crate) struct GitModule<'a> {
    name: &'a str,
    path: Option<&'a RelativePath>,
    url: Option<Url>,
    cargo_toml: Option<&'a RelativePath>,
    krate: Option<&'a str>,
    disabled: BTreeSet<&'a str>,
}

impl GitModule<'_> {
    /// Test if a feature is disabled.
    fn is_enabled(&self, feature: &str) -> bool {
        !self.disabled.contains(feature)
    }
}

/// Parse gitmodules from the given input.
pub(crate) fn parse_git_modules(input: &[u8]) -> Result<Vec<GitModule<'_>>> {
    let mut parser = gitmodules::Parser::new(input);

    let mut modules = Vec::new();

    while let Some(module) = parse_git_module(&mut parser)? {
        modules.push(module);
    }

    return Ok(modules);

    /// Parse a git module.
    pub(crate) fn parse_git_module<'a>(
        parser: &mut gitmodules::Parser<'a>,
    ) -> Result<Option<GitModule<'a>>> {
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

        Ok(Some(GitModule {
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
