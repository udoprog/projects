use std::collections::BTreeSet;
use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{anyhow, bail, Context, Result};
use chrono::{DateTime, Local, Utc};
use clap::{Parser, Subcommand};
use config::Config;
use model::UpdateParams;
use relative_path::RelativePath;
use reqwest::{header, Client, Method};
use serde::de::IntoDeserializer;
use serde::Deserialize;
use url::Url;

use crate::actions::{Actions, ActionsCheck};
use crate::badges::Badges;
use crate::file::{File, LineColumn};
use crate::model::{work_cargo_toml, Ci, CrateParams, Readme, Validation};
use crate::urls::{UrlError, Urls};
use crate::workspace::Package;

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
    tracing_subscriber::fmt()
        .with_env_filter(tracing_subscriber::EnvFilter::from_default_env())
        .try_init()
        .map_err(|e| anyhow::anyhow!("{e}"))?;

    entry().await
}

async fn entry() -> Result<()> {
    let root = find_root()?;

    let auth = std::fs::read_to_string(root.join(".auth")).with_context(|| anyhow!(".auth"))?;
    let auth = auth.trim();

    let templating = templates::Templating::new()?;

    let config = {
        let config_path = root.join(PROJECTS_TOML);
        config::load(&config_path, &templating)
            .with_context(|| config_path.to_string_lossy().into_owned())?
    };

    let opts = Opts::try_parse()?;

    let mut badges = Badges::default();

    for badge in &config.badges {
        badges.push(badge);
    }

    for (id, repo) in &config.repos {
        for badge in &repo.badges {
            badges.push_badge(id, badge);
        }

        if let Some(header) = &repo.header {
            badges.insert_header(id, header);
        }
    }

    let mut actions = Actions::default();
    actions.latest("actions/checkout", "v3");
    actions.check("actions-rs/toolchain", &ActionsRsToolchainActionsCheck);
    actions.deny("actions-rs/cargo", "using `run` is less verbose and faster");
    actions.deny(
        "actions-rs/toolchain",
        "using `run` is less verbose and faster",
    );

    let default_workflow = match &config.default_workflow {
        Some(workflow) => workflow.render(&config.global_render())?,
        None => String::default(),
    };

    let cx = Ctxt {
        root: &root,
        config: &config,
        badges: &badges,
        actions: &actions,
        default_workflow,
    };

    match &opts.action {
        Action::Run(cmd) => {
            let mut buf = Vec::new();
            let modules = load_gitmodules(&root, &mut buf)?;

            let mut validation = Vec::new();
            let mut urls = Urls::default();

            for module in &modules {
                run_module(&cx, cmd, module, &mut validation, &mut urls)?;
            }

            for validation in &validation {
                validate(&cx, cmd, validation)?;
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

            if cmd.url_checks {
                url_checks(&mut o, urls).await?;
            }
        }
        Action::For(cmd) => {
            let mut buf = Vec::new();
            let modules = load_gitmodules(&root, &mut buf)?;

            let command_repr = cmd.command.join(" ");

            let Some((command, args)) = cmd.command.split_first() else {
                return Err(anyhow!("missing command"));
            };

            for module in &modules {
                if should_skip(&cmd.modules, module) {
                    continue;
                }

                let Some(path) = module.path else {
                    continue;
                };

                let current_dir = path.to_path(&root);

                if cmd.cached {
                    let status = Command::new("git")
                        .args(["diff", "--cached", "--exit-code"])
                        .stdout(Stdio::null())
                        .current_dir(&current_dir)
                        .status()?;

                    if status.success() {
                        continue;
                    }
                }

                println!("{path}: {command_repr}");
                let status = Command::new(command)
                    .args(args)
                    .current_dir(&current_dir)
                    .status()?;
                println!("{status}");
            }
        }
        Action::Status(cmd) => {
            #[derive(Debug, Deserialize)]
            struct Workflow {
                status: String,
                #[serde(default)]
                conclusion: Option<String>,
                head_branch: String,
                head_sha: String,
                updated_at: DateTime<Utc>,
            }

            #[derive(Debug, Deserialize)]
            struct WorkflowRuns {
                workflow_runs: Vec<Workflow>,
            }

            let mut buf = Vec::new();
            let modules = load_gitmodules(&root, &mut buf)?;

            let client = Client::builder().build()?;

            let limit = cmd.limit.unwrap_or(1).max(1).to_string();

            for module in &modules {
                if should_skip(&cmd.modules, module) {
                    continue;
                }

                let (Some(path), Some((owner, repo))) = (module.path, module.repo().and_then(|repo| repo.split_once('/'))) else {
                    continue;
                };

                let current_dir = path.to_path(&root);
                let sha = head_commit(&current_dir).with_context(|| anyhow!("{}", module.name))?;
                let sha = sha.trim();

                let url = format!(
                    "https://api.github.com/repos/{owner}/{repo}/actions/workflows/ci.yml/runs"
                );

                let authorisation = format!("Bearer {auth}");

                let req = client
                    .request(Method::GET, url)
                    .header(header::USER_AGENT, "udoprog projects")
                    .header(header::AUTHORIZATION, &authorisation)
                    .query(&[("exclude_pull_requests", "true"), ("per_page", &limit)]);

                if let Some(url) = &module.url {
                    println!("{}: {url}", module.name);
                } else {
                    println!("{}: *no url*", module.name);
                }

                let res = req.send().await?;

                tracing::trace!("  {:?}", res.headers());

                if !res.status().is_success() {
                    println!("  {}", res.text().await?);
                    continue;
                }

                let runs: serde_json::Value = res.json().await?;

                if cmd.raw_json {
                    let mut out = std::io::stdout();
                    serde_json::to_writer_pretty(&mut out, &runs)?;
                    writeln!(out)?;
                }

                let runs: WorkflowRuns = WorkflowRuns::deserialize(runs.into_deserializer())?;

                for run in runs.workflow_runs {
                    let updated_at = run.updated_at.with_timezone(&Local);

                    let head = if sha == run.head_sha { "* " } else { "  " };

                    println!(
                        " {head}{sha} {branch}: {updated_at}: status: {}, conclusion: {}",
                        run.status,
                        run.conclusion.as_deref().unwrap_or("*in progress*"),
                        branch = run.head_branch,
                        sha = short(&run.head_sha),
                    );
                }
            }
        }
    }

    Ok(())
}

fn head_commit(current_dir: &Path) -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .stdout(Stdio::piped())
        .current_dir(&current_dir)
        .output()?;

    if !output.status.success() {
        return Err(anyhow!("status: {}", output.status));
    }

    Ok(String::from_utf8(output.stdout)?)
}

fn short(string: &str) -> impl std::fmt::Display + '_ {
    if let Some(sha) = string.get(..7) {
        return sha;
    }

    string
}

fn load_gitmodules<'a>(root: &Path, buf: &'a mut Vec<u8>) -> Result<Vec<Module<'a>>> {
    let gitmodules_path = root.join(".gitmodules");
    let mut modules = Vec::new();

    if gitmodules_path.is_file() {
        *buf = std::fs::read(root.join(".gitmodules"))?;

        modules.extend(
            parse_git_modules(buf).with_context(|| anyhow!("{}", gitmodules_path.display()))?,
        );
    } else {
        modules.push(module_from_git(&root)?);
    }

    Ok(modules)
}

#[derive(Default, Parser)]
struct Run {
    #[arg(long)]
    fix: bool,
    #[arg(long)]
    url_checks: bool,
    filters: Vec<String>,
}

#[derive(Default, Parser)]
struct For {
    /// Only run for git repos which have cached changes.
    #[arg(long)]
    cached: bool,
    /// Filter modules.
    #[arg(long, short)]
    modules: Vec<String>,
    /// Command to run.
    command: Vec<String>,
}

#[derive(Default, Parser)]
struct Status {
    /// Filter modules.
    #[arg(long, short)]
    modules: Vec<String>,
    /// Output raw JSON response.
    #[arg(long)]
    raw_json: bool,
    /// Limit number of workspace runs to inspect.
    #[arg(long)]
    limit: Option<u32>,
}

#[derive(Subcommand)]
enum Action {
    /// Run checks for each repo.
    Run(Run),
    /// Run a command for each repo.
    For(For),
    /// Get the build status for each repo.
    Status(Status),
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
    if should_skip(&run.filters, module) {
        return Ok(());
    }

    let (Some(module_path), Some(module_url), Some(repo)) = (module.path, &module.url, module.repo()) else {
        println!(
            ".gitmodules: {name}: missing `path` and/or `url`",
            name = module.name
        );
        return Ok(());
    };

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
        description: primary_crate.manifest.description()?,
    });

    let documentation = match &cx.config.documentation {
        Some(documentation) => Some(documentation.render(&params)?),
        None => None,
    };

    let url_string = module_url.to_string();

    let update_params = UpdateParams {
        license: cx.config.license(),
        readme: README_MD,
        repository: &url_string,
        homepage: &url_string,
        documentation: documentation.as_deref().unwrap_or_default(),
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
            cx.config.job_name(),
            cx.actions,
            &primary_crate.manifest,
            !workspace.is_single_crate(),
        );
        ci.validate(cx.root, validation)
            .with_context(|| anyhow!("ci validation: {}", cx.config.job_name()))?;
    }

    if module.is_enabled("readme") {
        build_readme(
            &cx,
            module.name,
            module_path,
            primary_crate,
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
                        description: package.manifest.description()?,
                    };

                    build_readme(
                        cx,
                        module.name,
                        &package.manifest_dir,
                        package,
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

/// Test if module should be skipped.
fn should_skip(filters: &[String], module: &Module) -> bool {
    !filters.is_empty() && !filters.iter().all(|filter| module.name.contains(filter))
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
                    let path = path.to_path(cx.root);

                    if let Some(parent) = path.parent() {
                        if !parent.is_dir() {
                            std::fs::create_dir_all(&parent)?;
                        }
                    }

                    std::fs::write(path, &cx.default_workflow)?;
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
        Validation::ActionMissingKey {
            path,
            key,
            expected,
            actual,
        } => {
            println!("{path}: {key}: action missing key, expected {expected}");

            match actual {
                Some(value) => {
                    println!("  actual:");
                    serde_yaml::to_writer(std::io::stdout(), value)?;
                }
                None => {
                    println!("  actual: *missing value*");
                }
            }
        }
        Validation::ActionOnMissingBranch { path, key, branch } => {
            println!("{path}: {key}: action missing branch `{branch}`");
        }
        Validation::ActionExpectedEmptyMapping { path, key } => {
            println!("{path}: {key}: action expected empty mapping");
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
    name: &str,
    readme_path: &RelativePath,
    package: &Package,
    params: CrateParams<'_>,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    let readme_path = readme_path.join(README_MD);

    let entry = 'entry: {
        for entry in package.entries() {
            if entry.to_path(&cx.root).is_file() {
                break 'entry entry;
            }
        }

        bail!("{name}: missing existing entrypoint")
    };

    let readme = Readme::new(name, &readme_path, &entry, cx.badges, params, cx.config);

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

    /// Repo name.
    fn repo(&self) -> Option<&str> {
        let url = self.url.as_ref()?;

        let Some("github.com") = url.domain() else {
            return None;
        };

        Some(url.path().trim_matches('/'))
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
