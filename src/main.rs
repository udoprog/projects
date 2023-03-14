mod actions;
mod config;
mod ctxt;
mod file;
mod gitmodules;
mod manifest;
mod model;
mod templates;
mod urls;
mod validation;
mod workspace;

use std::io::Write;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};

use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, Local, Utc};
use clap::{Parser, Subcommand};
use model::Module;
use reqwest::{header, Client, Method};
use serde::de::IntoDeserializer;
use serde::Deserialize;

use crate::actions::{Actions, ActionsCheck};

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

    let mut buf = Vec::new();
    let modules = model::load_gitmodules(&root, &mut buf)?;

    let cx = ctxt::Ctxt {
        root: &root,
        config: &config,
        actions: &actions,
        modules,
        default_workflow,
    };

    match opts.action.unwrap_or_default() {
        Action::Check(opts) => {
            crate::actions::check::entry(&cx, &opts, false).await?;
        }
        Action::Fix(opts) => {
            crate::actions::check::entry(&cx, &opts, true).await?;
        }
        Action::For(cmd) => {
            let command_repr = cmd.command.join(" ");

            let Some((command, args)) = cmd.command.split_first() else {
                return Err(anyhow!("missing command"));
            };

            for module in &cx.modules {
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

            let client = Client::builder().build()?;

            let limit = cmd.limit.unwrap_or(1).max(1).to_string();

            for module in &cx.modules {
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
    Check(crate::actions::check::Opts),
    /// Fix repo.
    Fix(crate::actions::check::Opts),
    /// Run a command for each repo.
    For(For),
    /// Get the build status for each repo.
    Status(Status),
}

#[derive(Default, Parser)]
#[command(author, version, about, long_about = None)]
#[command(propagate_version = true)]
struct Opts {
    #[command(subcommand)]
    action: Option<Action>,
}

impl Default for Action {
    fn default() -> Self {
        Self::Check(crate::actions::check::Opts::default())
    }
}

/// Test if module should be skipped.
fn should_skip(filters: &[String], module: &Module<'_>) -> bool {
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
