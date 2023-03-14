mod actions;
mod config;
mod ctxt;
mod file;
mod git;
mod gitmodules;
mod manifest;
mod model;
mod templates;
mod urls;
mod validation;
mod workspace;

use std::path::PathBuf;

use anyhow::{anyhow, Context, Result};
use clap::{Parser, Subcommand};
use model::Module;

use actions::{Actions, ActionsCheck};

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

    let github_auth = match std::fs::read_to_string(root.join(".github-auth")) {
        Ok(auth) => Some(auth.trim().to_owned()),
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
            tracing::warn!("no .github-auth found, heavy rate limiting will apply");
            None
        }
        Err(e) => return Err(anyhow::Error::from(e)).with_context(|| anyhow!(".github-auth")),
    };

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
        github_auth,
    };

    match opts.action.unwrap_or_default() {
        Action::Check(opts) => {
            actions::check::entry(&cx, &opts, false).await?;
        }
        Action::Fix(opts) => {
            actions::check::entry(&cx, &opts, true).await?;
        }
        Action::For(opts) => {
            actions::foreach::entry(&cx, &opts)?;
        }
        Action::Status(opts) => {
            actions::status::entry(&cx, &opts).await?;
        }
    }

    Ok(())
}

#[derive(Subcommand)]
enum Action {
    /// Run checks for each repo.
    Check(actions::check::Opts),
    /// Fix repo.
    Fix(actions::check::Opts),
    /// Run a command for each repo.
    For(actions::foreach::Opts),
    /// Get the build status for each repo.
    Status(actions::status::Opts),
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
        Self::Check(actions::check::Opts::default())
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
