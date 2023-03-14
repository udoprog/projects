use std::io::Write;

use anyhow::{anyhow, Context, Result};
use chrono::{DateTime, Local, Utc};
use clap::Parser;
use reqwest::{header, Client, Method};
use serde::{de::IntoDeserializer, Deserialize};

use crate::{ctxt::Ctxt, git};

#[derive(Default, Parser)]
pub(crate) struct Opts {
    /// Filter by the specified modules.
    #[arg(long = "module", short = 'm', name = "module")]
    modules: Vec<String>,
    /// Output raw JSON response.
    #[arg(long)]
    raw_json: bool,
    /// Limit number of workspace runs to inspect.
    #[arg(long)]
    limit: Option<u32>,
}

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

pub(crate) async fn entry(cx: &Ctxt<'_>, opts: &Opts) -> Result<()> {
    let client = Client::builder().build()?;

    let limit = opts.limit.unwrap_or(1).max(1).to_string();

    for module in &cx.modules {
        if crate::should_skip(&opts.modules, module) {
            continue;
        }

        let (Some(path), Some((owner, repo))) = (module.path, module.repo().and_then(|repo| repo.split_once('/'))) else {
            continue;
        };

        let current_dir = path.to_path(cx.root);
        let sha =
            git::rev_parse(&current_dir, "HEAD").with_context(|| anyhow!("{}", module.name))?;
        let sha = sha.trim();

        let url =
            format!("https://api.github.com/repos/{owner}/{repo}/actions/workflows/ci.yml/runs");

        let req = client
            .request(Method::GET, url)
            .header(header::USER_AGENT, "udoprog projects")
            .query(&[("exclude_pull_requests", "true"), ("per_page", &limit)]);

        let req = match &cx.github_auth {
            Some(auth) => req.header(header::AUTHORIZATION, &format!("Bearer {auth}")),
            None => req,
        };

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

        if opts.raw_json {
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

    Ok(())
}

fn short(string: &str) -> impl std::fmt::Display + '_ {
    if let Some(sha) = string.get(..7) {
        return sha;
    }

    string
}
