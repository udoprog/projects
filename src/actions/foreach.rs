use std::process::{Command, Stdio};

use anyhow::{anyhow, Result};
use clap::Parser;

use crate::ctxt::Ctxt;

#[derive(Default, Parser)]
pub(crate) struct Opts {
    /// Only run for git repos which have cached changes.
    #[arg(long)]
    cached: bool,
    /// Filter modules.
    #[arg(long, short)]
    modules: Vec<String>,
    /// Command to run.
    command: Vec<String>,
}

pub(crate) fn entry(cx: &Ctxt<'_>, opts: &Opts) -> Result<()> {
    let command_repr = opts.command.join(" ");

    let Some((command, args)) = opts.command.split_first() else {
        return Err(anyhow!("missing command"));
    };

    for module in &cx.modules {
        if crate::should_skip(&opts.modules, module) {
            continue;
        }

        let Some(path) = module.path else {
            continue;
        };

        let current_dir = path.to_path(&cx.root);

        if opts.cached {
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

    Ok(())
}
