use anyhow::{bail, anyhow, Context, Result};
use clap::Parser;

use crate::ctxt::Ctxt;
use crate::workspace::{self, Workspace};

/// Latest supported minor version.
const LATEST: u64 = 69;

#[derive(Default, Parser)]
pub(crate) struct Opts {
    /// Filter modules.
    #[arg(long, short)]
    modules: Vec<String>,
}

pub(crate) fn entry(cx: &Ctxt<'_>, opts: &Opts) -> Result<()> {
    for module in &cx.modules {
        if crate::should_skip(&opts.modules, module) {
            continue;
        }

        let workspace = workspace::open(cx, module)?;
        build(cx, &workspace).with_context(|| workspace.path().to_owned())?;
    }

    Ok(())
}

fn build(cx: &Ctxt, workspace: &Workspace) -> Result<()> {
    let primary = workspace
        .primary_crate()?
        .context("missing primary crate")?;

    let start = match primary.manifest.rust_version()? {
        Some(rust_version) => {
            let mut it = rust_version.split('.');
            let major: u64 = it.next().context("missing major")?.parse()?;
            let minor: u64 = it.next().context("missing minor")?.parse()?;

            if major != 1 {
                bail!("only major version 1 supported");
            }

            minor
        }
        None => LATEST,
    };

    let end = 0u64;

    dbg!(start, end);
    Ok(())
}
