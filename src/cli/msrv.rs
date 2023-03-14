use anyhow::Result;
use clap::Parser;

use crate::ctxt::Ctxt;

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

        let Some(path) = module.path else {
            continue;
        };

        let _ = path.to_path(&cx.root);
    }

    Ok(())
}
