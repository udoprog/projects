use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{anyhow, bail, Context, Result};
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

        let mut workspace = workspace::open(cx, module)?;
        build(cx, &mut workspace).with_context(|| workspace.path().to_owned())?;
    }

    Ok(())
}

fn build(cx: &Ctxt, workspace: &mut Workspace) -> Result<()> {
    let primary = workspace
        .primary_crate()?
        .context("missing primary crate")?;

    let current_dir = workspace.path().to_path(cx.root);

    let end = match primary.manifest.rust_version()? {
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

    let mut restore = Vec::new();

    for p in workspace.packages_mut() {
        let original = p.manifest_path.with_extension("toml.original");
        let original_path = original.to_path(cx.root);
        let manifest_path = p.manifest_path.to_path(cx.root);

        if original_path.is_file() {
            move_paths(&original_path, &manifest_path)?;
        }

        if p.manifest.remove_rust_version() {
            move_paths(&manifest_path, &original_path)?;
            tracing::info!("saving {}", manifest_path.display());
            p.manifest.save_to(&manifest_path)?;
            restore.push((original_path, manifest_path));
        }
    }

    let start = 0u64;

    let mut candidates = Candidates::new(start, end);

    while let Some(next) = candidates.current() {
        let version = format!("1.{next}");

        let output = Command::new("rustup")
            .args(["run", &version, "rustc", "--version"])
            .stdout(Stdio::null())
            .output()?;

        if !output.status.success() {
            tracing::info!("installing rust {version}");

            let status = Command::new("rustup")
                .args(["toolchain", "install", "--profile", "minimal", &version])
                .status()?;

            if !status.success() {
                bail!("failed to install {version}");
            }
        }

        tracing::info!("testing against rust {version}");

        let status = Command::new("rustup")
            .args(["run", &version, "cargo", "check"])
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .current_dir(&current_dir)
            .status()?;

        if status.success() {
            candidates.ok();
        } else {
            candidates.fail();
        }
    }

    Ok(())
}

fn move_paths(from: &Path, to: &Path) -> Result<()> {
    tracing::info!("moving {} -> {}", from.display(), to.display());

    if to.exists() {
        let _ = std::fs::remove_file(to).with_context(|| anyhow!("{}", to.display()));
    }

    std::fs::rename(from, to).with_context(|| anyhow!("{} -> {}", from.display(), to.display()))?;
    Ok(())
}

struct Candidates {
    start: u64,
    current: Option<u64>,
    end: u64,
}

impl Candidates {
    fn new(start: u64, end: u64) -> Self {
        Self {
            start,
            current: Some(midpoint(start, end)),
            end,
        }
    }

    fn current(&self) -> Option<u64> {
        let current = self.current?;

        if self.end == current && self.start == current {
            return None;
        }

        Some(current)
    }

    fn ok(&mut self) {
        if let Some(current) = self.current.take() {
            self.end = current;
            self.current = Some(midpoint(self.start, self.end));
        }
    }

    fn fail(&mut self) {
        if let Some(current) = self.current.take() {
            self.start = current;
            self.current = Some(midpoint(self.start, self.end));
        }
    }
}

fn midpoint(start: u64, end: u64) -> u64 {
    (start + (end - start) / 2).clamp(start, end)
}
