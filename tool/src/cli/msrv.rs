use std::collections::HashMap;
use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;

use crate::ctxt::Ctxt;
use crate::workspace::{self, Workspace};

/// Oldest verison where rust-version was introduced.
const RUST_VERSION_SUPPORTED: u64 = 56;
/// Oldest version to test by default.
const OLDEST: u64 = RUST_VERSION_SUPPORTED;
/// Latest version to test by default.
const LATEST: u64 = 68;

#[derive(Default, Parser)]
pub(crate) struct Opts {
    /// Filter modules.
    #[arg(long, short)]
    modules: Vec<String>,
    /// Verbose output.
    #[arg(long)]
    verbose: bool,
    /// Save new MSRV.
    #[arg(long)]
    save: bool,
    /// Oldest minor version to test. Default: 1.56.
    #[arg(long)]
    oldest: Option<u64>,
    /// Latest minor version to test. Default detected version from `rustc
    /// --version`.
    #[arg(long)]
    latest: Option<u64>,
    /// Command to test with.
    ///
    /// This is run through `rustup run <version> <command>`, the default
    /// command is `cargo check`.
    command: Vec<String>,
}

pub(crate) fn entry(cx: &Ctxt<'_>, opts: &Opts) -> Result<()> {
    for module in &cx.modules {
        if crate::should_skip(&opts.modules, module) {
            continue;
        }

        let mut workspace = workspace::open(cx, module)?;
        let span = tracing::info_span!("build", path = ?workspace.path());
        let _enter = span.enter();
        build(cx, &mut workspace, opts).with_context(|| workspace.path().to_owned())?;
    }

    Ok(())
}

/// Minor version from rustc.
fn rustc_version() -> Option<u64> {
    let output = Command::new("rustc")
        .arg("--version")
        .stdin(Stdio::null())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .output()
        .ok()?;

    let output = String::from_utf8(output.stdout).ok()?;
    tracing::info!("rustc: {output}");
    let version = output.split(' ').nth(1)?;
    let version = version.split('.').nth(1)?;
    let version = version.parse().ok()?;
    Some(version)
}

fn build(cx: &Ctxt, workspace: &mut Workspace, opts: &Opts) -> Result<()> {
    let primary = workspace
        .primary_crate()?
        .context("missing primary crate")?;

    let current_dir = workspace.path().to_path(cx.root);

    let rustc_minor = rustc_version();

    let end = match primary.manifest.rust_version()? {
        Some(rust_version) => {
            let mut it = rust_version.split('.');
            let major: u64 = it.next().context("missing major")?.parse()?;
            let minor: u64 = it.next().context("missing minor")?.parse()?;

            if major != 1 {
                bail!("only major version 1 supported");
            }

            rustc_minor.into_iter().chain([minor]).min()
        }
        None => rustc_minor.into_iter().chain([LATEST]).min(),
    };

    let start = opts.oldest.unwrap_or(OLDEST);

    let end = [opts.latest, end]
        .into_iter()
        .flatten()
        .max()
        .unwrap_or(LATEST)
        .max(start);

    tracing::info!("Testing Rust 1.{start}-1.{end}");
    let mut candidates = Candidates::new(start, end);

    let mut versions = HashMap::new();

    while let Some(current) = candidates.current() {
        if versions.contains_key(&current) {
            break;
        }

        let version = format!("1.{current}");

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

        let mut restore = Vec::new();
        let mut packages = workspace.packages().cloned().collect::<Vec<_>>();

        if current < RUST_VERSION_SUPPORTED {
            for p in &mut packages {
                let original = p.manifest_path.with_extension("toml.original");
                let original_path = original.to_path(cx.root);
                let manifest_path = p.manifest_path.to_path(cx.root);

                if p.manifest.remove_rust_version() {
                    move_paths(&manifest_path, &original_path)?;
                    tracing::info!("Saving {}", p.manifest_path);
                    p.manifest.save_to(&manifest_path)?;
                    restore.push((original_path, manifest_path));
                }
            }
        }

        tracing::trace!("Testing against rust {version}");

        let mut command = Command::new("rustup");
        command.args(["run", &version]);

        if !opts.command.is_empty() {
            command.args(&opts.command[..]);
        } else {
            command.args(["cargo", "check"]);
        }

        command.current_dir(&current_dir);

        if !opts.verbose {
            command.stdout(Stdio::null()).stderr(Stdio::null());
        }

        let status = command.status()?;

        if status.success() {
            tracing::info!("Rust {version}: check ok");
            candidates.ok();
            versions.insert(current, true);
        } else {
            tracing::info!("Rust {version}: check failed");
            candidates.fail();
            versions.insert(current, false);
        }

        for (from, to) in restore {
            move_paths(&from, &to)?;
        }
    }

    if let Some(minor) = candidates
        .current()
        .filter(|c| versions.get(c).copied().unwrap_or_default())
    {
        let version = format!("1.{minor}");
        tracing::info!("Supported msrv: Rust {version}");

        if opts.save {
            if minor >= RUST_VERSION_SUPPORTED {
                for p in workspace.packages_mut() {
                    if p.manifest.is_publish()? {
                        tracing::info!(
                            "Saving {} with rust-version = \"{version}\"",
                            p.manifest_path
                        );
                        p.manifest.set_rust_version(&version)?;
                        p.manifest.sort_package_keys()?;
                        p.manifest.save_to(p.manifest_path.to_path(cx.root))?;
                    }
                }
            } else {
                for p in workspace.packages_mut() {
                    if p.manifest.remove_rust_version() {
                        tracing::info!(
                            "Saving {} without rust-version (target version outdates rust-version)",
                            p.manifest_path
                        );
                        p.manifest.save_to(p.manifest_path.to_path(cx.root))?;
                    }
                }
            }
        }
    }

    Ok(())
}

fn move_paths(from: &Path, to: &Path) -> Result<()> {
    tracing::trace!("moving {} -> {}", from.display(), to.display());

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
        self.current
    }

    fn ok(&mut self) {
        if let Some(current) = self.current.take() {
            self.end = current;
            self.current = Some(midpoint(self.start, self.end));
        }
    }

    fn fail(&mut self) {
        if let Some(current) = self.current.take() {
            self.start = (current + 1).min(self.end);
            self.current = Some(midpoint(self.start, self.end));
        }
    }
}

fn midpoint(start: u64, end: u64) -> u64 {
    (start + (end - start) / 2).clamp(start, end)
}
