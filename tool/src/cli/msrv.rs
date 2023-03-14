use core::fmt;
use std::collections::HashMap;
use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;

use crate::ctxt::Ctxt;
use crate::workspace::{self, Workspace};

/// First version to support 2018 edition.
const EDITION_2018: u64 = 31;
/// First version to support 2021 edition.
const EDITION_2021: u64 = 56;
/// Oldest version where rust-version was introduced.
const RUST_VERSION_SUPPORTED: u64 = 56;
/// Oldest version to support workspaces.
const WORKSPACE: u64 = 12;
/// Oldest version to test by default.
const EARLIEST: u64 = RUST_VERSION_SUPPORTED;
/// Final fallback version to use if *nothing* else can be figured out.
const LATEST: u64 = 100;
/// Default command to build.
const DEFAULT_COMMAND: [&str; 3] = ["cargo", "build", "--all-targets"];

#[derive(Default, Parser)]
pub(crate) struct Opts {
    /// Filter by the specified modules.
    #[arg(long = "module", short = 'm', name = "module")]
    modules: Vec<String>,
    /// Verbose output.
    #[arg(long)]
    verbose: bool,
    /// Keep the existing Cargo.lock file. By default this is moved out of the
    /// way, to test that version selection selects a version of all
    /// dependencies which can compile.
    #[arg(long)]
    keep_cargo_lock: bool,
    /// Save new MSRV.
    #[arg(long)]
    save: bool,
    /// Earliest minor version to test. Default: 2021.
    ///
    /// Supports the following special values, apart from minor version numbers:
    /// * 2018 - The first Rust version to support 2018 edition.
    /// * 2021 - The first Rust version to support 2021 edition.
    /// * rust-version - The rust-version specified in the Cargo.toml of the
    ///   project. Note that the first version to support rust-version is 2021.
    /// * workspace - The first Rust version to support workspaces.
    /// * rustc - The version reported by your local rustc.
    #[arg(long, verbatim_doc_comment)]
    earliest: Option<String>,
    /// Latest minor version to test. Default is `rustc`.
    ///
    /// Supports the following special values, apart from minor version numbers:
    /// * 2018 - The first Rust version to support 2018 edition.
    /// * 2021 - The first Rust version to support 2021 edition.
    /// * rust-version - The rust-version specified in the Cargo.toml of the
    ///   project. Note that the first version to support rust-version is 2021.
    /// * workspace - The first Rust version to support workspaces.
    /// * rustc - The version reported by your local rustc.
    #[arg(long, verbatim_doc_comment)]
    latest: Option<String>,
    /// Command to test with.
    ///
    /// This is run through `rustup run <version> <command>`, the default
    /// command is `cargo build --all-targets`.
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

    let manifest_minor = match primary.manifest.rust_version()? {
        Some(rust_version) => {
            let mut it = rust_version.split('.');
            let major: u64 = it.next().context("missing major")?.parse()?;
            let minor: u64 = it.next().context("missing minor")?.parse()?;

            if major != 1 {
                bail!("only major version 1 supported");
            }

            rustc_minor.into_iter().chain([minor]).min()
        }
        None => None,
    };

    let opts_earliest = parse_minor_version(opts.earliest.as_deref(), rustc_minor, manifest_minor)?;
    let opts_latest = parse_minor_version(opts.latest.as_deref(), rustc_minor, manifest_minor)?;

    let earliest = opts_earliest.unwrap_or(EARLIEST);
    let latest = opts_latest
        .or(rustc_minor)
        .or(manifest_minor)
        .unwrap_or(LATEST)
        .max(earliest);

    let cargo_lock = primary.manifest_dir.join("Cargo.lock").to_path(cx.root);
    let cargo_lock_original = cargo_lock.with_extension("lock.original");

    tracing::info!("Testing Rust 1.{earliest}-1.{latest}");
    let mut candidates = Bisect::new(earliest, latest);

    while let Some(current) = candidates.current() {
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

        for p in &mut packages {
            let original = p.manifest_path.with_extension("toml.original");
            let original_path = original.to_path(cx.root);
            let manifest_path = p.manifest_path.to_path(cx.root);

            let save = if current < RUST_VERSION_SUPPORTED {
                p.manifest.set_rust_version(&version)?;
                true
            } else {
                p.manifest.remove_rust_version()
            };

            if save {
                move_paths(&manifest_path, &original_path)?;
                tracing::info!("Saving {}", p.manifest_path);
                p.manifest.save_to(&manifest_path)?;
                restore.push((original_path, manifest_path));
            }
        }

        if !opts.keep_cargo_lock && cargo_lock.is_file() {
            move_paths(&cargo_lock, &cargo_lock_original)?;
            restore.push((cargo_lock_original.clone(), cargo_lock.clone()));
        }

        tracing::trace!("Testing against rust {version}");

        let mut command = Command::new("rustup");
        command.args(["run", &version]);

        if !opts.command.is_empty() {
            tracing::info!("Testing Rust {version}: {}", CommandRepr(&opts.command[..]));
            command.args(&opts.command[..]);
        } else {
            tracing::info!(
                "Testing Rust {version}: {}",
                CommandRepr(&DEFAULT_COMMAND[..])
            );
            command.args(DEFAULT_COMMAND);
        }

        command.current_dir(&current_dir);

        if !opts.verbose {
            command.stdout(Stdio::null()).stderr(Stdio::null());
        }

        let status = command.status()?;

        if status.success() {
            tracing::info!("Rust {version}: check ok");
            candidates.ok();
        } else {
            tracing::info!("Rust {version}: check failed");
            candidates.fail();
        }

        for (from, to) in restore {
            move_paths(&from, &to)?;
        }
    }

    if let Some(minor) = candidates.get() {
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

fn parse_minor_version(
    string: Option<&str>,
    rustc_minor: Option<u64>,
    manifest_minor: Option<u64>,
) -> Result<Option<u64>> {
    Ok(match string {
        Some("rustc") => rustc_minor,
        Some("2018") => Some(EDITION_2018),
        Some("2021") => Some(EDITION_2021),
        Some("workspace") => Some(WORKSPACE),
        Some("rust-version") => manifest_minor,
        Some(n) => Some(n.parse()?),
        None => None,
    })
}

fn move_paths(from: &Path, to: &Path) -> Result<()> {
    tracing::trace!("moving {} -> {}", from.display(), to.display());

    if to.exists() {
        let _ = std::fs::remove_file(to).with_context(|| anyhow!("{}", to.display()));
    }

    std::fs::rename(from, to).with_context(|| anyhow!("{} -> {}", from.display(), to.display()))?;
    Ok(())
}

struct Bisect {
    versions: HashMap<u64, bool>,
    earliest: u64,
    current: u64,
    latest: u64,
}

impl Bisect {
    fn new(earliest: u64, latest: u64) -> Self {
        Self {
            versions: HashMap::new(),
            earliest,
            current: midpoint(earliest, latest),
            latest,
        }
    }

    /// Get the next version that needs to be tested.
    fn current(&self) -> Option<u64> {
        if self.versions.contains_key(&self.current) {
            return None;
        }

        Some(self.current)
    }

    /// Return a successfully tested version.
    fn get(&self) -> Option<u64> {
        if *self.versions.get(&self.current)? {
            return Some(self.current);
        }

        None
    }

    fn ok(&mut self) {
        self.versions.insert(self.current, true);
        self.latest = self.current;
        self.current = midpoint(self.earliest, self.latest);
    }

    fn fail(&mut self) {
        self.versions.insert(self.current, false);
        self.earliest = (self.current + 1).min(self.latest);
        self.current = midpoint(self.earliest, self.latest);
    }
}

fn midpoint(start: u64, end: u64) -> u64 {
    (start + (end - start) / 2).clamp(start, end)
}

struct CommandRepr<'a, S>(&'a [S]);

impl<S> fmt::Display for CommandRepr<'_, S>
where
    S: AsRef<str>,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut it = self.0.iter();
        let last = it.next_back();

        for part in it {
            write!(f, "{} ", part.as_ref())?;
        }

        if let Some(part) = last {
            write!(f, "{}", part.as_ref())?;
        }

        Ok(())
    }
}
