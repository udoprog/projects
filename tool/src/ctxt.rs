use core::fmt;
use std::path::Path;
use std::process::{Command, Stdio};

use serde::{Serialize, Serializer};

use crate::actions::Actions;
use crate::config::Config;
use crate::model::Module;

/// First version to support 2018 edition.
const EDITION_2018: RustVersion = RustVersion::new(1, 31);
/// First version to support 2021 edition.
const EDITION_2021: RustVersion = RustVersion::new(1, 56);
/// Oldest version to support workspaces.
const WORKSPACE: RustVersion = RustVersion::new(1, 12);

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub(crate) struct RustVersion {
    pub(crate) major: u64,
    pub(crate) minor: u64,
}

impl fmt::Display for RustVersion {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}.{}", self.major, self.minor)
    }
}

impl Serialize for RustVersion {
    #[inline]
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.collect_str(self)
    }
}

impl RustVersion {
    pub(crate) const fn new(major: u64, minor: u64) -> Self {
        Self { major, minor }
    }

    pub(crate) fn parse(string: &str) -> Option<Self> {
        let mut it = string.split('.');
        let major = it.next()?.parse().ok()?;
        let minor = it.next()?.parse().ok()?;
        Some(RustVersion { major, minor })
    }
}

#[derive(Debug, Clone, Copy, Serialize)]
pub(crate) struct RustVersions {
    pub(crate) rustc: Option<RustVersion>,
    pub(crate) rust_version: Option<RustVersion>,
    pub(crate) edition_2021: RustVersion,
    pub(crate) edition_2018: RustVersion,
    pub(crate) workspace: RustVersion,
}

impl RustVersions {
    pub(crate) fn new(rustc: Option<RustVersion>, rust_version: Option<RustVersion>) -> Self {
        Self {
            rustc,
            rust_version,
            edition_2021: EDITION_2021,
            edition_2018: EDITION_2018,
            workspace: WORKSPACE,
        }
    }
}

pub(crate) struct Ctxt<'a> {
    pub(crate) root: &'a Path,
    pub(crate) config: &'a Config,
    pub(crate) actions: &'a Actions<'a>,
    pub(crate) modules: Vec<Module<'a>>,
    pub(crate) github_auth: Option<String>,
    pub(crate) rustc_version: Option<RustVersion>,
}

/// Minor version from rustc.
pub(crate) fn rustc_version() -> Option<RustVersion> {
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
    RustVersion::parse(version)
}
