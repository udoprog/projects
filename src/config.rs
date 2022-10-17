use std::path::Path;

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};

pub(crate) struct Config {
    pub(crate) default_workflow: String,
    pub(crate) badges: Vec<ConfigBadge>,
    pub(crate) job_name: String,
    pub(crate) license: String,
    pub(crate) authors: Vec<String>,
}

/// Differently configured badges.
#[derive(Debug, Clone, Copy, Serialize, Deserialize)]
pub(crate) enum ConfigBadge {
    #[serde(rename = "github")]
    Github,
    #[serde(rename = "crates.io")]
    CratesIo,
    #[serde(rename = "docs.rs")]
    DocsRs,
    #[serde(rename = "github-actions")]
    GithubActions,
}

/// Load a configuration from the given path.
pub(crate) fn load<P>(path: P) -> Result<Config>
where
    P: AsRef<Path>,
{
    let path = path.as_ref();
    let string = std::fs::read_to_string(path)?;
    let config: model::Config = toml::from_str(&string)?;

    let parent = path.parent().unwrap_or(path);

    let default_workflow_path = config.default_workflow.to_path(parent);

    let default_workflow = std::fs::read_to_string(&default_workflow_path)
        .with_context(|| default_workflow_path.to_string_lossy().into_owned())?;

    Ok(Config {
        default_workflow,
        badges: config.badges,
        job_name: config.job_name,
        license: config.license,
        authors: config.authors,
    })
}

mod model {
    use relative_path::RelativePathBuf;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub(super) struct Config {
        pub(super) default_workflow: RelativePathBuf,
        #[serde(default)]
        pub(super) badges: Vec<super::ConfigBadge>,
        pub(super) job_name: String,
        pub(super) license: String,
        #[serde(default)]
        pub(super) authors: Vec<String>,
    }
}
