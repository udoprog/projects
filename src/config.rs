use std::path::Path;

use anyhow::{Context, Result};
use serde::Serialize;

use crate::badges::Badge;
use crate::model::CrateParams;
use crate::templates::{Template, Templating};

#[derive(Serialize)]
pub(crate) struct RenderParams<'a> {
    #[serde(rename = "crate")]
    pub(crate) crate_params: CrateParams<'a>,
    extra: &'a toml::Value,
}

pub(crate) struct Config {
    pub(crate) default_workflow: String,
    pub(crate) job_name: String,
    pub(crate) license: String,
    pub(crate) authors: Vec<String>,
    pub(crate) extra: toml::Value,
    pub(crate) documentation: Template,
    pub(crate) badges: Vec<ConfigBadge>,
}

impl Config {
    /// Set up render parameters.
    pub(crate) fn render_params<'a>(&'a self, krate: CrateParams<'a>) -> RenderParams<'a> {
        RenderParams {
            crate_params: krate,
            extra: &self.extra,
        }
    }
}

pub(crate) struct ConfigBadge {
    template: Template,
}

impl Badge for ConfigBadge {
    #[inline]
    fn build(&self, krate: CrateParams<'_>, config: &Config) -> Result<String> {
        let data = config.render_params(krate);
        self.template.render(&data)
    }
}

/// Load a configuration from the given path.
pub(crate) fn load<P>(path: P, templating: &Templating) -> Result<Config>
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

    let mut badges = Vec::new();

    for badge in config.badges {
        let template = templating.compile(&badge.template.trim())?;
        badges.push(ConfigBadge { template });
    }

    let documentation = templating.compile(&config.documentation)?;

    Ok(Config {
        default_workflow,
        job_name: config.job_name,
        license: config.license,
        authors: config.authors,
        extra: config.extra,
        documentation,
        badges,
    })
}

mod model {
    use relative_path::RelativePathBuf;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub(super) struct Config {
        pub(super) default_workflow: RelativePathBuf,
        pub(super) job_name: String,
        pub(super) license: String,
        #[serde(default)]
        pub(super) authors: Vec<String>,
        pub(crate) extra: toml::Value,
        pub(crate) documentation: String,
        pub(super) badges: Vec<Badge>,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub(super) struct Badge {
        /// The template of the badge.
        pub(super) template: String,
    }
}
