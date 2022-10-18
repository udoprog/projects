use std::path::Path;

use anyhow::{Context, Result};
use serde::Serialize;

use crate::badges::Badge;
use crate::model::CrateParams;
use crate::templates::{Template, Templating};

#[derive(Serialize)]
pub(crate) struct PerCrateRender<'a> {
    #[serde(rename = "crate")]
    pub(crate) crate_params: CrateParams<'a>,
    config: ConfigRender<'a>,
    #[serde(flatten)]
    extra: &'a toml::Value,
}

#[derive(Serialize)]
pub(crate) struct GlobalRender<'a> {
    config: ConfigRender<'a>,
    #[serde(flatten)]
    extra: &'a toml::Value,
}

#[derive(Serialize)]
struct ConfigRender<'a> {
    job_name: &'a str,
}

pub(crate) struct Config {
    pub(crate) default_workflow: Template,
    pub(crate) job_name: String,
    pub(crate) license: String,
    pub(crate) authors: Vec<String>,
    pub(crate) extra: toml::Value,
    pub(crate) documentation: Template,
    pub(crate) badges: Vec<ConfigBadge>,
}

impl Config {
    /// Global render parameters.
    pub(crate) fn global_render<'a>(&'a self) -> GlobalRender<'a> {
        GlobalRender {
            config: self.config_render(),
            extra: &self.extra,
        }
    }

    /// Set up render parameters.
    pub(crate) fn per_crate_render<'a>(&'a self, krate: CrateParams<'a>) -> PerCrateRender<'a> {
        PerCrateRender {
            crate_params: krate,
            config: self.config_render(),
            extra: &self.extra,
        }
    }

    fn config_render(&self) -> ConfigRender<'_> {
        ConfigRender {
            job_name: &&self.job_name,
        }
    }
}

pub(crate) struct ConfigBadge {
    template: Template,
}

impl Badge for ConfigBadge {
    #[inline]
    fn build(&self, krate: CrateParams<'_>, config: &Config) -> Result<String> {
        let data = config.per_crate_render(krate);
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

    let default_workflow = templating.compile(
        &std::fs::read_to_string(&default_workflow_path)
            .with_context(|| default_workflow_path.to_string_lossy().into_owned())?,
    )?;

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
