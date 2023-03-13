use core::fmt;
use std::collections::HashMap;
use std::path::Path;

use anyhow::{anyhow, Context, Result};
use relative_path::RelativePathBuf;
use serde::Serialize;

use crate::badges::Badge;
use crate::model::CrateParams;
use crate::templates::{Template, Templating};

/// Default job name.
const DEFAULT_JOB_NAME: &str = "CI";
/// Default license to use in configuration.
const DEFAULT_LICENSE: &str = "MIT/Apache-2.0";

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

pub(crate) struct Repo {
    /// Custom header template.
    pub(crate) header: Option<Template>,
    /// Custom badges for a specific project.
    pub(crate) badges: Vec<ConfigBadge>,
}

pub(crate) struct Config {
    pub(crate) default_workflow: Option<Template>,
    pub(crate) job_name: Option<String>,
    pub(crate) license: Option<String>,
    pub(crate) authors: Vec<String>,
    pub(crate) extra: toml::Value,
    pub(crate) documentation: Option<Template>,
    pub(crate) badges: Vec<ConfigBadge>,
    pub(crate) repos: HashMap<String, Repo>,
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
            job_name: self.job_name(),
        }
    }

    pub(crate) fn job_name(&self) -> &str {
        self.job_name.as_deref().unwrap_or(DEFAULT_JOB_NAME)
    }

    pub(crate) fn license(&self) -> &str {
        self.license.as_deref().unwrap_or(DEFAULT_LICENSE)
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
    enum Part {
        Key(String),
        Index(usize),
    }

    impl fmt::Display for Part {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                Part::Key(key) => {
                    write!(f, "{key}")
                }
                Part::Index(index) => {
                    write!(f, "[{index}]")
                }
            }
        }
    }

    struct Ctxt<'a> {
        path: Vec<Part>,
        templating: &'a Templating,
    }

    impl<'a> Ctxt<'a> {
        fn new(templating: &'a Templating) -> Self {
            Self {
                path: Vec::new(),
                templating,
            }
        }

        fn key(&mut self, key: &str) {
            self.path.push(Part::Key(key.to_owned()));
        }

        fn format_path(&self) -> String {
            use std::fmt::Write;

            if self.path.is_empty() {
                return format!(".");
            }

            let mut out = String::new();
            let mut it = self.path.iter();

            if let Some(p) = it.next() {
                write!(out, "{p}").unwrap();
            }

            for p in it {
                if let Part::Key(..) = p {
                    out.push('.');
                }

                write!(out, "{p}").unwrap();
            }

            out
        }

        fn bail(&self, args: impl fmt::Display) -> anyhow::Error {
            let path = self.format_path();
            anyhow::Error::msg(format!("{path}: {args}"))
        }

        /// Ensure table is empty.
        fn ensure_empty(&self, table: toml::Table) -> Result<()> {
            if let Some((key, value)) = table.into_iter().next() {
                return Err(self.bail(format_args!("got unsupported key `{key}`: {value}")));
            }

            Ok(())
        }

        fn table(&mut self, config: toml::Value) -> Result<toml::Table> {
            match config {
                toml::Value::Table(table) => Ok(table),
                other => return Err(self.bail(format_args!("expected table, got {other}"))),
            }
        }

        fn string(&mut self, value: toml::Value) -> Result<String> {
            match value {
                toml::Value::String(string) => Ok(string),
                other => Err(self.bail(format_args!("expected string, got {other}"))),
            }
        }

        fn array(&mut self, value: toml::Value) -> Result<Vec<toml::Value>> {
            match value {
                toml::Value::Array(array) => Ok(array),
                other => Err(self.bail(format_args!("expected array, got {other}"))),
            }
        }

        fn in_string<F, O>(
            &mut self,
            config: &mut toml::Table,
            key: &str,
            f: F,
        ) -> Result<Option<O>>
        where
            F: FnOnce(&mut Self, String) -> Result<O>,
        {
            let Some(value) = config.remove(key) else {
                return Ok(None);
            };

            self.key(key);
            let out = self.string(value)?;

            let out = match f(self, out) {
                Ok(out) => out,
                Err(e) => {
                    return Err(e.context(self.bail(format_args!("failed to process string"))));
                }
            };

            self.path.pop();
            Ok(Some(out))
        }

        fn in_array<F, O>(
            &mut self,
            config: &mut toml::Table,
            key: &str,
            mut f: F,
        ) -> Result<Option<Vec<O>>>
        where
            F: FnMut(&mut Self, toml::Value) -> Result<O>,
        {
            let Some(value) = config.remove(key) else {
                return Ok(None);
            };

            self.key(key);
            let array = self.array(value)?;
            let mut out = Vec::with_capacity(array.len());

            for (index, item) in array.into_iter().enumerate() {
                self.path.push(Part::Index(index));
                out.push(f(self, item)?);
                self.path.pop();
            }

            self.path.pop();
            Ok(Some(out))
        }

        fn badges(
            &mut self,
            config: &mut toml::Table,
        ) -> Result<Option<Vec<ConfigBadge>>, anyhow::Error> {
            let badges = self.in_array(config, "badges", |cx, value| {
                let mut value = cx.table(value)?;

                let Some(template) = cx.in_string(&mut value, "template", |cx, string| {
                    Ok(cx.templating.compile(string.trim())?)
                })? else {
                    return Err(cx.bail("missing .template"));
                };

                Ok(ConfigBadge { template })
            })?;

            Ok(badges)
        }

        fn repo(&mut self, config: toml::Value) -> Result<Repo> {
            let mut config = self.table(config)?;

            let header = self.in_string(&mut config, "header", |cx, string| {
                cx.templating.compile(&string)
            })?;

            let badges = self.badges(&mut config)?.unwrap_or_default();
            self.ensure_empty(config)?;
            Ok(Repo { header, badges })
        }
    }

    let path = path.as_ref();
    let mut cx = Ctxt::new(templating);

    let mut config: toml::Table = {
        let string = std::fs::read_to_string(path)?;
        let config = toml::from_str(&string)?;
        cx.table(config)?
    };

    let parent = path.parent().unwrap_or(path);

    let default_workflow = cx.in_string(&mut config, "default_workflow", |cx, string| {
        let path = RelativePathBuf::from(string).to_path(parent);
        let string =
            std::fs::read_to_string(&path).with_context(|| anyhow!("{}", path.display()))?;
        Ok(cx.templating.compile(&string)?)
    })?;

    let job_name = cx.in_string(&mut config, "job_name", |_, string| Ok(string))?;
    let license = cx.in_string(&mut config, "license", |_, string| Ok(string))?;

    let badges = cx.badges(&mut config)?.unwrap_or_default();

    let authors = cx
        .in_array(&mut config, "authors", |cx, item| cx.string(item))?
        .unwrap_or_default();

    let documentation = cx.in_string(&mut config, "documentation", |cx, string| {
        Ok(cx.templating.compile(&string)?)
    })?;

    let extra = config
        .remove("extra")
        .unwrap_or_else(|| toml::Value::Table(toml::map::Map::default()));

    let mut repos = HashMap::new();

    if let Some(config) = config.remove("repos") {
        cx.key("repos");

        for (id, value) in cx.table(config)? {
            cx.key(&id);
            repos.insert(id.to_owned(), cx.repo(value)?);
            cx.path.pop();
        }

        cx.path.pop();
    }

    cx.ensure_empty(config)?;

    Ok(Config {
        default_workflow,
        job_name,
        license,
        authors,
        extra,
        documentation,
        badges,
        repos,
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
