use std::ffi::OsStr;
use std::os::unix::prelude::OsStrExt;
use std::path::Path;

use crate::badges::{Badge, Badges};
use crate::model::{Ci, Model, Readme, Validation};
use crate::params::Params;
use anyhow::{anyhow, Context, Result};
use http::Uri;

mod badges;
mod cargo;
mod file;
mod gitmodules;
mod model;
mod params;

#[derive(Default)]
struct Opts {
    fix: bool,
}

fn main() -> Result<()> {
    let mut filters = Vec::new();
    let mut opts = Opts::default();

    for arg in std::env::args().skip(1) {
        if arg.starts_with("-") {
            match arg.as_str() {
                "--fix" => {
                    opts.fix = true;
                }
                _ => {
                    return Err(anyhow!("unsupported argument: {arg}"));
                }
            }

            continue;
        }

        filters.push(arg);
    }

    let mut badges = Badges::new();

    badges.push(&GithubBadge);
    badges.push(&CratesIoBadge);
    badges.push(&DocsRsBadge);
    badges.push(&BuildStatusBadge);

    let gitmodules_bytes = std::fs::read(".gitmodules")?;
    let modules = parse_git_modules(&gitmodules_bytes).context(".gitmodules")?;

    let mut models = Vec::new();

    let name = String::from("CI");

    for module in modules {
        if !filters.is_empty() {
            if !filters.iter().all(|filter| module.name.contains(filter)) {
                continue;
            }
        }

        let (path, url) = if let (Some(path), Some(url)) = (module.path, module.url) {
            (path, url)
        } else {
            println!(
                ".gitmodules: {name}: missing `path` and/or `url`",
                name = module.name
            );
            continue;
        };

        let cargo_toml = match module.cargo_toml {
            Some(cargo_toml) => path.join(cargo_toml),
            None => path.join("Cargo.toml"),
        };

        let cargo = cargo::open(&cargo_toml)
            .with_context(|| anyhow!("{path}: failed to read", path = cargo_toml.display()))?;
        let crate_name = cargo
            .crate_name()
            .with_context(|| anyhow!("{path}: failed to read", path = cargo_toml.display()))?;

        let lib_rs = cargo_toml.parent().unwrap_or(&path).join("src/lib.rs");

        let params = Params {
            repo: url.path().trim_matches('/').into(),
            crate_name: crate_name.into(),
        };

        models.push(Model {
            name: module.name,
            ci: Ci::new(path.join(".github/workflows"), name.clone()),
            readme: Readme::new(path.join("README.md"), lib_rs.clone(), &badges, params),
        });
    }

    let mut validation = Vec::new();

    for model in &mut models {
        model
            .validate(&mut validation)
            .with_context(|| anyhow!("model: {}", model.name))?;
    }

    for error in &validation {
        match error {
            Validation::MissingWorkflows { path } => {
                println!("{path}: Missing workflows directory", path = path.display());
            }
            Validation::MissingWorkflow { path, candidates } => {
                println!("{path}: Missing workflow", path = path.display());

                for candidate in candidates.iter() {
                    println! {
                        "  Candidate: {candidate}",
                        candidate = candidate.display()
                    };
                }

                if opts.fix {
                    if let [from] = candidates.as_ref() {
                        println!(
                            "{path}: Rename from {from}",
                            path = path.display(),
                            from = from.display()
                        );
                        std::fs::rename(from, path)?;
                    }
                }
            }
            Validation::WrongWorkflowName {
                path,
                actual,
                expected,
            } => {
                println! {
                    "{path}: Wrong workflow name: {actual} (actual) != {expected} (expected)",
                    path = path.display()
                };
            }
            Validation::MissingReadme { path } => {
                println! {
                    "{path}: Missing README", path = path.display()
                };
            }
            Validation::MissingLinks { path, new_file } => {
                println! {
                    "{path}: Bad Links", path = path.display()
                };

                if opts.fix {
                    println!("{path}: Fixing links", path = path.display());
                    std::fs::write(path, new_file.as_bytes())?;
                }
            }
            Validation::BadReadme { path, new_file } => {
                println! {
                    "{path}: Bad README.md", path = path.display()
                };

                if opts.fix {
                    println!("{path}: Fixing README.md", path = path.display());
                    std::fs::write(path, new_file.as_bytes())?;
                }
            }
        }
    }

    Ok(())
}

struct GithubBadge;

impl Badge for GithubBadge {
    fn build(&self, params: &Params) -> Result<String> {
        let Params { repo, .. } = params;
        let badge_repo = repo.replace('-', "--");
        Ok(format!("[<img alt=\"github\" src=\"https://img.shields.io/badge/github-{badge_repo}-8da0cb?style=for-the-badge&logo=github\" height=\"20\">](https://github.com/{repo})"))
    }
}

struct CratesIoBadge;

impl Badge for CratesIoBadge {
    fn build(&self, params: &Params) -> Result<String> {
        let Params { crate_name, .. } = params;
        Ok(format!("[<img alt=\"crates.io\" src=\"https://img.shields.io/crates/v/{crate_name}.svg?style=for-the-badge&color=fc8d62&logo=rust\" height=\"20\">](https://crates.io/crates/{crate_name})"))
    }
}

struct DocsRsBadge;

impl Badge for DocsRsBadge {
    fn build(&self, params: &Params) -> Result<String> {
        const BADGE_IMAGE: &str = "data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K";
        let Params { crate_name, .. } = params;
        let badge_crate_name = crate_name.replace('-', "--");
        Ok(format!("[<img alt=\"docs.rs\" src=\"https://img.shields.io/badge/docs.rs-{badge_crate_name}-66c2a5?style=for-the-badge&logoColor=white&logo={BADGE_IMAGE}\" height=\"20\">](https://docs.rs/{crate_name})"))
    }
}

struct BuildStatusBadge;

impl Badge for BuildStatusBadge {
    fn build(&self, params: &Params) -> Result<String> {
        let Params { repo, .. } = params;
        Ok(format!("[<img alt=\"build status\" src=\"https://img.shields.io/github/workflow/status/{repo}/CI/main?style=for-the-badge\" height=\"20\">](https://github.com/{repo}/actions?query=branch%3Amain)"))
    }
}

/// A git module.
#[derive(Debug, Clone)]
pub(crate) struct GitModule<'a> {
    #[allow(unused)]
    pub(crate) name: &'a str,
    pub(crate) path: Option<&'a Path>,
    pub(crate) url: Option<Uri>,
    pub(crate) cargo_toml: Option<&'a Path>,
}

/// Parse gitmodules from the given input.
pub(crate) fn parse_git_modules<'a>(input: &'a [u8]) -> Result<Vec<GitModule<'a>>> {
    let mut parser = gitmodules::Parser::new(input);

    let mut modules = Vec::new();

    while let Some(module) = parse_git_module(&mut parser)? {
        modules.push(module);
    }

    return Ok(modules);

    /// Parse a git module.
    pub(crate) fn parse_git_module<'a>(
        parser: &mut gitmodules::Parser<'a>,
    ) -> Result<Option<GitModule<'a>>> {
        let mut path = None;
        let mut url = None;
        let mut cargo_toml = None;

        let mut section = match parser.parse_section()? {
            Some(section) => section,
            None => return Ok(None),
        };

        while let Some((key, value)) = section.next_section()? {
            match key {
                "path" => {
                    let os_str = OsStr::from_bytes(value);
                    path = Some(Path::new(os_str));
                }
                "url" => {
                    let string = std::str::from_utf8(value)?;
                    url = Some(str::parse::<Uri>(string)?);
                }
                "cargo-toml" => {
                    let os_str = OsStr::from_bytes(value);
                    cargo_toml = Some(Path::new(os_str));
                }
                _ => {}
            }
        }

        Ok(Some(GitModule {
            name: section.name(),
            path,
            url,
            cargo_toml,
        }))
    }
}
