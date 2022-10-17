use std::collections::BTreeSet;
use std::path::Path;

use crate::actions::{Actions, ActionsCheck};
use crate::badges::{Badge, Badges};
use crate::file::LineColumn;
use crate::model::{work_cargo_toml, Ci, Readme, Validation};
use crate::params::Params;
use anyhow::{anyhow, Context, Result};
use http::Uri;
use model::UpdateParams;
use relative_path::RelativePath;

mod actions;
mod badges;
mod cargo;
mod file;
mod gitmodules;
mod model;
mod params;

const DOCS_RS: &str = "https://docs.rs";
const GITHUB_COM: &str = "https://github.com";

#[derive(Default)]
struct Opts {
    fix: bool,
}

fn main() -> Result<()> {
    let mut filters = Vec::new();
    let mut opts = Opts::default();

    for arg in std::env::args().skip(1) {
        if arg.starts_with('-') {
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

    let mut uses = Actions::default();
    uses.latest("actions/checkout", "v3");
    uses.check("actions-rs/toolchain", &ActionsRsToolchainActionsCheck);
    uses.deny("actions-rs/cargo", "using `run` is less verbose and faster");

    let gitmodules_bytes = std::fs::read(".gitmodules")?;
    let modules = parse_git_modules(&gitmodules_bytes).context(".gitmodules")?;

    let name = String::from("CI");

    let mut validation = Vec::new();
    let root = Path::new("");

    for module in &modules {
        if !filters.is_empty() && !filters.iter().all(|filter| module.name.contains(filter)) {
            continue;
        }

        let (path, url) = if let (Some(path), Some(url)) = (&module.path, &module.url) {
            (path.to_path(root), url)
        } else {
            println!(
                ".gitmodules: {name}: missing `path` and/or `url`",
                name = module.name
            );
            continue;
        };

        let cargo_toml = match &module.cargo_toml {
            Some(cargo_toml) => cargo_toml.to_path(&path),
            None => path.join("Cargo.toml"),
        };

        let cargo = cargo::open(&cargo_toml)
            .with_context(|| anyhow!("{path}: failed to read", path = cargo_toml.display()))?;
        let crate_name = cargo
            .crate_name()
            .with_context(|| anyhow!("{path}: failed to read", path = cargo_toml.display()))?;

        let documentation = format!("{DOCS_RS}/{crate_name}");

        let lib_rs = cargo_toml
            .parent()
            .as_deref()
            .unwrap_or(&path)
            .join("src")
            .join("lib.rs");

        let params = Params {
            repo: url.path().trim_matches('/').into(),
            crate_name: crate_name.into(),
        };

        let update_params = UpdateParams {
            license: "MIT/Apache-2.0",
            readme: "README.md",
            repository: url,
            homepage: url,
            documentation: &documentation,
        };

        work_cargo_toml(&cargo_toml, &cargo, &mut validation, &update_params)?;

        if module.is_enabled("ci") {
            let ci = Ci::new(
                path.join(".github").join("workflows"),
                name.clone(),
                &uses,
                &cargo,
                module.workspace,
            );
            ci.validate(&mut validation)
                .with_context(|| anyhow!("ci validation: {}", name))?;
        }

        if module.is_enabled("readme") {
            let readme = Readme::new(path.join("README.md"), lib_rs.clone(), &badges, &params);
            readme
                .validate(&mut validation)
                .with_context(|| anyhow!("readme validation: {}", name))?;
        }
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
            Validation::DeprecatedWorkflow { path } => {
                println!("{path}: Reprecated Workflow", path = path.display());
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
            Validation::OutdatedAction {
                path,
                name,
                actual,
                expected,
            } => {
                println! {
                    "{path}: Outdated action `{name}`: {actual} (actual) != {expected} (expected)",
                    path = path.display()
                };
            }
            Validation::DeniedAction { path, name, reason } => {
                println! {
                    "{path}: Denied action `{name}`: {reason}",
                    path = path.display()
                };
            }
            Validation::CustomActionsCheck { path, name, reason } => {
                println! {
                    "{path}: Action validation failed `{name}`: {reason}",
                    path = path.display()
                };
            }
            Validation::MissingReadme { path } => {
                println! {
                    "{path}: Missing README", path = path.display()
                };
            }
            Validation::MismatchedLibRs { path, new_file } => {
                if opts.fix {
                    println!("{path}: Fixing lib.rs", path = path.display());
                    std::fs::write(path, new_file.as_bytes())?;
                } else {
                    println! {
                        "{path}: Mismatched lib.rs", path = path.display()
                    };
                }
            }
            Validation::BadReadme { path, new_file } => {
                if opts.fix {
                    println!("{path}: Fixing README.md", path = path.display());
                    std::fs::write(path, new_file.as_bytes())?;
                } else {
                    println! {
                        "{path}: Bad README.md", path = path.display()
                    };
                }
            }
            Validation::ToplevelHeadings {
                path,
                file: new_file,
                range,
                line_offset,
            } => {
                let (LineColumn { line, column, .. }, string) =
                    new_file.line_column(range.start)?;
                let line = line_offset + line;
                let column = column + 4;

                println! {
                    "{path}:{line}:{column}: doc comment has toplevel headings", path = path.display()
                };

                println!("{string}");
            }
            Validation::MissingPreceedingBr {
                path,
                file: new_file,
                range,
                line_offset,
            } => {
                let (LineColumn { line, column }, string) = new_file.line_column(range.start)?;
                let line = line_offset + line;
                let column = column + 4;

                println! {
                    "{path}:{line}:{column}: missing preceeding <br>", path = path.display()
                };

                println!("{string}");
            }
            Validation::MissingFeature { path, feature } => {
                println! {
                    "{path}: missing features `{feature}`", path = path.display()
                };
            }
            Validation::NoFeatures { path } => {
                println! {
                    "{path}: trying featured build (--all-features, --no-default-features), but no features present", path = path.display()
                };
            }
            Validation::MissingEmptyFeatures { path } => {
                println! {
                    "{path}: missing empty features build", path = path.display()
                };
            }
            Validation::MissingAllFeatures { path } => {
                println! {
                    "{path}: missing all features build", path = path.display()
                };
            }
            Validation::CargoTomlIssues {
                path,
                cargo: modified_cargo,
                issues,
            } => {
                println! {
                    "{path}:", path = path.display()
                };

                for issue in issues {
                    println!("  {issue}");
                }

                if opts.fix {
                    if let Some(modified_cargo) = modified_cargo {
                        modified_cargo.save_to(path)?;
                    }
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
        Ok(format!("[<img alt=\"github\" src=\"https://img.shields.io/badge/github-{badge_repo}-8da0cb?style=for-the-badge&logo=github\" height=\"20\">]({GITHUB_COM}/{repo})"))
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
        Ok(format!("[<img alt=\"docs.rs\" src=\"https://img.shields.io/badge/docs.rs-{badge_crate_name}-66c2a5?style=for-the-badge&logoColor=white&logo={BADGE_IMAGE}\" height=\"20\">]({DOCS_RS}/{crate_name})"))
    }
}

struct BuildStatusBadge;

impl Badge for BuildStatusBadge {
    fn build(&self, params: &Params) -> Result<String> {
        let Params { repo, .. } = params;
        Ok(format!("[<img alt=\"build status\" src=\"https://img.shields.io/github/workflow/status/{repo}/CI/main?style=for-the-badge\" height=\"20\">]({GITHUB_COM}/{repo}/actions?query=branch%3Amain)"))
    }
}

/// A git module.
#[derive(Debug, Clone)]
pub(crate) struct GitModule<'a> {
    name: &'a str,
    path: Option<&'a RelativePath>,
    url: Option<Uri>,
    cargo_toml: Option<&'a RelativePath>,
    disabled: BTreeSet<&'a str>,
    workspace: bool,
}

impl GitModule<'_> {
    /// Test if a feature is disabled.
    fn is_enabled(&self, feature: &str) -> bool {
        !self.disabled.contains(feature)
    }
}

/// Parse gitmodules from the given input.
pub(crate) fn parse_git_modules(input: &[u8]) -> Result<Vec<GitModule<'_>>> {
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
        let mut disabled = BTreeSet::new();
        let mut workspace = false;

        let mut section = match parser.parse_section()? {
            Some(section) => section,
            None => return Ok(None),
        };

        while let Some((key, value)) = section.next_section()? {
            match key {
                "path" => {
                    let string = std::str::from_utf8(value)?;
                    path = Some(RelativePath::new(string));
                }
                "url" => {
                    let string = std::str::from_utf8(value)?;
                    url = Some(str::parse::<Uri>(string)?);
                }
                "cargo-toml" => {
                    let string = std::str::from_utf8(value)?;
                    cargo_toml = Some(RelativePath::new(string));
                }
                "disabled" => {
                    disabled = std::str::from_utf8(value)?
                        .split(',')
                        .map(str::trim)
                        .collect();
                }
                "workspace" => {
                    workspace = value == b"true";
                }
                _ => {}
            }
        }

        Ok(Some(GitModule {
            name: section.name(),
            path,
            url,
            cargo_toml,
            disabled,
            workspace,
        }))
    }
}

struct ActionsRsToolchainActionsCheck;

impl ActionsCheck for ActionsRsToolchainActionsCheck {
    fn check(&self, mapping: &serde_yaml::Mapping) -> Result<(), String> {
        let with = match mapping.get("with").and_then(|v| v.as_mapping()) {
            Some(with) => with,
            None => return Err(String::from("missing with")),
        };

        if with.get("toolchain").and_then(|v| v.as_str()).is_none() {
            return Err(String::from("missing toolchain"));
        }

        Ok(())
    }
}
