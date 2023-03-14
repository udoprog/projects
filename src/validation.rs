mod cargo;
mod ci;
mod readme;

use std::ops::Range;
use std::sync::Arc;

use anyhow::{anyhow, Context, Result};
use relative_path::RelativePathBuf;

use self::cargo::CargoIssue;
use self::ci::ActionExpected;
use crate::cargo::Manifest;
use crate::ctxt::Ctxt;
use crate::file::File;
use crate::model::{CrateParams, Module, UpdateParams};
use crate::urls::Urls;
use crate::workspace::{self};

pub(crate) enum Validation<'a> {
    DeprecatedWorkflow {
        path: RelativePathBuf,
    },
    MissingWorkflow {
        path: RelativePathBuf,
        candidates: Box<[RelativePathBuf]>,
    },
    WrongWorkflowName {
        path: RelativePathBuf,
        actual: String,
        expected: String,
    },
    /// Oudated version of an action.
    OutdatedAction {
        path: RelativePathBuf,
        name: String,
        actual: String,
        expected: String,
    },
    /// Deny use of the specific action.
    DeniedAction {
        path: RelativePathBuf,
        name: String,
        reason: String,
    },
    /// Actions check failed.
    CustomActionsCheck {
        path: RelativePathBuf,
        name: String,
        reason: String,
    },
    MissingReadme {
        path: RelativePathBuf,
    },
    MismatchedLibRs {
        path: RelativePathBuf,
        new_file: Arc<File>,
    },
    BadReadme {
        path: RelativePathBuf,
        new_file: Arc<File>,
    },
    ToplevelHeadings {
        path: RelativePathBuf,
        file: Arc<File>,
        range: Range<usize>,
        line_offset: usize,
    },
    MissingPreceedingBr {
        path: RelativePathBuf,
        file: Arc<File>,
        range: Range<usize>,
        line_offset: usize,
    },
    MissingFeature {
        path: RelativePathBuf,
        feature: String,
    },
    NoFeatures {
        path: RelativePathBuf,
    },
    MissingEmptyFeatures {
        path: RelativePathBuf,
    },
    MissingAllFeatures {
        path: RelativePathBuf,
    },
    CargoTomlIssues {
        path: RelativePathBuf,
        cargo: Option<Manifest>,
        issues: Vec<CargoIssue>,
    },
    ActionMissingKey {
        path: RelativePathBuf,
        key: &'a str,
        expected: ActionExpected,
        actual: Option<serde_yaml::Value>,
    },
    ActionOnMissingBranch {
        path: RelativePathBuf,
        key: &'a str,
        branch: &'a str,
    },
    ActionExpectedEmptyMapping {
        path: RelativePathBuf,
        key: &'a str,
    },
}

/// Run a single module.
pub(crate) fn build(
    cx: &Ctxt<'_>,
    module: &Module<'_>,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    let (Some(module_path), Some(module_url), Some(repo)) = (module.path, &module.url, module.repo()) else {
        println!(
            ".gitmodules: {name}: missing `path` and/or `url`",
            name = module.name
        );
        return Ok(());
    };

    let cargo_toml = match cx.config.cargo_toml(module.name) {
        Some(cargo_toml) => module_path.join(cargo_toml),
        None => module_path.join(workspace::CARGO_TOML),
    };

    let workspace = workspace::open(cx.root, &cargo_toml)?;

    let primary_crate = cx.config.crate_for(module.name).or(repo.split('/').last());

    let primary_crate = match workspace.primary_crate(primary_crate)? {
        Some(primary_crate) => primary_crate,
        None => return Err(anyhow!("{module_path}: cannot determine primary crate",)),
    };

    let params = cx.config.per_crate_render(CrateParams {
        repo: &repo,
        name: primary_crate.manifest.crate_name()?,
        description: primary_crate.manifest.description()?,
    });

    let documentation = match &cx.config.documentation {
        Some(documentation) => Some(documentation.render(&params)?),
        None => None,
    };

    let url_string = module_url.to_string();

    let update_params = UpdateParams {
        license: cx.config.license(),
        readme: readme::README_MD,
        repository: &url_string,
        homepage: &url_string,
        documentation: documentation.as_deref().unwrap_or_default(),
        authors: &cx.config.authors,
    };

    for package in workspace.packages() {
        if package.manifest.is_publish()? {
            cargo::work_cargo_toml(package, validation, &update_params)?;
        }
    }

    if cx.config.is_enabled(module.name, "ci") {
        ci::build(&cx, &primary_crate, module_path, &workspace, validation)
            .with_context(|| anyhow!("ci validation: {}", cx.config.job_name()))?;
    }

    if cx.config.is_enabled(module.name, "readme") {
        readme::build(
            cx,
            module.name,
            module_path,
            primary_crate,
            params.crate_params,
            validation,
            urls,
        )?;

        for package in workspace.packages() {
            if package.manifest_dir != module_path {
                if package.manifest.is_publish()? {
                    let crate_params = CrateParams {
                        repo: &repo,
                        name: package.manifest.crate_name()?,
                        description: package.manifest.description()?,
                    };

                    readme::build(
                        cx,
                        module.name,
                        &package.manifest_dir,
                        package,
                        crate_params,
                        validation,
                        urls,
                    )?;
                }
            }
        }
    }

    Ok(())
}
