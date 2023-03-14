use core::fmt;
use std::collections::HashSet;
use std::path::Path;

use anyhow::{anyhow, Context, Result};
use relative_path::{RelativePath, RelativePathBuf};
use serde_yaml::Value;

use crate::actions::Actions;
use crate::cargo::Manifest;
use crate::validation::Validation;
use crate::workspace::{Package, Workspace};

pub(crate) struct Ci<'a, 'validation, 'b> {
    root: &'a Path,
    path: &'a RelativePath,
    name: &'a str,
    actions: &'a Actions<'a>,
    manifest: &'a Manifest,
    workspace: bool,
    validation: &'validation mut Vec<Validation<'b>>,
}

pub(crate) enum ActionExpected {
    Sequence,
    Mapping,
}

impl fmt::Display for ActionExpected {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ActionExpected::Sequence => write!(f, "sequence"),
            ActionExpected::Mapping => write!(f, "mapping"),
        }
    }
}

enum CargoKind {
    Build,
    Test,
    None,
}

struct Cargo {
    #[allow(unused)]
    kind: CargoKind,
    features: CargoFeatures,
    missing_features: Vec<String>,
    features_list: Vec<String>,
}

enum RunIdentity {
    /// A cargo build command.
    Cargo(Cargo),
    /// Empty run identity.
    None,
}

enum CargoFeatures {
    Default,
    NoDefaultFeatures,
    AllFeatures,
}

/// Build ci validation.
pub(crate) fn build(
    cx: &crate::ctxt::Ctxt<'_>,
    primary_crate: &Package,
    module_path: &RelativePath,
    workspace: &Workspace,
    validation: &mut Vec<Validation<'_>>,
) -> Result<()> {
    let path = module_path.join(".github").join("workflows");

    let mut ci = Ci {
        root: cx.root,
        path: &path,
        name: cx.config.job_name(),
        actions: &cx.actions,
        manifest: &primary_crate.manifest,
        workspace: !workspace.is_single_crate(),
        validation,
    };

    validate(&mut ci)
}

/// Validate the current model.
fn validate(cx: &mut Ci<'_, '_, '_>) -> Result<()> {
    let deprecated_yml = cx.path.join("rust.yml");
    let expected_path = cx.path.join("ci.yml");

    let candidates =
        candidates(cx).with_context(|| anyhow!("list candidates: {path}", path = cx.path))?;

    let path = if !expected_path.to_path(cx.root).is_file() {
        let path = match &candidates[..] {
            [path] => Some(path.clone()),
            _ => None,
        };

        cx.validation.push(Validation::MissingWorkflow {
            path: expected_path,
            candidates: candidates.clone(),
        });

        match path {
            Some(path) => path,
            None => return Ok(()),
        }
    } else {
        expected_path
    };

    if deprecated_yml.to_path(cx.root).is_file() && candidates.len() > 1 {
        cx.validation.push(Validation::DeprecatedWorkflow {
            path: deprecated_yml,
        });
    }

    let bytes = std::fs::read(path.to_path(cx.root))?;
    let value: serde_yaml::Value =
        serde_yaml::from_slice(&bytes).with_context(|| anyhow!("{path}"))?;

    let name = value
        .get("name")
        .and_then(|name| name.as_str())
        .ok_or_else(|| anyhow!("{path}: missing .name"))?;

    if name != cx.name {
        cx.validation.push(Validation::WrongWorkflowName {
            path: path.clone(),
            actual: name.to_owned(),
            expected: cx.name.to_owned(),
        });
    }

    validate_jobs(cx, &path, &value)?;
    Ok(())
}

/// Get candidates.
fn candidates(ci: &Ci<'_, '_, '_>) -> std::io::Result<Box<[RelativePathBuf]>> {
    let dir = match std::fs::read_dir(ci.path.to_path(ci.root)) {
        Ok(dir) => dir,
        Err(e) if e.kind() == std::io::ErrorKind::NotFound => return Ok(Box::from([])),
        Err(e) => return Err(e.into()),
    };

    let mut paths = Vec::new();

    for e in dir {
        let e = e?;

        if let Some(name) = e.file_name().to_str() {
            paths.push(ci.path.join(name));
        }
    }

    Ok(paths.into())
}

/// Validate that jobs are modern.
fn validate_jobs(
    cx: &mut Ci<'_, '_, '_>,
    path: &RelativePath,
    value: &serde_yaml::Value,
) -> Result<()> {
    if let Some(value) = value.get("on") {
        validate_on(cx, value, path);
    }

    if let Some(jobs) = value.get("jobs").and_then(|v| v.as_mapping()) {
        for (_, job) in jobs {
            for action in job
                .get("steps")
                .and_then(|v| v.as_sequence())
                .into_iter()
                .flatten()
                .flat_map(|v| v.as_mapping())
            {
                if let Some(uses) = action.get("uses").and_then(|v| v.as_str()) {
                    if let Some((name, version)) = uses.split_once('@') {
                        if let Some(expected) = cx.actions.get_latest(name) {
                            if expected != version {
                                cx.validation.push(Validation::OutdatedAction {
                                    path: path.to_owned(),
                                    name: name.into(),
                                    actual: version.into(),
                                    expected: expected.into(),
                                });
                            }
                        }

                        if let Some(reason) = cx.actions.get_deny(name) {
                            cx.validation.push(Validation::DeniedAction {
                                path: path.to_owned(),
                                name: name.into(),
                                reason: reason.into(),
                            });
                        }

                        if let Some(check) = cx.actions.get_check(name) {
                            if let Err(reason) = check.check(action) {
                                cx.validation.push(Validation::CustomActionsCheck {
                                    path: path.to_owned(),
                                    name: name.into(),
                                    reason: reason.into(),
                                });
                            }
                        }
                    }
                }
            }

            if !cx.workspace {
                verify_single_project_build(cx, path, job);
            }
        }
    }

    Ok(())
}

fn validate_on(cx: &mut Ci<'_, '_, '_>, value: &Value, path: &RelativePath) {
    let Value::Mapping(m) = value else {
        cx.validation.push(Validation::ActionMissingKey {
            path: path.to_owned(),
            key: "on",
            expected: ActionExpected::Mapping,
            actual: Some(value.clone()),
        });

        return;
    };

    match m.get("pull_request") {
        Some(Value::Mapping(m)) => {
            if !m.is_empty() {
                cx.validation.push(Validation::ActionExpectedEmptyMapping {
                    path: path.to_owned(),
                    key: "on.pull_request",
                });
            }
        }
        value => {
            cx.validation.push(Validation::ActionMissingKey {
                path: path.to_owned(),
                key: "on.pull_request",
                expected: ActionExpected::Mapping,
                actual: value.cloned(),
            });
        }
    }

    match m.get("push") {
        Some(Value::Mapping(m)) => match m.get("branches") {
            Some(Value::Sequence(s)) => {
                if !s.iter().flat_map(|v| v.as_str()).any(|b| b == "main") {
                    cx.validation.push(Validation::ActionOnMissingBranch {
                        path: path.to_owned(),
                        key: "on.push.branches",
                        branch: "main",
                    });
                }
            }
            value => {
                cx.validation.push(Validation::ActionMissingKey {
                    path: path.to_owned(),
                    key: "on.push.branches",
                    expected: ActionExpected::Sequence,
                    actual: value.cloned(),
                });
            }
        },
        value => {
            cx.validation.push(Validation::ActionMissingKey {
                path: path.to_owned(),
                key: "on.push",
                expected: ActionExpected::Mapping,
                actual: value.cloned(),
            });
        }
    }
}

fn verify_single_project_build(
    cx: &mut Ci<'_, '_, '_>,
    path: &RelativePath,
    job: &serde_yaml::Value,
) {
    let mut cargo_combos = Vec::new();
    let features = cx.manifest.features();

    for step in job
        .get("steps")
        .and_then(|v| v.as_sequence())
        .into_iter()
        .flatten()
    {
        match step.get("run").and_then(|v| v.as_str()) {
            Some(command) => {
                let identity = identify_command(command, &features);

                match identity {
                    RunIdentity::Cargo(cargo) => {
                        for feature in &cargo.missing_features {
                            cx.validation.push(Validation::MissingFeature {
                                path: path.to_owned(),
                                feature: feature.clone(),
                            });
                        }

                        if matches!(cargo.kind, CargoKind::Build) {
                            cargo_combos.push(cargo);
                        }
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    if !cargo_combos.is_empty() {
        if features.is_empty() {
            for build in &cargo_combos {
                if !matches!(build.features, CargoFeatures::Default) {
                    cx.validation.push(Validation::NoFeatures {
                        path: path.to_owned(),
                    });
                }
            }
        } else {
            ensure_feature_combo(cx, path, &cargo_combos);
        }
    }
}

/// Ensure that feature combination is valid.
fn ensure_feature_combo(cx: &mut Ci<'_, '_, '_>, path: &RelativePath, cargos: &[Cargo]) -> bool {
    let mut all_features = false;
    let mut empty_features = false;

    for cargo in cargos {
        match cargo.features {
            CargoFeatures::Default => {
                return false;
            }
            CargoFeatures::NoDefaultFeatures => {
                empty_features = empty_features || cargo.features_list.is_empty();
            }
            CargoFeatures::AllFeatures => {
                all_features = true;
            }
        }
    }

    if !empty_features {
        cx.validation.push(Validation::MissingEmptyFeatures {
            path: path.to_owned(),
        });
    }

    if !all_features {
        cx.validation.push(Validation::MissingAllFeatures {
            path: path.to_owned(),
        });
    }

    false
}

fn identify_command(command: &str, features: &HashSet<String>) -> RunIdentity {
    let mut it = command.split(' ').peekable();

    if matches!(it.next(), Some("cargo")) {
        // Consume arguments.
        while it
            .peek()
            .filter(|p| p.starts_with('+') || p.starts_with('-'))
            .is_some()
        {
            it.next();
        }

        let kind = match it.next() {
            Some("build") => CargoKind::Build,
            Some("test") => CargoKind::Test,
            _ => CargoKind::None,
        };

        let (cargo_features, missing_features, features_list) = process_features(it, features);

        return RunIdentity::Cargo(Cargo {
            kind,
            features: cargo_features,
            missing_features,
            features_list,
        });
    }

    RunIdentity::None
}

fn process_features(
    mut it: std::iter::Peekable<std::str::Split<char>>,
    features: &HashSet<String>,
) -> (CargoFeatures, Vec<String>, Vec<String>) {
    let mut cargo_features = CargoFeatures::Default;
    let mut missing_features = Vec::new();
    let mut features_list = Vec::new();

    while let Some(arg) = it.next() {
        match arg {
            "--no-default-features" => {
                cargo_features = CargoFeatures::NoDefaultFeatures;
            }
            "--all-features" => {
                cargo_features = CargoFeatures::AllFeatures;
            }
            "--features" | "-F" => {
                if let Some(args) = it.next() {
                    for feature in args.split(',').map(|s| s.trim()) {
                        if !features.contains(feature) {
                            missing_features.push(feature.into());
                        }

                        features_list.push(feature.into());
                    }
                }
            }
            _ => {}
        }
    }

    (cargo_features, missing_features, features_list)
}
