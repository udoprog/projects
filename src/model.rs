use std::collections::{BTreeSet, HashSet};
use std::fmt;
use std::fs;
use std::io;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::actions::Actions;
use crate::badges::Badges;
use crate::cargo::CargoToml;
use crate::file::File;
use crate::params::Params;
use anyhow::{anyhow, Context, Result};
use toml_edit::Key;

macro_rules! cargo_issues {
    ($f:ident, $($issue:ident $({ $($field:ident: $ty:ty),* $(,)? })? => $description:expr),* $(,)?) => {
        pub(crate) enum CargoIssue {
            $($issue $({$($field: $ty),*})?,)*
        }

        impl fmt::Display for CargoIssue {
            fn fmt(&self, $f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(#[allow(unused_variables)] CargoIssue::$issue $({ $($field),* })? => $description,)*
                }
            }
        }
    }
}

cargo_issues! {
    f,
    MissingPackageLicense => write!(f, "package.license: missing"),
    WrongPackageLicense => write!(f, "package.license: wrong"),
    MissingPackageReadme => write!(f, "package.readme: missing"),
    WrongPackageReadme => write!(f, "package.readme: wrong"),
    MissingPackageRepository => write!(f, "package.repository: missing"),
    WrongPackageRepository => write!(f, "package.repository: wrong"),
    MissingPackageHomepage => write!(f, "package.homepage: missing"),
    WrongPackageHomepage => write!(f, "package.homepage: wrong"),
    MissingPackageDocumentation => write!(f, "package.documentation: missing"),
    WrongPackageDocumentation => write!(f, "package.documentation: wrong"),
    PackageDescription => write!(f, "package.description: missing"),
    PackageCategories => write!(f, "package.categories: missing"),
    PackageKeywords => write!(f, "package.keywords: missing"),
    PackageAuthorsEmpty => write!(f, "authors: empty"),
    PackageDependenciesEmpty => write!(f, "dependencies: empty"),
    PackageDevDependenciesEmpty => write!(f, "dev-dependencies: empty"),
    PackageBuildDependenciesEmpty => write!(f, "build-dependencies: empty"),
    KeysNotSorted { expected: Vec<CargoKey>, actual: Vec<CargoKey> } => {
        write!(f, "[package] keys out-of-order, expected: {expected:?}")
    }
}

macro_rules! cargo_keys {
    ($($ident:ident => $name:literal),* $(,)?) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
        pub(crate) enum CargoKey {
            $($ident,)*
        }

        impl fmt::Display for CargoKey {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(CargoKey::$ident { .. } => write!(f, $name),)*
                }
            }
        }

        fn cargo_key(key: &str) -> Option<CargoKey> {
            match key {
                $($name => Some(CargoKey::$ident),)*
                _ => None,
            }
        }
    };
}

// Order from: https://doc.rust-lang.org/cargo/reference/manifest.html
cargo_keys! {
    Name => "name",
    Version => "version",
    Authors => "authors",
    Edition => "edition",
    RustVersion => "rust-version",
    Description => "description",
    Documentation => "documentation",
    Readme => "readme",
    Homepage => "homepage",
    Repository => "repository",
    License => "license",
    Keywords => "keywords",
    Categories => "categories",
}

pub(crate) enum Validation {
    MissingWorkflows {
        path: PathBuf,
    },
    DeprecatedWorkflow {
        path: PathBuf,
    },
    MissingWorkflow {
        path: PathBuf,
        candidates: Box<[PathBuf]>,
    },
    WrongWorkflowName {
        path: PathBuf,
        actual: String,
        expected: String,
    },
    /// Oudated version of an action.
    OutdatedAction {
        path: PathBuf,
        name: String,
        actual: String,
        expected: String,
    },
    /// Deny use of the specific action.
    DeniedAction {
        path: PathBuf,
        name: String,
        reason: String,
    },
    /// Actions check failed.
    CustomActionsCheck {
        path: PathBuf,
        name: String,
        reason: String,
    },
    MissingReadme {
        path: PathBuf,
    },
    MismatchedLibRs {
        path: PathBuf,
        new_file: Rc<File>,
    },
    BadReadme {
        path: PathBuf,
        new_file: Rc<File>,
    },
    ToplevelHeadings {
        path: PathBuf,
        file: Rc<File>,
        range: Range<usize>,
        line_offset: usize,
    },
    MissingPreceedingBr {
        path: PathBuf,
        file: Rc<File>,
        range: Range<usize>,
        line_offset: usize,
    },
    MissingFeature {
        path: PathBuf,
        feature: String,
    },
    NoFeatures {
        path: PathBuf,
    },
    MissingEmptyFeatures {
        path: PathBuf,
    },
    MissingAllFeatures {
        path: PathBuf,
    },
    CargoTomlIssues {
        path: PathBuf,
        cargo: Option<CargoToml>,
        issues: Vec<CargoIssue>,
    },
}

pub(crate) struct Ci<'a> {
    pub(crate) path: PathBuf,
    pub(crate) name: String,
    actions: &'a Actions<'a>,
    cargo: &'a CargoToml,
    workspace: bool,
}

impl<'a> Ci<'a> {
    /// Construct a new CI config.
    pub(crate) fn new(
        path: PathBuf,
        name: String,
        actions: &'a Actions<'a>,
        cargo: &'a CargoToml,
        workspace: bool,
    ) -> Self {
        Self {
            path,
            name,
            actions,
            cargo,
            workspace,
        }
    }

    /// Get candidates.
    fn candidates(&self) -> Result<Box<[PathBuf]>> {
        let dir = fs::read_dir(&self.path)?;
        let mut paths = Vec::new();

        for e in dir {
            let e = e?;
            paths.push(e.path());
        }

        Ok(paths.into())
    }

    /// Validate the current model.
    pub(crate) fn validate(&self, validation: &mut Vec<Validation>) -> Result<()> {
        if !self.path.is_dir() {
            validation.push(Validation::MissingWorkflows {
                path: self.path.clone(),
            });

            return Ok(());
        }

        let deprecated_yml = self.path.join("rust.yml");
        let expected_path = self.path.join("ci.yml");

        let candidates = self
            .candidates()
            .with_context(|| anyhow!("list candidates: {path}", path = self.path.display()))?;

        let path = if !expected_path.is_file() {
            let path = match &candidates[..] {
                [path] => Some(path.clone()),
                _ => None,
            };

            validation.push(Validation::MissingWorkflow {
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

        if deprecated_yml.is_file() && candidates.len() > 1 {
            validation.push(Validation::DeprecatedWorkflow {
                path: deprecated_yml,
            });
        }

        let bytes = std::fs::read(&path)?;
        let value: serde_yaml::Value =
            serde_yaml::from_slice(&bytes).with_context(|| anyhow!("{}", path.display()))?;

        let name = value
            .get("name")
            .and_then(|name| name.as_str())
            .ok_or_else(|| anyhow!("{}: missing .name", path.display()))?;

        if name != self.name {
            validation.push(Validation::WrongWorkflowName {
                path: path.clone(),
                actual: name.to_owned(),
                expected: self.name.clone(),
            });
        }

        self.validate_jobs(&path, &value, validation)?;
        Ok(())
    }

    /// Validate that jobs are modern.
    fn validate_jobs(
        &self,
        path: &Path,
        value: &serde_yaml::Value,
        validation: &mut Vec<Validation>,
    ) -> Result<()> {
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
                            if let Some(expected) = self.actions.get_latest(name) {
                                if expected != version {
                                    validation.push(Validation::OutdatedAction {
                                        path: path.to_owned(),
                                        name: name.into(),
                                        actual: version.into(),
                                        expected: expected.into(),
                                    });
                                }
                            }

                            if let Some(reason) = self.actions.get_deny(name) {
                                validation.push(Validation::DeniedAction {
                                    path: path.to_owned(),
                                    name: name.into(),
                                    reason: reason.into(),
                                });
                            }

                            if let Some(check) = self.actions.get_check(name) {
                                if let Err(reason) = check.check(action) {
                                    validation.push(Validation::CustomActionsCheck {
                                        path: path.to_owned(),
                                        name: name.into(),
                                        reason: reason.into(),
                                    });
                                }
                            }
                        }
                    }
                }

                if !self.workspace {
                    self.verify_single_project_build(path, job, validation);
                }
            }
        }

        Ok(())
    }

    fn verify_single_project_build(
        &self,
        path: &Path,
        job: &serde_yaml::Value,
        validation: &mut Vec<Validation>,
    ) {
        let mut cargo_combos = Vec::new();
        let features = self.cargo.features();

        for step in job
            .get("steps")
            .and_then(|v| v.as_sequence())
            .into_iter()
            .flatten()
        {
            match step.get("run").and_then(|v| v.as_str()) {
                Some(command) => {
                    let identity = self.identify_command(command, &features);

                    match identity {
                        RunIdentity::Cargo(cargo) => {
                            for feature in &cargo.missing_features {
                                validation.push(Validation::MissingFeature {
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
                        validation.push(Validation::NoFeatures {
                            path: path.to_owned(),
                        });
                    }
                }
            } else {
                self.ensure_feature_combo(path, &cargo_combos, validation);
            }
        }
    }

    fn identify_command(&self, command: &str, features: &HashSet<String>) -> RunIdentity {
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

            let (cargo_features, missing_features, features_list) =
                self.process_features(it, features);

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
        &self,
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

    /// Ensure that feature combination is valid.
    fn ensure_feature_combo(
        &self,
        path: &Path,
        cargos: &[Cargo],
        validation: &mut Vec<Validation>,
    ) -> bool {
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
            validation.push(Validation::MissingEmptyFeatures {
                path: path.to_owned(),
            });
        }

        if !all_features {
            validation.push(Validation::MissingAllFeatures {
                path: path.to_owned(),
            });
        }

        false
    }
}

enum CargoFeatures {
    Default,
    NoDefaultFeatures,
    AllFeatures,
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

pub(crate) struct Readme<'a> {
    pub(crate) path: PathBuf,
    pub(crate) lib_rs: PathBuf,
    pub(crate) badges: &'a Badges<'a>,
    pub(crate) params: &'a Params,
}

impl<'a> Readme<'a> {
    /// Construct a new README config.
    pub(crate) fn new(
        path: PathBuf,
        lib_rs: PathBuf,
        badges: &'a Badges<'a>,
        params: &'a Params,
    ) -> Self {
        Self {
            path,
            lib_rs,
            badges,
            params,
        }
    }

    /// Validate the current model.
    pub(crate) fn validate(&self, validation: &mut Vec<Validation>) -> Result<()> {
        if !self.path.is_file() {
            validation.push(Validation::MissingReadme {
                path: self.path.clone(),
            });
        }

        if self.lib_rs.is_file() {
            let (file, new_file) = self.process_lib_rs()?;

            let checks = markdown_checks(&file)?;

            for (file, range) in checks.toplevel_headings {
                validation.push(Validation::ToplevelHeadings {
                    path: self.lib_rs.clone(),
                    file,
                    range,
                    line_offset: checks.line_offset,
                });
            }

            for (file, range) in checks.missing_preceeding_br {
                validation.push(Validation::MissingPreceedingBr {
                    path: self.lib_rs.clone(),
                    file,
                    range,
                    line_offset: checks.line_offset,
                });
            }

            let readme_from_lib_rs = readme_from_lib_rs(&new_file, self.params)?;

            if *file != *new_file {
                validation.push(Validation::MismatchedLibRs {
                    path: self.lib_rs.clone(),
                    new_file: new_file.clone(),
                });
            }

            let readme = match File::read(&self.path) {
                Ok(file) => file,
                Err(e) if e.kind() == io::ErrorKind::NotFound => File::new(),
                Err(e) => return Err(e.into()),
            };

            if readme != readme_from_lib_rs {
                validation.push(Validation::BadReadme {
                    path: self.path.clone(),
                    new_file: Rc::new(readme_from_lib_rs),
                });
            }
        }

        Ok(())
    }

    /// Process the lib rs.
    fn process_lib_rs(&self) -> Result<(Rc<File>, Rc<File>), anyhow::Error> {
        let lib_rs = File::read(&self.lib_rs)?;
        let mut new_file = File::new();

        for badge in self.badges.iter() {
            let string = badge.build(self.params)?;
            new_file.push(format!("//! {string}").as_bytes());
        }

        for line in lib_rs.lines() {
            if line
                .as_rust_comment()
                .filter(|comment| is_badge_comment(comment))
                .is_some()
            {
                continue;
            }

            let bytes = line.as_bytes();
            let bytes = trim_ascii_end(bytes);
            new_file.push(bytes);
        }

        return Ok((Rc::new(lib_rs), Rc::new(new_file)));

        pub const fn trim_ascii_end(mut bytes: &[u8]) -> &[u8] {
            while let [rest @ .., last] = bytes {
                if last.is_ascii_whitespace() {
                    bytes = rest;
                } else {
                    break;
                }
            }

            bytes
        }
    }
}

/// Test if line is a badge comment.
fn is_badge_comment(c: &[u8]) -> bool {
    if c.starts_with(b" [<img ") && c.ends_with(b")") {
        return true;
    }

    if c.starts_with(b" [![") && c.ends_with(b")") {
        return true;
    }

    false
}

#[derive(Default)]
struct MarkdownChecks {
    line_offset: usize,
    toplevel_headings: Vec<(Rc<File>, Range<usize>)>,
    missing_preceeding_br: Vec<(Rc<File>, Range<usize>)>,
}

/// Test if the specified file has toplevel headings.
fn markdown_checks(file: &File) -> Result<MarkdownChecks> {
    use pulldown_cmark::{Event, HeadingLevel, Parser, Tag};

    let mut comment = Vec::new();

    let mut initial = true;
    let mut checks = MarkdownChecks::default();

    for (offset, line) in file.lines().enumerate() {
        if initial {
            checks.line_offset = offset + 1;
        }

        if let Some(line) = line.as_rust_comment() {
            comment.push(std::str::from_utf8(line)?);
            initial = false;
        }
    }

    let comment = comment.join("\n");
    let file = Rc::new(File::from_vec(comment.as_bytes().to_vec()));

    let parser = Parser::new(&comment);
    let mut preceeding_newline = false;

    for (event, range) in parser.into_offset_iter() {
        match event {
            Event::Html(html) => {
                if html.trim() == "<br>" {
                    preceeding_newline = true;
                    continue;
                }
            }
            Event::Start(tag) => match tag {
                Tag::Heading(level, _, _) => {
                    if !preceeding_newline {
                        checks
                            .missing_preceeding_br
                            .push((file.clone(), range.clone()));
                    }

                    if matches!(level, HeadingLevel::H1) {
                        checks.toplevel_headings.push((file.clone(), range.clone()));
                    }
                }
                _ => {}
            },
            _ => {}
        }

        preceeding_newline = false;
    }

    Ok(checks)
}

/// Generate a readme.
fn readme_from_lib_rs(file: &File, params: &Params) -> Result<File> {
    let mut readme = File::new();

    let mut in_code_block = None::<bool>;
    let crate_name = &params.crate_name;

    readme.push(format!("# {crate_name}").as_bytes());
    readme.push(b"");

    for line in file.lines() {
        let comment = match line.as_rust_comment() {
            Some(comment) => std::str::from_utf8(comment)?,
            None => {
                continue;
            }
        };

        let comment = if let Some(" ") = comment.get(..1) {
            comment.get(1..).unwrap_or_default()
        } else {
            comment
        };

        if in_code_block == Some(true) && comment.trim_start().starts_with("# ") {
            continue;
        }

        if comment.starts_with("```") {
            if in_code_block.is_none() {
                let (parts, specs) = filter_code_block(comment);
                readme.push(format!("```{parts}").as_bytes());
                in_code_block = Some(specs.contains("rust"));
                continue;
            }

            in_code_block = None;
        }

        readme.push(comment.as_bytes());
    }

    readme.ensure_trailing_newline();
    Ok(readme)
}

/// Filter code block fragments.
fn filter_code_block(comment: &str) -> (String, BTreeSet<String>) {
    let parts = comment.get(3..).unwrap_or_default();
    let mut out = BTreeSet::new();

    for part in parts.split(',') {
        let part = part.trim();

        match part {
            "" => continue,
            "no_run" => continue,
            "should_panic" => continue,
            "ignore" => continue,
            "edition2018" => continue,
            "edition2021" => continue,
            _ => {}
        }

        out.insert(part.to_owned());
    }

    if out.is_empty() {
        out.insert(String::from("rust"));
    }

    (out.iter().cloned().collect::<Vec<_>>().join(","), out)
}

/// Update parameters.
pub(crate) struct UpdateParams<'a> {
    pub(crate) license: &'a str,
    pub(crate) readme: &'a str,
    pub(crate) repository: &'a str,
    pub(crate) homepage: &'a str,
    pub(crate) documentation: &'a str,
    pub(crate) authors: &'a [String],
}

/// Validate the main `Cargo.toml`.
pub(crate) fn work_cargo_toml(
    path: &Path,
    cargo: &CargoToml,
    validation: &mut Vec<Validation>,
    update: &UpdateParams<'_>,
) -> Result<()> {
    let mut modified_cargo = cargo.clone();
    let mut issues = Vec::new();
    let mut changed = false;

    macro_rules! check {
        ($get:ident, $insert:ident, $missing:ident, $wrong:ident) => {
            match cargo.$get()? {
                None => {
                    modified_cargo.$insert(update.$get)?;
                    issues.push(CargoIssue::$missing);
                    changed = true;
                }
                Some(value) if value != update.$get => {
                    modified_cargo.$insert(update.$get)?;
                    issues.push(CargoIssue::$wrong);
                    changed = true;
                }
                _ => {}
            }
        };
    }

    check! {
        license,
        insert_license,
        MissingPackageLicense,
        WrongPackageLicense
    };

    check! {
        readme,
        insert_readme,
        MissingPackageReadme,
        WrongPackageReadme
    };

    check! {
        repository,
        insert_repository,
        MissingPackageRepository,
        WrongPackageRepository
    };

    check! {
        homepage,
        insert_homepage,
        MissingPackageHomepage,
        WrongPackageHomepage
    };

    check! {
        documentation,
        insert_documentation,
        MissingPackageDocumentation,
        WrongPackageDocumentation
    };

    if cargo.description()?.is_none() {
        issues.push(CargoIssue::PackageDescription);
    }

    if cargo
        .categories()?
        .filter(|value| !value.is_empty())
        .is_none()
    {
        issues.push(CargoIssue::PackageCategories);
    }

    if cargo
        .keywords()?
        .filter(|value| !value.is_empty())
        .is_none()
    {
        issues.push(CargoIssue::PackageKeywords);
    }

    if cargo
        .authors()?
        .filter(|authors| !authors.is_empty())
        .is_none()
    {
        issues.push(CargoIssue::PackageAuthorsEmpty);
        changed = true;
        modified_cargo.insert_authors(update.authors.to_vec())?;
    }

    if matches!(cargo.dependencies(), Some(d) if d.is_empty()) {
        issues.push(CargoIssue::PackageDependenciesEmpty);
        changed = true;
        modified_cargo.remove_dependencies();
    }

    if matches!(cargo.dev_dependencies(), Some(d) if d.is_empty()) {
        issues.push(CargoIssue::PackageDevDependenciesEmpty);
        changed = true;
        modified_cargo.remove_dev_dependencies();
    }

    if matches!(cargo.build_dependencies(), Some(d) if d.is_empty()) {
        issues.push(CargoIssue::PackageBuildDependenciesEmpty);
        changed = true;
        modified_cargo.remove_build_dependencies();
    }

    let package = modified_cargo.package()?;
    let mut keys = Vec::new();

    for (key, _) in package.iter() {
        if let Some(key) = cargo_key(key) {
            keys.push(key);
        }
    }

    let mut sorted_keys = keys.clone();
    sorted_keys.sort();

    if keys != sorted_keys {
        issues.push(CargoIssue::KeysNotSorted {
            actual: keys,
            expected: sorted_keys,
        });
        modified_cargo.sort_package_keys(|a, _, b, _| {
            let a = cargo_key(a.to_string().trim())
                .map(SortKey::CargoKey)
                .unwrap_or(SortKey::Other(a));
            let b = cargo_key(b.to_string().trim())
                .map(SortKey::CargoKey)
                .unwrap_or(SortKey::Other(b));
            a.cmp(&b)
        })?;
        changed = true;
    }

    if let Some(description) = modified_cargo.description() {}

    if !issues.is_empty() {
        validation.push(Validation::CargoTomlIssues {
            path: path.into(),
            cargo: changed.then_some(modified_cargo),
            issues,
        });
    }

    return Ok(());

    #[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
    enum SortKey<'a> {
        CargoKey(CargoKey),
        Other(&'a Key),
    }
}
