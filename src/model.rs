use std::collections::{BTreeSet, HashSet};
use std::fmt;
use std::fs;
use std::io;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;

use anyhow::{anyhow, Context, Result};
use pulldown_cmark::{LinkType, Options};
use relative_path::{RelativePath, RelativePathBuf};
use serde::Serialize;
use toml_edit::Key;
use url::Url;

use crate::actions::Actions;
use crate::badges::Badges;
use crate::cargo::Manifest;
use crate::config::Config;
use crate::file::File;
use crate::urls::Urls;
use crate::workspace::Package;

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
    PackageCategoriesNotSorted => write!(f, "package.categories: not sorted"),
    PackageKeywords => write!(f, "package.keywords: missing"),
    PackageKeywordsNotSorted => write!(f, "package.keywords: not sorted"),
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
    Resolver => "resolver",
}

pub(crate) enum Validation {
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
}

pub(crate) struct Ci<'a> {
    path: &'a RelativePath,
    name: &'a str,
    actions: &'a Actions<'a>,
    manifest: &'a Manifest,
    workspace: bool,
}

impl<'a> Ci<'a> {
    /// Construct a new CI config.
    pub(crate) fn new(
        path: &'a RelativePath,
        name: &'a str,
        actions: &'a Actions<'a>,
        manifest: &'a Manifest,
        workspace: bool,
    ) -> Self {
        Self {
            path,
            name,
            actions,
            manifest,
            workspace,
        }
    }

    /// Get candidates.
    fn candidates(&self, root: &Path) -> io::Result<Box<[RelativePathBuf]>> {
        let dir = match fs::read_dir(self.path.to_path(root)) {
            Ok(dir) => dir,
            Err(e) if e.kind() == io::ErrorKind::NotFound => return Ok(Box::from([])),
            Err(e) => return Err(e.into()),
        };

        let mut paths = Vec::new();

        for e in dir {
            let e = e?;

            if let Some(name) = e.file_name().to_str() {
                paths.push(self.path.join(name));
            }
        }

        Ok(paths.into())
    }

    /// Validate the current model.
    pub(crate) fn validate(&self, root: &Path, validation: &mut Vec<Validation>) -> Result<()> {
        let deprecated_yml = self.path.join("rust.yml");
        let expected_path = self.path.join("ci.yml");

        let candidates = self
            .candidates(root)
            .with_context(|| anyhow!("list candidates: {path}", path = self.path))?;

        let path = if !expected_path.to_path(root).is_file() {
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

        if deprecated_yml.to_path(root).is_file() && candidates.len() > 1 {
            validation.push(Validation::DeprecatedWorkflow {
                path: deprecated_yml,
            });
        }

        let bytes = std::fs::read(path.to_path(root))?;
        let value: serde_yaml::Value =
            serde_yaml::from_slice(&bytes).with_context(|| anyhow!("{path}"))?;

        let name = value
            .get("name")
            .and_then(|name| name.as_str())
            .ok_or_else(|| anyhow!("{path}: missing .name"))?;

        if name != self.name {
            validation.push(Validation::WrongWorkflowName {
                path: path.clone(),
                actual: name.to_owned(),
                expected: self.name.to_owned(),
            });
        }

        self.validate_jobs(&path, &value, validation)?;
        Ok(())
    }

    /// Validate that jobs are modern.
    fn validate_jobs(
        &self,
        path: &RelativePath,
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
        path: &RelativePath,
        job: &serde_yaml::Value,
        validation: &mut Vec<Validation>,
    ) {
        let mut cargo_combos = Vec::new();
        let features = self.manifest.features();

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
        path: &RelativePath,
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

/// Badge building parameters.
#[derive(Debug, Clone, Copy, Serialize)]
pub(crate) struct CrateParams<'a> {
    pub(crate) repo: &'a str,
    pub(crate) name: &'a str,
}

pub(crate) struct Readme<'a> {
    pub(crate) path: &'a RelativePath,
    pub(crate) lib_rs: &'a RelativePath,
    pub(crate) badges: &'a Badges<'a>,
    pub(crate) crate_params: CrateParams<'a>,
    pub(crate) config: &'a Config,
}

impl<'a> Readme<'a> {
    /// Construct a new README config.
    pub(crate) fn new(
        path: &'a RelativePath,
        lib_rs: &'a RelativePath,
        badges: &'a Badges,
        crate_params: CrateParams<'a>,
        config: &'a Config,
    ) -> Self {
        Self {
            path,
            lib_rs,
            badges,
            crate_params,
            config,
        }
    }

    /// Validate the current model.
    pub(crate) fn validate(
        &self,
        root: &Path,
        validation: &mut Vec<Validation>,
        urls: &mut Urls,
    ) -> Result<()> {
        if !self.path.to_path(root).is_file() {
            validation.push(Validation::MissingReadme {
                path: self.path.to_owned(),
            });
        }

        if self.lib_rs.to_path(root).is_file() {
            let (file, new_file) = self.process_lib_rs(root)?;
            let checks = self.markdown_checks(&file, urls)?;

            for (file, range) in checks.toplevel_headings {
                validation.push(Validation::ToplevelHeadings {
                    path: self.lib_rs.to_owned(),
                    file,
                    range,
                    line_offset: checks.line_offset,
                });
            }

            for (file, range) in checks.missing_preceeding_br {
                validation.push(Validation::MissingPreceedingBr {
                    path: self.lib_rs.to_owned(),
                    file,
                    range,
                    line_offset: checks.line_offset,
                });
            }

            let readme_from_lib_rs = readme_from_lib_rs(&new_file, self.crate_params)?;

            if *file != *new_file {
                validation.push(Validation::MismatchedLibRs {
                    path: self.lib_rs.to_owned(),
                    new_file: new_file.clone(),
                });
            }

            let readme = match File::read(self.path.to_path(root)) {
                Ok(file) => file,
                Err(e) if e.kind() == io::ErrorKind::NotFound => File::new(),
                Err(e) => return Err(e.into()),
            };

            if readme != readme_from_lib_rs {
                validation.push(Validation::BadReadme {
                    path: self.path.to_owned(),
                    new_file: Arc::new(readme_from_lib_rs),
                });
            }
        }

        Ok(())
    }

    /// Process the lib rs.
    fn process_lib_rs(&self, root: &Path) -> Result<(Arc<File>, Arc<File>), anyhow::Error> {
        let lib_rs = File::read(self.lib_rs.to_path(root))?;
        let mut new_file = File::new();

        for badge in self.badges.iter() {
            let string = badge.build(self.crate_params, self.config)?;
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

        return Ok((Arc::new(lib_rs), Arc::new(new_file)));

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

    /// Test if the specified file has toplevel headings.
    fn markdown_checks(&self, file: &Arc<File>, urls: &mut Urls) -> Result<MarkdownChecks> {
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
        let file = Arc::new(File::from_vec(comment.as_bytes().to_vec()));

        let opts = Options::empty();

        let parser = Parser::new_with_broken_link_callback(&comment, opts, None);
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
                    Tag::Link(LinkType::Autolink, href, _) => {
                        self.visit_url(href.as_ref(), &file, &range, &checks, urls)?;
                    }
                    Tag::Link(LinkType::Inline, href, _) => {
                        self.visit_url(href.as_ref(), &file, &range, &checks, urls)?;
                    }
                    Tag::Link(LinkType::Shortcut, href, _) => {
                        self.visit_url(href.as_ref(), &file, &range, &checks, urls)?;
                    }
                    _ => {}
                },
                _ => {}
            }

            preceeding_newline = false;
        }

        Ok(checks)
    }

    /// Insert an URL.
    fn visit_url(
        &self,
        url: &str,
        file: &Arc<File>,
        range: &Range<usize>,
        checks: &MarkdownChecks,
        urls: &mut Urls,
    ) -> Result<()> {
        // Link to anchor does nothing.
        if url.starts_with('#') {
            return Ok(());
        }

        let error = match str::parse::<Url>(url) {
            Ok(url) if matches!(url.scheme(), "http" | "https") => {
                urls.insert(
                    url,
                    file.clone(),
                    range.clone(),
                    self.lib_rs,
                    checks.line_offset,
                );

                return Ok(());
            }
            Ok(url) => anyhow!("only 'http://' or 'https://' urls are supported, got `{url}`"),
            Err(e) => e.into(),
        };

        urls.insert_bad_url(
            url.to_owned(),
            error,
            file.clone(),
            range.clone(),
            self.lib_rs,
            checks.line_offset,
        );

        Ok(())
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
    toplevel_headings: Vec<(Arc<File>, Range<usize>)>,
    missing_preceeding_br: Vec<(Arc<File>, Range<usize>)>,
}

/// Generate a readme.
fn readme_from_lib_rs(file: &File, crate_params: CrateParams<'_>) -> Result<File> {
    let mut readme = File::new();

    let mut in_code_block = None::<bool>;
    let name = crate_params.name;

    readme.push(format!("# {name}").as_bytes());
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
    package: &Package,
    validation: &mut Vec<Validation>,
    update: &UpdateParams<'_>,
) -> Result<()> {
    let mut modified_manifest = package.manifest.clone();
    let mut issues = Vec::new();
    let mut changed = false;

    macro_rules! check {
        ($get:ident, $insert:ident, $missing:ident, $wrong:ident) => {
            match package.manifest.$get()? {
                None => {
                    modified_manifest.$insert(update.$get)?;
                    issues.push(CargoIssue::$missing);
                    changed = true;
                }
                Some(value) if value != update.$get => {
                    modified_manifest.$insert(update.$get)?;
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

    if package.manifest.description()?.is_none() {
        issues.push(CargoIssue::PackageDescription);
    }

    if let Some(categories) = package
        .manifest
        .categories()?
        .filter(|value| !value.is_empty())
    {
        let categories = categories
            .iter()
            .flat_map(|v| Some(v.as_str()?.to_owned()))
            .collect::<Vec<_>>();
        let mut sorted = categories.clone();
        sorted.sort();

        if categories != sorted {
            issues.push(CargoIssue::PackageCategoriesNotSorted);
            changed = true;
            modified_manifest.insert_categories(sorted)?;
        }
    } else {
        issues.push(CargoIssue::PackageCategories);
    }

    if let Some(keywords) = package
        .manifest
        .keywords()?
        .filter(|value| !value.is_empty())
    {
        let keywords = keywords
            .iter()
            .flat_map(|v| Some(v.as_str()?.to_owned()))
            .collect::<Vec<_>>();
        let mut sorted = keywords.clone();
        sorted.sort();

        if keywords != sorted {
            issues.push(CargoIssue::PackageKeywordsNotSorted);
            changed = true;
            modified_manifest.insert_keywords(sorted)?;
        }
    } else {
        issues.push(CargoIssue::PackageKeywords);
    }

    if package
        .manifest
        .authors()?
        .filter(|authors| !authors.is_empty())
        .is_none()
    {
        issues.push(CargoIssue::PackageAuthorsEmpty);
        changed = true;
        modified_manifest.insert_authors(update.authors.to_vec())?;
    }

    if matches!(package.manifest.dependencies(), Some(d) if d.is_empty()) {
        issues.push(CargoIssue::PackageDependenciesEmpty);
        changed = true;
        modified_manifest.remove_dependencies();
    }

    if matches!(package.manifest.dev_dependencies(), Some(d) if d.is_empty()) {
        issues.push(CargoIssue::PackageDevDependenciesEmpty);
        changed = true;
        modified_manifest.remove_dev_dependencies();
    }

    if matches!(package.manifest.build_dependencies(), Some(d) if d.is_empty()) {
        issues.push(CargoIssue::PackageBuildDependenciesEmpty);
        changed = true;
        modified_manifest.remove_build_dependencies();
    }

    {
        let package = modified_manifest.ensure_package()?;
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
            modified_manifest.sort_package_keys(|a, _, b, _| {
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
    }

    if !issues.is_empty() {
        validation.push(Validation::CargoTomlIssues {
            path: package.manifest_path.clone(),
            cargo: changed.then_some(modified_manifest),
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
