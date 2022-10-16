use std::collections::BTreeSet;
use std::fs;
use std::ops::Range;
use std::path::{Path, PathBuf};
use std::rc::Rc;

use crate::actions::Actions;
use crate::badges::Badges;
use crate::file::File;
use crate::params::Params;
use anyhow::{anyhow, Context, Result};

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
}

pub(crate) struct Ci<'a> {
    pub(crate) path: PathBuf,
    pub(crate) name: String,
    actions: &'a Actions<'a>,
}

impl<'a> Ci<'a> {
    /// Construct a new CI config.
    pub(crate) fn new(path: PathBuf, name: String, actions: &'a Actions<'a>) -> Self {
        Self {
            path,
            name,
            actions,
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
            for (_, value) in jobs {
                for action in value
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
            }
        }

        Ok(())
    }
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

            let readme = File::read(&self.path)?;

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
        let lib_rs = std::fs::read(&self.lib_rs)?;
        let file = crate::file::File::from_vec(lib_rs);
        let mut new_file = crate::file::File::new();
        let mut skipping = false;
        let mut done_skipping = false;

        for line in file.lines() {
            if let Some(comment) = line.as_rust_comment() {
                if !done_skipping && is_badge_comment(comment) {
                    if skipping {
                        continue;
                    }

                    for badge in self.badges.iter() {
                        let string = badge.build(self.params)?;
                        new_file.push(format!("//! {string}").as_bytes());
                    }

                    skipping = true;
                    continue;
                } else {
                    done_skipping = true;
                }
            }

            let bytes = line.as_bytes();
            let bytes = trim_ascii_end(bytes);
            new_file.push(bytes);
        }

        return Ok((Rc::new(file), Rc::new(new_file)));

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
    if c.starts_with(b" [<img alt=") && c.ends_with(b")") {
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
                        dbg!(level);
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
