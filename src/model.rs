use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

use crate::badges::Badges;
use crate::file::File;
use crate::params::Params;
use anyhow::{anyhow, Context, Result};

pub(crate) enum Validation {
    MissingWorkflows {
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
    MissingReadme {
        path: PathBuf,
    },
    MissingLinks {
        path: PathBuf,
        new_file: File,
    },
    BadReadme {
        path: PathBuf,
        new_file: File,
    },
}

pub(crate) struct Ci {
    pub(crate) path: PathBuf,
    pub(crate) name: String,
}

impl Ci {
    /// Construct a new CI config.
    pub(crate) fn new(path: PathBuf, name: String) -> Self {
        Self { path, name }
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

        let expected_path = self.path.join("ci.yml");

        let path = if !expected_path.is_file() {
            let candidates = self
                .candidates()
                .with_context(|| anyhow!("list candidates: {path}", path = self.path.display()))?;

            let path = match &candidates[..] {
                [path] => Some(path.clone()),
                _ => None,
            };

            validation.push(Validation::MissingWorkflow {
                path: expected_path,
                candidates,
            });

            match path {
                Some(path) => path,
                None => return Ok(()),
            }
        } else {
            expected_path
        };

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

            let readme_from_lib_rs = readme_from_lib_rs(&new_file, self.params)?;

            if file != new_file {
                validation.push(Validation::MissingLinks {
                    path: self.lib_rs.clone(),
                    new_file,
                });
            }

            let readme = File::read(&self.path)?;

            if readme != readme_from_lib_rs {
                validation.push(Validation::BadReadme {
                    path: self.path.clone(),
                    new_file: readme_from_lib_rs,
                });
            }
        }

        Ok(())
    }

    /// Process the lib rs.
    fn process_lib_rs(&self) -> Result<(File, File), anyhow::Error> {
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

            new_file.push(line.as_bytes());
        }

        Ok((file, new_file))
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

/// Generate a readme.
fn readme_from_lib_rs(file: &File, params: &Params) -> Result<File> {
    let mut readme = File::new();

    let mut in_code_block = false;
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

        if in_code_block && comment.trim_start().starts_with("# ") {
            continue;
        }

        if comment.starts_with("```") {
            if !in_code_block {
                let parts = filter_code_block(comment);
                readme.push(format!("```{parts}").as_bytes());
                in_code_block = true;
                continue;
            }

            in_code_block = false;
        }

        readme.push(comment.as_bytes());
    }

    readme.ensure_trailing_newline();
    Ok(readme)
}

/// Filter code block fragments.
fn filter_code_block(comment: &str) -> String {
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

    out.into_iter().collect::<Vec<_>>().join(",")
}
