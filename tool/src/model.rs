use std::fs;
use std::path::PathBuf;

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
}

pub(crate) struct Model<'a> {
    pub(crate) name: &'a str,
    pub(crate) ci: Ci,
    pub(crate) readme: Readme,
}

impl Model<'_> {
    /// Validate the current model.
    pub(crate) fn validate(&self, errors: &mut Vec<Validation>) -> Result<()> {
        self.ci.validate(errors)?;
        Ok(())
    }
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
                path: expected_path.clone(),
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

pub(crate) struct Readme {
    pub(crate) path: PathBuf,
}

impl Readme {
    /// Construct a new README config.
    pub(crate) fn new(path: PathBuf) -> Self {
        Self { path }
    }

    /// Validate the current model.
    pub(crate) fn validate(&self, validation: &mut Vec<Validation>) -> Result<()> {
        if !self.path.is_file() {
            validation.push(Validation::MissingReadme {
                path: self.path.clone(),
            });

            return Ok(());
        }

        Ok(())
    }
}
