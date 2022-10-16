use std::cell::RefCell;
use std::collections::HashSet;
use std::path::Path;

use anyhow::{anyhow, Result};
use toml_edit::Document;

/// A parsed `Cargo.toml`.
pub(crate) struct CargoToml {
    doc: Document,
    features: RefCell<Option<HashSet<String>>>,
}

impl CargoToml {
    /// Get the name of the crate.
    pub(crate) fn crate_name(&self) -> Result<&str> {
        let package = self
            .doc
            .get("package")
            .and_then(|table| table.as_table())
            .ok_or_else(|| anyhow!("missing `[package]`"))?;

        let name = package
            .get("name")
            .and_then(|item| item.as_str())
            .ok_or_else(|| anyhow!("missing `[package] name`"))?;

        Ok(name)
    }

    /// List of features.
    pub(crate) fn features(&self) -> HashSet<String> {
        let mut features = self.features.borrow_mut();

        if let Some(features) = &*features {
            return features.clone();
        }

        let mut new_features = HashSet::new();

        // Get explicit features.
        if let Some(table) = self.doc.get("features").and_then(|v| v.as_table()) {
            new_features.extend(
                table
                    .iter()
                    .filter(|(key, _)| *key != "default")
                    .map(|(key, _)| String::from(key)),
            );
        }

        // Get features from optional dependencies.
        if let Some(table) = self.doc.get("dependencies").and_then(|v| v.as_table()) {
            for (key, value) in table.iter() {
                let package = if let Some(package) = value.get("package").and_then(|v| v.as_str()) {
                    package
                } else {
                    key
                };

                if value
                    .get("optional")
                    .and_then(|v| v.as_bool())
                    .filter(|v| *v)
                    .is_some()
                {
                    new_features.insert(package.to_owned());
                }
            }
        }

        features.get_or_insert(new_features.into()).clone()
    }
}

/// Open a `Cargo.toml`.
pub(crate) fn open(path: &Path) -> Result<CargoToml> {
    let input = std::fs::read_to_string(path)?;
    let doc = input.parse()?;

    Ok(CargoToml {
        doc,
        features: RefCell::new(None),
    })
}
