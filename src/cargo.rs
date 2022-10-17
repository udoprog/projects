use std::collections::HashSet;
use std::path::Path;

use anyhow::{anyhow, Result};
use toml_edit::{Array, Document, Formatted, Item, Table, Value};

/// A parsed `Cargo.toml`.
#[derive(Debug, Clone)]
pub(crate) struct CargoToml {
    doc: Document,
}

macro_rules! field {
    ($get:ident, $insert:ident, $field:literal) => {
        pub(crate) fn $get(&self) -> Result<Option<&str>> {
            self.package_value($field, Item::as_str)
        }

        pub(crate) fn $insert(&mut self, $get: &str) -> Result<()> {
            let package = self.package_mut()?;
            package.insert(
                $field,
                Item::Value(Value::String(Formatted::new(String::from($get)))),
            );
            Ok(())
        }
    };
}

macro_rules! dependencies {
    ($get:ident, $remove:ident, $field:literal) => {
        pub(crate) fn $get(&self) -> Option<&Table> {
            self.doc.get($field).and_then(|table| table.as_table())
        }

        pub(crate) fn $remove(&mut self) {
            self.doc.remove($field);
        }
    };
}

impl CargoToml {
    field!(license, insert_license, "license");
    field!(readme, insert_readme, "readme");
    field!(repository, insert_repository, "repository");
    field!(homepage, insert_homepage, "homepage");
    field!(documentation, insert_documentation, "documentation");
    dependencies!(dependencies, remove_dependencies, "dependencies");
    dependencies!(
        dev_dependencies,
        remove_dev_dependencies,
        "dev-dependencies"
    );
    dependencies!(
        build_dependencies,
        remove_build_dependencies,
        "build-dependencies"
    );

    /// Get categories.
    pub(crate) fn categories(&self) -> Result<Option<&Array>> {
        self.package_value("categories", Item::as_array)
    }

    /// Get keywords.
    pub(crate) fn keywords(&self) -> Result<Option<&Array>> {
        self.package_value("keywords", Item::as_array)
    }

    /// Get description.
    pub(crate) fn description(&self) -> Result<Option<&str>> {
        self.package_value("description", Item::as_str)
    }

    /// Save to the given path.
    pub(crate) fn save_to<P>(&self, path: P) -> Result<()>
    where
        P: AsRef<Path>,
    {
        let string = self.doc.to_string();
        std::fs::write(path, string.as_bytes())?;
        Ok(())
    }

    /// Get the name of the crate.
    pub(crate) fn crate_name(&self) -> Result<&str> {
        let package = self.package()?;

        let name = package
            .get("name")
            .and_then(|item| item.as_str())
            .ok_or_else(|| anyhow!("missing `[package] name`"))?;

        Ok(name)
    }

    /// List of features.
    pub(crate) fn features(&self) -> HashSet<String> {
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
        if let Some(table) = self.dependencies() {
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

        new_features
    }

    /// Access `[package]` section.
    fn package(&self) -> Result<&Table> {
        self.doc
            .get("package")
            .and_then(|table| table.as_table())
            .ok_or_else(|| anyhow!("missing `[package]`"))
    }

    /// Access `[package]` section mutably.
    fn package_mut(&mut self) -> Result<&mut Table> {
        self.doc
            .get_mut("package")
            .and_then(|table| table.as_table_mut())
            .ok_or_else(|| anyhow!("missing `[package]`"))
    }

    /// Access a package value.
    fn package_value<T, O: ?Sized>(&self, name: &str, map: T) -> Result<Option<&O>>
    where
        T: FnOnce(&Item) -> Option<&O>,
    {
        Ok(self.package()?.get(name).and_then(map))
    }
}

/// Open a `Cargo.toml`.
pub(crate) fn open(path: &Path) -> Result<CargoToml> {
    let input = std::fs::read_to_string(path)?;
    let doc = input.parse()?;
    Ok(CargoToml { doc })
}
