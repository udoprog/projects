use std::path::Path;

use anyhow::{anyhow, Result};
use toml_edit::Document;

/// A parsed `Cargo.toml`.
pub(crate) struct CargoToml {
    doc: Document,
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
}

/// Open a `Cargo.toml`.
pub(crate) fn open(path: &Path) -> Result<CargoToml> {
    let input = std::fs::read_to_string(path)?;
    let doc = input.parse()?;

    Ok(CargoToml { doc })
}
