use std::collections::VecDeque;
use std::path::Path;

use crate::{
    manifest::{self, Manifest},
    model::{CrateParams, Module},
};
use anyhow::{anyhow, Context, Result};
use relative_path::{RelativePath, RelativePathBuf};

/// The default name of a cargo manifest `Cargo.toml`.
pub(crate) const CARGO_TOML: &str = "Cargo.toml";

/// Load a workspace starting at the given path.
pub(crate) fn open<P>(root: P, manifest_path: &RelativePath) -> Result<Workspace>
where
    P: AsRef<Path>,
{
    let root = root.as_ref();
    let manifest = manifest::open(manifest_path.to_path(root))?;

    let manifest_dir = manifest_path
        .parent()
        .ok_or_else(|| anyhow!("missing parent directory"))?;

    let mut queue = VecDeque::new();

    queue.push_back(Package {
        manifest_dir: manifest_dir.to_owned(),
        manifest_path: manifest_path.to_owned(),
        manifest,
    });

    let mut workspace = Workspace::default();

    while let Some(package) = queue.pop_front() {
        if let Some(workspace) = package.manifest.as_workspace() {
            let members = expand_members(root, &package, workspace.members())?;

            for manifest_dir in members {
                let manifest_path = manifest_dir.join(CARGO_TOML);

                let manifest = manifest::open(manifest_path.to_path(root))
                    .with_context(|| anyhow!("{manifest_path}"))?;

                queue.push_back(Package {
                    manifest_dir,
                    manifest_path,
                    manifest,
                });
            }
        }

        if package.manifest.is_package() {
            workspace.packages.push(package);
        }
    }

    Ok(workspace)
}

fn expand_members<'a>(
    root: &Path,
    package: &Package,
    iter: impl Iterator<Item = &'a RelativePath>,
) -> Result<Vec<RelativePathBuf>> {
    let mut output = Vec::new();
    let mut queue = VecDeque::new();

    for path in iter {
        queue.push_back(package.manifest_dir.join(path));
    }

    'outer: while let Some(p) = queue.pop_front() {
        let mut current = RelativePathBuf::new();
        let mut it = p.components();

        while let Some(c) = it.next() {
            if c.as_str() == "*" {
                let dirs = std::fs::read_dir(current.to_path(root))?;

                for e in dirs {
                    let e = e?;

                    if let Some(c) = e.file_name().to_str() {
                        let mut new = current.clone();
                        new.push(c);
                        new.push(it.as_relative_path());
                        queue.push_back(new);
                    }
                }

                continue 'outer;
            }

            current.push(c.as_str());
        }

        output.push(current);
    }

    Ok(output)
}

/// A single package in the workspace.
pub(crate) struct Package {
    pub(crate) manifest_dir: RelativePathBuf,
    pub(crate) manifest_path: RelativePathBuf,
    pub(crate) manifest: Manifest,
}

impl Package {
    /// Find the location of the entrypoint `lib.rs`.
    pub(crate) fn entries(&self) -> Vec<RelativePathBuf> {
        if let Some(path) = self
            .manifest
            .lib()
            .and_then(|lib| lib.get("path").and_then(|p| p.as_str()))
        {
            vec![self.manifest_dir.join(path)]
        } else {
            vec![
                self.manifest_dir.join("src").join("lib.rs"),
                self.manifest_dir.join("src").join("main.rs"),
            ]
        }
    }

    /// Construct crate parameters.
    pub(crate) fn crate_params<'a>(&'a self, module: &'a Module<'_>) -> Result<CrateParams<'a>> {
        Ok(CrateParams {
            repo: module.repo(),
            name: self.manifest.crate_name()?,
            description: self.manifest.description()?,
        })
    }
}

#[derive(Default)]
pub(crate) struct Workspace {
    packages: Vec<Package>,
}

impl Workspace {
    /// Test if this is a single crate workspace.
    pub(crate) fn is_single_crate(&self) -> bool {
        self.packages.len() == 1
    }

    /// Get list of packages.
    pub(crate) fn packages<'a>(&'a self) -> impl Iterator<Item = &'a Package> {
        self.packages.iter()
    }

    /// Find the primary crate in the workspace.
    pub(crate) fn primary_crate(&self, name: Option<&str>) -> Result<Option<&Package>> {
        // Single package, easy to determine primary crate.
        if let [package] = &self.packages[..] {
            return Ok(Some(package));
        }

        // Find a package which matches the name of the project.
        if let Some(name) = name {
            for package in &self.packages {
                if package.manifest.crate_name()? == name {
                    return Ok(Some(package));
                }
            }
        }

        Ok(None)
    }
}
