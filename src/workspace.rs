use std::collections::VecDeque;
use std::path::{Path, PathBuf};

use crate::cargo::{self, CargoToml};
use anyhow::{anyhow, Context, Result};
use relative_path::{RelativePath, RelativePathBuf};

/// Load a workspace starting at the given path.
pub(crate) fn open<P>(path: P) -> Result<Workspace>
where
    P: AsRef<Path>,
{
    let package = cargo::open(path.as_ref())?;
    let root = path
        .as_ref()
        .parent()
        .ok_or_else(|| anyhow!("missing parent directory"))?;

    let mut queue = VecDeque::new();
    queue.push_back((root.to_owned(), path.as_ref().to_owned(), package));

    let mut workspace = Workspace::default();

    while let Some((root, path, package)) = queue.pop_front() {
        if let Some(workspace) = package.workspace() {
            let members = expand_members(&root, workspace.members())?;

            for member in members {
                let root = member.to_path(&root);
                let path = root.join("Cargo.toml");
                let package = cargo::open(&path).with_context(|| anyhow!("{}", path.display()))?;
                queue.push_back((root, path, package));
            }
        }

        if package.is_package() {
            workspace.packages.push((root, path, package));
        }
    }

    Ok(workspace)
}

fn expand_members<'a>(
    root: &Path,
    iter: impl Iterator<Item = &'a RelativePath>,
) -> Result<Vec<RelativePathBuf>> {
    let mut output = Vec::new();
    let mut queue = VecDeque::new();

    for path in iter {
        queue.push_back(path.to_owned());
    }

    'outer: while let Some(p) = queue.pop_front() {
        let mut cur = RelativePathBuf::new();
        let mut it = p.components();

        while let Some(c) = it.next() {
            if c.as_str() == "*" {
                let dirs = std::fs::read_dir(cur.to_path(root))?;

                for e in dirs {
                    let e = e?;

                    if let Some(c) = e.file_name().to_str() {
                        let mut new = cur.clone();
                        new.push(c);
                        new.push(it.as_relative_path());
                        queue.push_back(new);
                    }
                }

                continue 'outer;
            }

            cur.push(c.as_str());
        }

        output.push(cur);
    }

    Ok(output)
}

#[derive(Default)]
pub(crate) struct Workspace {
    packages: Vec<(PathBuf, PathBuf, CargoToml)>,
}

impl Workspace {
    /// Test if this is a single crate workspace.
    pub(crate) fn is_single_crate(&self) -> bool {
        self.packages.len() == 1
    }

    /// Get list of packages.
    pub(crate) fn packages<'a>(
        &'a self,
    ) -> impl Iterator<Item = (&'a Path, &'a Path, &'a CargoToml)> {
        self.packages
            .iter()
            .map(|(root, path, package)| (root.as_path(), path.as_path(), package))
    }

    /// Find the primary crate in the workspace.
    pub(crate) fn primary_crate(
        &self,
        name: Option<&str>,
    ) -> Result<Option<(&Path, &Path, &CargoToml)>> {
        // Single package, easy to determine primary crate.
        if let [(root, path, single)] = &self.packages[..] {
            return Ok(Some((root, path, single)));
        }

        // Find a package which matches the name of the project.
        if let Some(name) = name {
            for (root, path, package) in &self.packages {
                if package.crate_name()? == name {
                    return Ok(Some((root, path, package)));
                }
            }
        }

        Ok(None)
    }
}
