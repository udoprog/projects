use std::path::Path;
use std::process::{Command, Stdio};

use anyhow::{anyhow, Result};

pub(crate) fn head_commit(current_dir: &Path) -> Result<String> {
    let output = Command::new("git")
        .args(["rev-parse", "HEAD"])
        .stdout(Stdio::piped())
        .current_dir(&current_dir)
        .output()?;

    if !output.status.success() {
        return Err(anyhow!("status: {}", output.status));
    }

    Ok(String::from_utf8(output.stdout)?)
}
