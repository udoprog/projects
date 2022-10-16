use std::path::Path;

use anyhow::Result;

/// A file loaded into memory.
#[derive(Debug, Clone, PartialEq)]
pub(crate) struct File {
    data: Vec<u8>,
}

impl File {
    /// Construct a new empty file.
    pub(crate) const fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Load a file from the given path.
    pub(crate) fn read<P>(path: P) -> Result<Self>
    where
        P: AsRef<Path>,
    {
        let bytes = std::fs::read(path)?;
        Ok(Self::from_vec(bytes))
    }

    /// Construct a new rust file wrapper.
    pub(crate) fn from_vec(data: Vec<u8>) -> Self {
        Self {
            data: data.to_vec(),
        }
    }

    /// Get bytes of the file.
    pub(crate) fn as_bytes(&self) -> &[u8] {
        &self.data
    }

    /// Iterate over comments.
    pub(crate) fn lines(&self) -> Lines<'_> {
        Lines {
            iter: self.data.split(|b| *b == b'\n'),
        }
    }

    /// Push a line onto the file.
    pub(crate) fn push(&mut self, line: &[u8]) {
        if !self.data.is_empty() {
            self.data.push(b'\n');
        }

        self.data.extend(line);
    }

    /// Ensure that file has a trailing newline.
    pub(crate) fn ensure_trailing_newline(&mut self) {
        if !self.data.is_empty() && !self.data.ends_with(b"\n") {
            self.data.push(b'\n');
        }
    }
}

pub(crate) struct Lines<'a> {
    iter: core::slice::Split<'a, u8, fn(&u8) -> bool>,
}

impl<'a> Iterator for Lines<'a> {
    type Item = Line<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(Line {
            line: self.iter.next()?,
        })
    }
}

pub(crate) struct Line<'a> {
    line: &'a [u8],
}

impl<'a> Line<'a> {
    /// Get underlying bytes for the line.
    pub(crate) fn as_bytes(&self) -> &[u8] {
        self.line
    }

    /// Get the comment.
    pub(crate) fn as_rust_comment(&self) -> Option<&'a [u8]> {
        if self.line.get(..3) == Some(&b"//!"[..]) {
            return self.line.get(3..);
        }

        None
    }
}
