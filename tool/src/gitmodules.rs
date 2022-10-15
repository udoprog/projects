//! Parsing for .gitmodules files.

use std::{ffi::OsStr, os::unix::prelude::OsStrExt, path::Path};

use thiserror::Error;

/// A git module.
#[derive(Debug, Clone)]
pub(crate) struct GitModule<'a> {
    #[allow(unused)]
    pub(crate) name: &'a str,
    pub(crate) path: Option<&'a Path>,
    #[allow(unused)]
    pub(crate) extra: Vec<(&'a str, &'a [u8])>,
}

/// Parse gitmodules from the given input.
pub(crate) fn parse<'a>(input: &'a [u8]) -> Result<Vec<GitModule<'a>>, ParseError> {
    let mut parser = Parser::new(input);

    let mut modules = Vec::new();

    while let Some(module) = parser.parse_section()? {
        modules.push(module);
    }

    return Ok(modules);
}

struct Parser<'a> {
    input: &'a [u8],
    cursor: usize,
}

impl<'a> Parser<'a> {
    fn new(input: &'a [u8]) -> Self {
        Self { input, cursor: 0 }
    }

    fn get(&mut self, n: usize) -> Option<u8> {
        self.skip_whitespace();
        Some(*self.input.get(self.cursor.checked_add(n)?)?)
    }

    #[inline]
    fn skip(&mut self) {
        self.cursor += 1;
    }

    fn skip_whitespace(&mut self) {
        while matches!(
            self.input.get(self.cursor),
            Some(b' ' | b'\t' | b'\n' | b'\r')
        ) {
            self.skip();
        }
    }

    /// Process value until end-of-line.
    fn until_eol(&mut self) -> Result<&'a [u8], ParseError> {
        let start = self.cursor;

        while !matches!(self.input.get(self.cursor), Some(b'\n') | None) {
            self.skip();
        }

        self.slice(start, self.cursor)
    }

    /// Test expectation.
    fn expect(&mut self, expected: u8) -> Result<(), ParseError> {
        if self.get(0) != Some(expected) {
            return Err(ParseError::Expected {
                expected,
                actual: self.get(0),
            });
        }

        self.skip();
        Ok(())
    }

    /// Get a slice.
    fn slice(&self, from: usize, to: usize) -> Result<&'a [u8], ParseError> {
        self.input.get(from..to).ok_or(ParseError::SliceError)
    }

    /// Get a slice as a string.
    fn slice_str(&self, from: usize, to: usize) -> Result<&'a str, ParseError> {
        std::str::from_utf8(self.slice(from, to)?).map_err(|_| ParseError::Utf8Error)
    }

    /// Parse an identifier.
    fn ident(&mut self) -> Result<&'a str, ParseError> {
        let start = self.cursor;

        while matches!(self.input.get(self.cursor), Some(b'a'..=b'z' | b'_' | b'-')) {
            self.cursor += 1;
        }

        self.slice_str(start, self.cursor)
    }

    /// Parse an identifier.
    fn quoted_string(&mut self) -> Result<&'a str, ParseError> {
        match self.get(0) {
            Some(b'"') => {
                self.skip();
            }
            _ => return Err(ParseError::ExpectedString),
        }

        let start = self.cursor;

        let end = loop {
            match self.input.get(self.cursor) {
                Some(b'"') => {
                    let end = self.cursor;
                    self.skip();
                    break end;
                }
                Some(..) => {
                    self.skip();
                }
                None => {
                    return Err(ParseError::UnclosedString);
                }
            }
        };

        self.slice_str(start, end)
    }

    /// Parse a section.
    fn parse_section(&mut self) -> Result<Option<GitModule<'a>>, ParseError> {
        if self.get(0).is_none() {
            return Ok(None);
        }

        self.expect(b'[')?;

        match self.ident()? {
            "submodule" => {}
            actual => {
                return Err(ParseError::ExpectedSlice {
                    expected: Box::from(&"submodule"[..]),
                    actual: Box::from(actual),
                })
            }
        }

        let name = self.quoted_string()?;
        self.expect(b']')?;

        let mut extra = Vec::new();
        let mut path = None;

        while let Some(b) = self.get(0) {
            // Another section coming.
            if matches!(b, b'[') {
                break;
            }

            let key = self.ident()?;
            self.expect(b'=')?;
            self.skip_whitespace();
            let value = self.until_eol()?;

            match key {
                "path" => {
                    let os_str = OsStr::from_bytes(value);
                    path = Some(Path::new(os_str));
                }
                _ => {
                    extra.push((key, value));
                }
            }
        }

        Ok(Some(GitModule { name, path, extra }))
    }
}

#[derive(Debug, Error)]
pub(crate) enum ParseError {
    #[error("slice error")]
    SliceError,
    #[error("utf-8 error")]
    Utf8Error,
    #[error("expected byte {expected}, but got {actual:?}")]
    Expected { expected: u8, actual: Option<u8> },
    #[error("expected {expected:?}, but got {actual:?}")]
    ExpectedSlice {
        expected: Box<str>,
        actual: Box<str>,
    },
    #[error("expected string")]
    ExpectedString,
    #[error("encountered an unclosed string")]
    UnclosedString,
}
