use crate::model::ReadmeParams;
use anyhow::Result;

/// Badge builder.
pub(crate) trait Badge {
    fn build(&self, params: &ReadmeParams<'_>) -> Result<String>;
}

/// Collection of badges to build.
pub(crate) struct Badges<'a> {
    builders: Vec<&'a dyn Badge>,
}

impl<'a> Badges<'a> {
    /// Construct a new badges builder.
    pub(crate) fn new() -> Self {
        Self {
            builders: Vec::new(),
        }
    }

    /// Iterator over badge builders.
    pub(crate) fn iter(&self) -> impl Iterator<Item = &'_ dyn Badge> {
        self.builders.iter().copied()
    }

    /// Push a badge builder.
    pub(crate) fn push(&mut self, badge: &'a dyn Badge) {
        self.builders.push(badge);
    }
}
