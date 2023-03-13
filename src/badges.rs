use std::collections::HashMap;

use anyhow::Result;

use crate::config::Config;
use crate::model::CrateParams;
use crate::templates::Template;

/// Badge builder.
pub(crate) trait Badge {
    fn build(&self, params: CrateParams<'_>, config: &Config) -> Result<String>;
}

/// Collection of badges to build.
#[derive(Default)]
pub(crate) struct Badges<'a> {
    builders: Vec<&'a dyn Badge>,
    repos: HashMap<&'a str, Vec<&'a dyn Badge>>,
    headers: HashMap<&'a str, &'a Template>,
}

impl<'a> Badges<'a> {
    /// Iterator over badge builders.
    pub(crate) fn iter(&self, repo: &str) -> impl Iterator<Item = &'_ dyn Badge> {
        let repos = self.repos.get(repo).into_iter().flatten().copied();
        self.builders.iter().copied().chain(repos)
    }

    /// Get the header for the given repo.
    pub(crate) fn header(&self, repo: &str) -> Option<&Template> {
        self.headers.get(repo).copied()
    }

    /// Push a badge builder.
    pub(crate) fn push(&mut self, badge: &'a dyn Badge) {
        self.builders.push(badge);
    }

    /// Per-project badges.
    pub(crate) fn push_badge(&mut self, repo: &'a str, badge: &'a dyn Badge) {
        self.repos.entry(repo).or_default().push(badge);
    }

    /// Per-project header templates.
    pub(crate) fn insert_header(&mut self, repo: &'a str, template: &'a Template) {
        self.headers.insert(repo, template);
    }
}
