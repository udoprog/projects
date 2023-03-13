use std::collections::HashMap;

use anyhow::Result;

use crate::config::{Config, Repo};
use crate::model::CrateParams;
use crate::templates::Template;

/// Badge builder.
pub(crate) trait Badge {
    /// Render badge as markdown.
    fn markdown(&self, params: CrateParams<'_>, config: &Config) -> Result<Option<String>>;

    /// Render badge as html.
    fn html(&self, params: CrateParams<'_>, config: &Config) -> Result<Option<String>>;
}

/// Collection of badges to build.
#[derive(Default)]
pub(crate) struct Repos<'a> {
    badges: Vec<&'a dyn Badge>,
    repos: HashMap<&'a str, &'a Repo>,
}

impl<'a> Repos<'a> {
    /// Iterator over badge builders.
    pub(crate) fn iter(&self, repo: &str) -> impl Iterator<Item = &'_ dyn Badge> {
        let repos = self
            .repos
            .get(repo)
            .into_iter()
            .flat_map(|repo| repo.badges.iter().map(|badge| badge as &dyn Badge));
        self.badges.iter().copied().chain(repos)
    }

    /// Get the header for the given repo.
    pub(crate) fn header(&self, repo: &str) -> Option<&Template> {
        self.repos.get(repo)?.header.as_ref()
    }

    /// Push a global badge.
    pub(crate) fn push_global_badge(&mut self, badge: &'a dyn Badge) {
        self.badges.push(badge);
    }

    /// Per-project header templates.
    pub(crate) fn insert_repo(&mut self, repo: &'a str, config: &'a Repo) {
        self.repos.insert(repo, config);
    }
}
