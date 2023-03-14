use std::collections::BTreeSet;
use std::io::Write;
use std::ops::Range;
use std::path::Path;
use std::sync::Arc;

use anyhow::{anyhow, bail, Context, Result};
use clap::Parser;
use pulldown_cmark::{LinkType, Options};
use relative_path::RelativePath;
use reqwest::Url;
use serde::Serialize;

use crate::config::Config;
use crate::ctxt::Ctxt;
use crate::file::{File, LineColumn};
use crate::model::{work_cargo_toml, Ci, CrateParams, UpdateParams, Validation};
use crate::repos::Repos;
use crate::urls::UrlError;
use crate::urls::Urls;
use crate::workspace::{self, Package};
use crate::Module;

/// Name of README to generate.
const README_MD: &str = "README.md";
/// Marker that is put into the generated header to indicate when it ends.
const HEADER_MARKER: &str = "<!--- header -->";

#[derive(Default, Parser)]
pub(crate) struct Opts {
    #[arg(long)]
    fix: bool,
    #[arg(long)]
    url_checks: bool,
    filters: Vec<String>,
}

/// Entrypoint to run action.
pub(crate) async fn entry(cx: &Ctxt<'_>, opts: &Opts) -> Result<()> {
    let mut validation = Vec::new();
    let mut urls = Urls::default();

    for module in &cx.modules {
        run_module(cx, &opts, module, &mut validation, &mut urls)?;
    }

    for validation in &validation {
        validate(cx, &opts, validation)?;
    }

    let o = std::io::stdout();
    let mut o = o.lock();

    for (url, test) in urls.bad_urls() {
        let path = &test.path;
        let (line, column, string) =
            temporary_line_fix(&test.file, test.range.start, test.line_offset)?;

        if let Some(error) = &test.error {
            writeln!(o, "{path}:{line}:{column}: bad url: `{url}`: {error}")?;
        } else {
            writeln!(o, "{path}:{line}:{column}: bad url: `{url}`")?;
        }

        writeln!(o, "{string}")?;
    }

    if opts.url_checks {
        url_checks(&mut o, urls).await?;
    }

    Ok(())
}

/// Run a single module.
fn run_module(
    cx: &Ctxt<'_>,
    run: &Opts,
    module: &Module<'_>,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    if crate::should_skip(&run.filters, module) {
        return Ok(());
    }

    let (Some(module_path), Some(module_url), Some(repo)) = (module.path, &module.url, module.repo()) else {
        println!(
            ".gitmodules: {name}: missing `path` and/or `url`",
            name = module.name
        );
        return Ok(());
    };

    let cargo_toml = match cx.config.cargo_toml(module.name) {
        Some(cargo_toml) => module_path.join(cargo_toml),
        None => module_path.join(workspace::CARGO_TOML),
    };

    let workspace = workspace::open(cx.root, &cargo_toml)?;

    let primary_crate = cx.config.crate_for(module.name).or(repo.split('/').last());

    let primary_crate = match workspace.primary_crate(primary_crate)? {
        Some(primary_crate) => primary_crate,
        None => return Err(anyhow!("{module_path}: cannot determine primary crate",)),
    };

    let params = cx.config.per_crate_render(CrateParams {
        repo: &repo,
        name: primary_crate.manifest.crate_name()?,
        description: primary_crate.manifest.description()?,
    });

    let documentation = match &cx.config.documentation {
        Some(documentation) => Some(documentation.render(&params)?),
        None => None,
    };

    let url_string = module_url.to_string();

    let update_params = UpdateParams {
        license: cx.config.license(),
        readme: README_MD,
        repository: &url_string,
        homepage: &url_string,
        documentation: documentation.as_deref().unwrap_or_default(),
        authors: &cx.config.authors,
    };

    for package in workspace.packages() {
        if package.manifest.is_publish()? {
            work_cargo_toml(package, validation, &update_params)?;
        }
    }

    if cx.config.is_enabled(module.name, "ci") {
        let path = module_path.join(".github").join("workflows");
        let ci = Ci::new(
            &path,
            cx.config.job_name(),
            cx.actions,
            &primary_crate.manifest,
            !workspace.is_single_crate(),
        );
        ci.validate(cx.root, validation)
            .with_context(|| anyhow!("ci validation: {}", cx.config.job_name()))?;
    }

    if cx.config.is_enabled(module.name, "readme") {
        build_readme(
            cx,
            module.name,
            module_path,
            primary_crate,
            params.crate_params,
            validation,
            urls,
        )?;

        for package in workspace.packages() {
            if package.manifest_dir != module_path {
                if package.manifest.is_publish()? {
                    let crate_params = CrateParams {
                        repo: &repo,
                        name: package.manifest.crate_name()?,
                        description: package.manifest.description()?,
                    };

                    build_readme(
                        cx,
                        module.name,
                        &package.manifest_dir,
                        package,
                        crate_params,
                        validation,
                        urls,
                    )?;
                }
            }
        }
    }

    Ok(())
}

/// Perform readme validation.
fn build_readme(
    cx: &Ctxt<'_>,
    name: &str,
    readme_path: &RelativePath,
    package: &Package,
    params: CrateParams<'_>,
    validation: &mut Vec<Validation>,
    urls: &mut Urls,
) -> Result<()> {
    let readme_path = readme_path.join(README_MD);

    let entry = 'entry: {
        for entry in package.entries() {
            if entry.to_path(cx.root).is_file() {
                break 'entry entry;
            }
        }

        bail!("{name}: missing existing entrypoint")
    };

    let readme = Readme::new(name, &readme_path, &entry, cx.badges, params, cx.config);

    readme
        .validate(cx.root, validation, urls)
        .with_context(|| anyhow!("{readme_path}: readme validation"))?;

    Ok(())
}

/// Report and apply a asingle validation.
fn validate(cx: &Ctxt<'_>, opts: &Opts, error: &Validation) -> Result<()> {
    Ok(match error {
        Validation::MissingWorkflow { path, candidates } => {
            println!("{path}: Missing workflow");

            for candidate in candidates.iter() {
                println!("  Candidate: {candidate}");
            }

            if opts.fix {
                if let [from] = candidates.as_ref() {
                    println!("{path}: Rename from {from}",);
                    std::fs::rename(from.to_path(cx.root), path.to_path(cx.root))?;
                } else {
                    let path = path.to_path(cx.root);

                    if let Some(parent) = path.parent() {
                        if !parent.is_dir() {
                            std::fs::create_dir_all(&parent)?;
                        }
                    }

                    std::fs::write(path, &cx.default_workflow)?;
                }
            }
        }
        Validation::DeprecatedWorkflow { path } => {
            println!("{path}: Reprecated Workflow");
        }
        Validation::WrongWorkflowName {
            path,
            actual,
            expected,
        } => {
            println!("{path}: Wrong workflow name: {actual} (actual) != {expected} (expected)");
        }
        Validation::OutdatedAction {
            path,
            name,
            actual,
            expected,
        } => {
            println!(
                "{path}: Outdated action `{name}`: {actual} (actual) != {expected} (expected)"
            );
        }
        Validation::DeniedAction { path, name, reason } => {
            println!("{path}: Denied action `{name}`: {reason}");
        }
        Validation::CustomActionsCheck { path, name, reason } => {
            println!("{path}: Action validation failed `{name}`: {reason}");
        }
        Validation::MissingReadme { path } => {
            println!("{path}: Missing README");
        }
        Validation::MismatchedLibRs { path, new_file } => {
            if opts.fix {
                println!("{path}: Fixing lib.rs");
                std::fs::write(path.to_path(cx.root), new_file.as_bytes())?;
            } else {
                println!("{path}: Mismatched lib.rs");
            }
        }
        Validation::BadReadme { path, new_file } => {
            if opts.fix {
                println!("{path}: Fixing README.md");
                std::fs::write(path.to_path(cx.root), new_file.as_bytes())?;
            } else {
                println!("{path}: Bad README.md");
            }
        }
        Validation::ToplevelHeadings {
            path,
            file,
            range,
            line_offset,
        } => {
            let (line, column, string) = temporary_line_fix(&file, range.start, *line_offset)?;
            println!("{path}:{line}:{column}: doc comment has toplevel headings");
            println!("{string}");
        }
        Validation::MissingPreceedingBr {
            path,
            file,
            range,
            line_offset,
        } => {
            let (line, column, string) = temporary_line_fix(&file, range.start, *line_offset)?;
            println!("{path}:{line}:{column}: missing preceeding <br>");
            println!("{string}");
        }
        Validation::MissingFeature { path, feature } => {
            println!("{path}: missing features `{feature}`");
        }
        Validation::NoFeatures { path } => {
            println!("{path}: trying featured build (--all-features, --no-default-features), but no features present");
        }
        Validation::MissingEmptyFeatures { path } => {
            println!("{path}: missing empty features build");
        }
        Validation::MissingAllFeatures { path } => {
            println!("{path}: missing all features build");
        }
        Validation::CargoTomlIssues {
            path,
            cargo: modified_cargo,
            issues,
        } => {
            println!("{path}:");

            for issue in issues {
                println!("  {issue}");
            }

            if opts.fix {
                if let Some(modified_cargo) = modified_cargo {
                    modified_cargo.save_to(path.to_path(cx.root))?;
                }
            }
        }
        Validation::ActionMissingKey {
            path,
            key,
            expected,
            actual,
        } => {
            println!("{path}: {key}: action missing key, expected {expected}");

            match actual {
                Some(value) => {
                    println!("  actual:");
                    serde_yaml::to_writer(std::io::stdout(), value)?;
                }
                None => {
                    println!("  actual: *missing value*");
                }
            }
        }
        Validation::ActionOnMissingBranch { path, key, branch } => {
            println!("{path}: {key}: action missing branch `{branch}`");
        }
        Validation::ActionExpectedEmptyMapping { path, key } => {
            println!("{path}: {key}: action expected empty mapping");
        }
    })
}

/// Perform url checks.
async fn url_checks<O>(o: &mut O, urls: Urls) -> Result<()>
where
    O: Write,
{
    let (tx, mut rx) = tokio::sync::mpsc::channel(1);

    let total = urls.check_urls();
    let checks = urls.check_urls_task(3, tx);
    tokio::pin!(checks);
    let mut count = 1;
    let mut completed = false;

    loop {
        tokio::select! {
            result = checks.as_mut(), if !completed => {
                result?;
                completed = true;
            }
            result = rx.recv() => {
                let result = match result {
                    Some(result) => result,
                    None => break,
                };

                match result {
                    Ok(_) => {}
                    Err(UrlError { url, status, tests }) => {
                        writeln!(o, "{count:>3}/{total} {url}: {status}")?;

                        for test in tests {
                            let path = &test.path;
                            let (line, column, string) = temporary_line_fix(&test.file, test.range.start, test.line_offset)?;
                            writeln!(o, "  {path}:{line}:{column}: {string}")?;
                        }
                    }
                }

                count += 1;
            }
        }
    }

    Ok(())
}

/// Temporary line comment fix which adjusts the line and column.
fn temporary_line_fix(file: &File, pos: usize, line_offset: usize) -> Result<(usize, usize, &str)> {
    let (LineColumn { line, column }, string) = file.line_column(pos)?;
    let line = line_offset + line;
    let column = column + 4;
    Ok((line, column, string))
}

pub(crate) struct Readme<'a> {
    pub(crate) name: &'a str,
    pub(crate) path: &'a RelativePath,
    pub(crate) entry: &'a RelativePath,
    pub(crate) repos: &'a Repos<'a>,
    pub(crate) crate_params: CrateParams<'a>,
    pub(crate) config: &'a Config,
}

impl<'a> Readme<'a> {
    /// Construct a new README config.
    pub(crate) fn new(
        name: &'a str,
        path: &'a RelativePath,
        entry: &'a RelativePath,
        repos: &'a Repos,
        crate_params: CrateParams<'a>,
        config: &'a Config,
    ) -> Self {
        Self {
            name,
            path,
            entry,
            repos,
            crate_params,
            config,
        }
    }

    /// Validate the current model.
    pub(crate) fn validate(
        &self,
        root: &Path,
        validation: &mut Vec<Validation<'_>>,
        urls: &mut Urls,
    ) -> Result<()> {
        if !self.path.to_path(root).is_file() {
            validation.push(Validation::MissingReadme {
                path: self.path.to_owned(),
            });
        }

        if self.entry.to_path(root).is_file() {
            let (file, new_file) = self.process_lib_rs(root)?;
            let checks = self.markdown_checks(&file, urls)?;

            for (file, range) in checks.toplevel_headings {
                validation.push(Validation::ToplevelHeadings {
                    path: self.entry.to_owned(),
                    file,
                    range,
                    line_offset: checks.line_offset,
                });
            }

            for (file, range) in checks.missing_preceeding_br {
                validation.push(Validation::MissingPreceedingBr {
                    path: self.entry.to_owned(),
                    file,
                    range,
                    line_offset: checks.line_offset,
                });
            }

            let readme_from_lib_rs = readme_from_lib_rs(&new_file, self.crate_params)?;

            if *file != *new_file {
                validation.push(Validation::MismatchedLibRs {
                    path: self.entry.to_owned(),
                    new_file: new_file.clone(),
                });
            }

            let readme = match File::read(self.path.to_path(root)) {
                Ok(file) => file,
                Err(e) if e.kind() == std::io::ErrorKind::NotFound => File::new(),
                Err(e) => return Err(e.into()),
            };

            if readme != readme_from_lib_rs {
                validation.push(Validation::BadReadme {
                    path: self.path.to_owned(),
                    new_file: Arc::new(readme_from_lib_rs),
                });
            }
        }

        Ok(())
    }

    /// Process the lib rs.
    fn process_lib_rs(&self, root: &Path) -> Result<(Arc<File>, Arc<File>), anyhow::Error> {
        /// Test if line is a badge comment.
        fn is_badge_comment(c: &[u8]) -> bool {
            let c = trim_ascii(c);

            if c == b"<div align=\"center\">" || c == b"</div>" {
                return true;
            }

            if c.starts_with(b"[<img ") && c.ends_with(b")") {
                return true;
            }

            if c.starts_with(b"[![") && c.ends_with(b")") {
                return true;
            }

            if c.starts_with(b"<a href") && c.ends_with(b"</a>") {
                return true;
            }

            false
        }

        pub const fn trim_ascii(bytes: &[u8]) -> &[u8] {
            trim_ascii_end(trim_ascii_start(bytes))
        }

        pub const fn trim_ascii_start(mut bytes: &[u8]) -> &[u8] {
            while let [first, rest @ ..] = bytes {
                if first.is_ascii_whitespace() {
                    bytes = rest;
                } else {
                    break;
                }
            }

            bytes
        }

        pub const fn trim_ascii_end(mut bytes: &[u8]) -> &[u8] {
            while let [rest @ .., last] = bytes {
                if last.is_ascii_whitespace() {
                    bytes = rest;
                } else {
                    break;
                }
            }

            bytes
        }

        #[derive(Serialize)]
        struct BadgeParams {
            html: Option<String>,
            markdown: Option<String>,
        }

        #[derive(Serialize)]
        struct HeaderParams<'a> {
            badges: &'a [BadgeParams],
            description: Option<&'a str>,
            is_more: bool,
        }

        let source = File::read(self.entry.to_path(root))?;
        let mut new_file = File::new();

        let mut badges = Vec::new();

        for badge in self.repos.iter(self.name) {
            badges.push(BadgeParams {
                markdown: badge.markdown(self.crate_params, self.config)?,
                html: badge.html(self.crate_params, self.config)?,
            });
        }

        let mut source_lines = source.lines().peekable();

        if let Some(header) = self.repos.header(self.name) {
            let mut found_marker = false;

            while let Some(line) = source_lines.peek().and_then(|line| line.as_rust_comment()) {
                let line = trim_ascii_start(line);

                if line.starts_with(b"#") {
                    break;
                }

                if line == HEADER_MARKER.as_bytes() {
                    found_marker = true;
                    source_lines.next();
                    break;
                }

                source_lines.next();
            }

            let header = header.render(&HeaderParams {
                badges: &badges,
                description: self.crate_params.description.map(str::trim),
                is_more: source_lines.peek().is_some(),
            })?;

            for string in header.split('\n') {
                if string.is_empty() {
                    new_file.push(b"//!");
                } else {
                    new_file.push(format!("//! {string}").as_bytes());
                }
            }

            // Add a header marker in case an existing marker was found and
            // there is nothing more in the header.
            if found_marker
                && source_lines
                    .peek()
                    .and_then(|line| line.as_rust_comment())
                    .is_some()
            {
                new_file.push(format!("//! {HEADER_MARKER}").as_bytes());
            }
        } else {
            while let Some(line) = source_lines.peek().and_then(|line| line.as_rust_comment()) {
                if !is_badge_comment(line) {
                    break;
                }

                source_lines.next();
            }

            for badge in badges {
                if let Some(markdown) = &badge.markdown {
                    new_file.push(format!("//! {markdown}").as_bytes());
                }
            }
        }

        for line in source_lines {
            let bytes = line.as_bytes();
            let bytes = trim_ascii_end(bytes);
            new_file.push(bytes);
        }

        Ok((Arc::new(source), Arc::new(new_file)))
    }

    /// Test if the specified file has toplevel headings.
    fn markdown_checks(&self, file: &Arc<File>, urls: &mut Urls) -> Result<MarkdownChecks> {
        use pulldown_cmark::{Event, HeadingLevel, Parser, Tag};

        let mut comment = Vec::new();

        let mut initial = true;
        let mut checks = MarkdownChecks::default();

        for (offset, line) in file.lines().enumerate() {
            if initial {
                checks.line_offset = offset + 1;
            }

            if let Some(line) = line.as_rust_comment() {
                comment.push(std::str::from_utf8(line)?);
                initial = false;
            }
        }

        let comment = comment.join("\n");
        let file = Arc::new(File::from_vec(comment.as_bytes().to_vec()));

        let opts = Options::empty();

        let parser = Parser::new_with_broken_link_callback(&comment, opts, None);
        let mut preceeding_newline = false;

        for (event, range) in parser.into_offset_iter() {
            match event {
                Event::Html(html) => {
                    if html.trim() == "<br>" {
                        preceeding_newline = true;
                        continue;
                    }
                }
                Event::Start(tag) => match tag {
                    Tag::Heading(level, _, _) => {
                        if !preceeding_newline {
                            checks
                                .missing_preceeding_br
                                .push((file.clone(), range.clone()));
                        }

                        if matches!(level, HeadingLevel::H1) {
                            checks.toplevel_headings.push((file.clone(), range.clone()));
                        }
                    }
                    Tag::Link(LinkType::Autolink, href, _) => {
                        self.visit_url(href.as_ref(), &file, &range, &checks, urls)?;
                    }
                    Tag::Link(LinkType::Inline, href, _) => {
                        self.visit_url(href.as_ref(), &file, &range, &checks, urls)?;
                    }
                    Tag::Link(LinkType::Shortcut, href, _) => {
                        self.visit_url(href.as_ref(), &file, &range, &checks, urls)?;
                    }
                    _ => {}
                },
                _ => {}
            }

            preceeding_newline = false;
        }

        Ok(checks)
    }

    /// Insert an URL.
    fn visit_url(
        &self,
        url: &str,
        file: &Arc<File>,
        range: &Range<usize>,
        checks: &MarkdownChecks,
        urls: &mut Urls,
    ) -> Result<()> {
        // Link to anchor does nothing.
        if url.starts_with('#') {
            return Ok(());
        }

        let error = match str::parse::<Url>(url) {
            Ok(url) if matches!(url.scheme(), "http" | "https") => {
                urls.insert(
                    url,
                    file.clone(),
                    range.clone(),
                    self.entry,
                    checks.line_offset,
                );

                return Ok(());
            }
            Ok(url) => anyhow!("only 'http://' or 'https://' urls are supported, got `{url}`"),
            Err(e) => e.into(),
        };

        urls.insert_bad_url(
            url.to_owned(),
            error,
            file.clone(),
            range.clone(),
            self.entry,
            checks.line_offset,
        );

        Ok(())
    }
}

#[derive(Default)]
struct MarkdownChecks {
    line_offset: usize,
    toplevel_headings: Vec<(Arc<File>, Range<usize>)>,
    missing_preceeding_br: Vec<(Arc<File>, Range<usize>)>,
}

/// Generate a readme.
fn readme_from_lib_rs(file: &File, crate_params: CrateParams<'_>) -> Result<File> {
    /// Filter code block fragments.
    fn filter_code_block(comment: &str) -> (String, BTreeSet<String>) {
        let parts = comment.get(3..).unwrap_or_default();
        let mut out = BTreeSet::new();

        for part in parts.split(',') {
            let part = part.trim();

            match part {
                "" => continue,
                "no_run" => continue,
                "should_panic" => continue,
                "ignore" => continue,
                "edition2018" => continue,
                "edition2021" => continue,
                _ => {}
            }

            out.insert(part.to_owned());
        }

        if out.is_empty() {
            out.insert(String::from("rust"));
        }

        (out.iter().cloned().collect::<Vec<_>>().join(","), out)
    }

    let mut readme = File::new();

    let mut in_code_block = None::<bool>;
    let name = crate_params.name;

    readme.push(format!("# {name}").as_bytes());
    readme.push(b"");

    for line in file.lines() {
        let comment = match line.as_rust_comment() {
            Some(comment) => std::str::from_utf8(comment)?,
            None => {
                continue;
            }
        };

        let comment = if let Some(" ") = comment.get(..1) {
            comment.get(1..).unwrap_or_default()
        } else {
            comment
        };

        if in_code_block == Some(true) && comment.trim_start().starts_with("# ") {
            continue;
        }

        if comment.starts_with("```") {
            if in_code_block.is_none() {
                let (parts, specs) = filter_code_block(comment);
                readme.push(format!("```{parts}").as_bytes());
                in_code_block = Some(specs.contains("rust"));
                continue;
            }

            in_code_block = None;
        }

        readme.push(comment.as_bytes());
    }

    readme.ensure_trailing_newline();
    Ok(readme)
}
