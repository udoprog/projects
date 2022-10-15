use anyhow::{anyhow, Context, Result};

mod gitmodules;
mod model;

use model::{Ci, Model, Readme, Validation};

fn main() -> Result<()> {
    let gitmodules_bytes = std::fs::read(".gitmodules")?;
    let modules = gitmodules::parse(&gitmodules_bytes).context(".gitmodules")?;

    let mut models = Vec::new();

    let name = String::from("CI");

    for module in modules {
        if let Some(path) = module.path {
            models.push(Model {
                name: module.name,
                ci: Ci::new(path.join(".github/workflows"), name.clone()),
                readme: Readme::new(path.join("README.md")),
            });
        }
    }

    let mut validation = Vec::new();

    for model in &mut models {
        model
            .validate(&mut validation)
            .with_context(|| anyhow!("model: {}", model.name))?;
    }

    for error in &validation {
        match error {
            Validation::MissingWorkflows { path } => {
                println!("{path}: Missing workflows directory", path = path.display());
            }
            Validation::MissingWorkflow { path, candidates } => {
                println!("{path}: Missing workflow", path = path.display());

                for candidate in candidates.iter() {
                    println! {
                        "  Candidate: {candidate}",
                        candidate = candidate.display()
                    };
                }
            }
            Validation::WrongWorkflowName {
                path,
                actual,
                expected,
            } => {
                println! {
                    "{path}: Wrong workflow name: {actual} (actual) != {expected} (expected)",
                    path = path.display()
                };
            }
            Validation::MissingReadme { path } => {
                println! {
                    "{path}: Missing README", path = path.display()
                };
            }
        }
    }

    Ok(())
}
