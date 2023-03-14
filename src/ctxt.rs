use std::path::Path;

use crate::actions::Actions;
use crate::config::Config;
use crate::model::Module;

pub(crate) struct Ctxt<'a> {
    pub(crate) root: &'a Path,
    pub(crate) config: &'a Config,
    pub(crate) actions: &'a Actions<'a>,
    pub(crate) modules: Vec<Module<'a>>,
    pub(crate) default_workflow: String,
}
