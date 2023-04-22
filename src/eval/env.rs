use crate::eval::Object;
use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(PartialEq, Debug, Clone)]
pub struct Env {
    pub obj: HashMap<String, Object>,
    pub parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Self {
            obj: HashMap::new(),
            parent: None,
        }
    }

    pub fn set(&mut self, name: &str, obj: Object) {
        self.obj.insert(name.to_string(), obj);
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        if let Some(o) = self.obj.get(name) {
            return Some(o.clone());
        }
        self.parent.as_ref().map(|p_env| {
            let b_env = p_env.borrow();
            return b_env.get(name);
        });
        None
    }
}
