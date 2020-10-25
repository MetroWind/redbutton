use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::value::Value;

struct EnvironmentInner
{
    parent: Option<Environment>,
    values: HashMap<String, Value>,
}

impl EnvironmentInner
{
    fn new() -> Self
    {
        Self{ parent: None, values: HashMap::new() }
    }
}

#[derive(Clone)]
pub struct Environment
{
    inner: Rc<RefCell<EnvironmentInner>>
}

impl Environment
{
    pub fn new() -> Self
    {
        Self::wrap(EnvironmentInner::new())
    }

    fn wrap(env: EnvironmentInner) -> Self
    {
        Self{ inner: Rc::new(RefCell::new(env)) }
    }

    pub fn derive(&self) -> Self
    {
        let result = Self::new();
        result.inner.borrow_mut().parent = Some(self.clone());
        result
    }

    pub fn members(&self) -> Vec<String>
    {
        let mut result = Vec::new();
        for (name, _) in &self.inner.borrow().values
        {
            result.push(name.clone());
        }
        result
    }

    pub fn find(&self, name: &str) -> Option<Value>
    {
        if let Some(v) = self.inner.borrow().values.get(name)
        {
            Some(v.clone())
        }
        else if let Some(env) = &self.inner.borrow().parent
        {
            env.find(name)
        }
        else
        {
            None
        }
    }

    pub fn set(&self, name: &str, value: Value) -> bool
    {
        if self.inner.borrow().values.contains_key(name)
        {
            self.inner.borrow_mut().values.insert(name.to_owned(), value);
            true
        }
        else if let Some(ref upper) = self.inner.borrow().parent
        {
            upper.set(name, value)
        }
        else
        {
            false
        }
    }

    pub fn define(&self, name: &str, value: Value)
    {
        self.inner.borrow_mut().values.insert(name.to_owned(), value);
    }

    pub fn merge(&self, rhs: Environment)
    {
        for (name, value) in &rhs.inner.borrow().values
        {
            self.define(name, value.clone());
        }
    }
}
