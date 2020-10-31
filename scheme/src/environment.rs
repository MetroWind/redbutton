use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use shared::error;
use shared::error::Error;

use crate::value::{Value, Procedure, ProcedureArguments};

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

    pub fn matchArgs(&self, f: &Procedure, mut arg_values: Vec<Value>) ->
        Result<(), Error>
    {
        match f.arguments()
        {
            ProcedureArguments::Fixed(names) =>
            {
                if names.len() != arg_values.len()
                {
                    return Err(rterr!("{} expects {} argument(s)",
                                      f, names.len()));
                }

                let mut arg_drain = arg_values.drain(..);
                for name in names
                {
                    self.define(name, arg_drain.next().unwrap());
                }
                Ok(())
            },

            ProcedureArguments::HeadTail((names, tail_name)) =>
            {
                if arg_values.len() < names.len()
                {
                    return Err(rterr!("{} expects at leasts {} arguments",
                                      f, names.len()));
                }

                let mut arg_drain = arg_values.drain(..);
                for name in names
                {
                    self.define(name, arg_drain.next().unwrap());
                }

                let rest: Vec<Value> = arg_drain.collect();
                if rest.is_empty()
                {
                    self.define(&tail_name, Value::null());
                }
                else
                {
                    self.define(&tail_name, Value::from(rest));
                }
                Ok(())
            },

            ProcedureArguments::Single(name) =>
            {
                if arg_values.is_empty()
                {
                    self.define(&name, Value::null());
                }
                else
                {
                    self.define(&name, Value::from(arg_values));
                }
                Ok(())
            },
        }

    }
}
