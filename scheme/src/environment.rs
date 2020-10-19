use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;

use crate::parser::SyntaxTreeNode;
use shared::error;
use shared::error::Error;

#[derive(Clone)]
pub struct Procedure
{
    pub name: Option<String>,
    env: Environment,
    arguments: Vec<String>,
    body: Vec<SyntaxTreeNode>,
}

impl Procedure
{
    pub fn fromArgsBody(env: Environment, args: Vec<String>,
                        body: Vec<SyntaxTreeNode>) -> Self
    {
        Self{ name: None, env: env, arguments: args, body: body }
    }

    pub fn arguments(&self) -> &Vec<String>
    {
        &self.arguments
    }

    pub fn body(&self) -> &Vec<SyntaxTreeNode>
    {
        &self.body
    }

    pub fn empty(&self) -> bool
    {
        self.body.is_empty()
    }
}

impl fmt::Debug for Procedure
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match &self.name
        {
            Some(name) => write!(f, "{}:procedure", name),
            None => write!(f, "procedure"),
        }
    }
}

impl fmt::Display for Procedure
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match &self.name
        {
            Some(name) => write!(f, "{}:procedure", name),
            None => write!(f, "procedure"),
        }
    }
}

impl PartialEq for Procedure
{
    fn eq(&self, other: &Self) -> bool
    {
        &self.body == &other.body
    }
}

type Func = fn(&[Value], Environment) -> Result<Value, Error>;

#[derive(Clone)]
pub struct Builtin
{
    name: String,
    func: Func,
}

impl Builtin
{
    pub fn new(name: &str, func: Func) -> Self
    {
        Self{ name: name.to_owned(), func: func }
    }

    pub fn call(&self, args: &[Value], env: Environment) -> Result<Value, Error>
    {
        (self.func)(args, env)
    }
}

impl PartialEq for Builtin
{
    fn eq(&self, other: &Self) -> bool
    {
        self.name == other.name
    }
}

impl fmt::Debug for Builtin
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        write!(f, "{}:builtin", self.name)
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Value
{
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    List(Vec<Value>),
    Bool(bool),
    Symbol(String),
    Builtin(Builtin),
    Procedure(Procedure),
}

impl Value
{
    pub const fn null() -> Self
    {
        Self::List(vec![])
    }

    pub fn isInt(&self) -> bool
    {
        if let Self::Integer(_) = self
        {
            true
        }
        else
        {
            false
        }
    }

    pub fn isFloat(&self) -> bool
    {
        if let Self::Float(_) = self
        {
            true
        }
        else
        {
            false
        }
    }

    pub fn asInt(&self) -> Option<i64>
    {
        match self
        {
            Self::Integer(x) => Some(*x),
            Self::Float(x) => Some(*x as i64),
            _ => None,
        }
    }

    pub fn asFloat(&self) -> Option<f64>
    {
        match self
        {
            Self::Float(x) => Some(*x),
            Self::Integer(x) => Some(*x as f64),
            _ => None,
        }
    }
}

impl fmt::Display for Value
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Self::Integer(x) => write!(f, "{}", x),
            Self::Float(x) => write!(f, "{}", x),
            Self::Char(x) => write!(f, "{}", x),
            Self::String(x) => write!(f, "{}", x),
            Self::Bool(x) => write!(f, "{}", x),
            _ => Err(fmt::Error),
        }
    }
}


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

    pub fn set(&self, name: &str, value: Value)
    {
        self.inner.borrow_mut().values.insert(name.to_owned(), value);
    }
}
