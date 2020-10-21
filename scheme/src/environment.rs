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

#[derive(Debug, Clone, PartialEq)]
pub struct Cons
{
    is_list: bool,
    head: Rc<RefCell<Value>>,
    tail: Box<Value>,
}

impl Cons
{
    pub fn new(head: Value, tail: Value) -> Self
    {
        let mut is_list = tail == Value::null();
        if !is_list
        {
            if let Value::List(c) = &tail
            {
                is_list = c.isList();
            }
        }

        Self{ is_list: is_list,
              head: Rc::new(RefCell::new(head)),
              tail: Box::new(tail),
        }
    }

    pub fn nullCons() -> Self
    {
        Self::new(Value::null(), Value::null())
    }

    pub fn car(&self) -> Value
    {
        self.head.borrow().clone()
    }

    pub fn cdr(&self) -> Value
    {
        *self.tail.clone()
    }

    pub fn isList(&self) -> bool
    {
        self.is_list
    }

    fn displayInner(&self, f: &mut fmt::Formatter<'_>, is_first: bool) -> fmt::Result
    {
        if is_first
        {
            write!(f, "(")?;
        }

        if self.is_list
        {
            let cdr = self.cdr();
            if cdr == Value::null()
            {
                return write!(f, "{})", self.car());
            }
            write!(f, "{} ", self.car());
        }
        else
        {
            write!(f, "{} . ", self.car());
        }

        let cdr = self.cdr();
        if let Value::List(cell) = cdr
        {
            cell.displayInner(f, false)
        }
        else
        {
            write!(f, "{})", cdr)
        }
    }
}

impl fmt::Display for Cons
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        self.displayInner(f, true)
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub enum Cell
// {
//     Element(Cons),
//     Null,
// }

// impl Cell
// {
//     pub fn newCons(head: Value, tail: Value) -> Self
//     {
//         Self::Element(Cons::new(head, tail))
//     }

//     pub fn nullCons() -> Self
//     {
//         Self::Element(Cons::nullCons())
//     }

//     pub fn car(&self) -> Option<Value>
//     {
//         if let Self::Element(c) = self
//         {
//             Some(c.car())
//         }
//         else
//         {
//             None
//         }
//     }

//     pub fn cdr(&self) -> Option<Value>
//     {
//         if let Self::Element(c) = self
//         {
//             Some(c.cdr())
//         }
//         else
//         {
//             None
//         }
//     }

// }

#[derive(Clone, PartialEq, Debug)]
pub enum Value
{
    Integer(i64),
    Float(f64),
    Char(char),
    String(String),
    List(Cons),
    Bool(bool),
    Symbol(String),
    Builtin(Builtin),
    Procedure(Procedure),
    Null,
}

impl Value
{
    pub const fn null() -> Self
    {
        Self::Null
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

    pub fn asList(&self) -> Option<Cons>
    {
        if let Self::List(c) = self
        {
            Some(c.clone())
        }
        else
        {
            None
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
            Self::List(x) => write!(f, "{}", x),
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
}
