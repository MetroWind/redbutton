use std::fmt;
use std::rc::Rc;
use std::cell::RefCell;

use crate::parser::SyntaxTreeNode;
use crate::environment::Environment;
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
            write!(f, "{} ", self.car())?;
        }
        else
        {
            write!(f, "{} . ", self.car())?;
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

    pub fn toBool(&self) -> bool
    {
        self != &Self::Bool(false)
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
            Self::Char(x) => write!(f, "#\\{}", x),
            Self::String(x) => write!(f, "\"{}\"", x),
            Self::Bool(x) => if *x { write!(f, "#t") } else { write!(f, "#f") },
            Self::List(x) => write!(f, "{}", x),
            Self::Symbol(x) => write!(f, "{}", x),
            _ => Err(fmt::Error),
        }
    }
}
