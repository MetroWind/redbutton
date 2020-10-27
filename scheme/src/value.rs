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

        let cdr = self.cdr();
        match cdr
        {
            Value::Null => write!(f, "{})", self.car()),
            Value::List(cell) =>
            {
                write!(f, "{} ", self.car())?;
                cell.displayInner(f, false)
            },
            _ => write!(f, "{} . {})", self.car(), self.tail),
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

macro_rules! numericalCompare
{
    ($name:ident, $op:tt) =>
    {
        pub fn $name(&self, rhs: &Self) -> bool
        {
            match self
            {
                Self::Integer(n) =>
                {
                    match rhs
                    {
                        Self::Integer(m) => n $op m,
                        Self::Float(m) => (*n as f64) $op *m,
                        _ => false,
                    }
                },
                Self::Float(n) =>
                {
                    match rhs
                    {
                        Self::Integer(m) => *n $op (*m as f64),
                        Self::Float(m) => n $op m,
                        _ => false,
                    }
                },
                _ => false,
            }
        }
    };
}

impl Value
{
    pub const fn null() -> Self
    {
        Self::Null
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

    numericalCompare!(numericalEq, ==);
    numericalCompare!(numericalGreaterThan, >);
    numericalCompare!(numericalLessThan, <);
    numericalCompare!(numericalGreaterOrEq, >=);
    numericalCompare!(numericalLessOrEq, <=);

    pub fn toBool(&self) -> bool
    {
        self != &Self::Bool(false)
    }

    pub fn list2ReversedVec(&self) -> Option<Vec<Value>>
    {
        let cons = if let Self::List(cons) = self
        {
            cons
        }
        else
        {
            return None;
        };

        let cdr = cons.cdr();
        match &cdr
        {
            Value::Null => Some(vec![cons.car(),]),
            Value::List(_) =>
            {
                let mut result = if let Some(v) = cdr.list2ReversedVec()
                {
                    v
                }
                else
                {
                    return None;
                };

                result.push(cons.car());
                Some(result)
            },
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
            Self::Char(x) => write!(f, "#\\{}", x),
            Self::String(x) => write!(f, "\"{}\"", x),
            Self::Bool(x) => if *x { write!(f, "#t") } else { write!(f, "#f") },
            Self::List(x) => write!(f, "{}", x),
            Self::Symbol(x) => write!(f, "{}", x),
            Self::Null => write!(f, "()"),
            _ => Err(fmt::Error),
        }
    }
}

impl From<Vec<Value>> for Value
{
    fn from(mut v: Vec<Value>) -> Self
    {
        if v.is_empty()
        {
            return Self::null();
        }

        let mut result = Self::null();

        while !v.is_empty()
        {
            result = Value::List(Cons::new(v.pop().unwrap(), result));
        }

        result
    }
}
