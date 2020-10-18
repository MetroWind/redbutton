use shared::error;
use shared::error::Error;

use crate::tokenizer::{Token, TokenValue};
use crate::parser::SyntaxTreeNode;
use crate::environment::{Environment, Value};
use crate::builtin;

pub struct Evaluator
{
    code: SyntaxTreeNode,
    env: Environment,
}

// Construct a RuntimeError
macro_rules! rterr
{
    ($msg:literal) => { error!(RuntimeError, $msg) };
    ($msg:literal $(, $x:expr)+) =>
    {
        error!(RuntimeError, format!($msg $(, $x)+))
    };
}

pub type EvalResult = Result<Value, Error>;

impl Evaluator
{
    pub fn new(code: SyntaxTreeNode) -> Self
    {
        Self{ code: code, env: builtin::getBuiltinEnv() }
    }

    fn evalAtom(&self, token: &Token) -> EvalResult
    {
        match token.value()
        {
            TokenValue::Bool(x) => Ok(Value::Bool(x.clone())),
            TokenValue::Char(x) => Ok(Value::Char(x.clone())),
            TokenValue::Float(x) => Ok(Value::Float(x.clone())),
            TokenValue::Integer(x) => Ok(Value::Integer(x.clone())),
            TokenValue::String(x) => Ok(Value::String(x.clone())),
            TokenValue::Ident(name) =>
            {
                if let Some(v) = self.env.find(name)
                {
                    Ok(v.clone())
                }
                else
                {
                    Err(rterr!("Undefined variable: {}", name))
                }
            },
            _ => unreachable!(),
        }
    }

    fn evalEach(&self, nodes: &[SyntaxTreeNode]) -> Result<Vec<Value>, Error>
    {
        let result: Result<Vec<Value>, Error> =
            nodes.iter().map(|node| self.evalNode(node)).collect();
        result
    }

    fn quoteAtom(&self, token: &Token) -> EvalResult
    {
        match token.value()
        {
            TokenValue::Bool(x) => Ok(Value::Bool(x.clone())),
            TokenValue::Char(x) => Ok(Value::Char(x.clone())),
            TokenValue::Float(x) => Ok(Value::Float(x.clone())),
            TokenValue::Integer(x) => Ok(Value::Integer(x.clone())),
            TokenValue::String(x) => Ok(Value::String(x.clone())),
            TokenValue::Ident(name) => Ok(Value::Symbol(name.clone())),
            _ => Err(rterr!("Unquotable")),
        }
    }

    fn evalQuote(&self, quoted: &SyntaxTreeNode) -> EvalResult
    {
        match quoted
        {
            SyntaxTreeNode::Atom(token) => self.quoteAtom(token),
            SyntaxTreeNode::Compound(nodes) =>
            {
                let result: Result<Vec<Value>, Error> =
                    nodes.iter().map(|node| self.evalQuote(node)).collect();
                Ok(Value::List(result?))
            },
        }
    }

    fn evalIf(&self, branch_true: &SyntaxTreeNode,
              branch_false: &[SyntaxTreeNode]) -> EvalResult
    {
        Err(rterr!("If is unimplemented"))
    }

    fn evalNamedCompound(&self, name: &str, rest: &[SyntaxTreeNode]) -> EvalResult
    {
        // Specials
        match name
        {
            "if" => return self.evalIf(&rest[0], &rest[1..]),
            _ => {},
        }

        // Procedures
        if let Some(value) = self.env.find(name)
        {
            match value
            {
                Value::Builtin(f) =>
                {
                    let args = self.evalEach(rest)?;
                    f.call(&args, self.env.clone())
                },
                Value::Procedure(_) => Err(rterr!("Procedure is not implemented")),
                _ => Err(rterr!("{} is not a procedure", name)),
            }
        }
        else
        {
            Err(rterr!("Undefined procedure: {}", name))
        }
    }

    fn evalCompound(&self, nodes: &[SyntaxTreeNode]) -> EvalResult
    {
        let head: &SyntaxTreeNode = &nodes[0];

        match head
        {
            SyntaxTreeNode::Atom(token) =>
            {
                match token.value()
                {
                    TokenValue::Ident(name) =>
                        self.evalNamedCompound(name, &nodes[1..]),
                    TokenValue::Quote =>
                        self.evalQuote(&nodes[1]),
                    _ => Err(rterr!("Unimplemented")),
                }
            },
            SyntaxTreeNode::Compound(head_nodes) =>
            {
                Err(rterr!("Unimplemented"))
            }
        }
    }

    fn evalNode(&self, node: &SyntaxTreeNode) -> EvalResult
    {
        match node
        {
            SyntaxTreeNode::Atom(token) =>
                self.evalAtom(token),
            SyntaxTreeNode::Compound(nodes) =>
                self.evalCompound(nodes),
        }
    }

    pub fn eval(&self) -> EvalResult
    {
        self.evalNode(&self.code)
    }

    pub fn evalSource(src: &str) -> EvalResult
    {
        let tokens = crate::tokenizer::tokenize(src)?;
        let root = SyntaxTreeNode::parse(tokens)?;
        Self::new(root).eval()
    }
}

#[cfg(test)]
mod tests
{
    use super::*;

    fn floatEq(lhs: Value, rhs: Value, delta: f64) -> bool
    {
        let l = if let Some(x) = lhs.asFloat()
        {
            x
        }
        else
        {
            return false;
        };

        let r = if let Some(x) = rhs.asFloat()
        {
            x
        }
        else
        {
            return false;
        };

        if (l - r).abs() < delta
        {
            true
        }
        else
        {
            println!("{} != {}", l, r);
            false
        }
    }

    fn assert_float_eq(lhs: Value, rhs: f64)
    {
        assert!(floatEq(lhs, Value::Float(rhs), 1e-5));
    }

    #[test]
    fn plus() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(+ 1 2)"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::evalSource(r#"(+ 1.6 2)"#)?;
        assert_float_eq(result, 3.6);
        let result = Evaluator::evalSource(r#"(+ 1 2.0)"#)?;
        assert_float_eq(result, 3.0);
        let result = Evaluator::evalSource(r#"(+ 1 (+ 2.5 1) 2)"#)?;
        assert_float_eq(result, 6.5);
        Ok(())
    }

    #[test]
    fn minus() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(- 1 2)"#)?;
        assert_eq!(result, Value::Integer(-1));
        let result = Evaluator::evalSource(r#"(- 1.6 2)"#)?;
        assert_float_eq(result, -0.4);
        let result = Evaluator::evalSource(r#"(- 1 2.0)"#)?;
        assert_float_eq(result, -1.0);
        let result = Evaluator::evalSource(r#"(- 1 (- 2.5 1) 2)"#)?;
        assert_float_eq(result, -2.5);
        Ok(())
    }

    #[test]
    fn multiply() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(* 1 2)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(* 1.6 2)"#)?;
        assert_float_eq(result, 3.2);
        let result = Evaluator::evalSource(r#"(* 1 2.0)"#)?;
        assert_float_eq(result, 2.0);
        let result = Evaluator::evalSource(r#"(* 1 (* 2.5 1) 2)"#)?;
        assert_float_eq(result, 5.0);
        Ok(())
    }

    #[test]
    fn divide() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(/ 2 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(/ 1 2)"#)?;
        assert_float_eq(result, 0.5);
        let result = Evaluator::evalSource(r#"(/ 1.6 2)"#)?;
        assert_float_eq(result, 0.8);
        let result = Evaluator::evalSource(r#"(/ 1 2.0)"#)?;
        assert_float_eq(result, 0.5);
        let result = Evaluator::evalSource(r#"(/ 1 (/ 2.5 1) 2)"#)?;
        assert_float_eq(result, 1.0 / 2.5 / 2.0);
        Ok(())
    }

    #[test]
    fn quote() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"'1"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::evalSource(r#"'a"#)?;
        assert_eq!(result, Value::Symbol(String::from("a")));
        let result = Evaluator::evalSource(r#"'(a b)"#)?;
        if let Value::List(vs) = result
        {
            assert_eq!(vs.len(), 2);
            assert_eq!(vs[0], Value::Symbol(String::from("a")));
            assert_eq!(vs[1], Value::Symbol(String::from("b")));
        }
        else
        {
            return Err(error!(RuntimeError, "Test failed"));
        }
        Ok(())
    }
}
