use shared::error;
use shared::error::Error;

use crate::tokenizer::{Token, TokenValue};
use crate::parser::SyntaxTreeNode;
use crate::environment::{Environment, Value, Procedure, Cons};
use crate::builtin;

pub struct Evaluator
{
    code: Vec<SyntaxTreeNode>,
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
    pub fn new(code: Vec<SyntaxTreeNode>) -> Self
    {
        Self{ code: code, env: builtin::getBuiltinEnv() }
    }

    fn evalAtom(&self, env: Environment, token: &Token) -> EvalResult
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
                if let Some(v) = env.find(name)
                {
                    Ok(v.clone())
                }
                else
                {
                    Err(rterr!("Undefined variable: {}", name))
                }
            },
            _ => Err(rterr!("Invalid token")),
        }
    }

    fn evalEach(&self, env: Environment, nodes: &[SyntaxTreeNode]) ->
        Result<Vec<Value>, Error>
    {
        let result: Result<Vec<Value>, Error> =
            nodes.iter().map(|node| self.evalNode(env.clone(), node)).collect();
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

    fn evalQuotedPair(&self, pair_literal: &[SyntaxTreeNode]) -> EvalResult
    {
        if pair_literal.len() != 3
        {
            return Err(rterr!("Invalid dot-pair"));
        }

        Ok(Value::List(Cons::new(self.evalQuote(&pair_literal[0])?,
                                 self.evalQuote(&pair_literal[2])?)))
    }

    fn evalQuote(&self, quoted: &SyntaxTreeNode) -> EvalResult
    {
        match quoted
        {
            SyntaxTreeNode::Atom(token) => self.quoteAtom(token),
            SyntaxTreeNode::Compound(nodes) =>
            {
                // Is it like '(a . b)?
                if nodes.len() >= 2
                {
                    if let SyntaxTreeNode::Atom(token) = &nodes[1]
                    {
                        if token.value() == &TokenValue::Dot
                        {
                            return self.evalQuotedPair(&nodes[..]);
                        }
                    }
                }

                let mut last = Value::null();
                for node in nodes.iter().rev()
                {
                    last = Value::List(Cons::new(self.evalQuote(node)?, last));
                }

                Ok(last)
            },
        }
    }

    fn evalIf(&self, env: Environment, condition: &SyntaxTreeNode, branch_true: &SyntaxTreeNode,
              branch_false: &[SyntaxTreeNode]) -> EvalResult
    {
        if let Value::Bool(c) = self.evalNode(env.clone(), condition)?
        {
            if c
            {
                self.evalNode(env, branch_true)
            }
            else
            {
                if branch_false.len() > 0
                {
                    Ok(self.evalEach(env, branch_false)?.pop().unwrap())
                }
                else
                {
                    Ok(Value::null())
                }
            }
        }
        else
        {
            Err(rterr!("Invalid condition in if"))
        }
    }

    fn evalCond(&self, env: Environment, branches: &[SyntaxTreeNode]) -> EvalResult
    {
        for branch in branches
        {
            let exprs = if let Some(xs) = branch.getNodes() {xs}
            else { return Err(rterr!("Invalid cond branch")); };

            if exprs.len() < 2
            {
                return Err(rterr!("Invalid cond branch"));
            }

            if let Some(token) = exprs[0].getToken()
            {
                if let TokenValue::Ident(name) = token.value()
                {
                    if &name[..] == "else"
                    {
                        return Ok(self.evalEach(env.clone(), &exprs[1..])?.pop().unwrap())
                    }
                }
            }

            let cond = if let Value::Bool(c) = self.evalNode(env.clone(), &exprs[0])?
            {
                c
            }
            else
            {
                return Err(rterr!("Invalid cond condition"));
            };

            if cond
            {
                return Ok(self.evalEach(env, &exprs[1..])?.pop().unwrap())
            }
        }
        Ok(Value::null())
    }

    fn evalLetStar(&self, env: Environment, binding_node: &SyntaxTreeNode,
                   rest: &[SyntaxTreeNode]) -> EvalResult
    {
        let bindings = if let Some(ns) = binding_node.getNodes(){ ns }
        else { return Err(rterr!("Invalid let* bindings")); };
        if rest.is_empty()
        {
            return Err(rterr!("Empty body in let*"));
        }

        let env = env.derive();

        for binding in bindings
        {
            let nodes = if let Some(ns) = binding.getNodes(){ ns }
            else { return Err(rterr!("Invalid let* bindings")); };
            if nodes.len() != 2
            {
                return Err(rterr!("Invalid let* binding"));
            }
            let token = nodes[0].getToken().ok_or_else(
                || rterr!("Invalid variable in let* binding"))?;
            let name = token.value().getIdentifier().ok_or_else(
                || rterr!("Invalid variable in let* binding"))?;

            let value = self.evalNode(env.clone(), &nodes[1])?;
            env.define(name, value);
        }

        Ok(self.evalEach(env, rest)?.pop().unwrap())
    }

    fn evalLambda(&self, env: Environment, formals: &SyntaxTreeNode,
                  body: &[SyntaxTreeNode]) -> EvalResult
    {
        let arg_nodes = formals.getNodes().ok_or_else(
            || rterr!("Arguments in lambda should be a list"))?;
        let mut args = Vec::new();
        for node in arg_nodes
        {
            args.push(node.getIdentifier().ok_or_else(
                || rterr!("Arguments in lambda should be identifiers"))?
                      .to_owned());
        }

        let mut new_body = Vec::new();
        for node in body
        {
            new_body.push(node.clone());
        }
        Ok(Value::Procedure(Procedure::fromArgsBody(env, args, new_body)))
    }

    fn evalBegin(&self, env: Environment, body: &[SyntaxTreeNode]) ->
        EvalResult
    {
        if body.is_empty()
        {
            Ok(Value::null())
        }
        else
        {
            Ok(self.evalEach(env, body)?.pop().unwrap())
        }
    }

    fn evalDefineFunction(&self, env: Environment, sig: &[SyntaxTreeNode],
                          body: &[SyntaxTreeNode]) -> EvalResult
    {
        if sig.is_empty()
        {
            return Err(rterr!("Invalid function definition"));
        }

        let name = sig[0].getIdentifier().ok_or_else(
            || rterr!("Define: function name should be an identifier"))?;
        let mut args = Vec::new();
        for arg in &sig[1..]
        {
            let arg_name = arg.getIdentifier().ok_or_else(
                || rterr!("Define: function argument should be an indentifier"))?;
            args.push(arg_name.to_owned());
        }
        let mut body_form = Vec::new();
        for expr in body
        {
            body_form.push(expr.clone());
        }
        let mut f = Procedure::fromArgsBody(env.clone(), args, body_form);
        f.name = Some(name.to_owned());
        let value = Value::Procedure(f);
        env.define(name, value);

        Ok(Value::null())
    }

    fn evalDefine(&self, env: Environment, var: &SyntaxTreeNode,
                  form: &[SyntaxTreeNode]) -> EvalResult
    {
        match var
        {
            SyntaxTreeNode::Atom(_) =>
            {
                let name = var.getIdentifier().ok_or_else(
                    || rterr!("Invalid identifier in define"))?;
                if form.len() != 1
                {
                    return Err(rterr!("Wrong number of expressions in define"));
                }

                let mut value = self.evalNode(env.clone(), &form[0])?;
                if let Value::Procedure(f) = &mut value
                {
                    f.name = Some(name.to_owned());
                }
                env.define(name, value);
                Ok(Value::null())
            },

            // In principle, function definition using define is not a
            // special form. But here I treat it as one for
            // simplicity.
            SyntaxTreeNode::Compound(sig) =>
                self.evalDefineFunction(env, &sig, form),
        }
    }

    fn evalSet(&self, env: Environment, var: &SyntaxTreeNode,
               form: &[SyntaxTreeNode]) -> EvalResult
    {
        match var
        {
            SyntaxTreeNode::Atom(_) =>
            {
                let name = var.getIdentifier().ok_or_else(
                    || rterr!("Invalid identifier in define"))?;
                if form.len() != 1
                {
                    return Err(rterr!("Wrong number of expressions in define"));
                }

                let mut value = self.evalNode(env.clone(), &form[0])?;
                if let Value::Procedure(f) = &mut value
                {
                    f.name = Some(name.to_owned());
                }

                if !env.set(name, value)
                {
                    Err(rterr!("Undefined variable: {}", name))
                }
                else
                {
                    Ok(Value::null())
                }
            },

            // In principle, function definition using define is not a
            // special form. But here I treat it as one for
            // simplicity.
            SyntaxTreeNode::Compound(sig) =>
                self.evalDefineFunction(env, &sig, form),
        }
    }

    fn evalNamedCompound(&self, env: Environment, name: &str,
                         rest: &[SyntaxTreeNode]) -> EvalResult
    {
        // Specials
        match name
        {
            "if" => return self.evalIf(env, &rest[0], &rest[1], &rest[2..]),
            "cond" => return self.evalCond(env, rest),
            "let*" => return self.evalLetStar(env, &rest[0], &rest[1..]),
            "lambda" => return self.evalLambda(env, &rest[0], &rest[1..]),
            "begin" => return self.evalBegin(env, rest),
            "define" => return self.evalDefine(env, &rest[0], &rest[1..]),
            "set!" => return self.evalSet(env, &rest[0], &rest[1..]),
            _ => {},
        }

        // Procedures
        if let Some(value) = env.find(name)
        {
            self.evalHeadTail(env, value, rest)
        }
        else
        {
            Err(rterr!("Undefined procedure: {}", name))
        }
    }

    fn evalProcedureCall(&self, env: Environment, f: &Procedure,
                         mut args: Vec<Value>) -> EvalResult
    {
        let arg_names = f.arguments();
        if args.len() != arg_names.len()
        {
            return Err(rterr!(
                "Wrong number of arguments for procedure {}. Expect {}, got {}.",
                f, arg_names.len(), args.len()));
        }
        if f.empty()
        {
            return Ok(Value::null());
        }

        let env = env.derive();
        for i in (0..args.len()).rev()
        {
            env.define(&arg_names[i], args.pop().unwrap());
        }
        Ok(self.evalEach(env, f.body())?.pop().unwrap())
    }

    fn evalHeadTail(&self, env: Environment, head: Value,
                    tail: &[SyntaxTreeNode]) -> EvalResult
    {
        let args = self.evalEach(env.clone(), tail)?;
        match head
        {
            Value::Builtin(f) => f.call(&args, env),
            Value::Procedure(f) => self.evalProcedureCall(env, &f, args),
            _ => Err(rterr!("Calling a non-procedure")),
        }
    }

    fn evalCompound(&self, env: Environment, nodes: &[SyntaxTreeNode]) -> EvalResult
    {
        let head: &SyntaxTreeNode = &nodes[0];

        // TODO: See if nodes[1] is a dot. If it is, it’s an error.

        match head
        {
            SyntaxTreeNode::Atom(token) =>
            {
                match token.value()
                {
                    TokenValue::Ident(name) =>
                        self.evalNamedCompound(env, name, &nodes[1..]),
                    TokenValue::Quote =>
                        self.evalQuote(&nodes[1]),
                    _ => Err(rterr!("Unimplemented")),
                }
            },
            SyntaxTreeNode::Compound(_) =>
            {
                let head = self.evalNode(env.clone(), head)?;
                self.evalHeadTail(env, head, &nodes[1..])
            }
        }
    }

    fn evalNode(&self, env: Environment, node: &SyntaxTreeNode) -> EvalResult
    {
        match node
        {
            SyntaxTreeNode::Atom(token) =>
                self.evalAtom(env, token),
            SyntaxTreeNode::Compound(nodes) =>
                self.evalCompound(env, nodes),
        }
    }

    pub fn eval(&self) -> EvalResult
    {
        let mut result = self.evalEach(self.env.clone(), &self.code)?;
        if result.is_empty()
        {
            Ok(Value::null())
        }
        else
        {
            Ok(result.pop().unwrap())
        }
    }

    pub fn evalSource(src: &str) -> EvalResult
    {
        let tokens = crate::tokenizer::tokenize(src)?;
        let roots = SyntaxTreeNode::parse(tokens)?;
        Self::new(roots).eval()
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
            assert_eq!(vs.car(), Value::Symbol("a".to_owned()));
            if let Value::List(tail) = vs.cdr()
            {
                assert_eq!(tail.car(), Value::Symbol("b".to_owned()));
                assert_eq!(tail.cdr(), Value::null());
            }
        }
        else
        {
            return Err(error!(RuntimeError, "Test failed"));
        }
        Ok(())
    }

    #[test]
    fn if_simple() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(if #t 1)"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::evalSource(r#"(if #f 1)"#)?;
        assert_eq!(result, Value::null());
        let result = Evaluator::evalSource(r#"(if #f 1 0)"#)?;
        assert_eq!(result, Value::Integer(0));
        let result = Evaluator::evalSource(r#"(if #f 1 2 3)"#)?;
        assert_eq!(result, Value::Integer(3));
        assert!(Evaluator::evalSource(r#"(if 1 1 )"#).is_err());
        Ok(())
    }

    #[test]
    fn calculated_head() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"((if #f + *) 3 4)"#)?;
        assert_eq!(result, Value::Integer(12));
        Ok(())
    }

    #[test]
    fn cond() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(cond (#t 1) (#t 2))"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::evalSource(r#"(cond (#f 1) (#t (+ 1 1)))"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(cond (#f 1) (#t 2 3))"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::evalSource(r#"(cond (#f 1) (else 2 3))"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::evalSource(r#"(cond (else 2) (#t 1))"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(cond (#f 1))"#)?;
        assert_eq!(result, Value::null());
        Ok(())
    }

    #[test]
    fn letstar() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(let* ((x 1)) (+ x 1))"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(
            r#"(let* ((x 1) (y 2)) (+ x 1) (+ x y))"#)?;
        assert_eq!(result, Value::Integer(3));
        Ok(())
    }

    #[test]
    fn procedure() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"((lambda (x) (+ x 1)) 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"((lambda (x) 0 (+ x 1)) 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn begin() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(begin 1 2)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(begin 1 (begin 3 4))"#)?;
        assert_eq!(result, Value::Integer(4));
        Ok(())
    }

    #[test]
    fn define_variable() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(define x 2) x"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(define f (lambda (x) (+ x 1)))
(f 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn define_function() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(define (f x) (+ x 1)) (f 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::evalSource(r#"(define (f x) (+ x 1))
(define (g x) (* (f x) 2))
(g 1)"#)?;
        assert_eq!(result, Value::Integer(4));
        let result = Evaluator::evalSource(r#"(define x 100)
(define (f x) (+ x 1))
(f 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn set() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"(define x 2) (set! x 1) x"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::evalSource(r#"(define x 2)
(define (f y)
  (set! x y))
(f 1) x"#)?;
        assert_eq!(result, Value::Integer(1));
        assert!(Evaluator::evalSource(r#"(define x 2) (set! y 1)"#).is_err());
        Ok(())
    }

    #[test]
    fn list_display() -> Result<(), Error>
    {
        let result = Evaluator::evalSource(r#"'(1 2)"#)?;
        assert_eq!(format!("{}", result), "(1 2)");
        let result = Evaluator::evalSource(r#"'((1 3) 2)"#)?;
        assert_eq!(format!("{}", result), "((1 3) 2)");
        let result = Evaluator::evalSource(r#"'((1 2) . 3)"#)?;
        assert_eq!(format!("{}", result), "((1 2) . 3)");
        let result = Evaluator::evalSource(r#"'((1 2) . ())"#)?;
        assert_eq!(format!("{}", result), "((1 2))");
        Ok(())
    }

}
