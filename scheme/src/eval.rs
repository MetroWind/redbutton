use shared::error;
use shared::error::Error;

use crate::tokenizer::{Token, TokenValue};
use crate::parser::SyntaxTreeNode;
use crate::environment::Environment;
use crate::value::{Value, Procedure, Cons, ProcedureArguments};
use crate::builtin;

pub struct Evaluator
{
    env: Environment,
}

pub type EvalResult = Result<Value, Error>;

impl Evaluator
{
    pub fn new() -> Self
    {
        Self{ env: builtin::getBuiltinEnv() }
    }

    pub fn env(&self) -> Environment
    {
        self.env.clone()
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
        let mut new_body = Vec::new();
        for node in body
        {
            new_body.push(node.clone());
        }
        Ok(Value::Procedure(Procedure::fromArgsBody(
            env, ProcedureArguments::fromLambdaFormal(formals)?, new_body)))
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
        let mut body_form = Vec::new();
        for expr in body
        {
            body_form.push(expr.clone());
        }
        let mut f = Procedure::fromArgsBody(
            env.clone(), ProcedureArguments::fromDefineSig(sig)?, body_form);
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

    fn evalAnd(&self, env: Environment, rest: &[SyntaxTreeNode]) -> EvalResult
    {
        let mut result = Value::Bool(true);
        for expr in rest
        {
            let value = self.evalNode(env.clone(), expr)?;
            if value.toBool()
            {
                result = value;
            }
            else
            {
                return Ok(value);
            }
        }
        Ok(result)
    }

    fn evalOr(&self, env: Environment, rest: &[SyntaxTreeNode]) -> EvalResult
    {
        let mut result = Value::Bool(false);
        for expr in rest
        {
            let value = self.evalNode(env.clone(), expr)?;
            if value.toBool()
            {
                return Ok(value);
            }
            else
            {
                result = value;
            }
        }
        Ok(result)
    }

    fn evalApply(&self, env: Environment, proc: &SyntaxTreeNode,
                 args: &[SyntaxTreeNode]) -> EvalResult
    {
        if args.len() != 1
        {
            return Err(rterr!("Invalid apply arguments"));
        }

        let func = self.evalNode(env.clone(), proc)?;
        let args_or_null = self.evalNode(env.clone(), &args[0])?;
        let true_args: Vec<Value> = if args_or_null == Value::null()
        {
            Vec::new()
        }
        else
        {
            args_or_null.list2Vec()
                .ok_or_else(|| rterr!("Invalid apply arguments"))?
        };

        match func
        {
            Value::Builtin(f) => f.call(&true_args, env),
            Value::Procedure(f) => self.evalProcedureCall(env, &f, true_args),
            _ => Err(rterr!("Calling a non-procedure {}", func)),
        }
    }

    fn evalNamedCompound(&self, env: Environment, name: &str,
                         rest: &[SyntaxTreeNode]) -> EvalResult
    {
        // Special forms
        match name
        {
            "if" => return self.evalIf(env, &rest[0], &rest[1], &rest[2..]),
            "cond" => return self.evalCond(env, rest),
            "let*" => return self.evalLetStar(env, &rest[0], &rest[1..]),
            "lambda" => return self.evalLambda(env, &rest[0], &rest[1..]),
            "begin" => return self.evalBegin(env, rest),
            "define" => return self.evalDefine(env, &rest[0], &rest[1..]),
            "set!" => return self.evalSet(env, &rest[0], &rest[1..]),
            "and" => return self.evalAnd(env, rest),
            "or" => return self.evalOr(env, rest),
            "apply" => return self.evalApply(env, &rest[0], &rest[1..]),
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
                         args: Vec<Value>) -> EvalResult
    {
        let env = env.derive();
        env.matchArgs(f, args)?;

        if f.empty()
        {
            return Ok(Value::null());
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
            _ => Err(rterr!("Calling a non-procedure {}", head)),
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
                    _ => Err(rterr!("Unimplemented: {}", token.src())),
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

    pub fn eval(&self, code: Vec<SyntaxTreeNode>) -> EvalResult
    {
        let mut result = self.evalEach(self.env.clone(), &code)?;
        if result.is_empty()
        {
            Ok(Value::null())
        }
        else
        {
            Ok(result.pop().unwrap())
        }
    }

    pub fn evalSource(&self, src: &str) -> EvalResult
    {
        let tokens = crate::tokenizer::tokenize(src)?;
        let roots = SyntaxTreeNode::parse(tokens)?;
        self.eval(roots)
    }

    pub fn justEvalSource(src: &str) -> EvalResult
    {
        let tokens = crate::tokenizer::tokenize(src)?;
        let roots = SyntaxTreeNode::parse(tokens)?;
        Self::new().eval(roots)
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
        let result = Evaluator::justEvalSource(r#"(+ 1 2)"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::justEvalSource(r#"(+ 1.6 2)"#)?;
        assert_float_eq(result, 3.6);
        let result = Evaluator::justEvalSource(r#"(+ 1 2.0)"#)?;
        assert_float_eq(result, 3.0);
        let result = Evaluator::justEvalSource(r#"(+ 1 (+ 2.5 1) 2)"#)?;
        assert_float_eq(result, 6.5);
        Ok(())
    }

    #[test]
    fn minus() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(- 1 2)"#)?;
        assert_eq!(result, Value::Integer(-1));
        let result = Evaluator::justEvalSource(r#"(- 1.6 2)"#)?;
        assert_float_eq(result, -0.4);
        let result = Evaluator::justEvalSource(r#"(- 1 2.0)"#)?;
        assert_float_eq(result, -1.0);
        let result = Evaluator::justEvalSource(r#"(- 1 (- 2.5 1) 2)"#)?;
        assert_float_eq(result, -2.5);
        Ok(())
    }

    #[test]
    fn multiply() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(* 1 2)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(* 1.6 2)"#)?;
        assert_float_eq(result, 3.2);
        let result = Evaluator::justEvalSource(r#"(* 1 2.0)"#)?;
        assert_float_eq(result, 2.0);
        let result = Evaluator::justEvalSource(r#"(* 1 (* 2.5 1) 2)"#)?;
        assert_float_eq(result, 5.0);
        Ok(())
    }

    #[test]
    fn divide() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(/ 2 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(/ 1 2)"#)?;
        assert_float_eq(result, 0.5);
        let result = Evaluator::justEvalSource(r#"(/ 1.6 2)"#)?;
        assert_float_eq(result, 0.8);
        let result = Evaluator::justEvalSource(r#"(/ 1 2.0)"#)?;
        assert_float_eq(result, 0.5);
        let result = Evaluator::justEvalSource(r#"(/ 1 (/ 2.5 1) 2)"#)?;
        assert_float_eq(result, 1.0 / 2.5 / 2.0);
        Ok(())
    }

    #[test]
    fn quote() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"'1"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"'a"#)?;
        assert_eq!(result, Value::Symbol(String::from("a")));
        let result = Evaluator::justEvalSource(r#"'(a b)"#)?;
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
        let result = Evaluator::justEvalSource(r#"(if #t 1)"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"(if #f 1)"#)?;
        assert_eq!(result, Value::null());
        let result = Evaluator::justEvalSource(r#"(if #f 1 0)"#)?;
        assert_eq!(result, Value::Integer(0));
        let result = Evaluator::justEvalSource(r#"(if #f 1 2 3)"#)?;
        assert_eq!(result, Value::Integer(3));
        assert!(Evaluator::justEvalSource(r#"(if 1 1 )"#).is_err());
        Ok(())
    }

    #[test]
    fn calculated_head() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"((if #f + *) 3 4)"#)?;
        assert_eq!(result, Value::Integer(12));
        Ok(())
    }

    #[test]
    fn cond() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(cond (#t 1) (#t 2))"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"(cond (#f 1) (#t (+ 1 1)))"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(cond (#f 1) (#t 2 3))"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::justEvalSource(r#"(cond (#f 1) (else 2 3))"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::justEvalSource(r#"(cond (else 2) (#t 1))"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(cond (#f 1))"#)?;
        assert_eq!(result, Value::null());
        Ok(())
    }

    #[test]
    fn letstar() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(let* ((x 1)) (+ x 1))"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(
            r#"(let* ((x 1) (y 2)) (+ x 1) (+ x y))"#)?;
        assert_eq!(result, Value::Integer(3));
        Ok(())
    }

    #[test]
    fn procedure() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"((lambda (x) (+ x 1)) 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"((lambda (x) 0 (+ x 1)) 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn lambda_alt() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"((lambda x x) 1 2)"#)?;
        assert_eq!(result.to_string(), "(1 2)");
        let result = Evaluator::justEvalSource(r#"((lambda x x))"#)?;
        assert_eq!(result, Value::null());
        let result = Evaluator::justEvalSource(
            r#"((lambda (x y . z) z) 3 4 5 6)"#)?;
        assert_eq!(result.to_string(), "(5 6)");
        let result = Evaluator::justEvalSource(
            r#"((lambda (x y . z) z) 3 4)"#)?;
        assert_eq!(result, Value::null());
        Ok(())
    }

    #[test]
    fn begin() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(begin 1 2)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(begin 1 (begin 3 4))"#)?;
        assert_eq!(result, Value::Integer(4));
        Ok(())
    }

    #[test]
    fn define_variable() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(define x 2) x"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(define f (lambda (x) (+ x 1)))
(f 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn define_function() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(define (f x) (+ x 1)) (f 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        let result = Evaluator::justEvalSource(r#"(define (f x) (+ x 1))
(define (g x) (* (f x) 2))
(g 1)"#)?;
        assert_eq!(result, Value::Integer(4));
        let result = Evaluator::justEvalSource(r#"(define x 100)
(define (f x) (+ x 1))
(f 1)"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn define_function_alt() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(define (f . x) x) (f 1)"#)?;
        assert_eq!(result.to_string(), "(1)");
        let result = Evaluator::justEvalSource(r#"(define (f . x) x) (f)"#)?;
        assert_eq!(result, Value::null());
        let result = Evaluator::justEvalSource(
            r#"(define (f x . y) (+ x (car (cdr y)))) (f 1 2 3)"#)?;
        assert_eq!(result, Value::Integer(4));
        Ok(())
    }

    #[test]
    fn set() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(define x 2) (set! x 1) x"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"(define x 2)
(define (f y)
  (set! x y))
(f 1) x"#)?;
        assert_eq!(result, Value::Integer(1));
        assert!(Evaluator::justEvalSource(r#"(define x 2) (set! y 1)"#).is_err());
        Ok(())
    }

    #[test]
    fn list_display() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"'(1 2)"#)?;
        assert_eq!(format!("{}", result), "(1 2)");
        let result = Evaluator::justEvalSource(r#"'((1 3) 2)"#)?;
        assert_eq!(format!("{}", result), "((1 3) 2)");
        let result = Evaluator::justEvalSource(r#"'((1 2) . 3)"#)?;
        assert_eq!(format!("{}", result), "((1 2) . 3)");
        let result = Evaluator::justEvalSource(r#"'((1 2) . ())"#)?;
        assert_eq!(format!("{}", result), "((1 2))");
        Ok(())
    }

    #[test]
    fn car() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(car '(1))"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"(car '((1)))"#)?;
        assert_eq!(format!("{}", result), "(1)");
        let result = Evaluator::justEvalSource(r#"(car '(1) '(2))"#);
        assert!(result.is_err());
        Ok(())
    }

    #[test]
    fn cdr() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(cdr '(1))"#)?;
        assert_eq!(result, Value::null());
        let result = Evaluator::justEvalSource(r#"(cdr '(() (1)))"#)?;
        assert_eq!(format!("{}", result), "((1))");
        let result = Evaluator::justEvalSource(r#"(cdr '(1) '(2))"#);
        assert!(result.is_err());
        Ok(())
    }

    #[test]
    fn and() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(and 1 2 'c '(f g))"#)?;
        assert_eq!(result.to_string(), "(f g)");
        let result = Evaluator::justEvalSource(r#"(and)"#)?;
        assert_eq!(result, Value::Bool(true));

        // (if) is not evaluated.
        let result = Evaluator::justEvalSource(r#"(and #f (if))"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn or() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(or 1 (if))"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"(or)"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn not() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(not #t)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(not 3)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(not #f)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(not '())"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn equal() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(equal2 1 1.0)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(equal2 1 2)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(equal2 "aaa" "aaa")"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(equal2 "aaa" 'aaa)"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn num_less_than() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(< 1 1)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(< 1 2)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(< 1 1.01 1.02 2)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(< 1 1.0 1.01)"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn num_less_or_equal() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(<= 1 1)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(<= 1 2)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(<= 1 0.99)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(<= 1 1.0 1.01)"#)?;
        assert_eq!(result, Value::Bool(true));
        Ok(())
    }

    #[test]
    fn num_greater_than() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(> 1 1)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(> 2 1)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(> 2 1.02 1.01 1)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(> 1.01 1.0 1)"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn num_greater_or_equal() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(>= 1 1)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(>= 2 1)"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(>= 0.99 1)"#)?;
        assert_eq!(result, Value::Bool(false));
        let result = Evaluator::justEvalSource(r#"(>= 1.01 1.0 1)"#)?;
        assert_eq!(result, Value::Bool(true));
        Ok(())
    }

    #[test]
    fn nullp() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(null? '())"#)?;
        assert_eq!(result, Value::Bool(true));
        let result = Evaluator::justEvalSource(r#"(null? 0)"#)?;
        assert_eq!(result, Value::Bool(false));
        Ok(())
    }

    #[test]
    fn cons() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(cons 'a 2)"#)?;
        assert_eq!(result.to_string(), "(a . 2)");
        let result = Evaluator::justEvalSource(r#"(cons 'a '())"#)?;
        assert_eq!(result.to_string(), "(a)");
        Ok(())
    }

    #[test]
    fn list() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(list 'a 2)"#)?;
        assert_eq!(result.to_string(), "(a 2)");
        let result = Evaluator::justEvalSource(r#"(list 'a '())"#)?;
        assert_eq!(result.to_string(), "(a ())");
        let result = Evaluator::justEvalSource(r#"(list)"#)?;
        assert_eq!(result.to_string(), "()");
        Ok(())
    }

    #[test]
    fn append() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(append '(x) '(y))"#)?;
        assert_eq!(result.to_string(), "(x y)");
        let result = Evaluator::justEvalSource(r#"(append '())"#)?;
        assert_eq!(result.to_string(), "()");
        let result = Evaluator::justEvalSource(r#"(append '(a) '(b c d))"#)?;
        assert_eq!(result.to_string(), "(a b c d)");
        let result = Evaluator::justEvalSource(r#"(append '(a (b)) '((c)))"#)?;
        assert_eq!(result.to_string(), "(a (b) (c))");
        let result = Evaluator::justEvalSource(r#"(append '(a b) '(c . d))"#)?;
        assert_eq!(result.to_string(), "(a b c . d)");
        let result = Evaluator::justEvalSource(r#"(append '() 'a)"#)?;
        assert_eq!(result.to_string(), "a");
        Ok(())
    }

    #[test]
    fn reverse() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(reverse '(a b c))"#)?;
        assert_eq!(result.to_string(), "(c b a)");
        let result = Evaluator::justEvalSource(r#"(reverse '())"#)?;
        assert_eq!(result.to_string(), "()");
        let result = Evaluator::justEvalSource(r#"(reverse '(a (b c) d (e (f))))"#)?;
        assert_eq!(result.to_string(), "((e (f)) d (b c) a)");
        Ok(())
    }

    #[test]
    fn quotient() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(quotient 13 4)"#)?;
        assert_eq!(result, Value::Integer(3));
        Ok(())
    }

    #[test]
    fn remainder() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(remainder 13 4)"#)?;
        assert_eq!(result, Value::Integer(1));
        let result = Evaluator::justEvalSource(r#"(remainder -13 4)"#)?;
        assert_eq!(result, Value::Integer(-1));
        Ok(())
    }

    #[test]
    fn string_length() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(string-length "abc")"#)?;
        assert_eq!(result, Value::Integer(3));
        let result = Evaluator::justEvalSource(r#"(string-length "测试")"#)?;
        assert_eq!(result, Value::Integer(2));
        Ok(())
    }

    #[test]
    fn substring() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(substring "abcdef" 2 4)"#)?;
        assert_eq!(result, Value::String("cd".to_owned()));
        let result = Evaluator::justEvalSource(r#"(substring "abcdef" 2 2)"#)?;
        assert_eq!(result, Value::String(String::new()));
        Ok(())
    }

    #[test]
    fn string_append() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(string-append "a")"#)?;
        assert_eq!(result, Value::String("a".to_owned()));
        let result = Evaluator::justEvalSource(r#"(string-append "a" "b")"#)?;
        assert_eq!(result, Value::String("ab".to_owned()));
        let result = Evaluator::justEvalSource(r#"(string-append "a" "b" "c")"#)?;
        assert_eq!(result, Value::String("abc".to_owned()));
        Ok(())
    }

    #[test]
    fn apply() -> Result<(), Error>
    {
        let result = Evaluator::justEvalSource(r#"(apply + '(1 2 3))"#)?;
        assert_eq!(result, Value::Integer(6));
        let result = Evaluator::justEvalSource(r#"(define (f) 1) (apply f '())"#)?;
        assert_eq!(result, Value::Integer(1));
        Ok(())
    }

}
