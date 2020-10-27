use shared::error;
use shared::error::Error;

use crate::value::{self, Value, Builtin};
use crate::environment::Environment;
use crate::eval;

/// Register a builtin to a environment.
///
/// Example: `register_builtin!(env, "+", add)` regiesters function
/// `add` as a builtin named `+` in environment `env`.
pub fn registerBuiltin(env: &Environment, name: &str,
                       f: fn(&[Value], Environment) -> Result<Value, Error>)
{
    env.define(name, Value::Builtin(Builtin::new(name, f)));
}

#[macro_export]
macro_rules! builtin_args_check
{
    ($name: literal, $args:ident = $count: literal) =>
    {
        if $args.len() != $count
        {
            return Err(rterr!("{} expects {} argument(s)", $name, $count));
        }
    };
}

#[macro_export]
macro_rules! builtin_define_arg
{
    ($func_name: literal, $args:ident[$index: literal]: String) =>
    {
        if let Value::String(x__) = &$args[$index]
        {
            x__
        }
        else
        {
            return Err(rterr!("{}'s argument {} should be a {}",
                              $func_name, $index, stringify!($variant)));
        }
    };

    ($func_name: literal, $args:ident[$index: literal]: $variant: ident) =>
    {
        if let Value::$variant(x__) = $args[$index]
        {
            x__
        }
        else
        {
            return Err(rterr!("{}'s argument {} should be a {}",
                              $func_name, $index, stringify!($variant)));
        }
    };
}

macro_rules! make_arithm
{
    ($name:ident, $op:tt) =>
    {
        fn $name(args: &[Value], _: Environment) -> Result<Value, Error>
        {
            if args.len() < 2
            {
                return Err(rterr!("Too few argument for {}", stringify!($name)));
            }

            let mut result_int: i64 = args[0].asInt().ok_or_else(
                || rterr!("Invalid argument for {}", stringify!($name)))?;
            let mut result_float: f64 = args[0].asFloat().ok_or_else(
                || rterr!("Invalid argument for {}", stringify!($name)))?;
            let mut float = args[0].isFloat();

            for arg in &args[1..]
            {
                match arg
                {
                    Value::Integer(x) =>
                    {
                        if float
                        {
                            result_float $op *x as f64;
                        }
                        else
                        {
                            result_int $op x;
                        }
                    },
                    Value::Float(x) =>
                    {
                        if !float
                        {
                            float = true;
                            result_float = result_int as f64;
                        }
                        result_float $op x;
                    },
                    _ => return Err(error!(
                        RuntimeError,
                        format!("Invalid argument for {}", stringify!($name)))),
                }
            }
            if float
            {
                Ok(Value::Float(result_float))
            }
            else
            {
                Ok(Value::Integer(result_int))
            }
        }
    };
}

make_arithm!(add, +=);
make_arithm!(minus, -=);
make_arithm!(multiply, *=);

fn divide(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() < 2
    {
        return Err(rterr!("Too few argument for /"));
    }

    let mut result_int: i64 = args[0].asInt().ok_or_else(
        || rterr!("Invalid argument for /"))?;
    let mut result_float: f64 = args[0].asFloat().ok_or_else(
        || rterr!("Invalid argument for /"))?;
    let mut float = args[0].isFloat();

    for arg in &args[1..]
    {
        if float
        {
            let rhs = arg.asFloat().ok_or_else(
                || rterr!("Invalid argument for /"))?;
            result_float /= rhs;
        }
        else
        {
            match arg
            {
                Value::Integer(x) =>
                {
                    if result_int % x == 0
                    {
                        result_int /= *x;
                    }
                    else
                    {
                        float = true;
                        result_float = result_int as f64;
                        result_float /= *x as f64;
                    }
                },
                Value::Float(x) =>
                {
                    if !float
                    {
                        float = true;
                        result_float = result_int as f64;
                    }
                    result_float /= *x;
                },
                _ => return Err(rterr!("Invalid argument for /")),
            }
        }
    }
    if float
    {
        Ok(Value::Float(result_float))
    }
    else
    {
        Ok(Value::Integer(result_int))
    }
}

fn car(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() != 1
    {
        return Err(rterr!("Car expects 1 argument"));
    }

    match &args[0]
    {
        Value::List(cons) => Ok(cons.car()),
        _ => Err(rterr!("Car expects cons argument")),
    }
}

fn cdr(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() != 1
    {
        return Err(rterr!("Cdr expects 1 argument"));
    }

    match &args[0]
    {
        Value::List(cons) => Ok(cons.cdr()),
        _ => Err(rterr!("Cdr expects cons argument")),
    }
}

fn not(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() != 1
    {
        return Err(rterr!("“Not” expects 1 argument"));
    }

    Ok(Value::Bool(!args[0].toBool()))
}

fn numEqual(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() < 2
    {
        return Err(rterr!("= expects >1 arguments"));
    }

    let num = &args[0];
    for arg in &args[1..]
    {
        if !num.numericalEq(arg)
        {
            return Ok(Value::Bool(false));
        }
    }

    Ok(Value::Bool(true))
}

fn numLessThan(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() < 2
    {
        return Err(rterr!("< expects >1 arguments"));
    }

    let mut prev: &Value = &args[0];
    for arg in &args[1..]
    {
        if !prev.numericalLessThan(arg)
        {
            return Ok(Value::Bool(false));
        }
        prev = arg;
    }

    Ok(Value::Bool(true))
}

fn numLessOrEq(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() < 2
    {
        return Err(rterr!("<= expects >1 arguments"));
    }

    let mut prev = &args[0];
    for arg in &args[1..]
    {
        if !prev.numericalLessOrEq(arg)
        {
            return Ok(Value::Bool(false));
        }
        prev = arg;
    }

    Ok(Value::Bool(true))
}

fn numGreaterThan(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() < 2
    {
        return Err(rterr!("> expects >1 arguments"));
    }

    let mut prev: &Value = &args[0];
    for arg in &args[1..]
    {
        if !prev.numericalGreaterThan(arg)
        {
            return Ok(Value::Bool(false));
        }
        prev = arg;
    }

    Ok(Value::Bool(true))
}

fn numGreaterOrEq(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() < 2
    {
        return Err(rterr!(">= expects >1 arguments"));
    }

    let mut prev = &args[0];
    for arg in &args[1..]
    {
        if !prev.numericalGreaterOrEq(arg)
        {
            return Ok(Value::Bool(false));
        }
        prev = arg;
    }

    Ok(Value::Bool(true))
}

fn nullp(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() != 1
    {
        return Err(rterr!("Null? expects 1 argument"));
    }

    Ok(Value::Bool(args[0] == Value::null()))
}

fn cons(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() != 2
    {
        return Err(rterr!("Cons expects 2 argument"));
    }

    Ok(Value::List(value::Cons::new(args[0].clone(), args[1].clone())))
}

fn list(args: &[Value], e: Environment) -> Result<Value, Error>
{
    if args.is_empty()
    {
        Ok(Value::null())
    }
    else
    {
        cons(&[args[0].clone(), list(&args[1..], e.clone())?], e)
    }
}

fn append1(arg: &Value, to: Value) -> Result<Value, Error>
{
    match arg
    {
        Value::List(cons) =>
        {
            if cons.cdr() == Value::null()
            {
                Ok(Value::List(value::Cons::new(cons.car(), to)))
            }
            else
            {
                Ok(Value::List(value::Cons::new(
                    cons.car(), append1(&cons.cdr(), to)?)))
            }
        },
        Value::Null => Ok(to),
        _ => Err(rterr!("Append expects lists")),
    }
}

fn append(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.is_empty()
    {
        return Err(rterr!("Append expects >= 1 arguments"));
    }

    let mut result = args.last().unwrap().clone();
    for arg in args[..args.len() - 1].iter().rev()
    {
        result = append1(arg, result)?;
    }
    Ok(result)
}

// fn listLength(arg: &Value) -> Result<i64, Error>
// {
//     match arg
//     {
//         Value::Null => Ok(0),
//         Value::List(cons) => Ok(1 + listLength(&cons.cdr())?),
//         _ => Err(rterr!("Length: Not a proper list")),
//     }
// }

// fn length(args: &[Value], _: Environment) -> Result<Value, Error>
// {
//     if args.len() == 1
//     {
//         listLength(&args[0]).map(|n| Value::Integer(n))
//     }
//     else
//     {
//         Err(rterr!("Length expects 1 argument"))
//     }
// }

fn reverse(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() != 1
    {
        return Err(rterr!("Reverse expects 1 argument"));
    }

    if args[0] == Value::null()
    {
        return Ok(Value::null());
    }

    let v = args[0].list2ReversedVec().
        ok_or_else(|| rterr!("Reverse expects a list"))?;
    Ok(Value::from(v))
}

fn display(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() == 2
    {
        return Err(rterr!("Display to port is currently unimplemented"));
    }

    if args.len() != 1
    {
        return Err(rterr!("Display expects 1 argument"));
    }

    print!("{}", args[0]);
    Ok(Value::null())
}

fn newline(args: &[Value], _: Environment) -> Result<Value, Error>
{
    if args.len() == 1
    {
        return Err(rterr!("Newline to port is currently unimplemented"));
    }

    if !args.is_empty()
    {
        return Err(rterr!("newline does not take arguments"));
    }

    println!("");
    Ok(Value::null())
}

fn load(args: &[Value], env: Environment) -> Result<Value, Error>
{
    if args.len() != 1
    {
        return Err(rterr!("Load expects 1 argument"));
    }

    let filename: &str = if let Value::String(s) = &args[0]
    {
        s
    }
    else
    {
        return Err(rterr!("Load expects a string"));
    };

    let src = std::fs::read_to_string(filename)
        .map_err(|_| rterr!("Load: failed to open {}", filename))?;

    let tokens = crate::tokenizer::tokenize(&src)?;
    let roots = crate::parser::SyntaxTreeNode::parse(tokens)?;
    let e = eval::Evaluator::new();
    e.eval(roots)?;
    env.merge(e.env());
    Ok(Value::null())
}

pub fn getBuiltinEnv() -> Environment
{
    let result = Environment::new();
    registerBuiltin(&result, "+", add);
    registerBuiltin(&result, "-", minus);
    registerBuiltin(&result, "*", multiply);
    registerBuiltin(&result, "/", divide);
    registerBuiltin(&result, "car", car);
    registerBuiltin(&result, "cdr", cdr);
    registerBuiltin(&result, "not", not);
    registerBuiltin(&result, "=", numEqual);
    registerBuiltin(&result, "<", numLessThan);
    registerBuiltin(&result, "<=", numLessOrEq);
    registerBuiltin(&result, ">", numGreaterThan);
    registerBuiltin(&result, ">=", numGreaterOrEq);
    registerBuiltin(&result, "null?", nullp);
    registerBuiltin(&result, "cons", cons);
    registerBuiltin(&result, "list", list);
    registerBuiltin(&result, "append", append);
    registerBuiltin(&result, "reverse", reverse);
    registerBuiltin(&result, "display", display);
    registerBuiltin(&result, "newline", newline);
    registerBuiltin(&result, "load", load);
    result
}
