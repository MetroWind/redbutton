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
    ($name: literal, $args:ident $op:tt $count: literal) =>
    {
        if !($args.len() $op $count)
        {
            return Err(rterr!("{} expects number of argument(s) {} {}",
                              $name, stringify!($op), $count));
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

fn equal2(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Equal2", args == 2);

    let result = match args[0]
    {
        Value::Integer(_) => args[0].numericalEq(&args[1]),
        Value::Float(_) => args[0].numericalEq(&args[1]),
        _ => args[0] == args[1],
    };

    Ok(Value::Bool(result))
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

    print!("{}", args[0].printStr()?);
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

fn quotient(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("quotient", args == 2);
    let lhs = builtin_define_arg!("quotient", args[0]: Integer);
    let rhs = builtin_define_arg!("quotient", args[1]: Integer);

    Ok(Value::Integer(lhs / rhs))
}

fn remainder(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("remainder", args == 2);
    let lhs = builtin_define_arg!("remainder", args[0]: Integer);
    let rhs = builtin_define_arg!("remainder", args[1]: Integer);

    Ok(Value::Integer(lhs % rhs))
}

fn toString(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("->string", args == 1);
    Ok(Value::String(args[0].to_string()))
}

fn strLen(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("String-length", args == 1);
    let s = builtin_define_arg!("String-length", args[0]: String);
    Ok(Value::Integer(s.chars().count() as i64))
}

fn substring(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Substring", args == 3);
    let s = builtin_define_arg!("Substring", args[0]: String);
    let begin = builtin_define_arg!("Substring", args[1]: Integer) as usize;
    let end = builtin_define_arg!("Substring", args[2]: Integer) as usize;

    Ok(Value::String(String::from(&s[begin..end])))
}

fn stringAppend(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("String-append", args >= 1);
    let mut s = builtin_define_arg!("String-append", args[0]: String).to_owned();

    for arg in &args[1..]
    {
        if let Value::String(ss) = arg
        {
            s = s + ss;
        }
        else
        {
            return Err(rterr!("String-append expects strings"));
        }
    }

    Ok(Value::String(s))
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
    registerBuiltin(&result, "equal2", equal2);
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
    registerBuiltin(&result, "quotient", quotient);
    registerBuiltin(&result, "remainder", remainder);
    registerBuiltin(&result, "->string", toString);
    registerBuiltin(&result, "string-length", strLen);
    registerBuiltin(&result, "substring", substring);
    registerBuiltin(&result, "string-append", stringAppend);
    result
}
