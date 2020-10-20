use shared::error;
use shared::error::Error;

use crate::environment::{Value, Environment, Builtin};

// Construct a RuntimeError
macro_rules! rterr
{
    ($msg:literal) => { error!(RuntimeError, $msg) };
    ($msg:literal $(, $x:expr)+) =>
    {
        error!(RuntimeError, format!($msg $(, $x)+))
    };
}

/// Register a builtin to a environment.
///
/// Example: `register_builtin!(env, "+", add)` regiesters function
/// `add` as a builtin named `+` in environment `env`.
pub fn registerBuiltin(env: &Environment, name: &str,
                       f: fn(&[Value], Environment) -> Result<Value, Error>)
{
    env.define(name, Value::Builtin(Builtin::new(name, f)));
}

macro_rules! make_arithm
{
    ($name:ident, $op:tt) =>
    {
        pub fn $name(args: &[Value], _: Environment) -> Result<Value, Error>
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

pub fn divide(args: &[Value], _: Environment) -> Result<Value, Error>
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

pub fn getBuiltinEnv() -> Environment
{
    let result = Environment::new();
    registerBuiltin(&result, "+", add);
    registerBuiltin(&result, "-", minus);
    registerBuiltin(&result, "*", multiply);
    registerBuiltin(&result, "/", divide);
    result
}
