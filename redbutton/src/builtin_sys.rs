use shared::error;
use shared::error::Error;

use scheme::value::Value;
use scheme::builtin::registerBuiltin;
use scheme::environment::Environment;

fn env(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Get-env", args == 1);
    let var: &str = builtin_define_arg!("Get-env", args[0]: String);

    if let Ok(value) = std::env::var(var)
    {
        Ok(Value::String(value))
    }
    else
    {
        Ok(Value::null())
    }
}

fn confRepo(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Conf-repo", args == 1);
    let path: &str = builtin_define_arg!("Conf-repo", args[0]: String);

    std::env::set_current_dir(path).map_err(
        |e| rterr!("Failed to change dir to {}, {}", path, e))?;
    Ok(Value::null())
}

pub fn getBuiltinEnv() -> Environment
{
    let result = Environment::new();
    registerBuiltin(&result, "get-env", env);
    registerBuiltin(&result, "conf-repo", confRepo);
    result
}
