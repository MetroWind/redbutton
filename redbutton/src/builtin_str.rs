use shared::error;
use shared::error::Error;

use scheme::value::Value;
use scheme::builtin::registerBuiltin;
use scheme::environment::Environment;

use crate::utils::SimpleTemplate;

fn format(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Format", args >= 1);
    let template_str: &str = builtin_define_arg!("Format", args[0]: String);

    let mut template = SimpleTemplate::new(&template_str);
    for kv_pair in &args[1..]
    {
        let cons = if let Value::List(c) = kv_pair
        {
            c
        }
        else
        {
            return Err(rterr!("Invalid key-value pair in format"));
        };

        let key = if let Value::String(s) = cons.car()
        {
            s
        }
        else
        {
            return Err(rterr!("Invalid key in format"));
        };

        template = template.apply(&key, cons.cdr());
    }
    Ok(Value::String(template.result()))
}

pub fn getBuiltinEnv() -> Environment
{
    let result = Environment::new();
    registerBuiltin(&result, "format", format);
    result
}

#[cfg(test)]
mod tests
{
    use super::*;
    use scheme::eval::Evaluator;

    fn getEval() -> Evaluator
    {
        let e = Evaluator::new();
        e.env().merge(getBuiltinEnv());
        e
    }

    #[test]
    fn format() -> Result<(), Error>
    {
        let result = getEval().evalSource(
            r#"(format "It's a %{test}~" (cons "test" (list)))"#)?;
        assert_eq!(result, Value::String("It's a ()~".to_owned()));
        let result = getEval().evalSource(
            r#"(format "It's %{another} %{test}~"
(cons "test" (list))
'("another" . 222))"#)?;
        assert_eq!(result, Value::String("It's 222 ()~".to_owned()));
        Ok(())
    }

}
