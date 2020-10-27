use std::fs;
use std::path::{Path, PathBuf};

use shared::error;
use shared::error::Error;

use scheme::value::{Value, Cons};
use scheme::builtin::{self, registerBuiltin};
use scheme::environment::Environment;

fn exists(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("File-exists", args = 1);
    let filename: &str = builtin_define_arg!("File-exists", args[0]: String);

    Ok(Value::Bool(Path::new(filename).exists()))
}

fn pathSplitBase(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Path-split-base", args = 1);
    let filename: &str = builtin_define_arg!("Path-split-base", args[0]: String);

    let path = Path::new(filename);
    let file = path.file_name().ok_or_else(
        || rterr!("Invalid filename in path-replace-parent: {}", filename))?
        .to_str().ok_or_else(
            || rterr!("path-split-base failed due to non-utf8 path"))?
        .to_owned();
    let dir = if let Some(parent) = path.parent()
    {
        parent.to_str().ok_or_else(
            || rterr!("path-split-base failed due to non-utf8 path"))?.to_owned()
    }
    else
    {
        String::new()
    };

    Ok(Value::List(Cons::new(Value::String(dir), Value::String(file))))
}

fn listDir(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("List-dir", args = 1);
    let dir: &str = builtin_define_arg!("List-dir", args[0]: String);

    let mut result: Vec<Value> = Vec::new();
    for entry in fs::read_dir(dir).map_err(
        |_| rterr!("Failed to list-dir: {}", dir))?
    {
        let dir = entry.map_err(|e| rterr!("Failed to list-dir: {}", e))?;
        result.push(Value::String(dir.path().to_str().ok_or_else(
            || rterr!("Failed to list-dir due to non-utf8 path"))?.to_owned()));
    }

    Ok(Value::from(result))
}

fn splitPath(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Split-path", args = 1);
    let path: &str = builtin_define_arg!("Split-path", args[0]: String);

    let segs: Vec<Value> = path.split(std::path::MAIN_SEPARATOR)
        .map(|s| Value::String(s.to_owned())).collect();

    Ok(Value::from(segs))
}

fn pathJoin2(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Path-join-file", args = 1);
    let base: &str = builtin_define_arg!("Path-join-file", args[0]: String);
    let file: &str = builtin_define_arg!("Path-join-file", args[0]: String);
    unimplemented!();
    Ok(Value::null())
}

pub fn getBuiltinEnv() -> Environment
{
    let result = Environment::new();
    registerBuiltin(&result, "file-exists", exists);
    // “/a/b/c” -> (“/a/b” . “c”)
    registerBuiltin(&result, "path-split-base", pathSplitBase);
    registerBuiltin(&result, "list-dir", listDir);
    registerBuiltin(&result, "split-path", splitPath);
    result
}
