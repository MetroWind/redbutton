use std::fs;
use std::io::prelude::*;
use std::path::Path;

use shared::error;
use shared::error::Error;

use scheme::value::{Value, Cons};
use scheme::builtin::registerBuiltin;
use scheme::environment::Environment;

fn exists(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("File-exists", args == 1);
    let filename: &str = builtin_define_arg!("File-exists", args[0]: String);

    Ok(Value::Bool(Path::new(filename).exists()))
}

fn fileType(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("File-type", args == 1);
    let filename: &str = builtin_define_arg!("File-type", args[0]: String);

    let meta = fs::metadata(filename).map_err(
        |_| rterr!("Failed to get file type for {}", filename))?;
    let file_type = meta.file_type();
    if file_type.is_dir()
    {
        Ok(Value::Symbol("dir".to_owned()))
    }
    else if file_type.is_file()
    {
        Ok(Value::Symbol("file".to_owned()))
    }
    else if file_type.is_symlink()
    {
        Ok(Value::Symbol("symlink".to_owned()))
    }
    else
    {
        Ok(Value::Symbol("other".to_owned()))
    }
}

fn mkdir(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("mkdir", args == 1);
    let filename: &str = builtin_define_arg!("mkdir", args[0]: String);
    fs::create_dir_all(filename).map_err(
        |_| rterr!("Failed to create dir {}", filename))?;
    Ok(Value::null())
}

fn linkFile(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("link-file", args == 2);
    let src: &str = builtin_define_arg!("link-file", args[0]: String);
    let dest: &str = builtin_define_arg!("link-file", args[1]: String);

    std::os::unix::fs::symlink(src, dest).map_err(
        |_| rterr!("Failed to link file {} --> {}", src, dest))?;
    Ok(Value::null())
}

fn copyFile(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("copy-file", args == 2);
    let src: &str = builtin_define_arg!("copy-file", args[0]: String);
    let dest: &str = builtin_define_arg!("copy-file", args[1]: String);

    fs::copy(src, dest).map_err(
        |_| rterr!("Failed to copy file {} --> {}", src, dest))?;
    Ok(Value::null())
}

fn pathSplitBase(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Path-split-base", args == 1);
    let filename: &str = builtin_define_arg!("Path-split-base", args[0]: String);

    let path = Path::new(filename);
    let file = if let Some(f) = path.file_name()
    {
        Value::String(f.to_str().ok_or_else(
            || rterr!("path-split-base failed due to non-utf8 path"))?
                      .to_owned())
    }
    else
    {
        Value::null()
    };

    let dir = if let Some(parent) = path.parent()
    {
        parent.to_str().ok_or_else(
            || rterr!("path-split-base failed due to non-utf8 path"))?.to_owned()
    }
    else
    {
        filename.to_owned()
    };

    Ok(Value::List(Cons::new(Value::String(dir), file)))
}

fn listDir(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("List-dir", args == 1);
    let dir: &str = builtin_define_arg!("List-dir", args[0]: String);

    let mut result: Vec<Value> = Vec::new();
    for entry in fs::read_dir(dir).map_err(
        |_| rterr!("Failed to list-dir: {}", dir))?
    {
        let dir = entry.map_err(|e| rterr!("Failed to list-dir: {}", e))?;
        result.push(Value::String(dir.path().file_name().unwrap().to_str()
                                  .ok_or_else(
            || rterr!("Failed to list-dir due to non-utf8 path"))?.to_owned()));
    }

    Ok(Value::from(result))
}

fn pathSplit(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Path-split", args == 1);
    let path: &str = builtin_define_arg!("Path-split", args[0]: String);

    let segs: Vec<Value> = path.split(std::path::MAIN_SEPARATOR)
        .map(|s| Value::String(s.to_owned())).collect();

    Ok(Value::from(segs))
}

fn pathJoin2(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("Path-join2", args == 2);
    let base: &str = builtin_define_arg!("Path-join2", args[0]: String);
    let file: &str = builtin_define_arg!("Path-join2", args[1]: String);

    Ok(Value::String(format!("{}{}{}", base, std::path::MAIN_SEPARATOR, file)))
}

fn readFile(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("File-read", args == 1);
    let file = builtin_define_arg!("File-read", args[0]: String);

    let contents = fs::read_to_string(file).map_err(
        |e| rterr!("Failed to read file: {}, {}", file, e))?;
    Ok(Value::String(contents))
}

fn writeFile(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("File-write", args == 2);
    let filename = builtin_define_arg!("File-write", args[0]: String);
    let data = builtin_define_arg!("File-write", args[1]: String);

    let mut file = fs::File::create(filename).map_err(
        |e| rterr!("Failed to create file: {}, {}", filename, e))?;
    file.write_all(data.as_bytes()).map_err(
        |e| rterr!("Failed to write file: {}, {}", filename, e))?;

    Ok(Value::null())
}

fn appendFile(args: &[Value], _: Environment) -> Result<Value, Error>
{
    builtin_args_check!("File-append", args == 2);
    let filename = builtin_define_arg!("File-append", args[0]: String);
    let data = builtin_define_arg!("File-append", args[1]: String);

    let mut file = fs::OpenOptions::new().write(true).append(true)
        .open(filename).map_err(
        |e| rterr!("Failed to open file: {}, {}", filename, e))?;
    file.write_all(data.as_bytes()).map_err(
        |e| rterr!("Failed to append to file: {}, {}", filename, e))?;

    Ok(Value::null())
}

pub fn getBuiltinEnv() -> Environment
{
    let result = Environment::new();
    registerBuiltin(&result, "file-exists", exists);
    // “/a/b/c” -> (“/a/b” . “c”)
    registerBuiltin(&result, "path-split-base", pathSplitBase);
    registerBuiltin(&result, "list-dir", listDir);
    registerBuiltin(&result, "path-split", pathSplit);
    registerBuiltin(&result, "path-join2", pathJoin2);
    registerBuiltin(&result, "file-read", readFile);
    registerBuiltin(&result, "file-write", writeFile);
    registerBuiltin(&result, "file-append", appendFile);
    registerBuiltin(&result, "mkdir", mkdir);
    registerBuiltin(&result, "link-file", linkFile);
    registerBuiltin(&result, "copy-file", copyFile);
    registerBuiltin(&result, "file-type", fileType);
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
    fn exists() -> Result<(), Error>
    {
        // This can actually fail quite easily. So don’t be blocked on
        // it if this fails.
        let exe = std::env::current_exe().map_err(|_| rterr!("Failed to get $0"))?;
        let result = getEval().evalSource(&format!(r#"(file-exists {:?})"#, exe))?;
        assert_eq!(result, Value::Bool(true));
        Ok(())
    }

    #[test]
    fn path_split_base() -> Result<(), Error>
    {
        let result = getEval().evalSource(r#"(path-split-base "/a/b/c")"#)?;
        assert_eq!(result.to_string(), r#"("/a/b" . "c")"#);
        let result = getEval().evalSource(r#"(path-split-base "/c")"#)?;
        assert_eq!(result.to_string(), r#"("/" . "c")"#);
        let result = getEval().evalSource(r#"(path-split-base "/")"#)?;
        assert_eq!(result.to_string(), r#"("/")"#);
        Ok(())
    }

    #[test]
    fn list_dir() -> Result<(), Error>
    {
        let result = getEval().evalSource(r#"(list-dir ".")"#)?;
        Ok(())
    }
}
