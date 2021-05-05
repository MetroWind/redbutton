use std::fs;
use std::path::Path;

use shared::error;
use shared::error::Error;

use crate::tokenizer::tokenize;
use crate::parser::SyntaxTreeNode;
use crate::eval::Evaluator;

use std::env;

const LIB_DIR: &str = "scheme_lib";

pub fn findLibDir() -> String
{
    if let Ok(path) = env::var("SCHEME_LIB_DIR")
    {
        path
    }
    else
    {
        LIB_DIR.to_owned()
    }
}

pub fn findStd() -> String
{
    Path::new(&findLibDir()).join("std.sch").to_str().unwrap().to_owned()
}

pub fn getStdEval() -> Result<Evaluator, Error>
{
    let eval = Evaluator::new();
    {
        let std_src = fs::read_to_string(findStd())
            .map_err(|_| rterr!("Failed to load standard library"))?;

        let tokens = tokenize(&std_src)?;
        let roots = SyntaxTreeNode::parse(tokens)?;
        eval.eval(roots)?;
    };
    Ok(eval)
}

pub fn loadLibs(e: &Evaluator, lib_files: &[&str]) -> Result<(), Error>
{
    for f in lib_files
    {
        let lib_src = fs::read_to_string(f)
            .map_err(|_| rterr!("Failed to load file {}", f))?;
        e.evalSource(&lib_src)?;
    }
    Ok(())
}

pub fn runScheme(src: &str, lib_files: &[&str]) -> Result<(), Error>
{
    let e = getStdEval()?;
    loadLibs(&e, lib_files)?;
    e.evalSource(&src)?;
    Ok(())
}

pub fn runSchemeFile(filename: &str, lib_files: &[&str]) -> Result<(), Error>
{
    let src = fs::read_to_string(filename)
        .map_err(|_| rterr!("Failed to load file {}", filename))?;
    runScheme(&src, lib_files)
}
