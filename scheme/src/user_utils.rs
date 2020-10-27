use std::fs;

use shared::error;
use shared::error::Error;

use crate::tokenizer::tokenize;
use crate::parser::SyntaxTreeNode;
use crate::eval::Evaluator;
use crate::runtime_env::findStd;

pub fn getEval(lib_files: Vec<&str>) -> Result<Evaluator, Error>
{
    let eval = Evaluator::new();
    {
        let std_src = fs::read_to_string(findStd())
            .map_err(|_| rterr!("Failed to load standard library"))?;

        let tokens = tokenize(&std_src)?;
        let roots = SyntaxTreeNode::parse(tokens)?;
        eval.eval(roots)?;
    };

    for f in lib_files
    {
        let lib_src = fs::read_to_string(f)
            .map_err(|_| rterr!("Failed to load file {}", f))?;
        eval.evalSource(&lib_src)?;
    }
    Ok(eval)
}

pub fn runScheme(src: &str, lib_files: Vec<&str>) -> Result<(), Error>
{
    getEval(lib_files)?.evalSource(&src)?;
    Ok(())
}

pub fn runSchemeFile(filename: &str, lib_files: Vec<&str>) -> Result<(), Error>
{
    let src = fs::read_to_string(filename)
        .map_err(|_| rterr!("Failed to load file {}", filename))?;
    runScheme(&src, lib_files)
}
