#![allow(non_snake_case)]
use std::fs;
use std::path::Path;

#[macro_use]
extern crate shared;

#[macro_use]
extern crate scheme;

mod builtin_file;
mod builtin_str;
mod utils;

use std::io::{self, Read};

use clap;

use shared::error;
use shared::error::Error;

use scheme::eval::Evaluator;
use scheme::user_utils;

fn readStdin() -> Result<String, Error>
{
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).map_err(
        |_| error!(RuntimeError, "Failed to read stdin."))?;
    Ok(buffer)
}

fn getLib() -> String
{
    Path::new(&user_utils::findLibDir()).join("redbutton.sch").to_str().unwrap()
        .to_owned()
}

fn getEval() -> Result<Evaluator, Error>
{
    let e = user_utils::getStdEval()?;
    e.env().merge(builtin_str::getBuiltinEnv());
    e.env().merge(builtin_file::getBuiltinEnv());
    user_utils::loadLibs(&e, vec![&getLib()])?;
    Ok(e)
}

fn runScheme(src: &str) -> Result<(), Error>
{
    let e = getEval()?;
    e.evalSource(&src)?;
    Ok(())
}

fn runSchemeFile(filename: &str) -> Result<(), Error>
{
    let src = fs::read_to_string(filename)
        .map_err(|_| rterr!("Failed to load file {}", filename))?;
    runScheme(&src)
}

fn main() -> Result<(), Error>
{
    let matches = clap::App::new("Red Button")
        .version("0.1")
        .author("MetroWind")
        .about("A simple scheme interpreter")
        .arg(clap::Arg::with_name("FILE")
             .help("Scheme source file to eval. Read stdin if not issued.")
             .required(false))
        .arg(clap::Arg::with_name("ListEnv")
             .long("inspect-env")
             .help("List names in the default environment and exit."))
        .get_matches();

    if matches.is_present("ListEnv")
    {
        let e = getEval()?;
        let mut names = e.env().members();
        names.sort();
        for name in names
        {
            println!("{}", name);
        }
        return Ok(())
    }

    if let Some(filename) = matches.value_of("FILE")
    {
        runSchemeFile(filename)
    }
    else
    {
        let src = readStdin()?;
        runScheme(&src)
    }
}
