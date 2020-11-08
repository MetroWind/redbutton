#![allow(non_snake_case)]
use std::fs;
use std::path::{Path, PathBuf};

#[macro_use]
extern crate shared;

#[macro_use]
extern crate scheme;

mod builtin_file;
mod builtin_str;
mod builtin_sys;
mod utils;

use std::io::{self, Read};

use clap;

use shared::error;
use shared::error::Error;

use scheme::eval::Evaluator;
use scheme::user_utils;

const CONF_SCRIPT_NAME: &str = "redbutton-conf.sch";
const USER_CONF_NAME: &str = "redbutton.sch";

fn readStdin() -> Result<String, Error>
{
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).map_err(
        |_| error!(RuntimeError, "Failed to read stdin."))?;
    Ok(buffer)
}

fn getLib() -> String
{
    Path::new(&user_utils::findLibDir()).join(USER_CONF_NAME).to_str().unwrap()
        .to_owned()
}

fn getEval(extra_libs: &[&str]) -> Result<Evaluator, Error>
{
    let e = user_utils::getStdEval()?;
    e.env().merge(builtin_sys::getBuiltinEnv());
    e.env().merge(builtin_str::getBuiltinEnv());
    e.env().merge(builtin_file::getBuiltinEnv());
    let lib_rb = getLib();
    let mut libs: Vec<&str> = vec![&lib_rb];
    libs.extend(extra_libs.iter());
    user_utils::loadLibs(&e, libs)?;
    Ok(e)
}

fn runScheme(src: &str, extra_libs: &[&str]) -> Result<(), Error>
{
    let e = getEval(extra_libs)?;
    e.evalSource(&src)?;
    Ok(())
}

fn runSchemeFile(filename: &str, extra_libs: &[&str]) -> Result<(), Error>
{
    let src = fs::read_to_string(filename)
        .map_err(|_| rterr!("Failed to load file {}", filename))?;
    runScheme(&src, extra_libs)
}

fn config(user_conf: &str, section: &str) -> Result<(), Error>
{
    let e: Evaluator = getEval(&[user_conf])?;

    std::env::set_current_dir(section).map_err(
        |e| rterr!("Failed to cd into {}, {}", section, e))?;
    if !Path::new(CONF_SCRIPT_NAME).exists()
    {
        return Err(rterr!("Config script not found for {}", section));
    }

    let src = fs::read_to_string(CONF_SCRIPT_NAME)
        .map_err(|_| rterr!("Failed to load file {}", CONF_SCRIPT_NAME))?;
    e.evalSource(&src)?;
    Ok(())
}

fn findUserConf() -> Option<PathBuf>
{
    let dir = if let Ok(p) = std::env::var("XDG_CONFIG_HOME")
    {
        PathBuf::from(p)
    }
    else if let Ok(p) = std::env::var("HOME")
    {
        Path::new(&p).join(".config")
    }
    else
    {
        return None;
    };

    let file = dir.join(USER_CONF_NAME);
    if file.exists()
    {
        Some(file)
    }
    else
    {
        None
    }
}

fn main() -> Result<(), Error>
{
    let opts = clap::App::new("Red Button")
        .version("0.1")
        .author("MetroWind")
        .about("A simple scheme interpreter")
        .arg(clap::Arg::with_name("ListEnv")
             .long("inspect-env")
             .help("List names in the default environment and exit."))
        .subcommand(
            clap::App::new("eval")
                .about("Evaluate a file")
                .arg(clap::Arg::with_name("FILE")
                     .help("Scheme source file to eval. Read stdin if not issued.")
                     .required(false)))
        .subcommand(
            clap::App::new("config")
                .about("Deploy a set of configs")
                .arg(clap::Arg::with_name("NAME")
                     .multiple(true)
                     .required(true)
                     .help("Name of the config")))
        .get_matches();

    if opts.is_present("ListEnv")
    {
        let e = getEval(&Vec::new())?;
        let mut names = e.env().members();
        names.sort();
        for name in names
        {
            println!("{}", name);
        }
        return Ok(())
    }

    // Deal with sub-commands.
    if let Some(ref sub_opts) = opts.subcommand_matches("eval")
    {
        let conf = findUserConf()
            .ok_or_else(|| rterr!("User config not found"))?
            .to_str().unwrap().to_owned();

        if let Some(filename) = sub_opts.value_of("FILE")
        {
            runSchemeFile(filename, &[&conf])
        }
        else
        {
            let src = readStdin()?;
            runScheme(&src, &[&conf])
        }
    }
    else if let Some(ref sub_opts) = opts.subcommand_matches("config")
    {
        let conf = findUserConf()
            .ok_or_else(|| rterr!("User config not found"))?
            .to_str().unwrap().to_owned();

        for name in sub_opts.values_of("NAME").unwrap()
        {
            config(&conf, &name)?;
        }
        Ok(())
    }
    else
    {
        println!("{}", opts.usage());
        Ok(())
    }
}
