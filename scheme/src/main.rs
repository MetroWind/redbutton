#![allow(non_snake_case)]

use std::io::{self, Read};

use clap;

use shared::error;
use shared::error::Error;

use scheme::user_utils;

fn readStdin() -> Result<String, Error>
{
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).map_err(
        |_| error!(RuntimeError, "Failed to read stdin."))?;
    Ok(buffer)
}

fn main() -> Result<(), Error>
{
    let matches = clap::App::new("MetroScheme")
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
        let e = user_utils::getStdEval()?;
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
        user_utils::runSchemeFile(filename, vec![])
    }
    else
    {
        let src = readStdin()?;
        user_utils::runScheme(&src, vec![])
    }
}
