use std::io::{self, Read};

use scheme::{tokenizer, parser, eval};

use shared::error;
use shared::error::Error;

fn readStdin() -> Result<String, Error>
{
    let mut buffer = String::new();
    io::stdin().read_to_string(&mut buffer).map_err(
        |_| error!(RuntimeError, "Failed to read stdin."))?;
    Ok(buffer)
}

fn main() -> Result<(), Error>
{
    let src = readStdin()?;
    eval::Evaluator::evalSource(&src)?;
    Ok(())
}
