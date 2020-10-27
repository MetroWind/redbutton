use std::error::Error as StdError;
use std::fmt;

#[macro_export]
macro_rules! error
{
    ( $err_type:ident, $msg:expr ) =>
    {
        {
            Error::$err_type(String::from($msg))
        }
    };
}

// Construct a RuntimeError
#[macro_export]
macro_rules! rterr
{
    ($msg:literal) => { error!(RuntimeError, $msg) };
    ($msg:literal $(, $x:expr)+) =>
    {
        error!(RuntimeError, format!($msg $(, $x)+))
    };
}

#[derive(Debug, Clone, PartialEq)]
pub enum Error
{
    ParseError(String),
    RuntimeError(String),
}

impl fmt::Display for Error
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
    {
        match self
        {
            Error::ParseError(msg) => write!(f, "Parsing error: {}", msg),
            Error::RuntimeError(msg) => write!(f, "Runtime error: {}", msg),
        }
    }
}

impl StdError for Error
{
    fn source(&self) -> Option<&(dyn StdError + 'static)> {None}
}
