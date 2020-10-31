#![allow(dead_code)]

use std::fmt;
use std::ffi::OsStr;
use std::process::Command;

use shared::error;
use shared::error::Error;

pub struct SimpleTemplate
{
    tplt: String,
}

impl SimpleTemplate
{
    pub fn new(s: &str) -> Self
    {
        Self {tplt: s.to_string()}
    }

    pub fn apply<ValueType: fmt::Display>(self, key: &str, value: ValueType)
                                          -> Self
    {
        let pattern = regex::Regex::new(&format!(r"%\{{{}\}}", key)).unwrap();
        Self::new(&pattern.replace_all(&self.tplt, &format!("{}", value)[..])
                  .into_owned())
    }

    pub fn result(self) -> String
    {
        self.tplt
    }
}

#[test]
fn testTemplate()
{
    let t = SimpleTemplate::new("%{user}，你已经是一个键盘侠啦！快来和大家打个招呼吧~");
    assert_eq!(&t.apply("user", "abc").result(),
               "abc，你已经是一个键盘侠啦！快来和大家打个招呼吧~");
}

pub fn run<I, S>(command: I) -> Result<(), Error>
    where I: IntoIterator<Item = S>,
          S: AsRef<OsStr> + fmt::Display + Clone
{
    let mut iter = command.into_iter();
    let prog = iter.next().unwrap();
    let status = Command::new(prog.clone()).args(iter).status().map_err(
        |_| error!(RuntimeError, format!("Failed to run {}", prog)))?;

    if status.success()
    {
        Ok(())
    }
    else if let Some(code) = status.code()
    {
        Err(error!(RuntimeError, format!("{} failed with {}", prog, code)))
    }
    else
    {
        Err(error!(RuntimeError, format!("{} was terminated", prog)))
    }
}

pub fn runWithOutput<I, S>(command: I) -> Result<(Vec<u8>, Vec<u8>), Error>
    where I: IntoIterator<Item = S>,
          S: AsRef<OsStr> + fmt::Display + Clone
{
    let mut iter = command.into_iter();
    let prog = iter.next().unwrap();
    let output = Command::new(prog.clone()).args(iter).output().map_err(
        |_| error!(RuntimeError, format!("Failed to run {}", prog)))?;

    if output.status.success()
    {
        Ok((output.stdout, output.stderr))
    }
    else if let Some(code) = output.status.code()
    {
        Err(error!(RuntimeError, format!("{} failed with {}", prog, code)))
    }
    else
    {
        Err(error!(RuntimeError, format!("{} was terminated", prog)))
    }
}
