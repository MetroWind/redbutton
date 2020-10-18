#![allow(non_snake_case)]

pub mod tokenizer;
pub mod parser;
pub mod environment;
pub mod eval;
pub mod builtin;

#[cfg(test)]
mod tests
{
    #[test]
    fn it_works()
    {
        assert_eq!(2 + 2, 4);
    }
}
