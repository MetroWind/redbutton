extern crate proc_macro;
use proc_macro::TokenStream;

#[proc_macro]
pub fn libstd(_: TokenStream) -> TokenStream
{
    let src = ::std::fs::read_to_string("scheme_lib/std.sch").unwrap();
    let src = format!("r#\"{}\"#", src);
    src.parse().unwrap()
}

#[proc_macro]
pub fn librb(_: TokenStream) -> TokenStream
{
    let src = ::std::fs::read_to_string("scheme_lib/redbutton.sch").unwrap();
    let src = format!("r#\"{}\"#", src);
    src.parse().unwrap()
}
