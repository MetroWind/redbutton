use std::env;

const STD_PATH: &str = "scheme_lib/std.sch";

pub fn findStd() -> String
{
    if let Ok(path) = env::var("SCHEME_STD_PATH")
    {
        path
    }
    else
    {
        STD_PATH.to_owned()
    }
}
