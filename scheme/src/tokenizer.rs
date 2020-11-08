use std::convert::TryFrom;

use shared::error;
use shared::error::Error;

#[derive(Debug, PartialEq, Clone)]
enum RawTokenKind
{
    ParenLeft,
    ParenRight,
    Dot,
    String,
    Quote,
    // Quasiquote
    Quasi,
    Comma,
    // ,@
    SeqComma,
    // Identifiers and non-string literals.
    Stuff,
}

#[derive(Debug, PartialEq, Clone)]
struct RawToken
{
    src: String,
    kind: RawTokenKind,
}

impl RawToken
{
    pub fn new(src: &str, kind: RawTokenKind) -> Self
    {
        Self{ src: src.to_owned(), kind: kind }
    }

    pub fn withString(src: String, kind: RawTokenKind) -> Self
    {
        Self{ src: src, kind: kind }
    }
}

#[derive(PartialEq)]
enum State
{
    Generic,
    String,
    Stuff,
    Comma,
    DotMaybe,
}

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue
{
    ParenLeft,
    ParenRight,
    Dot,
    String(String),
    Quote,
    Quasi,
    Comma,
    SeqComma,
    Float(f64),
    Integer(i64),
    Ident(String),
    Char(char),
    Bool(bool),
}

impl TokenValue
{
    pub fn getIdentifier(&self) -> Option<&str>
    {
        if let Self::Ident(name) = self
        {
            Some(&name)
        }
        else
        {
            None
        }
    }
}

// impl fmt::Display for TokenValue
// {
//     fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result
//     {
//         match self
//         {
//             Self::ParenLeft => write!(f, "("),
//             Self::ParenRight => write!(f, "("),
//             Self::Bool(x) => write!(f, "{}", x),
//             Self::Char(x) => write!(f, "{}", x),
//             Self::Comma => write!(f, ","),
//             Self::Dot => write!(f, "."),
//             Self::Float(x) => write!(f, "{}", x),
//             Self::Ident(x) => write!(f, "{}", x),
//             Self::Integer(x) => write!(f, "{}", x),
//             Self::Quasi(x) => write!(f, "{}"),

//     }
// }


#[derive(Debug, PartialEq, Clone)]
pub struct Token
{
    src: String,
    value: TokenValue,
}

impl Token
{
    #[allow(dead_code)]
    pub fn new(src: &str, v: TokenValue) -> Self
    {
        Self{ src: String::from(src), value: v }
    }

    pub fn value(&self) -> &TokenValue
    {
        &self.value
    }

    pub fn src(&self) -> &str
    {
        &self.src
    }

    fn convertSharp(s: &str) -> Result<TokenValue, Error>
    {
        if s.len() < 2
        {
            return Err(error!(
                ParseError, format!("Invalid sharp literal: {}", s)));
        }

        match s.chars().nth(1).unwrap()
        {
            't' => if s.len() == 2
            {
                Ok(TokenValue::Bool(true))
            }
            else
            {
                Err(error!(ParseError,
                           format!("Invalid sharp literal: {}", s)))
            },

            'f' => if s.len() == 2
            {
                Ok(TokenValue::Bool(false))
            }
            else
            {
                Err(error!(ParseError,
                           format!("Invalid sharp literal: {}", s)))
            },

            '\\' => if s.len() == 3
            {
                Ok(TokenValue::Char(s.chars().nth(2).unwrap()))
            }
            else
            {
                match &s[2..]
                {
                    "space" => Ok(TokenValue::Char(' ')),
                    "newline" => Ok(TokenValue::Char('\n')),
                    _ => Err(error!(
                        ParseError, format!("Invalid char: {}", s))),
                }
            },

            // Binary
            'b' => Ok(TokenValue::Integer(
                i64::from_str_radix(&s[2..], 2).map_err(
                    |_| error!(ParseError,
                               format!("Invalid binary literal: {}", s)))?)),
            // Octal
            'o' => Ok(TokenValue::Integer(
                i64::from_str_radix(&s[2..], 8).map_err(
                    |_| error!(ParseError,
                               format!("Invalid octal literal: {}", s)))?)),
            // Decimal
            'd' => Ok(TokenValue::Integer(
                i64::from_str_radix(&s[2..], 10).map_err(
                    |_| error!(ParseError,
                               format!("Invalid decimal literal: {}", s)))?)),
            // Hex
            'x' => Ok(TokenValue::Integer(
                i64::from_str_radix(&s[2..], 16).map_err(
                    |_| error!(ParseError,
                               format!("Invalid hex literal: {}", s)))?)),

            _ => Err(error!(ParseError, format!("Invalid sharp literal: {}", s))),
        }
    }

    fn convertNumber(s: &str) -> Option<TokenValue>
    {
        if let Ok(n) = s.parse::<i64>()
        {
            Some(TokenValue::Integer(n))
        }
        else if let Ok(x) = s.parse::<f64>()
        {
            Some(TokenValue::Float(x))
        }
        else
        {
            None
        }
    }

    fn convertStuff(s: &str) -> Result<TokenValue, Error>
    {
        let first_char = s.chars().next().unwrap();
        if first_char == '#'
        {
            return Self::convertSharp(s);
        }

        let num_maybe = Self::convertNumber(s);
        if let Some(v) = num_maybe
        {
            return Ok(v);
        }

        // E.g. -#x10ff
        if first_char == '+' || first_char == '-'
        {
            if s.len() == 1
            {
                return Ok(TokenValue::Ident(s.to_owned()));
            }

            if s.chars().nth(1).unwrap() == '#'
            {
                if let Ok(v) = Self::convertSharp(&s[1..])
                {
                    if first_char == '-'
                    {
                        return match v
                        {
                            TokenValue::Float(x) => Ok(TokenValue::Float(-x)),
                            TokenValue::Integer(x) =>
                                Ok(TokenValue::Integer(-x)),
                            _ => Err(error!(ParseError,
                                           format!("Invalid literal: {}", s))),
                        };
                    }
                    else
                    {
                        return Ok(v);
                    }
                }
                else
                {
                    return Err(error!(
                        ParseError, format!("Invalid sharp literal: {}", s)));
                }
            }
        }

        // It’s probably an identifier...
        Ok(TokenValue::Ident(s.to_owned()))
    }
}

impl TryFrom<RawToken> for Token
{
    type Error = Error;
    fn try_from(t: RawToken) -> Result<Self, Error>
    {
        let v: TokenValue = match t.kind
        {
            RawTokenKind::ParenLeft => TokenValue::ParenLeft,
            RawTokenKind::ParenRight => TokenValue::ParenRight,
            RawTokenKind::Dot => TokenValue::Dot,
            RawTokenKind::Quote => TokenValue::Quote,
            RawTokenKind::Quasi => TokenValue::Quasi,
            RawTokenKind::Comma => TokenValue::Comma,
            RawTokenKind::SeqComma => TokenValue::SeqComma,
            RawTokenKind::String => TokenValue::String(t.src.clone()),
            RawTokenKind::Stuff => Self::convertStuff(&t.src)?,
        };
        Ok(Self{ src: t.src, value: v })
    }
}

fn tokenize0(src: &str) -> Result<Vec<RawToken>, Error>
{
    let mut result: Vec<RawToken> = Vec::new();
    let mut token_buffer: Vec<char> = Vec::new();
    let mut state = State::Generic;
    let mut last = ' ';
    let chars: Vec<char> = src.chars().collect();
    let mut i = 0;
    while i < chars.len()
    {
        let c = chars[i];

        // Skip comments.
        if state != State::String && c == ';'
        {
            loop
            {
                i += 1;
                if chars[i] == '\n' // Comment ends here.
                {
                    break;
                }
            }
            i += 1;             // Skip the newline.
            continue;
        }

        match state
        {
            State::Generic =>
            {
                token_buffer.clear();

                if c == '('
                {
                    result.push(RawToken::new("(", RawTokenKind::ParenLeft));
                }
                else if c == ')'
                {
                    result.push(RawToken::new(")", RawTokenKind::ParenRight));
                }
                else if c.is_whitespace()
                {
                }
                else if c == '.'
                {
                    state = State::DotMaybe;
                }
                else if c == '"'
                {
                    state = State::String;
                }
                else if c == '\''
                {
                    result.push(RawToken::new("'", RawTokenKind::Quote));
                }
                else if c == '`'
                {
                    result.push(RawToken::new("`", RawTokenKind::Quasi));
                }
                else if c == ','
                {
                    state = State::Comma;
                }
                else
                {
                    token_buffer.push(c);
                    state = State::Stuff;
                }
            },

            State::String =>
            {
                if last == '\\'
                {
                    match c
                    {
                        'n' => token_buffer.push('\n'),
                        't' => token_buffer.push('\t'),
                        '\\' => token_buffer.push('\\'),
                        '"' => token_buffer.push('"'),
                        _ => { return Err(error!(ParseError, format!(
                            "Invalid escape sequence: \\{}", c))); },
                    }
                }
                else if c != '\\'
                {
                    if c == '"'
                    {
                        result.push(RawToken::withString(
                            token_buffer.iter().collect(), RawTokenKind::String));
                        state = State::Generic;
                    }
                    else
                    {
                        token_buffer.push(c);
                    }
                }
            },

            State::Stuff =>
            {
                if c.is_whitespace()
                {
                    result.push(RawToken::withString(
                        token_buffer.iter().collect(), RawTokenKind::Stuff));
                    state = State::Generic;
                }
                else if c == ')'
                {
                    result.push(RawToken::withString(
                        token_buffer.iter().collect(), RawTokenKind::Stuff));
                    result.push(RawToken::new(")", RawTokenKind::ParenRight));
                    state = State::Generic;
                }
                else if c == '('
                {
                    result.push(RawToken::withString(
                        token_buffer.iter().collect(), RawTokenKind::Stuff));
                    result.push(RawToken::new("(", RawTokenKind::ParenLeft));
                    state = State::Generic;
                }
                else
                {
                    token_buffer.push(c);
                }
            },

            State::Comma =>
            {
                if c == '@'
                {
                    result.push(RawToken::new(",@", RawTokenKind::SeqComma));
                    state = State::Generic;
                }
                else
                {
                    result.push(RawToken::new(",", RawTokenKind::Comma));
                    state = State::Stuff;
                    token_buffer.push(c);
                }
            },

            State::DotMaybe =>
            {
                if c.is_whitespace()
                {
                    result.push(RawToken::new(".", RawTokenKind::Dot));
                    state = State::Generic;
                }
                else
                {
                    state = State::Stuff;
                    token_buffer.push('.');
                    token_buffer.push(c);
                }
            },
        }

        last = c;
        i += 1;
    }
    if !token_buffer.is_empty() && state == State::Stuff
    {
        result.push(RawToken::withString(
            token_buffer.iter().collect(), RawTokenKind::Stuff));
        state = State::Generic;
    }

    if state == State::Generic
    {
        Ok(result)
    }
    else
    {
        Err(error!(ParseError, "Invalid ending"))
    }
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, Error>
{
    let result: Result<Vec<Token>, Error> = tokenize0(src)?.into_iter()
        .map(|rt| Token::try_from(rt)).collect();
    result
}

#[cfg(test)]
mod tests
{
    use super::*;

    macro_rules! RT {
        ( $s:literal, $x:ident ) => {
            RawToken::new($s, RawTokenKind::$x)
        };
    }

    macro_rules! Tk {
        ( $s:literal, $x:ident $(, $v:expr)? ) => {
            Token::new($s, TokenValue::$x$(($v))?)
        };
    }

    #[test]
    fn tok0_too_simple() -> Result<(), Error>
    {
        {
            let src = "";
            let tokens = tokenize0(src)?;
            assert!(tokens.is_empty());
        }
        {
            let src = r#"+"#;
            let tokens = tokenize0(src)?;
            assert_eq!(tokens, vec![RT!("+", Stuff),]);
        }
        {
            let src = r#"1 + 2"#;
            let tokens = tokenize0(src)?;
            assert_eq!(tokens,
                       vec![RT!("1", Stuff),
                            RT!("+", Stuff),
                            RT!("2", Stuff),
                       ]);
        }
        Ok(())
    }

    #[test]
    fn tok0_invalid_ending()
    {
        let src = r#""abc"#;
        let tokens = tokenize0(src);
        assert!(tokens.is_err());
    }

    #[test]
    fn tok0_simple() -> Result<(), Error>
    {
        let src = r#"(+ 1 #\2)"#;
        let tokens = tokenize0(src)?;
        assert_eq!(
            tokens,
            vec![RT!("(", ParenLeft),
                 RT!("+", Stuff),
                 RT!("1", Stuff),
                 RT!("#\\2", Stuff),
                 RT!(")", ParenRight),
            ]);
        Ok(())
    }

    #[test]
    fn tok0_string() -> Result<(), Error>
    {
        let src = r#"(+ "abc" "d\nf")"#;
        let tokens = tokenize0(src)?;
        assert_eq!(tokens,
                   vec![RT!("(", ParenLeft),
                        RT!("+", Stuff),
                        RT!("abc", String),
                        RT!("d\\\nf", String),
                        RT!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn tok0_quote() -> Result<(), Error>
    {
        {
            let src = r#"'("abc")"#;
            let tokens = tokenize0(src)?;
            assert_eq!(tokens,
                       vec![
                           RT!("'", Quote),
                           RT!("(", ParenLeft),
                           RT!("abc", String),
                           RT!(")", ParenRight),
                       ]);
        }
        {
            let src = r#"'"abc""#;
            let tokens = tokenize0(src)?;
            assert_eq!(tokens,
                       vec![
                           RT!("'", Quote),
                           RT!("abc", String),
                       ]);
        }
        {
            let src = r#"('abc)"#;
            let tokens = tokenize0(src)?;
            assert_eq!(tokens,
                       vec![
                           RT!("(", ParenLeft),
                           RT!("'", Quote),
                           RT!("abc", Stuff),
                           RT!(")", ParenRight),
                       ]);
        }
        Ok(())
    }

    #[test]
    fn tok0_cons() -> Result<(), Error>
    {
        let src = r#"(1 . 2)"#;
        let tokens = tokenize0(src)?;
        assert_eq!(tokens,
                   vec![
                       RT!("(", ParenLeft),
                       RT!("1", Stuff),
                       RT!(".", Dot),
                       RT!("2", Stuff),
                       RT!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn tok0_cons2() -> Result<(), Error>
    {
        let src = r#"'((1) . 2)"#;
        let tokens = tokenize0(src)?;
        assert_eq!(tokens,
                   vec![
                       RT!("'", Quote),
                       RT!("(", ParenLeft),
                       RT!("(", ParenLeft),
                       RT!("1", Stuff),
                       RT!(")", ParenRight),
                       RT!(".", Dot),
                       RT!("2", Stuff),
                       RT!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn tok0_quasi() -> Result<(), Error>
    {
        let src = r#"`(,v ,@(w))"#;
        let tokens = tokenize0(src)?;
        assert_eq!(tokens,
                   vec![
                       RT!("`", Quasi),
                       RT!("(", ParenLeft),
                       RT!(",", Comma),
                       RT!("v", Stuff),
                       RT!(",@", SeqComma),
                       RT!("(", ParenLeft),
                       RT!("w", Stuff),
                       RT!(")", ParenRight),
                       RT!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn token_simple_number() -> Result<(), Error>
    {
        let src = r#"(+ 1 1.0 +1 -1.0)"#;
        let tokens = tokenize(src)?;
        assert_eq!(tokens,
                   vec![
                       Tk!("(", ParenLeft),
                       Tk!("+", Ident, "+".to_owned()),
                       Tk!("1", Integer, 1),
                       Tk!("1.0", Float, 1.0),
                       Tk!("+1", Integer, 1),
                       Tk!("-1.0", Float, -1.0),
                       Tk!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn token_simple_identifier() -> Result<(), Error>
    {
        let src = r#"(+ _ab -)"#;
        let tokens = tokenize(src)?;
        assert_eq!(tokens,
                   vec![
                       Tk!("(", ParenLeft),
                       Tk!("+", Ident, "+".to_owned()),
                       Tk!("_ab", Ident, "_ab".to_owned()),
                       Tk!("-", Ident, "-".to_owned()),
                       Tk!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn token_weird_identifier() -> Result<(), Error>
    {
        let src = r#"({} .. -->)"#;
        let tokens = tokenize(src)?;
        assert_eq!(tokens,
                   vec![
                       Tk!("(", ParenLeft),
                       Tk!("{}", Ident, "{}".to_owned()),
                       Tk!("..", Ident, "..".to_owned()),
                       Tk!("-->", Ident, "-->".to_owned()),
                       Tk!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn token_sharp_number() -> Result<(), Error>
    {
        let src = r#"(#x1f +#b100 -#o10)"#;
        let tokens = tokenize(src)?;
        assert_eq!(tokens,
                   vec![
                       Tk!("(", ParenLeft),
                       Tk!("#x1f", Integer, 31),
                       Tk!("+#b100", Integer, 4),
                       Tk!("-#o10", Integer, -8),
                       Tk!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn token_sharp_char() -> Result<(), Error>
    {
        let src = r#"(#\2 #\A #\space #\newline)"#;
        let tokens = tokenize(src)?;
        assert_eq!(tokens,
                   vec![
                       Tk!("(", ParenLeft),
                       Tk!("#\\2", Char, '2'),
                       Tk!("#\\A", Char, 'A'),
                       Tk!("#\\space", Char, ' '),
                       Tk!("#\\newline", Char, '\n'),
                       Tk!(")", ParenRight),
                   ]);
        Ok(())
    }

    #[test]
    fn token_invalid_sharp()
    {
        {
            let src = r#"#\22"#;
            let tokens = tokenize(src);
            assert!(tokens.is_err());
        }
        {
            let src = r#"#\random"#;
            let tokens = tokenize(src);
            assert!(tokens.is_err());
        }
        {
            let src = r#"#b123"#;
            let tokens = tokenize(src);
            assert!(tokens.is_err());
        }
        {
            let src = r#"-#\2"#;
            let tokens = tokenize(src);
            assert!(tokens.is_err());
        }
        {
            let src = r#"#random"#;
            let tokens = tokenize(src);
            assert!(tokens.is_err());
        }
    }

}
