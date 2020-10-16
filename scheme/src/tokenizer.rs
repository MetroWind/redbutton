use shared::error;
use shared::error::Error;

#[derive(Debug, PartialEq)]
pub struct TokenData<T: Default>
{
    src: String,
    data: T,
}

impl<T: Default> TokenData<T>
{
    pub fn new(src: &str) -> Self
    {
        Self{ src: src.to_owned(), data: T::default() }
    }

    pub fn fromString(src: String) -> Self
    {
        Self{ src: src, data: T::default() }
    }

}

type RawTokenData = TokenData<()>;

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

#[derive(Debug, PartialEq, Clone)]
pub struct Token
{
    src: String,
    value: TokenValue,
}

impl From<RawToken> for Token
{
    fn convertSharp(s: &str) -> Result<TokenValue, Error>
    {
        match s[1]
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
                Ok(TokenValue::Char(s[2]))
            }
            else
            {
                match s[2..]
                {
                    "space" => Ok(TokenValue::Char(' ')),
                    "newline" => Ok(TokenValue::Char('\n')),
                    _ => Err(error!(
                        ParseError, format!("Invalid char: {}", s))),
                }
            },

            // Binary
            'b' => Ok(TokenValue::Integer(i64::from_str_radix(s[2..], 2)?)),
            // Octal
            'o' => Ok(TokenValue::Integer(i64::from_str_radix(s[2..], 8)?)),
            // Decimal
            'd' => Ok(TokenValue::Integer(i64::from_str_radix(s[2..], 10)?)),
            // Hex
            'x' => Ok(TokenValue::Integer(i64::from_str_radix(s[2..], 16)?)),

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
        if s[0] == '#'
        {
            Self::convertSharp(s)
        }

        if s[0] == '+' || s[0] == '-'
        {
            if s[1] == '#'
            {
                if let Ok(v) = Self::convertSharp(s[1..])
                {
                    if s[0] == '-'
                    {
                        v.value
    }

    pub fn from(t: RawToken) -> Self
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
            RawTokenKind::String => TokenValue::String(t.src),
            RawTokenKind::Stuff => Self::convertStuff(&t.src),
        };

    }
}

fn tokenize0(src: &str) -> Result<Vec<RawToken>, Error>
{
    let mut result: Vec<RawToken> = Vec::new();
    let mut token_buffer: Vec<char> = Vec::new();
    let mut state = State::Generic;
    let mut last = ' ';
    for c in src.chars()
    {
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
                else
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
    }
    Ok(result)
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
        let src = r#"'("abc")"#;
        let tokens = tokenize0(src)?;
        assert_eq!(tokens,
                   vec![
                       RT!("'", Quote),
                       RT!("(", ParenLeft),
                       RT!("abc", String),
                       RT!(")", ParenRight),
                   ]);
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
}
