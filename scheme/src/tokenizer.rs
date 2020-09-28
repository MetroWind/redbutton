use shared::error;
use shared::error::Error;

#[derive(Debug, PartialEq)]
pub struct TokenData
{
    data: String,
}

impl TokenData
{
    pub fn new(src: &str) -> Self
    {
        Self{ data: src.to_owned() }
    }

    pub fn fromString(src: String) -> Self
    {
        Self{ data: src }
    }

}

#[derive(Debug, PartialEq)]
pub enum RawToken
{
    ParenLeft,
    ParenRight,
    Dot,
    String(TokenData),
    Quote,
    // Quasiquote
    Quasi,
    Comma,
    // ,@
    SeqComma,
    Stuff(TokenData),
}

enum State
{
    Generic,
    String,
    Stuff,
    Comma,
    DotMaybe,
}

pub fn tokenize0(src: &str) -> Result<Vec<RawToken>, Error>
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
                    result.push(RawToken::ParenLeft);
                }
                else if c == ')'
                {
                    result.push(RawToken::ParenRight);
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
                    result.push(RawToken::Quote);
                }
                else if c == '`'
                {
                    result.push(RawToken::Quasi);
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
                        result.push(RawToken::String(TokenData::fromString(
                            token_buffer.iter().collect())));
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
                    result.push(RawToken::Stuff(TokenData::fromString(
                        token_buffer.iter().collect())));
                    state = State::Generic;
                }
                else if c == ')'
                {
                    result.push(RawToken::Stuff(TokenData::fromString(
                        token_buffer.iter().collect())));
                    result.push(RawToken::ParenRight);
                    state = State::Generic;
                }
                else if c == '('
                {
                    result.push(RawToken::Stuff(TokenData::fromString(
                        token_buffer.iter().collect())));
                    result.push(RawToken::ParenLeft);
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
                    result.push(RawToken::SeqComma);
                    state = State::Generic;
                }
                else
                {
                    result.push(RawToken::Comma);
                    state = State::Stuff;
                }
            },

            State::DotMaybe =>
            {
                if c.is_whitespace()
                {
                    result.push(RawToken::Dot);
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

    #[test]
    fn tok0_simple() -> Result<(), Error>
    {
        let src = "(+ 1 2)";
        let tokens = tokenize0(src)?;
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], RawToken::ParenLeft);
        assert_eq!(tokens[1], RawToken::Stuff(TokenData::new("+")));
        assert_eq!(tokens[2], RawToken::Stuff(TokenData::new("1")));
        assert_eq!(tokens[3], RawToken::Stuff(TokenData::new("2")));
        assert_eq!(tokens[4], RawToken::ParenRight);
        Ok(())
    }
}
