use shared::error;
use shared::error::Error;

use crate::tokenizer::{Token, TokenValue};

#[derive(Clone, PartialEq)]
pub enum SyntaxTreeNode
{
    Atom(Token),
    Compound(Vec<SyntaxTreeNode>),
}

impl SyntaxTreeNode
{
    pub fn new(token: Token) -> Self
    {
        Self::Atom(token)
    }

    pub fn getNodes(&self) -> Option<&Vec<SyntaxTreeNode>>
    {
        if let Self::Compound(xs) = self
        {
            Some(xs)
        }
        else
        {
            None
        }
    }

    pub fn getToken(&self) -> Option<&Token>
    {
        if let Self::Atom(t) = self
        {
            Some(t)
        }
        else
        {
            None
        }
    }

    /// Assume the node is an identifier. Return the identifier as a
    /// str.
    pub fn getIdentifier(&self) -> Option<&str>
    {
        if let Some(t) = self.getToken()
        {
            t.value().getIdentifier()
        }
        else
        {
            None
        }
    }

    pub fn parse(tokens: Vec<Token>) -> Result<Vec<Self>, Error>
    {
        let mut eater = TokenEater::new(tokens);
        return eater.parse();
    }

    #[allow(dead_code)]
    fn toDotInner(&self, i: &mut usize) -> (Vec<String>, String)
    {
        let mut output = Vec::new();
        let self_name = format!("node{}", i);
        *i += 1;
        match self
        {
            Self::Atom(t) =>
            {
                output.push(format!(r#"{} [label="{}"];"#, self_name, t.src()));
            },
            Self::Compound(children) =>
            {
                output.push(format!(r#"{} [shape=circle, label="", height=.2, width=.2];"#, self_name));
                for t in children
                {
                    let (mut lines, name) = t.toDotInner(i);
                    output.append(&mut lines);
                    output.push(format!("{} -> {};", self_name, name));
                }
            }
        }
        return (output, self_name);
    }

    #[allow(dead_code)]
    pub fn toDot(&self) -> String
    {
        let mut output = Vec::new();
        let mut i: usize = 0;
        output.push(String::from("digraph G {"));
        output.push(String::from(r#"
        graph [fontname = "Rec Mono Duotone"];
        node [fontname = "Rec Mono Duotone"];
        edge [fontname = "Rec Mono Duotone"];"#));

        output.append(&mut self.toDotInner(&mut i).0);
        output.push(String::from("}"));
        output.join("\n")
    }
}

struct TokenEater
{
    tokens: Vec<Token>,
    pos: usize,
}

impl TokenEater
{
    pub fn new(tokens: Vec<Token>) -> Self
    {
        Self{ tokens: tokens, pos: 0 }
    }

    fn advance(&mut self)
    {
        self.pos += 1;
    }

    fn current(&self) -> &Token
    {
        &self.tokens[self.pos]
    }

    fn parseOne(&mut self) -> Result<SyntaxTreeNode, Error>
    {
        let current: Token = self.current().clone();
        match current.value()
        {
            TokenValue::ParenLeft =>
            {
                self.advance();
                return Ok(SyntaxTreeNode::Compound(self.consumeToRightParen()?));
            },
            TokenValue::Quote | TokenValue::Quasi | TokenValue::Comma |
            TokenValue::SeqComma =>
            {
                let quote = SyntaxTreeNode::new(current);
                self.advance();
                let quoted = self.parseOne()?;
                return Ok(SyntaxTreeNode::Compound(vec![quote, quoted]));
            },
            TokenValue::Bool(_) | TokenValue::Char(_) | TokenValue::Float(_) |
            TokenValue::Ident(_) | TokenValue::Integer(_) |
            TokenValue::String(_) | TokenValue::Dot =>
            {
                let node = SyntaxTreeNode::new(current);
                self.advance();
                return Ok(node);
            }
            TokenValue::ParenRight =>
            {
                self.advance();
                return Err(error!(ParseError, "Invalid right paren"));
            }
        }
    }

    fn parse(&mut self) -> Result<Vec<SyntaxTreeNode>, Error>
    {
        let mut result = Vec::new();
        while self.pos < self.tokens.len()
        {
            result.push(self.parseOne()?);
        }
        Ok(result)
    }

    fn consumeToRightParen(&mut self) -> Result<Vec<SyntaxTreeNode>, Error>
    {
        let mut nodes = Vec::new();
        loop
        {
            if self.current().value() == &TokenValue::ParenRight
            {
                self.advance();
                break;
            }

            nodes.push(self.parseOne()?);
        }
        return Ok(nodes);
    }
}

#[cfg(test)]
mod tests
{
    use super::*;

    #[test]
    fn useless_test() -> Result<(), Error>
    {
        let src = r#"(+ 1 2)"#;
        let tokens = crate::tokenizer::tokenize(src)?;
        SyntaxTreeNode::parse(tokens)?;
        Ok(())
    }

    #[test]
    fn parse() -> Result<(), Error>
    {
        let src = r#"1 2"#;
        let tokens = crate::tokenizer::tokenize(src)?;
        let root = SyntaxTreeNode::parse(tokens)?;
        assert_eq!(root.len(), 2);
        Ok(())
    }
}
