use shared::error;
use shared::error::Error;

use crate::tokenizer::{Token, TokenValue};

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

    pub fn parse(tokens: Vec<Token>) -> Result<Self, Error>
    {
        let mut eater = TokenEater::new(tokens);
        return eater.parse();
    }

    /// Assume the node is an identifier. Return the identifier as a
    /// string.
    pub fn getIdentifierName(&self) -> Result<&str, Error>
    {
        if let Self::Atom(token) = self
        {
            if let TokenValue::Ident(name) = token.value()
            {
                return Ok(name);
            }
        }
        Err(error!(RuntimeError, "Node is not an identifier"))
    }

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

    fn parse(&mut self) -> Result<SyntaxTreeNode, Error>
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
                let quoted = self.parse()?;
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

            nodes.push(self.parse()?);
        }

        // (a . b) -> (cons a b)
        if nodes.len() >= 2
        {
            if let SyntaxTreeNode::Atom(token) = &nodes[1]
            {
                if token.value() == &TokenValue::Dot
                {
                    if nodes.len() != 3
                    {
                        return Err(error!(ParseError, "Invalid dot expression"));
                    }
                    nodes.swap(0, 1);
                    nodes[0] = SyntaxTreeNode::Atom(
                        Token::new("", TokenValue::Ident(String::from("cons"))));
                }
            }
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
        let root = SyntaxTreeNode::parse(tokens)?;
        Ok(())
    }
}
