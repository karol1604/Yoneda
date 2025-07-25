use crate::{
    lexer::{Lexer, Token},
    term::Term,
};
use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    UnexpectedToken(Token),
    ExpectedToken { expected: Token, found: Token },
    ExpectedParamAfterLambda,
}

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParserError::UnexpectedToken(t) => write!(f, "Unexpected token `{:?}`", t),
            ParserError::ExpectedToken { expected, found } => {
                write!(f, "Expected `{:?}`, found `{:?}`", expected, found)
            }
            ParserError::ExpectedParamAfterLambda => write!(f, "Expected parameter after λ"),
        }
    }
}
pub fn parse_expr(lexer: &mut Lexer) -> Result<Term, ParserError> {
    Ok(parse_app(lexer)?)
}

fn parse_app(lexer: &mut Lexer) -> Result<Term, ParserError> {
    let mut expr = parse_atom(lexer)?;
    while let Token::Ident(_) | Token::LParen | Token::Lambda = lexer.peek() {
        let rhs = parse_atom(lexer)?;
        expr = Term::App {
            func: Box::new(expr),
            arg: Box::new(rhs),
        };
    }
    Ok(expr)
}

fn parse_atom(lexer: &mut Lexer) -> Result<Term, ParserError> {
    match lexer.next() {
        Token::Ident(c) => Ok(Term::Free(c)),
        Token::LParen => {
            let expr = parse_expr(lexer);
            match lexer.next() {
                Token::RParen => expr,
                _ => Err(ParserError::ExpectedToken {
                    expected: Token::RParen,
                    found: lexer.peek(),
                }),
            }
        }
        Token::Lambda => {
            if let Token::Ident(param) = lexer.next() {
                if let Token::Dot = lexer.next() {
                    let body = parse_expr(lexer)?;
                    Ok(Term::Lam {
                        name: param,
                        body: Box::new(body),
                        ty: None,
                    })
                } else {
                    Err(ParserError::ExpectedToken {
                        expected: Token::Dot,
                        found: lexer.peek(),
                    })
                }
            } else {
                Err(ParserError::ExpectedParamAfterLambda)
            }
        }
        t => Err(ParserError::UnexpectedToken(t)),
    }
}
