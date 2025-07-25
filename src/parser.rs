use std::{fmt::Display};
use crate::{
    lexer::{Lexer, Token},
    term::Term,
};

pub fn parse_expr(lexer: &mut Lexer) -> Term {
    parse_app(lexer)
}
fn parse_app(lexer: &mut Lexer) -> Term {
    let mut expr = parse_atom(lexer);
    while let Token::Ident(_) | Token::LParen | Token::Lambda = lexer.peek() {
        let rhs = parse_atom(lexer);
        expr = Term::App {
            func: Box::new(expr),
            arg: Box::new(rhs),
        };
    }
    expr
}
fn parse_atom(lexer: &mut Lexer) -> Term {
    match lexer.next() {
        Token::Ident(c) => Term::Free(c),
        Token::LParen => { 
            let expr = parse_expr(lexer);
            match lexer.next() {
                Token::RParen => expr,
                _ => panic!("expected `)`"),
            }
        }
        Token::Lambda => {
            if let Token::Ident(param) = lexer.next() {
                if let Token::Dot = lexer.next() {
                    let body = parse_expr(lexer);
                    Term::Lam {
                        name: param,
                        body: Box::new(body),
                        ty: None,
                    }
                } else {
                    panic!("expected `.` after parameter");
                }
            } else {
                panic!("expected parameter after lambda");
            }
        }
        t => panic!("unexpected token: {:?}", t),
    }
}
