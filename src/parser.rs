use std::{fmt::Display};
use crate::{
    lexer::{Lexer, Token},
    term::Term,
};

/*
pub fn expr(input: &str) -> S {
    let mut lexer = Lexer::new(input);
    expr_bp(&mut lexer, 0)
}

pub fn expr_bp(lexer: &mut Lexer, min_bp: u8) -> S {
    let mut lhs = match lexer.next() {
        Token::Atom(it) =>
            S::Atom(it),
        t => panic!("bad token: {:?}", t),
    };

    loop {
        let op = match lexer.peek() {
            Token::Eof => break,
            Token::Op(op) => op,
            t => panic!("bad token: {:?}", t),
        };

        let (l_bp, r_bp) = infix_binding_power(op);
        if l_bp < min_bp {
            break;
        }

        lexer.next();
        let rhs = expr_bp(lexer, r_bp);

        lhs = S::Cons(op, vec![lhs, rhs]);
    }

    lhs
}

pub fn infix_binding_power(op: char) -> (u8, u8) {
    match op {
        '+' | '-' => (1, 2),
        '*' | '/' => (3, 4),
        _ => panic!("bad op: {:?}", op),
    }
}
 */
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
        Token::Ident(c) => Expr::Var(c),
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
