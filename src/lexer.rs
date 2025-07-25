#[derive(Debug, Clone, PartialEq, Eq)]

use std::fmt::{Display, Formatter};

pub enum Token {
    Lambda,
    Dot,
    LParen,
    RParen,
    Ident(String), //variable names like x, y, z
    Eof, //end of imput, special token
}

pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut chars = input.chars().peekable();
        let mut tokens = Vec::new();
        while let Some(&c) = chars.peek() {
            match c {
                c if c.is_whitespace() => {
                    chars.next();
                }
                '\\' | 'λ' => {
                    chars.next();
                    tokens.push(Token::Lambda);
                }
                '.' => {
                    chars.next();
                    tokens.push(Token::Dot);
                }
                '(' => {
                    chars.next();
                    tokens.push(Token::LParen);
                }

                ')' => {
                    chars.next();
                    tokens.push(Token::RParen);
                }
                c if c.is_alphabetic() => {
                    let mut ident = String::new();
                    while let Some(&c) = chars.peek() {
                        if c.is_alphanumeric() {
                            ident.push(c);
                            chars.next();
                        } else{ break; }
                    }
                    tokens.push(Token::Ident(ident));
                }
                _ => { panic!("unexpected character: {}", c); }
            }
        }
        tokens.reverse();
        Lexer{tokens}
    }

    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }
    pub fn peek(&mut self) -> Token {
        self.tokens.last().cloned().unwrap_or(Token::Eof)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(String),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}
impl Display for Expr {
    fn fmt (&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Var(x) => write!(f, "{}", x),
            Expr::App(lhs, rhs) => write!(f, "({} {})", lhs, rhs),
            Expr::Lam(param, body) => write!(f, "(λ{}.{})", param, body),
        }
    }
}
/*
impl fmt::Display for S {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i),
            S::Cons(head, rest) => {
                write!(f, "({}", head)?;
                for s in rest {
                    write!(f, " {}", s)?;
                }
                write!(f, ")")
            }
        }

    }
}
*/
