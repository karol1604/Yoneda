use std::fmt;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]

pub enum Token{
    Lambda,
    Dot,
    LParen,
    RParen,
    Ident(char), //variable names like x, y, z
    Eof, //end of imput, special token
}
pub struct Lexer {
    tokens: Vec<Token>,
}
impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut tokens = input
            .chars()
            .filter(|it| !it.is_ascii_whitespace())
            .map(|c| match c {
                '\\'| 'λ' => Token::Lambda,
                '.' => Token::Dot,
                '(' => Token::LParen,
                ')' => Token::RParen,
                'a'..='z' |'A'..='Z' => Token::Ident(c),
                _ => panic!("Unknwon character: {}",c),
            })
            .collect::<Vec<_>>();
        tokens.reverse();
        Lexer { tokens }
    }
    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }
    pub fn peek(&mut self) -> Token {
        self.tokens.last().copied().unwrap_or(Token::Eof)
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Var(char),
    Lam(char, Box<Expr>),
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