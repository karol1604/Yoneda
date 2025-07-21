use std::{fmt::Display, iter::Peekable, str::CharIndices};

mod span;
use span::Span;

#[derive(Debug, PartialEq, Clone)]
pub enum TokenKind {
    RParen,
    LParen,
    Dot,
    Lambda,
    Term(String),
    EOF,
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::Dot => write!(f, "."),
            TokenKind::Lambda => write!(f, "λ"),
            TokenKind::Term(term) => write!(f, "{}", term),
            TokenKind::EOF => write!(f, "EOF"),
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_kind: TokenKind,
    span: Span,
}

pub struct Lexer<'src> {
    chars: Peekable<CharIndices<'src>>,
    current: Option<char>,
    position: usize,
}

impl<'src> Lexer<'src> {
    pub fn new(input: &'src str) -> Self {
        let chars = input.char_indices();
        //let current = chars.next().map(|(_, c)| c);
        Lexer {
            chars: chars.peekable(),
            current: None,
            position: 0,
        }
    }

    fn advance(&mut self) {
        if let Some((_, c)) = self.chars.next() {
            self.current = Some(c);
            self.position += 1;
        } else {
            self.current = None;
        }
    }

    fn collect_term(&mut self) -> String {
        let mut term = String::new();
        while let Some(c) = self.current {
            if c.is_alphanumeric() || c == '_' {
                term.push(c);
            } else {
                break;
            }

            if let Some(&(_, next_c)) = self.chars.peek() {
                if next_c.is_alphanumeric() || next_c == '_' {
                    self.advance();
                } else {
                    break;
                }
            } else {
                self.advance();
                break;
            }
        }
        term
    }

    fn skip_whitespace(&mut self) {
        while self.chars.peek().is_some_and(|(_, c)| c.is_whitespace()) {
            self.chars.next();
        }
    }

    pub fn make_token(&mut self) -> Token {
        let span_start = self.position;
        self.advance();
        let token_kind = match self.current {
            Some(' ') | Some('\n') | Some('\t') => {
                self.skip_whitespace();
                return self.make_token(); // Recurse to get the next token
            }
            Some(')') => TokenKind::RParen,
            Some('(') => TokenKind::LParen,
            Some('\\') | Some('λ') => TokenKind::Lambda,
            Some('.') => TokenKind::Dot,
            Some(c) if c.is_alphanumeric() || c == '_' => {
                let term = self.collect_term();
                TokenKind::Term(term)
            }
            None => TokenKind::EOF,
            _ => panic!(
                "Unexpected character: {:?} at position {}",
                self.current, self.position
            ),
        };
        let span_end = self.position;
        let span = Span {
            start: span_start,
            end: span_end,
        };

        Token { token_kind, span }
    }

    pub fn tokenize(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while self.chars.peek().is_some() {
            tokens.push(self.make_token());
            println!("Current: {:?}", self.current);
        }
        tokens
    }
}
