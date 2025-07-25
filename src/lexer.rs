#[derive(Debug, Clone, PartialEq, Eq)]

pub enum Token {
    Lambda,
    Dot,
    LParen,
    RParen,
    Ident(String), //variable names like x, y, z
    Eof,           //end of imput, special token
}

pub struct Lexer {
    tokens: Vec<Token>,
}

impl Lexer {
    pub fn new(input: &str) -> Lexer {
        let mut chars = input.chars().peekable();
        let mut tokens = Vec::new();

        while let Some(&c) = chars.peek() {
            if c.is_ascii_whitespace() {
                chars.next();
                continue;
            }

            tokens.push(match c {
                '\\' | 'λ' => {
                    chars.next();
                    Token::Lambda
                }
                '.' => {
                    chars.next();
                    Token::Dot
                }
                '(' => {
                    chars.next();
                    Token::LParen
                }
                ')' => {
                    chars.next();
                    Token::RParen
                }
                'a'..='z' | 'A'..='Z' => {
                    let mut ident = String::new();
                    while let Some(&ch) = chars.peek() {
                        if ch.is_ascii_alphanumeric() || ch == '_' {
                            ident.push(ch);
                            chars.next();
                        } else {
                            break;
                        }
                    }
                    Token::Ident(ident)
                }
                _ => panic!("unknown character: {}", c),
            });
        }

        tokens.reverse();
        Lexer { tokens }
    }

    pub fn next(&mut self) -> Token {
        self.tokens.pop().unwrap_or(Token::Eof)
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.last().unwrap_or_else(|| &Token::Eof).clone()
    }
}
