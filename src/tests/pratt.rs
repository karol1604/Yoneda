use crate::lexer::{Lexer, Token};
use crate::parser::parse_expr;

use pretty_assertions::assert_eq;

#[test]
fn tests() {
    let mut lexer = Lexer::new("λx.x");
    let expr = match parse_expr(&mut lexer) {
        Ok(e) => e,
        Err(e) => panic!("Failed to parse expression: {}", e),
    };

    assert_eq!(expr.to_string(), "λx.x");

    let mut lexer = Lexer::new("(λx.x x) (λx.x x)");
    let expr = match parse_expr(&mut lexer) {
        Ok(e) => e,
        Err(e) => panic!("Failed to parse expression: {}", e),
    };

    assert_eq!(expr.to_string(), "(λx.x x) (λx.x x)");
}

#[test]
fn multi_character() {
    let input = "λxyz. xyz";
    let mut lexer = Lexer::new(input);
    //token stream starts correctly
    assert_eq!(lexer.next(), Token::Lambda);
    assert_eq!(lexer.next(), Token::Ident("xyz".to_string()));
    assert_eq!(lexer.next(), Token::Dot);

    // AST and parser test:
    let mut lexer = Lexer::new(input);
    let ast = match parse_expr(&mut lexer) {
        Ok(e) => e,
        Err(e) => panic!("Failed to parse expression: {}", e),
    };

    assert_eq!(format!("{}", ast), "λxyz.xyz");
}
