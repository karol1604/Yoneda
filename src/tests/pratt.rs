
use crate::parser::{parse_expr};
use crate::lexer::{Token, Lexer};

use pretty_assertions::assert_eq;

#[test]
fn tests() {
    let mut lexer = Lexer::new("λx.x");
    let expr = parse_expr(&mut lexer);
    assert_eq!(expr.to_string(), "λx.x");

    let mut lexer = Lexer::new("(λx.x x) (λx.x x)");
    let expr = parse_expr(&mut lexer);
    assert_eq!(expr.to_string(), "(λx.x x) (λx.x x)");
    println!("{}", expr);
}


#[test]
fn multi_character(){
    let input = "λxyz. xyz";
    let mut lexer = Lexer::new(input);
    //token stream starts correctly
    assert_eq!(lexer.next(), Token::Lambda);
    assert_eq!(lexer.next(), Token::Ident("xyz".to_string()));
    assert_eq!(lexer.next(), Token::Dot);

    // AST and parser test:
    let mut lexer = Lexer::new(input);
    let ast = parse_expr(&mut lexer);
    assert_eq!(format!("{}", ast), "λxyz.xyz");
}


        
