use crate::lexer::Lexer;
use crate::parser::parse_expr;
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

