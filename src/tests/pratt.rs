
use crate::parser::{expr};
use pretty_assertions::assert_eq;

#[test]
fn tests() {
    let s = expr("1");
    assert_eq!(s.to_string(), "1");

    let s = expr("1 + 2 * 3");
    println!("Parsed expression: {}", s.to_string());
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr("a + b * c * d + e");
    println!("Parsed expression: {}", s.to_string());
    assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");
}