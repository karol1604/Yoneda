mod lexer;
mod parser;
mod term;
mod types;

use std::{
    collections::HashMap,
    io::{self, Write},
};
use term::{app, typed_lam, var};
use types::Type;

use crate::{
    lexer::Expr,
    parser::parse_expr,
    term::{Term, lam, let_in},
    types::{TypeEnv, TypeScheme, infer, infer_with_env},
};

fn expr_to_term(expr: Expr) -> Term {
    match expr {
        Expr::Var(name) => var(name.as_str()),
        Expr::Lam(name, body) => lam(name.as_str(), expr_to_term(*body)),
        Expr::App(lhs, rhs) => app(expr_to_term(*lhs), expr_to_term(*rhs)),
        //Expr::Let(name, value, body) => let_in(name, expr_to_term(*value), expr_to_term(*body)),
    }
}
fn main() {
    let id = lam("x", lam("y", lam("z", lam("w", app(var("w"), var("x"))))));
    let id = let_in("id", lam("x", var("x")), lam("z", app(var("id"), var("z"))));
    match infer(&id) {
        Ok(ty) => println!("⊢ {} : {}", id, ty),
        Err(e) => println!("Error inferring type: {}", e),
    }
    //let id = lam(
    //    "f",
    //    lam("g", lam("x", app(var("f"), app(var("g"), var("x"))))),
    //);

    let mut prelude = TypeEnv::new();

    // if you *know* a's type:
    prelude.insert(
        "true".into(),
        TypeScheme {
            forall: vec![],
            body: Type::Base("Bool".into()),
        },
    );
    let id = let_in("id", lam("x", var("x")), var("id"));

    match infer_with_env(&id, &prelude) {
        Ok(ty) => println!("⊢ {} : {}", id, ty),
        Err(e) => println!("Error inferring type: {}", e),
    }

    let mut ctx: HashMap<String, Type> = HashMap::new();
    ctx.insert("y".into(), Type::Base("α".into()));
    ctx.insert("z".into(), Type::Base("α".into()));
    let expr = app(
        app(
            typed_lam(
                "x",
                typed_lam("y", var("x"), Type::Base("α".into())),
                Type::Base("α".into()),
            ),
            var("y"),
        ),
        var("z"),
    );
    println!("expr: {}", expr);
    //let _ = typed_eval_dbr(expr, &mut ctx);
    //println!("result: {}", result);

    let test = "(λxasdadsdh.λy.x) y z";
    let mut lexer = lexer::Lexer::new(test);
    let expr = parse_expr(&mut lexer);
    println!("expr_p: {}", expr);

    loop {
        print!("λ> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("read error");
            continue;
        }

        if input.trim() == ":q" {
            break;
        }
        let input = input.trim();

        let mut lexer = lexer::Lexer::new(input);
        let expr = parse_expr(&mut lexer);
        let term = expr_to_term(expr);
        match infer(&term) {
            Ok(ty) => println!("⊢ {} : {}", term, ty),
            Err(e) => println!("Error inferring type: {}", e),
        }
    }

    /*
    println!(
        "tok: {}",
        lexer
            .tokenize()
            .iter()
            .map(|t| t.token_kind.clone().to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
     */
}

#[cfg(test)]
mod tests;
