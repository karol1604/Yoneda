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
    parser::parse_expr,
    term::{lam, let_in},
    types::{TypeEnv, TypeScheme, infer, infer_with_env},
};

fn main() {
    let id = lam("x", lam("y", lam("z", lam("w", app(var("w"), var("x"))))));
    let id = let_in("id", lam("x", var("x")), lam("z", app(var("id"), var("z"))));
    let id = typed_lam(
        "x",
        var("x"),
        Type::Arrow(
            Box::new(Type::Base("int".into())),
            Box::new(Type::Base("int".into())),
        ),
    );
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
        let term = parse_expr(&mut lexer);
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
