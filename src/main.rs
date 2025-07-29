mod lexer;
mod parser;
mod repl;
mod term;
mod types;

use std::{
    collections::HashMap,
    error::Error,
    io::{self, Write},
};
use term::{app, typed_lam, var};
use types::Type;

use crate::{
    parser::parse_expr,
    repl::{Command, Repl},
    term::{eval_dbr, lam, let_in},
    types::{TypeEnv, TypeScheme, infer, infer_with_env},
};

fn main() {
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
    match parse_expr(&mut lexer) {
        Ok(term) => println!("Parsed term: {}", term),
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };

    let prelude = TypeEnv::new();
    let mut repl = repl::Repl::new(prelude);
    repl.add_command(Box::new(repl::commands::DefCommand));
    repl.add_command(Box::new(repl::commands::ShowEnvCommand));
    repl.run();

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
