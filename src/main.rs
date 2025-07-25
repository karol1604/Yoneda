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
    term::{eval_dbr, lam, let_in},
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
    match parse_expr(&mut lexer) {
        Ok(term) => println!("Parsed term: {}", term),
        Err(e) => {
            eprintln!("Parse error: {}", e);
            return;
        }
    };

    let mut prelude = TypeEnv::new();
    loop {
        let mut to_parse = true;

        print!("λ> ");
        io::stdout().flush().unwrap();

        let mut input = String::new();
        if io::stdin().read_line(&mut input).is_err() {
            eprintln!("read error");
            continue;
        }

        let input = input.trim();

        if input == ":q" {
            break;
        }

        if input.starts_with(":def") {
            to_parse = false;
            let parts = input.split(' ').collect::<Vec<_>>();

            if parts[1] == "arrow" {
                if parts.len() != 5 {
                    eprintln!("Usage: :def arrow <name> <type1> <type2>");
                    continue;
                }
                println!("Defined `{}` as `{} -> {}`.", parts[2], parts[3], parts[4]);
                prelude.insert(
                    parts[2].to_string(),
                    TypeScheme {
                        forall: vec![],
                        body: Type::Arrow(
                            Box::new(Type::Base(parts[3].to_string())),
                            Box::new(Type::Base(parts[4].to_string())),
                        ),
                    },
                );
            } else {
                if parts.len() != 3 {
                    eprintln!("Usage: :def <name> <type>");
                    continue;
                }
                println!("Defined `{}` as `{}`.", parts[1], parts[2]);
                prelude.insert(
                    parts[1].to_string(),
                    TypeScheme {
                        forall: vec![],
                        body: Type::Base(parts[2].to_string()),
                    },
                );
            }
        }

        if to_parse {
            let mut lexer = lexer::Lexer::new(input);
            let term = match parse_expr(&mut lexer) {
                Ok(term) => term,
                Err(e) => {
                    eprintln!("Parse error: {}", e);
                    continue;
                }
            };

            match infer_with_env(&term, &prelude) {
                Ok(ty) => {
                    let reduced = eval_dbr(term.clone());
                    println!("⊢ {} : {}", reduced, ty);
                }
                Err(e) => println!("Error inferring type: {}", e),
            }
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
