mod parser;
mod term;
mod types;

use std::collections::HashMap;
use term::{app, typed_eval_dbr, typed_lam, var};
use types::Type;

use crate::{term::lam, types::infer};

fn main() {
    //let id = lam("x", lam("y", lam("z", lam("w", var("w")))));
    let id = lam(
        "f",
        lam("g", lam("x", app(var("f"), app(var("g"), var("x"))))),
    );

    match infer(&id) {
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
    let _ = typed_eval_dbr(expr, &mut ctx);
    //println!("result: {}", result);
    //
    let test = "(λxasdasd.λy.x) yasdasd zz ";
    let mut lexer = parser::Lexer::new(test);
    println!(
        "tok: {}",
        lexer
            .tokenize()
            .iter()
            .map(|t| t.token_kind.clone().to_string())
            .collect::<Vec<_>>()
            .join(" ")
    );
}

#[cfg(test)]
mod tests;
