mod parser;
mod term;
mod types;

use std::collections::HashMap;
use term::{app, typed_eval_dbr, typed_lam, var};
use types::Type;

use crate::types::infer;

fn main() {
    let id = typed_lam(
        "x",
        var("x"),
        Type::Arrow(
            Box::new(Type::Base("α".into())),
            Box::new(Type::Base("α".into())),
        ),
    );

    if let Ok(ty) = infer(&id) {
        println!("Type of id: {}", ty);
    } else {
        println!("Failed to infer type of id");
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
    //let test = "(λxasdasd.λy.x) yasdasd zz ";
    //let mut lexer = parser::Lexer::new(test);
    //println!(
    //    "tok: {}",
    //    lexer
    //        .tokenize()
    //        .iter()
    //        .map(|t| t.token_kind.clone().to_string())
    //        .collect::<Vec<_>>()
    //        .join(" ")
    //);
}

#[cfg(test)]
mod tests;
