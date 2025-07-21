mod term;

use std::collections::HashMap;
use term::{Type, app, eval_dbr, lam, typed_eval_dbr, typed_lam, var};

fn main() {
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
}

#[cfg(test)]
mod tests;
