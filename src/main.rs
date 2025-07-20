mod term;

use std::collections::HashMap;
use term::{Type, app, eval_dbr, lam, typed_eval_dbr, typed_lam, var};

fn main() {
    let mut ctx: HashMap<String, Type> = HashMap::new();
    //ctx.insert(
    //    "g".into(),
    //    Type::Arrow(
    //        Box::new(Type::Base("Any".into())),
    //        Box::new(Type::Base("V".into())),
    //    ),
    //);
    //ctx.insert("t".into(), Type::Base("Any".into()));

    // id_int = λx: Nay. x
    //let id_int = lam("x", var("x"));
    //println!("id_int: {}", id_int);
    //let _ = eval_dbr_typed(id_int, &mut ctx);
    //
    //let expr = app(lam("x", var("x")), var("b"));
    //println!("expr: {}", expr);
    //let _ = eval_dbr_typed(expr, &mut ctx);

    //let expr = app(
    //    app(
    //        typed_lam(
    //            "x",
    //            var("x"),
    //            Type::Arrow(
    //                Box::new(Type::Base("Any".into())),
    //                Box::new(Type::Base("V".into())),
    //            ),
    //        ),
    //        var("g"),
    //    ),
    //    var("t"),
    //);
    //println!("expr: {}", expr);
    //let _ = typed_eval_dbr(expr, &mut ctx);

    ctx.insert("y".into(), Type::Base("Any".into()));
    ctx.insert("z".into(), Type::Base("Any".into()));
    let expr = app(
        app(
            typed_lam(
                "x",
                typed_lam("y", var("x"), Type::Base("Any".into())),
                Type::Base("Any".into()),
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
