mod term;

use std::collections::HashMap;
use term::{Type, app, eval_dbr, eval_dbr_typed, lam, type_of, var};

fn main() {
    let mut ctx: HashMap<String, Type> = HashMap::new();
    ctx.insert("b".into(), Type::Base("Any".into()));

    // id_int = λx: Int. x
    let id_int = lam("x", var("x"));

    // term = id_int b
    //let term = app(id_int, var("b"));
    let _ = eval_dbr_typed(id_int, &mut ctx);
    //println!("id_int: {:?}", type_of(&term, &mut ctx));
    let _ = eval_dbr_typed(app(lam("x", var("x")), var("b")), &mut ctx);
    //println!("id_int b: {:?}", type_of(&b, &mut ctx));
}

#[cfg(test)]
mod tests;

