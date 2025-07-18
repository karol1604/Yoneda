mod term;
use std::collections::HashMap;

use term::{Type, app, eval_dbr, lam, type_of, var};

fn main() {
    let mut ctx: HashMap<String, Type> = HashMap::new();
    ctx.insert("b".into(), Type::Base("Int".into()));

    // id_int = λx: Int. x
    let id_int = lam("x", Type::Base("Int".into()), var("x"));

    // term = id_int b
    //let term = app(id_int, var("b"));
    let term = eval_dbr(id_int);

    println!("Term: {:?}", type_of(&term, &mut ctx));
}

#[cfg(test)]
mod tests;
