mod term;
use term::{app, eval_dbr, from_debruijn, lam, var};

#[cfg(test)]
mod tests;

fn main() {
    println!("Starting evaluation of lambda expressions...");

    let id = lam("x", var("x"));
    let id_app = app(id, var("y"));
    let result = eval_dbr(id_app);
    let mut ctx = vec![];
    println!("=> {}", from_debruijn(&result, &mut ctx));
}
