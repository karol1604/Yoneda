use std::fmt::Display;

#[derive(Clone)]
enum Expr { //Expr can be a Variabel, lambda function or Application
    Var(String),
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

fn var(name: &str) -> Expr {
    Expr::Var(name.to_string())
}

fn lam(param: &str, body: Expr) -> Expr {
    Expr::Lam(param.to_string(), Box::new(body))
}

fn app(func: Expr, arg: Expr) -> Expr {
    Expr::App(Box::new(func), Box::new(arg))
}

impl Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Var(name) => write!(f, "{}", name),
            Expr::Lam(param, body) => write!(f, "λ{}.{}", param, body),
            Expr::App(func, arg) => {
                match func.as_ref() {
                    Expr::Lam(_, _) => write!(f, "({})", func),
                    _ => write!(f, "{}", func),
                }?;
                match arg.as_ref() {
                    Expr::Lam(_, _) => write!(f, " ({})", arg),
                    _ => write!(f, " {}", arg),
                }
            }
        }
    }
}

fn subst(root: Expr, var: &str, value: &Expr) -> Expr {
    match root {
        Expr::Var(v) => {
            if v == var {
                value.clone()
            } else {
                Expr::Var(v)
            }
        }
        Expr::Lam(v, body) => {
            if v == var {
                Expr::Lam(v, body) // No substitution needed for bound variable
            } else {
                Expr::Lam(v, Box::new(subst(*body, var, value)))
            }
        }
        Expr::App(left, right) => Expr::App(
            Box::new(subst(*left, var, value)),
            Box::new(subst(*right, var, value)),
        ),
    }
}

// FIXME: This is a very naive evaluation function. We should check for clashing variable names
// as they can lead to incorrect substitutions.
// For example, `λx . (λy.x)` applied to `y` should not substitute `y` with `x` in the body of the
// first lambda as it would return the identity function `λy.y` but we want something like `λy_1 . y`.
fn eval(expr: Expr) -> Expr {
    match expr {
        Expr::App(func, arg) => {
            if let Expr::Lam(param, body) = *func {
                // Substitute the argument into the body of the lambda
                subst(*body, &param, &arg)
            } else {
                Expr::App(func, arg)
            }
        }
        _ => expr,
    }
}

fn main() {
    let id = app(lam("x", var("x")), var("y"));
    println!("Hello lambda: {} evals to {}", id, eval(id.clone()));

    // NOTE: this is incorrect obv
    let t = lam("x", lam("y", var("x")));
    let t_app = app(t.clone(), var("y"));
    println!("function: {} evals to {}", t, eval(t_app.clone()));
}
