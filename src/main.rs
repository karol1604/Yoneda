use std::{collections::HashSet, fmt::Display};

#[derive(Clone)]
enum Expr {
    Var(usize),
    Free(String), // unbound variable, not sure about this
    Lam(String, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
}

fn collect_free_vars(expr: &Expr) -> HashSet<String> {
    let mut free_vars = HashSet::new();
    match expr {
        Expr::Var(_) => {}
        Expr::Free(name) => {
            free_vars.insert(name.clone());
        }
        Expr::Lam(_, body) => {
            free_vars.extend(collect_free_vars(body));
        }
        Expr::App(l, r) => {
            free_vars.extend(collect_free_vars(l));
            free_vars.extend(collect_free_vars(r));
        }
    }

    free_vars
}

/// Converts a named expression to a de Bruijn index expression.
/// `ctx` is used to keep track of variable names in scope and their indices.
fn to_debruijn(expr: &NamedExpr, ctx: &mut Vec<String>) -> Expr {
    match expr {
        NamedExpr::Var(name) => {
            let idx = ctx.iter().rev().position(|n| n == name);

            if let Some(idx) = idx {
                Expr::Var(idx)
            } else {
                Expr::Free(name.clone())
            }
        }
        NamedExpr::Lam(param, body) => {
            ctx.push(param.clone());
            let res = Expr::Lam(param.clone(), Box::new(to_debruijn(body, ctx)));
            ctx.pop();
            res
        }
        NamedExpr::App(lam, arg) => Expr::App(
            Box::new(to_debruijn(lam, ctx)),
            Box::new(to_debruijn(arg, ctx)),
        ),
    }
}

/// Converts a de Bruijn index expression back to a named expression.
/// `ctx` is used to keep track of variable names in scope.
/// Currently, the program does not keep track of the original names of variables,
/// so it generates new names of the form `x_i` using the `fresh_var_name` function.
fn from_debruijn(expr: &Expr, ctx: &mut Vec<String>) -> NamedExpr {
    match expr {
        Expr::Var(i) => {
            let idx = ctx.len() - 1 - i;
            NamedExpr::Var(ctx[idx].clone())
        }
        Expr::Lam(orig, body) => {
            let free_vars = collect_free_vars(body);

            let name = if ctx.contains(orig) || free_vars.contains(orig) {
                // If the original name is already in the context or is a free variable,
                // generate a new name.
                fresh_var_name(ctx)
            } else {
                orig.clone()
            };
            //let name = fresh_var_name(ctx);
            ctx.push(name.clone());
            println!(
                "current context: {}",
                ctx.iter()
                    .map(|s| s.as_str())
                    .collect::<Vec<_>>()
                    .join(", ")
            );

            let res = NamedExpr::Lam(name, Box::new(from_debruijn(body, ctx)));
            ctx.pop();
            res
        }
        Expr::App(lam, arg) => NamedExpr::App(
            Box::new(from_debruijn(lam, ctx)),
            Box::new(from_debruijn(arg, ctx)),
        ),
        Expr::Free(name) => NamedExpr::Var(name.clone()),
    }
}

fn shift(expr: &Expr, d: isize, cuttof: usize) -> Expr {
    match expr {
        Expr::Var(idx) => {
            if *idx >= cuttof {
                Expr::Var(((*idx as isize) + d) as usize)
            } else {
                Expr::Var(*idx)
            }
        }
        Expr::Free(name) => Expr::Free(name.clone()),
        Expr::Lam(name, body) => Expr::Lam(name.clone(), Box::new(shift(body, d, cuttof + 1))),
        Expr::App(lam, arg) => Expr::App(
            Box::new(shift(lam, d, cuttof)),
            Box::new(shift(arg, d, cuttof)),
        ),
    }
}

fn fresh_var_name(ctx: &[String]) -> String {
    let mut i = 0;
    loop {
        let name = format!("x_{}", i);
        if !ctx.contains(&name) {
            return name;
        }
        i += 1;
    }
}

#[derive(Clone)]
enum NamedExpr {
    //Expr can be a Variable, lambda function or Application
    Var(String),
    Lam(String, Box<NamedExpr>),
    App(Box<NamedExpr>, Box<NamedExpr>),
}

fn var(name: &str) -> NamedExpr {
    NamedExpr::Var(name.to_string())
}

fn lam(param: &str, body: NamedExpr) -> NamedExpr {
    NamedExpr::Lam(param.to_string(), Box::new(body))
}

fn app(func: NamedExpr, arg: NamedExpr) -> NamedExpr {
    NamedExpr::App(Box::new(func), Box::new(arg))
}

impl Display for NamedExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NamedExpr::Var(name) => write!(f, "{}", name),
            NamedExpr::Lam(param, body) => write!(f, "λ{}.{}", param, body),
            NamedExpr::App(func, arg) => {
                match func.as_ref() {
                    NamedExpr::Lam(_, _) => write!(f, "({})", func),
                    _ => write!(f, "{}", func),
                }?;
                match arg.as_ref() {
                    NamedExpr::Lam(_, _) => write!(f, " ({})", arg),
                    _ => write!(f, " {}", arg),
                }
            }
        }
    }
}

/// Substitutes `var` with `value` in `root` a.k.a beta-reduction (`[var := value]root`).
fn subst(root: &Expr, var: usize, value: &Expr) -> Expr {
    // [var := value]root
    match root {
        Expr::Var(idx) => {
            if *idx == var {
                shift(value, var as isize, 0)
            } else {
                Expr::Var(*idx)
            }
        }
        Expr::Free(name) => Expr::Free(name.clone()),
        Expr::Lam(name, body) => Expr::Lam(name.clone(), Box::new(subst(body, var + 1, value))),
        Expr::App(lam, arg) => Expr::App(
            Box::new(subst(lam, var, value)),
            Box::new(subst(arg, var, value)),
        ),
    }
}

fn eval(expr: Expr) -> Expr {
    match expr {
        Expr::App(func, arg) => {
            let func_evaled = eval(*func);
            let arg_evaled = eval(*arg);

            match func_evaled {
                Expr::Lam(_name, body) => {
                    let arg_shifted = shift(&arg_evaled, 1, 0);
                    let res = subst(&body, 0, &arg_shifted);
                    eval(shift(&res, -1, 0))
                }
                // NOTE: wrong?
                func_not_lam => Expr::App(Box::new(func_not_lam), Box::new(arg_evaled)),
            }
        }
        Expr::Lam(name, body) => Expr::Lam(name.clone(), Box::new(eval(*body))),
        _ => expr,
    }
}

fn eval_dbr(expr: NamedExpr) -> NamedExpr {
    //println!("lambda: {} evals to {}", expr, eval_named(expr.clone()));
    let mut ctx = vec![];
    let debruijn_expr = to_debruijn(&expr, &mut ctx);
    let res = eval(debruijn_expr);
    let mut out_ctx = vec![];
    let printed = from_debruijn(&res, &mut out_ctx);
    println!("Debruijn: {} evals to {}\n", expr, printed);
    printed
}

fn main() {
    //let id = app(lam("x", var("x")), var("a"));
    //let id = lam("x", var("y"));
    //eval_dbr(id);
    //
    //let test = app(lam("x", lam("y", app(var("x"), var("y")))), var("z"));
    //eval_dbr(test);
    //
    //let t = lam("x", lam("y", var("x")));
    //let t_app = app(app(t.clone(), var("y")), var("z"));
    //eval_dbr(t_app);
    //
    //eval_dbr(var("x"));
    //
    //let expr = app(
    //    app(
    //        lam("x", lam("y", app(var("x"), var("y")))),
    //        lam("z", var("z")),
    //    ),
    //    var("a"),
    //);
    //eval_dbr(expr);
    //
    //let expr = app(
    //    lam("x", app(var("x"), lam("y", var("y")))),
    //    lam("z", app(var("z"), var("z"))),
    //);
    //eval_dbr(expr);
    //
    //let expr = app(
    //    lam("x", app(lam("x", var("x")), app(var("x"), var("x")))),
    //    lam("y", var("y")),
    //);
    //eval_dbr(expr);
    //
    //let expr = app(
    //    app(
    //        lam(
    //            "a",
    //            lam("b", app(var("a"), lam("c", app(var("b"), var("c"))))),
    //        ),
    //        lam("x", app(var("x"), var("x"))),
    //    ),
    //    lam("y", var("y")),
    //);
    //eval_dbr(expr);
    //
    //let expr = app(
    //    app(
    //        app(
    //            lam(
    //                "f",
    //                lam(
    //                    "g",
    //                    lam("x", app(var("f"), app(var("g"), app(var("g"), var("x"))))),
    //                ),
    //            ),
    //            var("inc"), // imagine this is +1
    //        ),
    //        var("dbl"), // imagine this is *2
    //    ),
    //    var("z"),
    //);
    //eval_dbr(expr);
    //
    //let expr = app(
    //    app(
    //        lam("x", lam("y", app(var("x"), app(var("y"), var("y"))))),
    //        lam("a", lam("b", app(var("b"), var("a")))),
    //    ),
    //    lam("c", var("c")),
    //);
    //eval_dbr(expr);

    // BUG: evaluates incorrectly
    let expr = app(
        lam("x", app(var("x"), var("x"))),
        lam("y", lam("z", var("y"))),
    );
    eval_dbr(expr);

    let expr = app(lam("x", lam("y", var("x"))), var("y"));
    eval_dbr(expr);

    let expr = app(
        app(
            lam(
                "x",
                lam("y", app(var("x"), lam("x", app(var("y"), var("x"))))),
            ),
            lam("a", var("a")),
        ),
        lam("b", var("b")),
    );
    eval_dbr(expr);

    let expr = app(
        lam("f", app(var("f"), lam("x", var("x")))),
        lam("g", lam("h", app(var("h"), var("g")))),
    );
    eval_dbr(expr);

    let expr = app(
        app(
            lam(
                "x",
                lam("y", app(var("x"), lam("x", app(var("y"), var("x"))))),
            ),
            lam("z", var("z")),
        ),
        lam("w", app(var("w"), var("w"))),
    );
    eval_dbr(expr);

    let expr = app(
        app(
            lam(
                "x",
                lam("y", app(var("y"), lam("z", app(var("x"), var("z"))))),
            ),
            lam("u", var("u")),
        ),
        lam("v", var("v")),
    );
    eval_dbr(expr);

    let expr = app(
        lam(
            "x",
            app(
                lam("y", app(var("x"), lam("x", app(var("y"), var("x"))))),
                lam("z", var("z")),
            ),
        ),
        lam("w", var("w")),
    );
    eval_dbr(expr);

    let expr = app(
        app(
            // S = λx.λy.λz. x z (y z)
            lam(
                "x",
                lam(
                    "y",
                    lam("z", app(app(var("x"), var("z")), app(var("y"), var("z")))),
                ),
            ),
            // K = λx.λy. x
            lam("x", lam("y", var("x"))),
        ),
        // K = λx.λy. x
        lam("x", lam("y", var("x"))),
    );
    eval_dbr(expr);

    let expr = app(
        // PRED = λn.λf.λx.
        //   n (λg.λh. h (g f)) (λu. x) (λu. u)
        lam(
            "n",
            lam(
                "f",
                lam(
                    "x",
                    app(
                        app(
                            app(
                                var("n"),
                                lam("g", lam("h", app(var("h"), app(var("g"), var("f"))))),
                            ),
                            lam("u", var("x")),
                        ),
                        lam("u", var("u")),
                    ),
                ),
            ),
        ),
        // TWO = λf.λx. f (f x)
        lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
    );
    eval_dbr(expr);
}
