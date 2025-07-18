use std::{collections::HashSet, fmt::Display};

pub enum Type {
    Base(String),                // atomic type
    Arrow(Box<Type>, Box<Type>), // T1 -> T2
}

type Ctx = Vec<String>; // Context of variable names

#[derive(Clone, Debug, PartialEq)]
pub enum Term {
    Free(String), // Unbound

    Bound(usize), // Debruijn index

    Lam {
        name: String, // Name of the parameter
        //ty: Type,
        body: Box<Term>,
    },

    App {
        func: Box<Term>,
        arg: Box<Term>,
    },
}

impl Display for Term {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Term::Free(name) => write!(f, "{}", name),
            Term::Bound(index) => write!(f, "#{}", index),
            Term::Lam { name, body } => write!(f, "λ{}.{}", name, body),
            Term::App { func, arg } => {
                match func.as_ref() {
                    Term::Lam { .. } => write!(f, "({})", func),
                    _ => write!(f, "{}", func),
                }?;
                match arg.as_ref() {
                    Term::Lam { .. } => write!(f, " ({})", arg),
                    _ => write!(f, " {}", arg),
                }
            }
        }
    }
}

fn collect_free_vars(expr: &Term) -> HashSet<String> {
    let mut free_vars = HashSet::new();
    match expr {
        Term::Bound(_) => {}
        Term::Free(name) => {
            free_vars.insert(name.clone());
        }
        Term::Lam { body, .. } => {
            free_vars.extend(collect_free_vars(body));
        }
        Term::App { func, arg } => {
            free_vars.extend(collect_free_vars(func));
            free_vars.extend(collect_free_vars(arg));
        }
    }

    free_vars
}

/// Converts a named expression to a de Bruijn index expression.
/// `ctx` is used to keep track of variable names in scope and their indices.
fn to_debruijn(expr: &Term, ctx: &mut Ctx) -> Term {
    match expr {
        Term::Bound(idx) => Term::Bound(*idx),
        Term::Free(name) => {
            let idx = ctx.iter().rev().position(|n| n == name);

            if let Some(idx) = idx {
                Term::Bound(idx)
            } else {
                Term::Free(name.clone())
            }
        }
        Term::Lam { name, body } => {
            ctx.push(name.clone());
            let res = Term::Lam {
                name: name.clone(),
                body: Box::new(to_debruijn(body, ctx)),
            };
            ctx.pop();
            res
        }
        Term::App { func, arg } => Term::App {
            func: Box::new(to_debruijn(func, ctx)),
            arg: Box::new(to_debruijn(arg, ctx)),
        },
    }
}

/// Converts a de Bruijn index expression back to a named expression.
/// `ctx` is used to keep track of variable names in scope.
/// Currently, the program does not keep track of the original names of variables,
/// so it generates new names of the form `x_i` using the `fresh_var_name` function.
pub fn from_debruijn(expr: &Term, ctx: &mut Ctx) -> Term {
    match expr {
        Term::Bound(i) => {
            let idx = ctx.len() - 1 - i;
            Term::Free(ctx[idx].clone())
        }
        Term::Free(name) => Term::Free(name.clone()),
        Term::Lam { name: orig, body } => {
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
            //println!(
            //    "current context: {}",
            //    ctx.iter()
            //        .map(|s| s.as_str())
            //        .collect::<Vec<_>>()
            //        .join(", ")
            //);

            let res = Term::Lam {
                name,
                body: Box::new(from_debruijn(body, ctx)),
            };
            ctx.pop();

            res
        }
        Term::App { func, arg } => Term::App {
            func: Box::new(from_debruijn(func, ctx)),
            arg: Box::new(from_debruijn(arg, ctx)),
        },
    }
}

fn shift(expr: &Term, d: isize, cutoff: usize) -> Term {
    match expr {
        Term::Bound(idx) => {
            if *idx >= cutoff {
                Term::Bound(((*idx as isize) + d) as usize)
            } else {
                Term::Bound(*idx)
            }
        }
        Term::Free(name) => Term::Free(name.clone()),
        Term::Lam { name, body } => Term::Lam {
            name: name.clone(),
            body: Box::new(shift(body, d, cutoff + 1)),
        },
        Term::App { func, arg } => Term::App {
            func: Box::new(shift(func, d, cutoff)),
            arg: Box::new(shift(arg, d, cutoff)),
        },
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

pub fn var(name: &str) -> Term {
    Term::Free(name.to_string())
}

pub fn lam(param: &str, body: Term) -> Term {
    Term::Lam {
        name: param.to_string(),
        body: Box::new(body),
    }
}

pub fn app(func: Term, arg: Term) -> Term {
    Term::App {
        func: Box::new(func),
        arg: Box::new(arg),
    }
}

/// Substitutes `var` with `value` in `root` a.k.a beta-reduction (`[var := value]root`).
fn subst(root: &Term, var: usize, value: &Term) -> Term {
    // [var := value]root
    match root {
        Term::Bound(idx) => {
            if *idx == var {
                shift(value, var as isize, 0)
            } else {
                Term::Bound(*idx)
            }
        }
        Term::Free(name) => Term::Free(name.clone()),
        Term::Lam { name, body } => Term::Lam {
            name: name.clone(),
            body: Box::new(subst(body, var + 1, value)),
        },
        Term::App { func, arg } => Term::App {
            func: Box::new(subst(func, var, value)),
            arg: Box::new(subst(arg, var, value)),
        },
    }
}

fn eval(expr: Term) -> Term {
    match expr {
        Term::App { func, arg } => {
            let func_evaled = eval(*func);
            let arg_evaled = eval(*arg);

            match func_evaled {
                Term::Lam { body, .. } => {
                    let arg_shifted = shift(&arg_evaled, 1, 0);
                    let res = subst(&body, 0, &arg_shifted);
                    eval(shift(&res, -1, 0))
                }
                // NOTE: wrong?
                func_not_lam => Term::App {
                    func: Box::new(func_not_lam),
                    arg: Box::new(arg_evaled),
                },
            }
        }
        Term::Lam { name, body } => Term::Lam {
            name,
            body: Box::new(eval(*body)),
        },
        _ => expr,
    }
}

pub fn eval_dbr(expr: Term) -> Term {
    //println!("lambda: {} evals to {}", expr, eval_named(expr.clone()));
    let mut ctx = vec![];
    let debruijn_expr = to_debruijn(&expr, &mut ctx);
    let res = eval(debruijn_expr);
    let mut out_ctx = vec![];
    let printed = from_debruijn(&res, &mut out_ctx);
    printed
}
