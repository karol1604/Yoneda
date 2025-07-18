mod term;
use term::{app, eval_dbr, lam, var};

fn main() {
    println!("Starting evaluation of lambda expressions...");

    let id = app(lam("x", var("x")), var("a"));
    eval_dbr(id);

    let test = app(lam("x", lam("y", app(var("x"), var("y")))), var("z"));
    eval_dbr(test);

    let t = lam("x", lam("y", var("x")));
    let t_app = app(app(t.clone(), var("y")), var("z"));
    eval_dbr(t_app);

    eval_dbr(var("x"));

    let expr = app(
        app(
            lam("x", lam("y", app(var("x"), var("y")))),
            lam("z", var("z")),
        ),
        var("a"),
    );
    eval_dbr(expr);

    let expr = app(
        lam("x", app(var("x"), lam("y", var("y")))),
        lam("z", app(var("z"), var("z"))),
    );
    eval_dbr(expr);

    let expr = app(
        lam("x", app(lam("x", var("x")), app(var("x"), var("x")))),
        lam("y", var("y")),
    );
    eval_dbr(expr);

    let expr = app(
        app(
            lam(
                "a",
                lam("b", app(var("a"), lam("c", app(var("b"), var("c"))))),
            ),
            lam("x", app(var("x"), var("x"))),
        ),
        lam("y", var("y")),
    );
    eval_dbr(expr);

    let expr = app(
        app(
            app(
                lam(
                    "f",
                    lam(
                        "g",
                        lam("x", app(var("f"), app(var("g"), app(var("g"), var("x"))))),
                    ),
                ),
                var("inc"), // imagine this is +1
            ),
            var("dbl"), // imagine this is *2
        ),
        var("z"),
    );
    eval_dbr(expr);


    let expr = app(
        app(
            lam("x", lam("y", app(var("x"), app(var("y"), var("y"))))),
            lam("a", lam("b", app(var("b"), var("a")))),
        ),
        lam("c", var("c")),
    );
    eval_dbr(expr);

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

    let expr = app(
        // fst = λp. p (λx.λy.x)
        lam("p", app(var("p"), lam("x", lam("y", var("x"))))),
        // pair TWO THREE = (λx.λy.λf. f x y) TWO THREE
        app(
            app(
                lam(
                    "x",
                    lam("y", lam("f", app(app(var("f"), var("x")), var("y")))),
                ),
                // TWO  = λf.λx. f (f x)
                lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
            ),
            // THREE = λf.λx. f (f (f x))
            lam(
                "f",
                lam("x", app(var("f"), app(var("f"), app(var("f"), var("x"))))),
            ),
        ),
    );
    eval_dbr(expr);

    let expr = app(
        // snd = λp. p (λx.λy. y)
        lam("p", app(var("p"), lam("x", lam("y", var("y"))))),
        // pair TWO THREE
        app(
            app(
                // pair = λx.λy.λf. f x y
                lam(
                    "x",
                    lam("y", lam("f", app(app(var("f"), var("x")), var("y")))),
                ),
                // TWO   = λf.λx. f (f x)
                lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
            ),
            // THREE = λf.λx. f (f (f x))
            lam(
                "f",
                lam("x", app(var("f"), app(var("f"), app(var("f"), var("x"))))),
            ),
        ),
    );
    eval_dbr(expr);

    let expr = app(
        // MULT = λm.λn.λf. m (n f)
        lam(
            "m",
            lam("n", lam("f", app(var("m"), app(var("n"), var("f"))))),
        ),
        // TWO = λf.λx. f (f x)
        lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
    );
    // then apply MULT to THREE
    let expr = app(
        expr,
        lam(
            "f",
            lam("x", app(var("f"), app(var("f"), app(var("f"), var("x"))))),
        ),
    );
    eval_dbr(expr);

    let expr = app(
        // EXP = λm.λn. n m
        lam("m", lam("n", app(var("n"), var("m")))),
        // TWO
        lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
    );
    // then apply EXP TWO to THREE
    let expr = app(
        expr,
        lam(
            "f",
            lam("x", app(var("f"), app(var("f"), app(var("f"), var("x"))))),
        ),
    );
    eval_dbr(expr);

    let expr = app(
        // OR = λp.λq. p p q
        lam("p", lam("q", app(app(var("p"), var("p")), var("q")))),
        // TRUE  = λa.λb. a
        lam("a", lam("b", var("a"))),
    );
    let expr = app(
        expr,
        // FALSE = λa.λb. b
        lam("a", lam("b", var("b"))),
    );
    eval_dbr(expr);

    let expr = app(
        // ISZERO = λn. n (λx.FALSE) TRUE
        lam(
            "n",
            app(
                app(
                    var("n"),
                    // λx.FALSE
                    lam("x", lam("a", lam("b", var("b")))),
                ),
                // TRUE
                lam("a", lam("b", var("a"))),
            ),
        ),
        // ZERO = λf.λx. x
        lam("f", lam("x", var("x"))),
    );
    eval_dbr(expr);

    let expr = app(
        app(
            // EXP = λm.λn. n m
            lam("m", lam("n", app(var("n"), var("m")))),
            // THREE = λf.λx. f (f (f x))
            lam(
                "f",
                lam("x", app(var("f"), app(var("f"), app(var("f"), var("x"))))),
            ),
        ),
        // TWO = λf.λx. f (f x)
        lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
    );
    eval_dbr(expr);
}

#[cfg(test)]
mod tests;
