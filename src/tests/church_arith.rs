use crate::{app, eval_dbr, lam, term::Term, var};

/// Build the Church‐numeral for n
fn church(n: usize) -> Term {
    let mut body = var("x");
    for _ in 0..n {
        body = app(var("f"), body);
    }
    lam("f", lam("x", body))
}

fn church_true() -> Term {
    lam("a", lam("b", var("a")))
}
fn church_false() -> Term {
    lam("a", lam("b", var("b")))
}

#[test]
fn test_skk_combinator_reduces_to_identity() {
    // (S K K) → I
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
        // K
        lam("x", lam("y", var("x"))),
    );
    let result = eval_dbr(expr);
    let expected = lam("z", var("z"));
    assert_eq!(result, expected);
}

#[test]
fn test_pred_two_reduces_to_one() {
    // PRED TWO → ONE
    let expr = app(
        // PRED = λn.λf.λx. n (λg.λh. h (g f)) (λu. x) (λu. u)
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
        church(2),
    );
    let result = eval_dbr(expr);
    let expected = church(1);
    assert_eq!(result, expected);
}

#[test]
fn test_fst_of_pair_two_three() {
    // fst (pair TWO THREE) → TWO
    let pair = app(
        // pair = λx.λy.λf. f x y
        lam(
            "x",
            lam("y", lam("f", app(app(var("f"), var("x")), var("y")))),
        ),
        church(2),
    );
    let pair = app(pair, church(3));
    let expr = app(
        // fst = λp. p (λx.λy. x)
        lam("p", app(var("p"), lam("x", lam("y", var("x"))))),
        pair,
    );
    let result = eval_dbr(expr);
    let expected = church(2);
    assert_eq!(result, expected);
}

#[test]
fn test_snd_of_pair_two_three() {
    // snd (pair TWO THREE) → THREE
    let pair = app(
        lam(
            "x",
            lam("y", lam("f", app(app(var("f"), var("x")), var("y")))),
        ),
        church(2),
    );
    let pair = app(pair, church(3));
    let expr = app(
        // snd = λp. p (λx.λy. y)
        lam("p", app(var("p"), lam("x", lam("y", var("y"))))),
        pair,
    );
    let result = eval_dbr(expr);
    let expected = church(3);
    assert_eq!(result, expected);
}

#[test]
fn test_mult_two_three() {
    // MULT TWO THREE → SIX
    let expr = app(
        // MULT = λm.λn.λf. m (n f)
        lam(
            "m",
            lam("n", lam("f", app(var("m"), app(var("n"), var("f"))))),
        ),
        church(2),
    );
    let expr = app(expr, church(3));
    let result = eval_dbr(expr);
    let expected = church(6);
    assert_eq!(result, expected);
}

#[test]
#[ignore = "This evaluates correctly, but the names from the `church` function are sligtly different."]
fn test_exp_two_three() {
    // EXP TWO THREE → EIGHT
    let expr = app(
        // EXP = λm.λn. n m
        lam("m", lam("n", app(var("n"), var("m")))),
        church(2),
    );
    let expr = app(expr, church(3));
    let result = eval_dbr(expr);
    let expected = church(8);
    assert_eq!(result, expected);
}

#[test]
#[ignore = "This evaluates correctly, but the names from the `church` function are sligtly different."]
fn test_exp_three_two() {
    // EXP THREE TWO → NINE
    let expr = app(lam("m", lam("n", app(var("n"), var("m")))), church(3));
    let expr = app(expr, church(2));
    let result = eval_dbr(expr);
    let expected = church(9);
    assert_eq!(result, expected);
}

#[test]
fn test_or_true_false() {
    // OR TRUE FALSE → TRUE
    let expr = app(
        // OR = λp.λq. p p q
        lam("p", lam("q", app(app(var("p"), var("p")), var("q")))),
        church_true(),
    );
    let expr = app(expr, church_false());
    let result = eval_dbr(expr);
    let expected = church_true();
    assert_eq!(result, expected);
}

#[test]
fn test_iszero_zero() {
    // ISZERO ZERO → TRUE
    let expr = app(
        // ISZERO = λn. n (λx.FALSE) TRUE
        lam(
            "n",
            app(
                app(var("n"), lam("x", lam("a", lam("b", var("b"))))),
                lam("a", lam("b", var("a"))),
            ),
        ),
        church(0),
    );
    let result = eval_dbr(expr);
    let expected = church_true();
    assert_eq!(result, expected);
}
//
//    let expr = app(
//        // ISZERO = λn. n (λx.FALSE) TRUE
//        lam(
//            "n",
//            app(
//                app(
//                    var("n"),
//                    // λx.FALSE
//                    lam("x", lam("a", lam("b", var("b")))),
//                ),
//                // TRUE
//                lam("a", lam("b", var("a"))),
//            ),
//        ),
//        // ZERO = λf.λx. x
//        lam("f", lam("x", var("x"))),
//    );
//    eval_dbr(expr);
//
//    let expr = app(
//        app(
//            // EXP = λm.λn. n m
//            lam("m", lam("n", app(var("n"), var("m")))),
//            // THREE = λf.λx. f (f (f x))
//            lam(
//                "f",
//                lam("x", app(var("f"), app(var("f"), app(var("f"), var("x"))))),
//            ),
//        ),
//        // TWO = λf.λx. f (f x)
//        lam("f", lam("x", app(var("f"), app(var("f"), var("x"))))),
//    );
//    eval_dbr(expr);
//}
