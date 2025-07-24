use crate::term::{app, lam, var, eval_dbr};
use pretty_assertions::assert_eq;

#[test]
fn id_fn() {
    let expr = app(lam("x", var("x")), var("a"));
    assert_eq!(eval_dbr(expr), var("a"));

    let expr = app(lam("x", lam("y", app(var("x"), var("y")))), var("z"));
    assert_eq!(eval_dbr(expr), lam("y", app(var("z"), var("y"))));

    let expr = app(
        app(
            lam("x", lam("y", app(var("x"), var("y")))),
            lam("z", var("z")),
        ),
        var("a"),
    );
    assert_eq!(eval_dbr(expr), var("a"));
}

#[test]
fn const_fn() {
    let t = lam("x", lam("y", var("x")));
    let t_app = app(app(t.clone(), var("y")), var("z"));

    assert_eq!(eval_dbr(t_app), var("y"));
}

#[test]
fn test_apply_to_identity_yields_identity() {
    // (λx. x (λy.y)) (λz. z z) → λy.y
    let expr = app(
        lam("x", app(var("x"), lam("y", var("y")))),
        lam("z", app(var("z"), var("z"))),
    );
    let result = eval_dbr(expr);
    let expected = lam("y", var("y"));
    assert_eq!(result, expected);
}

#[test]
fn test_inner_identity_composition_yields_identity() {
    // (λx. (λx.x) (x x)) (λy.y) → λx.x
    let expr = app(
        lam("x", app(lam("x", var("x")), app(var("x"), var("x")))),
        lam("y", var("y")),
    );
    let result = eval_dbr(expr);
    let expected = lam("y", var("y"));
    assert_eq!(result, expected);
}

#[test]
fn test_nested_abstractions_then_application() {
    // (λa.λb. a (λc. b c))
    //   (λx. x x)
    //   (λy. y)
    // → λc. c
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
    let result = eval_dbr(expr);
    let expected = lam("c", var("c"));
    assert_eq!(result, expected);
}

#[test]
fn test_function_composition_structure() {
    // (((λf.λg.λx. f (g (g x))) inc) dbl) z
    // → inc (dbl (dbl z))
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
                var("inc"),
            ),
            var("dbl"),
        ),
        var("z"),
    );
    let result = eval_dbr(expr);
    let expected = app(var("inc"), app(var("dbl"), app(var("dbl"), var("z"))));
    assert_eq!(result, expected);
}

#[test]
fn test_abstraction_with_shadowing_and_application() {
    // (λx.λy. x (y y))
    //   (λa.λb. b a)
    //   (λc. c)
    // → λb. b (λc.c)
    let expr = app(
        app(
            lam("x", lam("y", app(var("x"), app(var("y"), var("y"))))),
            lam("a", lam("b", app(var("b"), var("a")))),
        ),
        lam("c", var("c")),
    );
    let result = eval_dbr(expr);
    let expected = lam("b", app(var("b"), lam("c", var("c"))));
    assert_eq!(result, expected);
}

#[test]
fn test_k_combinator_with_free_variable() {
    // (λx.λy. x) y → λx_0. y
    let expr = app(lam("x", lam("y", var("x"))), var("y"));
    let result = eval_dbr(expr);
    let expected = lam("x_0", var("y"));
    assert_eq!(result, expected);
}

#[test]
fn test_nested_shadowing_and_binding() {
    // ((λx.λy. x (λx. y x)) (λa.a)) (λb.b)
    // → λx. x
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
    let result = eval_dbr(expr);
    let expected = lam("x", var("x"));
    assert_eq!(result, expected);
}

#[test]
fn test_apply_function_to_identity_argument() {
    // (λf. f (λx.x))
    //   (λg.λh. h g)
    // → λh. h (λx.x)
    let expr = app(
        lam("f", app(var("f"), lam("x", var("x")))),
        lam("g", lam("h", app(var("h"), var("g")))),
    );
    let result = eval_dbr(expr);
    let expected = lam("h", app(var("h"), lam("x", var("x"))));
    assert_eq!(result, expected);
}

#[test]
fn test_double_shadowing_leading_to_self_app() {
    // ((λx.λy. x (λx. y x)) (λz.z)) (λw. w w)
    // → λx. x x
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
    let result = eval_dbr(expr);
    let expected = lam("x", app(var("x"), var("x")));
    assert_eq!(result, expected);
}

#[test]
fn test_flipped_application_then_reduce_to_identity() {
    // ((λx.λy. y (λz. x z)) (λu.u)) (λv.v)
    // → λz. z
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
    let result = eval_dbr(expr);
    let expected = lam("z", var("z"));
    assert_eq!(result, expected);
}

#[test]
fn test_application_inside_abstraction() {
    // (λx. (λy. x (λx. y x)) (λz.z)) (λw.w)
    // → λx. x
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
    let result = eval_dbr(expr);
    let expected = lam("x", var("x"));
    assert_eq!(result, expected);
}
#[test]
fn test_identity_app() {
    // (λx. x) (λy. y) → λy. y
    let identity = lam("x", var("x"));
    let argument = lam("y", var("y"));
    let expr = app(identity, argument);
    let result = eval_dbr(expr);
    let expected = lam("y", var("y"));
    assert_eq!(result, expected);
}
