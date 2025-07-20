use std::collections::HashMap;

use crate::term::{Type, TypeCtx, app, typed_eval_dbr, typed_lam, var};
use pretty_assertions::assert_eq;

fn any() -> Type {
    Type::Base("Any".into())
}

#[test]
fn id_fn_typed() {
    // we'll need two base types A and B
    let a_ty = Type::Base("A".into());
    let b_ty = Type::Base("B".into());

    // A → A and A → B
    let arrow_aa = Type::Arrow(Box::new(a_ty.clone()), Box::new(a_ty.clone()));
    let arrow_ab = Type::Arrow(Box::new(a_ty.clone()), Box::new(b_ty.clone()));

    // 1) (λx:A. x) a  ↦  a
    {
        let mut ctx: TypeCtx = HashMap::new();
        ctx.insert("a".into(), a_ty.clone());

        let id_a = typed_lam("x", var("x"), a_ty.clone());
        let expr = app(id_a, var("a"));
        assert_eq!(typed_eval_dbr(expr, &mut ctx), var("a"));
    }

    // 2) (λx:A→B. λy:A. x y) z  ↦  λy:A. z y
    {
        let mut ctx: TypeCtx = HashMap::new();
        // z must have type A→B
        ctx.insert("z".into(), arrow_ab.clone());

        let inner = typed_lam("y", app(var("x"), var("y")), a_ty.clone());
        let outer = typed_lam("x", inner, arrow_ab.clone());
        let expr = app(outer, var("z"));

        let expected = typed_lam("y", app(var("z"), var("y")), a_ty.clone());
        assert_eq!(typed_eval_dbr(expr, &mut ctx), expected);
    }

    // 3) ((λx:A→A. λy:A. x y) (λz:A. z)) a  ↦  a
    {
        let mut ctx: TypeCtx = HashMap::new();
        ctx.insert("a".into(), a_ty.clone());

        let id_a = typed_lam("z", var("z"), a_ty.clone());
        let inner = typed_lam("y", app(var("x"), var("y")), a_ty.clone());
        let outer = typed_lam("x", inner, arrow_aa.clone());

        let expr = app(app(outer, id_a), var("a"));
        assert_eq!(typed_eval_dbr(expr, &mut ctx), var("a"));
    }
}

#[test]
fn const_fn_typed() {
    // final result is `y`, so we only need y:Any in the context
    let mut ctx: TypeCtx = HashMap::new();
    ctx.insert("y".into(), any());

    // t = λx:Any. λy:Any. x
    let t = typed_lam("x", typed_lam("y", var("x"), any()), any());
    let t = app(t, var("y"));
    // t y z
    //let expr = app(app(t.clone(), var("y")), var("z"));

    // Should reduce to `y`
    assert_eq!(typed_eval_dbr(t, &mut ctx), var("y"));
}
