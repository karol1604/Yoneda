use crate::{
    term::{app, lam, let_in, var},
    types::{Type, TypeEnv, TypeError, TypeScheme, infer, infer_with_env},
};

fn assert_same_var_arrow(ty: &Type) {
    if let Type::Arrow(p, r) = ty {
        assert_eq!(
            p.as_ref(),
            r.as_ref(),
            "param and result should be the same type variable"
        );
        assert!(
            matches!(p.as_ref(), Type::Var(_)),
            "param is not a type variable"
        );
    } else {
        panic!("expected arrow type, got {ty:?}");
    }
}

fn prelude_with_int(val: &str) -> TypeEnv {
    let mut env = TypeEnv::new();
    env.insert(
        val.into(),
        TypeScheme {
            forall: vec![],
            body: Type::Base("Int".into()),
        },
    );
    env
}

#[test]
fn identity() {
    // λx. x
    let id = lam("x", var("x"));
    let ty = infer(&id).expect("must type-check");
    assert_same_var_arrow(&ty);
}

#[test]
fn const_function() {
    // λx. λy. x
    let const_fn = lam("x", lam("y", var("x")));
    let ty = infer(&const_fn).expect("must type-check");

    // should be  α -> β -> α
    if let Type::Arrow(p, res) = ty {
        assert!(matches!(*p, Type::Var(_)));
        if let Type::Arrow(p2, r2) = *res {
            assert!(matches!(*p2, Type::Var(_)));
            assert_eq!(*p, *r2, "outer param and final result must match");
            assert_ne!(
                *p, *p2,
                "inner variable must be distinct from the outer one"
            );
        } else {
            panic!("second arrow missing");
        }
    } else {
        panic!("not an arrow");
    }
}

#[test]
fn let_polymorphism() {
    // let id = λx.x in λz. id z
    let term = let_in("id", lam("x", var("x")), lam("z", app(var("id"), var("z"))));

    let ty = infer(&term).expect("should type-check");
    assert_same_var_arrow(&ty); // 'a -> 'a
}

#[test]
fn instantiate_to_int() {
    // let id = λx.x in id 3     (3 : Int)
    let term = let_in("id", lam("x", var("x")), app(var("id"), var("3")));

    let env = prelude_with_int("3");
    let ty = infer_with_env(&term, &env).expect("type-checks");
    assert_eq!(ty, Type::Base("Int".into()));
}

#[test]
fn unbound_variable_error() {
    let term = var("ghost"); // never defined
    let err = infer(&term).unwrap_err();
    matches!(err, TypeError::UnboundVariable(_));
}

#[test]
fn occurs_check_error() {
    // λx. x x     – classic Ω; should fail (α = α → β loop)
    let omega = lam("x", app(var("x"), var("x")));
    let err = infer(&omega).unwrap_err();
    // any error is okay, but occurs-check is the usual one
    matches!(err, TypeError::Mismatch { .. });
}
