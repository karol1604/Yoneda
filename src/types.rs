use crate::term::Term;
use std::{collections::HashMap, fmt::Display};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Type {
    Var(TypeVar),                // type meta variable
    Base(String),                // atomic type
    Arrow(Box<Type>, Box<Type>), // T1 -> T2
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Var(tv) => write!(f, "{}", tv),
            Type::Base(name) => write!(f, "{}", name),
            Type::Arrow(t1, t2) => write!(f, "({} -> {})", t1, t2),
        }
    }
}

/// Tiny wrapper around a type variable index.
#[derive(Default)]
struct Fresh(usize);
impl Fresh {
    fn fresh(&mut self) -> TypeVar {
        let v = self.0;
        self.0 += 1;
        TypeVar(v)
    }
}

/// Placeholder for an unknown type.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Copy)]
pub struct TypeVar(usize);

impl Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut n = self.0;
        let mut res = String::new();
        loop {
            res.push((b'a' + (n % 26) as u8) as char);
            n /= 26;
            if n == 0 {
                break;
            }
            n -= 1;
        }
        write!(f, "'{}", res)
    }
}

/// ∀ α . Type
/// Represents a type scheme that can be instantiated with type variables.
#[derive(Debug, Clone, PartialEq, Eq)]
struct TypeScheme {
    forall: Vec<TypeVar>,
    body: Type,
}

impl TypeScheme {
    fn instantiate(&self, fresh: &mut Fresh) -> Type {
        let mut subst = TypeSubst::default();
        for &type_var in &self.forall {
            subst.0.insert(type_var, Type::Var(fresh.fresh()));
        }
        subst.apply(&self.body)
    }
}

type TypeEnv = HashMap<String, TypeScheme>;

#[derive(Default, Clone)]
struct TypeSubst(HashMap<TypeVar, Type>);

impl TypeSubst {
    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(tv) => self.0.get(tv).cloned().unwrap_or(Type::Var(*tv)),
            Type::Base(_) => ty.clone(),
            Type::Arrow(t1, t2) => Type::Arrow(Box::new(self.apply(t1)), Box::new(self.apply(t2))),
        }
    }
}

pub enum TypeError {
    UnboundVariable(String),
    UnificationFailed(Type, Type),
}

fn infer_term_type(
    term: &Term,
    env: &TypeEnv,
    fresh: &mut Fresh,
) -> Result<(TypeSubst, Type), TypeError> {
    match term {
        Term::Free(name) => {
            let scheme = env
                .get(name)
                .ok_or(TypeError::UnboundVariable(name.clone()))?;
            Ok((
                TypeSubst::default(),      // No substitution needed for free variables
                scheme.instantiate(fresh), // Return the body of the type scheme
            ))
        }
        Term::Lam { name, body, .. } => {
            let tv = Type::Var(fresh.fresh());
            let mut new_env = env.clone();
            new_env.insert(
                name.clone(),
                TypeScheme {
                    forall: vec![],
                    body: tv.clone(),
                },
            );

            let (subst, body_type) = infer_term_type(body, &new_env, fresh)?;
            Ok((
                subst.clone(),
                Type::Arrow(Box::new(subst.apply(&tv)), Box::new(body_type)),
            ))
        }
        Term::App { func, arg } => todo!(),
        Term::Bound(_) => unreachable!("Bound variables should not be present in a surface term."),
    }
}

pub fn infer(term: &Term) -> Result<Type, TypeError> {
    let mut fresh = Fresh::default();
    let (s, t) = infer_term_type(term, &TypeEnv::new(), &mut fresh)?;
    Ok(s.apply(&t)) // Placeholder for the actual type
}
