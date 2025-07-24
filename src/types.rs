use crate::term::Term;
use std::{
    collections::{HashMap, HashSet},
    fmt::Display,
};

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
pub struct TypeScheme {
    pub forall: Vec<TypeVar>,
    pub body: Type,
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

pub type TypeEnv = HashMap<String, TypeScheme>;

#[derive(Default, Clone)]
struct TypeSubst(HashMap<TypeVar, Type>);

impl TypeSubst {
    fn singleton(tv: TypeVar, ty: Type) -> Self {
        Self([(tv, ty)].into())
    }

    fn apply(&self, ty: &Type) -> Type {
        match ty {
            Type::Var(tv) => self.0.get(tv).cloned().unwrap_or(Type::Var(*tv)),
            Type::Base(_) => ty.clone(),
            Type::Arrow(t1, t2) => Type::Arrow(Box::new(self.apply(t1)), Box::new(self.apply(t2))),
        }
    }

    fn apply_scheme(&self, scheme: &TypeScheme) -> TypeScheme {
        let filtered = TypeSubst(
            self.0
                .iter()
                .filter(|(tv, _)| !scheme.forall.contains(tv))
                .map(|(k, v)| (*k, v.clone()))
                .collect(),
        );
        TypeScheme {
            forall: scheme.forall.clone(),
            body: filtered.apply(&scheme.body),
        }
    }

    fn apply_env(&self, env: &TypeEnv) -> TypeEnv {
        env.iter()
            .map(|(k, v)| (k.clone(), self.apply_scheme(v)))
            .collect()
    }

    fn compose(self, other: TypeSubst) -> TypeSubst {
        let mut out = TypeSubst(
            self.0
                .into_iter()
                .map(|(k, v)| (k, other.apply(&v)))
                .collect(),
        );
        out.0.extend(other.0);
        out
    }
}

#[derive(Debug)]
pub enum TypeError {
    UnboundVariable(String),
    Mismatch(Type, Type),
}

impl Display for TypeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeError::UnboundVariable(name) => write!(f, "Unbound variable `{}`", name),
            TypeError::Mismatch(t1, t2) => {
                write!(f, "Type mismatch: expected `{}`, found `{}`", t1, t2)
            }
        }
    }
}

fn free_type_vars(ty: &Type) -> HashSet<TypeVar> {
    use Type::*;
    match ty {
        Var(tv) => [*tv].into(),
        Base(_) => HashSet::new(),
        Arrow(t1, t2) => {
            let mut vars = free_type_vars(t1);
            vars.extend(free_type_vars(t2));
            vars
        }
    }
}

fn ftv_scheme(sc: &TypeScheme) -> HashSet<TypeVar> {
    let mut s = free_type_vars(&sc.body);
    for q in &sc.forall {
        s.remove(q);
    }
    s
}

fn ftv_env(env: &TypeEnv) -> HashSet<TypeVar> {
    env.values().flat_map(ftv_scheme).collect()
}

fn bind(var: TypeVar, ty: Type) -> Result<TypeSubst, TypeError> {
    if ty == Type::Var(var) {
        return Ok(TypeSubst::default());
    } else if free_type_vars(&ty).contains(&var) {
        // this would mean an infite type
        Err(TypeError::Mismatch(Type::Var(var), ty))
    } else {
        Ok(TypeSubst::singleton(var, ty))
    }
}

fn unify(t1: &Type, t2: &Type) -> Result<TypeSubst, TypeError> {
    use Type::*;
    match (t1, t2) {
        (Var(tv1), Var(tv2)) if tv1 == tv2 => Ok(TypeSubst::default()),

        (Var(tv), _) => bind(*tv, t2.clone()),
        (_, Var(tv)) => bind(*tv, t1.clone()),

        (Base(a), Base(b)) if a == b => Ok(TypeSubst::default()),

        (Arrow(a1, b1), Arrow(a2, b2)) => {
            let s1 = unify(a1, a2)?;
            let s2 = unify(&s1.apply(b1), &s1.apply(b2))?;
            Ok(s1.compose(s2))
        }
        _ => Err(TypeError::Mismatch(t1.clone(), t2.clone())),
    }
}

fn generalise(env: &TypeEnv, ty: &Type) -> TypeScheme {
    let qs = free_type_vars(ty)
        .difference(&ftv_env(env))
        .cloned()
        .collect();

    TypeScheme {
        forall: qs,
        body: ty.clone(),
    }
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
                TypeSubst::default(), // No substitution needed for free variables
                scheme.instantiate(fresh),
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
        Term::App { func, arg } => {
            let (subst1, func_type) = infer_term_type(func, env, fresh)?;
            let (subst2, arg_type) = infer_term_type(arg, &subst1.apply_env(env), fresh)?;
            let type_var = Type::Var(fresh.fresh());
            let subst3 = unify(
                &subst2.apply(&func_type),
                &Type::Arrow(Box::new(arg_type), Box::new(type_var.clone())),
            )?;

            let subst = subst1.compose(subst2).compose(subst3);
            Ok((
                subst.clone(),
                subst.apply(&type_var), // Return the type of the application
            ))
        }
        Term::Bound(_) => unreachable!("Bound variables should not be present in a surface term."),
        Term::Let {
            name,
            value,
            in_term,
        } => {
            let (subst1, type1) = infer_term_type(value, env, fresh)?;

            let env1 = subst1.apply_env(env);
            let sigma = generalise(&env1, &type1);

            let mut env2 = env1.clone();
            env2.insert(name.clone(), sigma);

            let (subst2, type2) = infer_term_type(in_term, &env2, fresh)?;

            Ok((subst1.compose(subst2), type2))
        }
    }
}

/// Infers the type of a term in the Hindley-Milner type system.
/// Used only on terms that have no free variables.
pub fn infer(term: &Term) -> Result<Type, TypeError> {
    let mut fresh = Fresh::default();
    let (s, t) = infer_term_type(term, &TypeEnv::new(), &mut fresh)?;
    Ok(s.apply(&t))
}

/// Infers the type of a term in the Hindley-Milner type system,
/// taking into account a given type environment.
pub fn infer_with_env(term: &Term, env: &TypeEnv) -> Result<Type, TypeError> {
    let mut fresh = Fresh::default();
    let (s, t) = infer_term_type(term, env, &mut fresh)?;
    Ok(s.apply(&t))
}
