use std::fmt::Display;

use crate::error::*;
use crate::context::Context;
use super::*;
use std::ops::Deref;

#[derive(Clone)]
pub struct VarContext(Vec<Var>);

impl VarContext {
    pub fn depth(&self) -> usize {
        self.0.len()
    }
    
    pub fn push_var(&mut self, var: &Var) {
        self.0.push(var.clone());
    }
    
    pub fn lookup_var(&self, d: usize) -> &Var {
        &self.0[d]
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Meta {
    Sort,
    Typed(Term)
}

#[derive(Clone)]
pub struct CheckingContext<'a> {
    local: Vec<(Var, Term, Option<Term>)>,
    let_defs: Vec<(String, Term, Term)>,
    meta: Vec<(Meta, Option<Term>)>,
    pub global: &'a Context
}

impl<'a> CheckingContext<'a> {
    pub fn new(global: &'a Context) -> Self {
        Self {
            local: Vec::new(),
            let_defs: Vec::new(),
            meta: Vec::new(),
            global
        }
    }
    
    pub fn depth(&self) -> usize {
        self.local.len()
    }
    
    pub fn push_var(&mut self, var: &Var, ty: &Term) {
        let var = renaming(var, self);
        self.local.push((var, ty.clone(), None));
    }
    
    pub fn pop_var(&mut self) {
        self.local.pop();
    }
    
    pub fn lookup_var(&self, d: usize) -> &Var {
        &self.local[d].0
    }
    
    pub fn lookup_var_type(&self, d: usize) -> &Term {
        &self.local[d].1
    }
    
    pub fn is_free_var(&self, d: usize) -> bool {
        self.local[d].2.is_none()
    }
    
    pub fn lookup_var_term(&self, d: usize) -> &Term {
        self.local[d].2.as_ref().unwrap()
    }
    
    pub fn make_bounded_var(&mut self, d: usize, t: Term) {
        self.local[d].2 = Some(t);
    }
    
    pub fn to_var_context(&self) -> VarContext {
        VarContext(
            self.local.iter().map(|(v, _, _)|v.clone()).collect()
        )
    }
    
    pub fn lookup_def_term(&self, name: &str) -> Option<&Term> {
        for (n, _, expr) in self.let_defs.iter().rev() {
            if n == name {
                return Some(expr);
            }
        }
        
        self.global.defs.get(name).map(|(_, expr)|expr)
    }

    pub fn lookup_def_type(&self, name: &str) -> Option<Term> {
        for (n, ty, _) in self.let_defs.iter().rev() {
            if n == name {
                return Some(ty.clone());
            }
        }

        for (n, (ty, cons)) in self.global.datas.iter() {
            if n == name {
                return Some(ty.clone());
            }
            for (n, ty) in cons {
                if n == name {
                    return Some(ty.clone());
                }
            }
        }

        self.global.temps.borrow().get(name)
            .or(self.global.defs.get(name).map(|(ty, _)|ty))
            .cloned()
    }
    
    /// 因case表达式导致的上下文分裂。用于分类讨论的term的类型为`splitT`，分类的构造器名为`cname`，分类构造器引入的变量为`cargs`。
    /// 这会引入新的变量，并根据构造器的类型定义限制原有的变量。
    /// 如果用于分类讨论的是一个变量，由`splitx`指示，那么该变量也会被限制。
    pub fn split_check(&self, splitx: Option<usize>, splitT: &Term, cname: &str, cargs: &Vec<Arg<Var>>, mut t: Term) -> Result<(Self, Vec<Arg<Var>>, Term)> {
        let mut ctx = self.clone();
        let d = ctx.depth();
        
        let (_, cons) = Term::Def(cname.to_string()).infer_type(&mut ctx)?;
        let args_types = cons.arg_types();

        let mut cargs_it = cargs.iter().peekable();
        let mut arg_vars = Vec::new();
        for arg_type in args_types.iter() {
            match arg_type {
                Arg::Exp(_) => match cargs_it.next() {
                    Some(Arg::Exp(arg)) => arg_vars.push(Arg::Exp(arg.clone())),
                    _ => return Err(Error::new(
                        ErrorKind::Typecheck,
                        format!("expected an explicit argument in case {}", cname)
                    ))
                }
                Arg::Imp(_) => match cargs_it.peek() {
                    Some(Arg::Imp(arg)) => {
                        cargs_it.next();
                        arg_vars.push(Arg::Imp(arg.clone()))
                    },
                    _ => {
                        t = t.lift_since(1, ctx.depth());
                        arg_vars.push(Arg::Imp(Var::Anon));
                    }
                }
            }
        }
        if cargs_it.next().is_some() {
            return Err(Error::new(
                ErrorKind::Typecheck,
                format!("expected no more arguments for case {}", cname)
            ));
        }
        
        for (arg, argT) in arg_vars.iter().zip(args_types.into_iter()) {
            let argT = argT.unwrap().clone().check_sort(&mut ctx)?.eval(&mut ctx);
            ctx.push_var(arg.as_ref().unwrap(), &argT);
        }
        
        let (t1, T1) = Term::call(
            Term::Def(cname.to_string()),
            arg_vars.iter().enumerate().map(
                |(i, arg)|arg.as_ref().map(|_|Term::Var(d+i))
            ).collect()
            // (0..cargs.len()).map(|i|cargs[i].as_ref().map(|_|Term::Var(d+i))).collect()
            // (d..d+cargs.len()).map(|v|Term::Var(v)).collect()
        ).infer_type(&mut ctx)?;
        ctx.unify_split(d, &splitT, &T1)?;
        if let Some(var) = splitx {
            ctx.make_bounded_var(var, t1);
        }
        Ok((ctx, arg_vars, t))
    }
    
    fn unify_split(&mut self, d: usize, s: &Term, t: &Term) -> Result<()> {
        match (s, t) {
            (Term::Var(d1), _) if d1 < &d => match t {
                _ if !self.is_free_var(*d1) =>
                    return self.unify_split(d, &self.lookup_var_term(*d1).clone(), t),
                Term::Var(d2) if d2 < &d => {}
                _ => return Ok(self.make_bounded_var(*d1, t.clone()))
            }
            (_, Term::Var(d2)) if d2 < &d => match s {
                _ if !self.is_free_var(*d2) =>
                    return self.unify_split(d, s, &self.lookup_var_term(*d2).clone()),
                Term::Var(d1) if d1 < &d => {}
                _ => return Ok(self.make_bounded_var(*d2, s.clone()))
            }
            
            (Term::Lam(_, _, s), Term::Lam(_, _, t)) => {
                self.unify_split(d, s, t)?;
                return Ok(());
            },
            (Term::Pi(_, A, B), Term::Pi(_, C, D)) => {
                self.unify_split(d, A, C)?;
                self.unify_split(d, B, D)?;
                return Ok(());
            }
            (Term::App(s1, Arg::Exp(t1)), Term::App(s2, Arg::Exp(t2)))
                | (Term::App(s1, Arg::Imp(t1)), Term::App(s2, Arg::Imp(t2)))  => {
                self.unify_split(d, s1, s2)?;
                self.unify_split(d, t1, t2)?;
                return Ok(());
            }
            (Term::Type, Term::Type) => return Ok(()),
            (Term::Def(n1), Term::Def(n2)) if n1 == n2 => return Ok(()),
            (Term::Meta(id), _) if self.meta[*id].1.is_some() =>
                return self.unify_split(d, &self.meta[*id].1.clone().unwrap(), t),
            (_, Term::Meta(id)) if self.meta[*id].1.is_some() =>
                return self.unify_split(d, s, &self.meta[*id].1.clone().unwrap()),
            _ => {}
        }
        
        Err(Error::new(
            ErrorKind::Typecheck,
            format!("failed to unify {} and {} in case splitting\n{}", s.print(self), t.print(self), self)
        ))
    }
    
    pub fn add_meta_sort(&mut self) -> Term {
        self.meta.push((Meta::Sort, None));
        Term::Meta(self.meta.len()-1)
    }
    
    pub fn add_meta_typed(&mut self, T: &Term) -> Term {
        self.meta.push((Meta::Typed(T.clone()), None));
        Term::Meta(self.meta.len()-1)
    }
    
    pub fn lookup_meta(&self, id: usize) -> &Meta {
        &self.meta[id].0
    }
    
    pub fn unify_meta(&mut self, s: &Term, t: &Term) -> Result<()> {
        let s = s.clone()._resolve_meta(self).eval(self);
        let t = t.clone()._resolve_meta(self).eval(self);
        self._unify_meta(&s, &t)
    }
    
    fn _unify_meta(&mut self, s: &Term, t: &Term) -> Result<()> {
        match (s, t) {
            (Term::Meta(id), _) if self.meta[*id].1.is_some() =>
                self._unify_meta(&self.meta[*id].1.clone().unwrap(), t),
            (_, Term::Meta(id)) if self.meta[*id].1.is_some() =>
                self._unify_meta(s, &self.meta[*id].1.clone().unwrap()),
            
            (Term::Meta(id1), Term::Meta(id2)) if id1 == id2 => Ok(()),
            
            (Term::Meta(id), _) if !t.occurs_meta(*id, self) => {
                // let t = match &self.meta[*id].0 {
                //     Meta::Sort => t.clone().check_sort(self)?,
                //     Meta::Typed(T) => t.clone().check_type(&T.clone(), self)?
                // };
                self.meta[*id].1 = Some(t.clone());
                Ok(())
            }
            (_, Term::Meta(id)) if !s.occurs_meta(*id, self) => {
                // let s = match &self.meta[*id].0 {
                //     Meta::Sort => s.clone().check_sort(self)?,
                //     Meta::Typed(T) => s.clone().check_type(&T.clone(), self)?
                // };
                self.meta[*id].1 = Some(s.clone());
                Ok(())
            }
            
            (Term::Var(d1), Term::Var(d2)) if d1 == d2 => Ok(()),
            (Term::Lam(x, S, s), Term::Lam(_, T, t)) => {
                match (S, T) {
                    (Some(S), Some(T)) => {
                        self.unify_meta(S, T)?;
                        self.push_var(x.as_ref().unwrap(), S);
                    }
                    (Some(S), None) | (None, Some(S)) =>
                        self.push_var(x.as_ref().unwrap(), S),
                    (None, None) => self.push_var(x.as_ref().unwrap(), &Term::Unknown)
                }
                self._unify_meta(s, t)?;
                self.pop_var();
                Ok(())
            },
            (Term::Pi(x, A, B), Term::Pi(_, C, D)) => {
                self._unify_meta(A, C)?;
                self.push_var(x.as_ref().unwrap(), A);
                self._unify_meta(B, D)?;
                self.pop_var();
                Ok(())
            }
            (Term::App(s1, Arg::Exp(t1)), Term::App(s2, Arg::Exp(t2)))
                |  (Term::App(s1, Arg::Imp(t1)), Term::App(s2, Arg::Imp(t2))) => {
                self._unify_meta(s1, s2)?;
                self._unify_meta(t1, t2)?;
                Ok(())
            }
            (Term::Type, Term::Type) => Ok(()),
            (Term::Def(n1), Term::Def(n2)) if n1 == n2 => Ok(()),
            (Term::Case(s, cases1), Term::Case(t, cases2)) if cases1.len() == cases2.len() => {
                self._unify_meta(s, t)?;
                for (case1, case2) in cases1.iter().zip(cases2.iter()) {
                    if case1.0 != case2.0 {
                        return Err(Error::new(
                            ErrorKind::Typecheck,
                            format!("failed to unify {} and {}\n{}", s.print(self), t.print(self), self)
                        ));
                    }
                    for arg in &case1.1 {
                        self.push_var(arg.as_ref().unwrap(), &Term::Unknown);
                    }
                    self._unify_meta(&case1.2, &case2.2)?;
                    for _ in &case1.1 {
                        self.pop_var();
                    }
                }
                Ok(())
            }
            _ => Err(Error::new(
                ErrorKind::Typecheck,
                format!("failed to unify {} and {}\n{}", s.print(self), t.print(self), self)
            ))
        }
    }
}

impl Term {
    pub fn resolve_meta(self, ctx: &CheckingContext) -> Result<Term> {
        for (i, (_, t)) in ctx.meta.iter().enumerate() {
            if t.is_none() {
                return Err(Error::new(
                    ErrorKind::Typecheck,
                    format!("unresolved meta variable __{}", i)
                ))
            }
        }
        Ok(self._resolve_meta(ctx))
    }
    
    pub fn _resolve_meta(self, ctx: &CheckingContext) -> Term {
        match self {
            Term::Lam(x, T, t) =>
                Term::Lam(x, T.map(|T|T._resolve_meta(ctx).into()), t._resolve_meta(ctx).into()),
            Term::Pi(x, A, B) =>
                Term::Pi(x, A._resolve_meta(ctx).into(), B._resolve_meta(ctx).into()),
            Term::App(s, t) =>
                Term::App(s._resolve_meta(ctx).into(), t.map(|t|t._resolve_meta(ctx).into())),
            Term::Case(t, cases) =>
                Term::Case(
                    t._resolve_meta(ctx).into(),
                    cases.into_iter().map(|(cname, cargs, t)|{
                        (cname, cargs, t._resolve_meta(ctx))
                    }).collect()
                ),
            Term::Meta(id) if ctx.meta[id].1.is_some() => ctx.meta[id].1.clone().unwrap()._resolve_meta(ctx),
            t => t
        }
    }


    pub fn occurs_meta(&self, id: usize, ctx: &CheckingContext) -> bool {
        match self {
            Term::Lam(_, T, t) => {
                if let Some(T) = T.deref() {
                    if T.occurs_meta(id, ctx) {
                        return true;
                    }
                }
                t.occurs_meta(id, ctx)
            }
            Term::Pi(_, A, B) => A.occurs_meta(id, ctx) || B.occurs_meta(id, ctx),
            Term::App(s, t) => s.occurs_meta(id, ctx) || t.as_ref().unwrap().occurs_meta(id, ctx),
            Term::Case(s, cases) => {
                if s.occurs_meta(id, ctx) {
                    return true;
                }
                for (_, _, t) in cases {
                    if t.occurs_meta(id, ctx) {
                        return true;
                    }
                }
                false
            }
            Term::Meta(id1) => id1 == &id,
            _ => false
        }
    }
}

impl Display for CheckingContext<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, (x, T, t)) in self.local.iter().enumerate() {
            match x {
                Var::Named(name) => write!(f, "{}", name)?,
                Var::Anon => write!(f, "_{}", i)?
            }
            write!(f, " : {}", T.print(self))?;
            if let Some(t) = t {
                write!(f, " = {}", t.print(self))?;
            }
            writeln!(f, "")?;
        }
        Ok(())
    }
}

fn renaming(var: &Var, ctx: &CheckingContext) -> Var {
    match var {
        Var::Named(ref name) => {
            let mut name = name.clone();
            loop {
                let mut occurs = false;
                for d in 0..ctx.depth() {
                    if let Var::Named(name1) = ctx.lookup_var(d) {
                        if name1 == &name {
                            occurs = true;
                            break;
                        }
                    }
                }
                if occurs {
                    name.push('\'');
                } else {
                    break;
                }
            }
            Var::Named(name)
        }
        Var::Anon => Var::Anon,
    }
}