use crate::error::*;
use super::*;
use std::collections::HashSet;

impl Term {
    /// 检查此term是否是一个Sort，并返回改进（添加一些额外信息）后的term。
    pub fn check_sort(self, ctx: &mut CheckingContext) -> Result<Term> {
        match self {
            Term::Pi(x, A, B) => {
                let A = A.check_sort(ctx)?;
                ctx.push_var(x.as_ref().unwrap(), &A);
                let B = B.check_sort(ctx)?;
                ctx.pop_var();
                Ok(Term::Pi(x, A.into(), B.into()))
            }
            Term::Type => Ok(Term::Type),
            Term::Unknown => Err(Error::new(
                    ErrorKind::Typecheck,
                    format!("unknown hole founded\n\ncontext:\n{}-------\ngoal: Sort", ctx)
                )),
            Term::Meta(id) if ctx.lookup_meta(id) == &Meta::Sort => {
                Ok(Term::Meta(id))
            }
            _ => self.check_type(&Term::Type, ctx)
        }
    }

    /// 检查此term是否具有指定的类型T，并返回改进后的term。
    /// 假定T一定是一个Sort。
    pub fn check_type(self, T: &Term, ctx: &mut CheckingContext) -> Result<Term> {
        match &self {
            Term::Var(..) | Term::Pi(..) | Term::Def(..) | Term::App(..) => {
                let (t, T1) = self.infer_type(ctx)?;
                match (&T, &T1) {
                    (Term::Pi(Arg::Imp(_), ..), Term::Pi(Arg::Imp(_), ..)) => {}
                    (_, Term::Pi(Arg::Imp(_), A, ..)) =>
                        return Term::App(t.into(), Arg::Imp(ctx.add_meta_typed(A).into()))
                                .check_type(&T, ctx),
                    _ => {}
                }
                ctx.unify_meta(T, &T1)?;
                return Ok(t);
            }
            
            _ => {}
        }
        
        match (self, T) {
            (Term::Lam(Arg::Imp(x), S, t), Term::Pi(Arg::Imp(..), A, B)) => {
                if let Some(ref S) = S {
                    ctx.unify_meta(S, A)?;
                }
                ctx.push_var(&x, A);
                let t = t.check_type(B, ctx)?;
                ctx.pop_var();
                Ok(Term::Lam(Arg::Imp(x), Some(A.clone()), t.into()))
            }
            
            (t, Term::Pi(Arg::Imp(_), ..)) => {
                let t = Term::Lam(Arg::Imp(Var::Anon), None, t.lift_since(1, ctx.depth()).into());
                Ok(t.check_type(T, ctx)?)
            }
            
            (Term::Lam(Arg::Exp(x), S, t), Term::Pi(Arg::Exp(..), A, B)) => {
                if let Some(ref S) = S {
                    ctx.unify_meta(S, A)?;
                }
                ctx.push_var(&x, A);
                let t = t.check_type(B, ctx)?;
                ctx.pop_var();
                Ok(Term::Lam(Arg::Exp(x), Some(A.clone()), t.into()))
            }
            
            (Term::Case(s, cases), T) => {
                let (s, S) = s.infer_type(ctx)?;

                let splitted_var = if let Term::Var(ref d) = s {
                    Some(*d)
                } else {
                    None
                };

                if let Term::Def(dname) = S.caller() {
                    if let Some((_, case_defs)) = ctx.global.datas.get(dname) {
                        let mut coverage = HashSet::new();
                        let mut new_cases = Vec::new();
                        for (cname, cargs, t) in cases {
                            if coverage.contains(&cname) || !case_defs.contains_key(&cname) {
                                return Err(Error::new(
                                    ErrorKind::Context,
                                    format!("case {} already used or not exists", cname)
                                ))
                            }
                            coverage.insert(cname.clone());
                            let (mut new_ctx, cargs, t) = ctx.split_check(splitted_var, &S, &cname, &cargs, t)?;
                            let T = T.clone()
                                .lift_since(new_ctx.depth()-ctx.depth(), ctx.depth())
                                .eval(&mut new_ctx);
                            let t = t.check_type(&T, &mut new_ctx)?
                                ._resolve_meta(&mut new_ctx);
                            new_cases.push((cname, cargs, t));
                        }
                        for (cname, _) in case_defs {
                            if !coverage.contains(cname) {
                                return Err(Error::new(
                                    ErrorKind::Context,
                                    format!("can not coverage all cases for {}", dname)
                                ))
                            }
                        }
                        return Ok(Term::Case(s.into(), new_cases));
                    }
                }
                Err(Error::new(
                    ErrorKind::Typecheck,
                    format!("{} is not a data type, so it can't split", T.print(ctx))
                ))
            }
            
            (Term::Meta(id), T) => {
                match ctx.lookup_meta(id) {
                    Meta::Sort => ctx.unify_meta(T, &Term::Type)?,
                    Meta::Typed(T1) => ctx.unify_meta(T, &T1.clone())?
                }
                Ok(Term::Meta(id))
            }
            
            (Term::Unknown, T) => Err(Error::new(
                ErrorKind::Typecheck,
                format!("unknown hole founded\n\ncontext:\n{}-------\ngoal: {}", ctx, T.print(ctx))
            )),
            (t, T) => Err(Error::new(
                ErrorKind::Typecheck,
                format!("failed to check {} having a type {}\n\ncontext:\n{}-------", t.print(ctx), T.print(ctx), ctx)
            ))
        }
    }

    /// 推导该term的类型，返回改进后的term及其类型。
    pub fn infer_type(self, ctx: &mut CheckingContext) -> Result<(Term, Term)> {
        match self {
            Term::Var(d) => Ok((
                    Term::Var(d),
                    ctx.lookup_var_type(d).clone()
                        .lift_since(ctx.depth() - d, d)
                        .eval(ctx)
                )),
            Term::Lam(x, S, t) => {
                if let Some(S) = S {
                    let S = S.check_sort(ctx)?.eval(ctx);
                    ctx.push_var(x.as_ref().unwrap(), &S);
                    let (t, T) = t.infer_type(ctx)?;
                    ctx.pop_var();
                    Ok((
                        Term::Lam(x.clone(), Some(S.clone().into()), t.into()),
                        Term::Pi(x, S.into(), T.into())
                    ))
                } else {
                    Err(Error::new(
                        ErrorKind::Typecheck,
                        format!("failed to infer type for variable in lambda {}", Term::Lam(x, S, t).print(ctx))
                    ))
                }
            }
            Term::Pi(x, A, B) => {
                let A = A.check_type(&Term::Type, ctx)?;
                ctx.push_var(x.as_ref().unwrap(), &A);
                let B = B.check_type(&Term::Type, ctx)?;
                ctx.pop_var();
                Ok((Term::Pi(x, A.into(), B.into()), Term::Type))
            }
            Term::App(s, t) => {
                let (s, S) = s.infer_type(ctx)?;
                
                match (S, t) {
                    (Term::Pi(Arg::Exp(_), A, B), Arg::Exp(t)) => {
                        let t = t.check_type(&A, ctx)?;
                        let T = B.subst(ctx.depth(), &t);
                        Ok((Term::App(s.into(), Arg::Exp(t.into())), T))
                    }
                    (Term::Pi(Arg::Imp(_), A, B), Arg::Imp(t)) => {
                        let t = t.check_type(&A, ctx)?;
                        let T = B.subst(ctx.depth(), &t);
                        Ok((Term::App(s.into(), Arg::Imp(t.into())), T))
                    }
                    (Term::Pi(Arg::Imp(_), A, _), Arg::Exp(t)) => {
                        let s = Term::App(s.into(), Arg::Imp(ctx.add_meta_typed(&A).into()));
                        Ok(Term::App(s.into(), Arg::Exp(t)).infer_type(ctx)?)
                    }
                    
                    (Term::Pi(Arg::Exp(_), _, _), Arg::Imp(t)) => Err(Error::new(
                        ErrorKind::Typecheck,
                        format!("apply of {} on {} lose its implicit argument", s.print(ctx), t.print(ctx))
                    )),
                    (S, t) => Err(Error::new(
                        ErrorKind::Typecheck,
                        format!("{} has type {}, which is not a function and can not apply on {}", s.print(ctx), S.print(ctx), t.unwrap().print(ctx))
                    ))
                }
            }
            Term::Type => Err(Error::new(
                ErrorKind::Typecheck,
                "can't infer type of Type".into()
            )),
            Term::Def(name) => {
                let T = ctx.lookup_def_type(&name).map(|t| t.lift(ctx.depth()))
                    .ok_or(Error::new(
                        ErrorKind::Typecheck,
                        format!("definition of {} not found", name)))?;
                Ok((Term::Def(name), T))
            }
            Term::Case(..) => Err(Error::new(
                ErrorKind::Typecheck,
                "can't infer type of a case expression".into()
            )),
            Term::Unknown => Err(Error::new(
                ErrorKind::Typecheck,
                format!("unknown hole founded\n\ncontext:\n{}-------\ngoal: ?", ctx)
            )),
            Term::Meta(_) => panic!()
        }
    }
}