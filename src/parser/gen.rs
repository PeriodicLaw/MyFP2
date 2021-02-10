use super::expr::*;
use crate::core::*;
use crate::error::*;
use std::ops::Deref;
use crate::parser::stmt::SyntaxDef;
use std::collections::HashMap;

pub fn gen_term(expr: Expr, vars: &mut Vec<Option<String>>, ctx: &mut CheckingContext) -> Result<Term> {
    // println!("gen {:?} vars {:?}", expr, vars);
    match expr {
        Expr::Named(name) => {
            for (i, n) in vars.iter().enumerate().rev() {
                if let Some(n) = n {
                    if n == &name {
                        // println!("found var {}", i);
                        return Ok(Term::Var(i));
                    }
                }
            }
            Ok(Term::Def(name))
        }
        Expr::Func(names, tyA, tyB) => {
            let names = names
                .map(|names|names.into_iter().map(|name|Option::Some(name)).collect())
                .unwrap_or(vec![None]);
            for name in &names {
                vars.push(name.clone());
            }
            let mut t = gen_term(*tyB, vars, ctx)?;
            for name in names.iter().rev() {
                let var = if let Some(name) = name {Var::Named(name.clone())} else {Var::Anon};
                vars.pop();
                let A = gen_term(*tyA.clone(), vars, ctx)?;
                t = Term::Pi(Arg::Exp(var), Box::new(A), Box::new(t));
            }
            Ok(t)
        }
        Expr::ImpFunc(names, tyA, tyB) => {
            let names: Vec<_> = names.into_iter().map(|name|Option::Some(name)).collect();
            for name in &names {
                vars.push(name.clone());
            }
            let mut t = gen_term(*tyB, vars, ctx)?;
            for name in names.iter().rev() {
                let var = if let Some(name) = name {Var::Named(name.clone())} else {Var::Anon};
                vars.pop();
                let A = if let Some(tyA) = tyA.deref() {
                    gen_term(tyA.clone(), vars, ctx)?
                } else {
                    ctx.add_meta_sort()
                };
                t = Term::Pi(Arg::Imp(var), Box::new(A), Box::new(t));
            }
            Ok(t)
        }
        Expr::Type => Ok(Term::Type),
        Expr::Lam(name, ty, expr) => {
            let var = Var::Named(name.clone());
            let T = if let Some(ty) = *ty {
                Some(Box::new(gen_term(ty, vars, ctx)?))
            } else {
                None
            };
            vars.push(Some(name));
            let t = gen_term(*expr, vars, ctx)?;
            vars.pop();
            Ok(Term::Lam(Arg::Exp(var), T, Box::new(t)))
        }
        Expr::ImpLam(name, ty, expr) => {
            let var = Var::Named(name.clone());
            let T = if let Some(ty) = *ty {
                Some(Box::new(gen_term(ty, vars, ctx)?))
            } else {
                None
            };
            vars.push(Some(name));
            let t = gen_term(*expr, vars, ctx)?;
            vars.pop();
            Ok(Term::Lam(Arg::Imp(var), T, Box::new(t)))
        }
        Expr::App(expr0, expr1) => {
            let s = gen_term(*expr0, vars, ctx)?;
            let t = gen_term(*expr1, vars, ctx)?;
            Ok(Term::App(Box::new(s), Arg::Exp(Box::new(t))))
        }
        Expr::ImpApp(expr0, expr1) => {
            let s = gen_term(*expr0, vars, ctx)?;
            let t = gen_term(*expr1, vars, ctx)?;
            Ok(Term::App(Box::new(s), Arg::Imp(Box::new(t))))
        }
        // Expr::LetDef(name, ty, expr0, expr1) => {
        //     let S = if let Some(ty) = *ty {
        //         Some(Box::new(gen_term(ty, vars, ctx)?))
        //     } else {
        //         None
        //     };
        //     let s = gen_term(*expr0, vars, ctx)?;
        //     let t = gen_term(*expr1, vars, ctx)?;
        //     Ok(Term::LetIn(name, S, Box::new(s), Box::new(t)))
        // }
        Expr::Unknown => Ok(Term::Unknown),
        Expr::Case(expr, case_exprs) => {
            let t = gen_term(*expr, vars, ctx)?;
            let mut cases = Vec::new();
            for (cname, cvars, expr) in case_exprs {
                let l = cvars.len();
                vars.append(&mut cvars.iter().map(|(var, _)|Some(var.clone())).collect());
                cases.push((
                    cname,
                    cvars.into_iter().map(|(var, exp)|{
                        if exp {
                            Arg::Exp(Var::Named(var))
                        } else {
                            Arg::Imp(Var::Named(var))
                        }
                    }).collect(),
                    gen_term(expr, vars, ctx)?
                ));
                for _ in 0..l {
                    vars.pop();
                }
            }
            Ok(Term::Case(Box::new(t), cases))
        }
        Expr::Syntax(exprs) => {
            for (def, expr) in &ctx.global.syntax {
                if let Some(expr) = (||{
                    let mut mapping = HashMap::new();
                    for (def, expr) in def.iter().zip(exprs.iter()) {
                        match (def, expr) {
                            (SyntaxDef::Name(defname), SyntaxExpr::Expr(expr)) if !mapping.contains_key(defname) => {
                                mapping.insert(defname.clone(), expr.clone());
                            }
                            (SyntaxDef::Symbol(defsym), SyntaxExpr::Symbol(sym)) if defsym == sym => {}
                            _ => return None
                        }
                    }
                    Some(expr.clone().subst(&mapping))
                })() {
                    return gen_term(expr, vars, ctx);
                }
            }
            Err(Error::new(
                ErrorKind::Parser,
                "no corresponding syntax found".into()
            ))
        }
    }
}

impl Expr {
    fn subst(self, mapping: &HashMap<String, Expr>) -> Expr {
        match self {
            Expr::Named(name) => match mapping.get(&name) {
                Some(expr) => expr.clone(),
                None => Expr::Named(name)
            }
            Expr::Func(x, ty0, ty1) =>
                Expr::Func(x, ty0.subst(mapping).into(), ty1.subst(mapping).into()),
            Expr::ImpFunc(x, ty0, ty1) =>
                Expr::ImpFunc(x, ty0.map(|ty|ty.subst(mapping)).into(), ty1.subst(mapping).into()),
            Expr::Lam(x, ty, expr) =>
                Expr::Lam(x, ty.map(|ty|ty.subst(mapping)).into(), expr.subst(mapping).into()),
            Expr::ImpLam(x, ty, expr) =>
                Expr::ImpLam(x, ty.map(|ty|ty.subst(mapping)).into(), expr.subst(mapping).into()),
            Expr::App(expr0, expr1) =>
                Expr::App(expr0.subst(mapping).into(), expr1.subst(mapping).into()),
            Expr::ImpApp(expr0, expr1) =>
                Expr::ImpApp(expr0.subst(mapping).into(), expr1.subst(mapping).into()),
            Expr::Case(expr, cases) => Expr::Case(
                expr.subst(mapping).into(),
                cases.into_iter().map(|(cname, cargs, expr)|{
                    (cname, cargs, expr.subst(mapping).into())
                }).collect()
            ),
            Expr::Syntax(exprs) => Expr::Syntax(exprs.into_iter().map(|expr|{
                match expr {
                    SyntaxExpr::Symbol(sym) => SyntaxExpr::Symbol(sym),
                    SyntaxExpr::Expr(expr) => SyntaxExpr::Expr(expr.subst(mapping))
                }
            }).collect()),
            t => t
        }
    }
}
