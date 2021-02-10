use std::fmt::{Display, Formatter, Result, Debug};
use std::ops::Deref;

use super::*;
use super::context::VarContext;

impl Var {
    pub fn print(&self, d: usize) -> String {
        match self {
            Var::Named(name) => name.clone(),
            Var::Anon => format!("_{}", d)
        }
    }
}

impl Term {
    pub fn print(&self, ctx: &CheckingContext) -> PrintTerm {
        PrintTerm(&self, ctx.to_var_context())
    }
    
    fn print1(&self, ctx: &VarContext) -> PrintTerm {
        PrintTerm(&self, ctx.clone())
    }
    
    fn print2(&self, ctx: VarContext) -> PrintTerm {
        PrintTerm(&self, ctx)
    }
}

pub struct PrintTerm<'a>(&'a Term, VarContext);

impl Display for PrintTerm<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let ctx = &self.1;
        let d = ctx.depth();
        match self.0 {
            Term::Var(d) => write!(f, "{}", ctx.lookup_var(*d).print(*d)),
            Term::Lam(x, T, t) => {
                write!(f, "(λ ")?;
                match x {
                    Arg::Exp(x) =>
                        if let Some(T) = T {
                            write!(f, "({} : {}) ", x.print(d), T.print1(ctx))?;
                        } else {
                            write!(f, "{} ", x.print(d))?;
                        }
                    Arg::Imp(x) => {
                        write!(f, "{{{}", x.print(d))?;
                        if let Some(T) = T {
                            write!(f, " : {}", T.print1(ctx))?;
                        }
                        write!(f, "}} ")?;
                    }
                }
                let mut ctx = ctx.clone();
                ctx.push_var(x.as_ref().unwrap());
                write!(f, "{})", t.print2(ctx))
            }
            Term::Pi(x, A, B) => {
                match x {
                    Arg::Exp(x) => if B.occurs_var(ctx.depth()) {
                        write!(f, "({} : {}) -> ", x.print(d), A.print1(ctx))?;
                    } else {
                        if let Term::Pi(..) = A.deref() {
                            write!(f, "({}) -> ", A.print1(ctx))?;
                        } else {
                            write!(f, "{} -> ", A.print1(ctx))?;
                        }
                    }
                    Arg::Imp(x) => write!(f, "{{{} : {}}} -> ", x.print(d), A.print1(ctx))?
                }
                
                let mut ctx = ctx.clone();
                ctx.push_var(x.as_ref().unwrap());
                write!(f, "{}", B.print2(ctx))
            }
            Term::App(s, t) => match t {
                Arg::Exp(t) => match t.deref() {
                    Term::App(..) => write!(f, "{} ({})", s.print1(ctx), t.print1(ctx)),
                    _ => write!(f, "{} {}", s.print1(ctx), t.print1(ctx))
                }
                Arg::Imp(t) => write!(f, "{} {{{}}}", s.print1(ctx), t.print1(ctx))
            }
            Term::Type => write!(f, "Type"),
            Term::Def(name) => write!(f, "{}", name),
            Term::Case(t, cases) => {
                write!(f, "(case {} of", t.print1(ctx))?;
                for (cname, vars, t) in cases {
                    write!(f, " | {} ", cname)?;
                    let mut ctx = ctx.clone();
                    for var in vars {
                        match var {
                            Arg::Exp(var) => write!(f, "{} ", var.print(ctx.depth()))?,
                            Arg::Imp(var) => write!(f, "{{{}}} ", var.print(ctx.depth()))?
                        }
                        ctx.push_var(var.as_ref().unwrap());
                    }
                    write!(f, "=> {}", t.print2(ctx))?;
                }
                write!(f, ")")
            }
            Term::Unknown => write!(f, "?"),
            Term::Meta(id) => write!(f, "__{}", id)
        }
    }
}

impl Debug for PrintTerm<'_> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        Display::fmt(self, f)
    }
}