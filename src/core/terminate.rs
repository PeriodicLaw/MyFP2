use super::*;
use crate::error::*;
use std::ops::Deref;

impl Term {
    pub fn check_terminate(&self, fname: &str, ctx: &mut CheckingContext, vars: &mut Vec<usize>) -> Result<()> {
        match self {
            Term::Var(..) | Term::Type | Term::Unknown | Term::Meta(..) => Ok(()),
            Term::Lam(x, T, t) => {
                vars.push(ctx.depth());
                ctx.push_var(x.as_ref().unwrap(), T.as_ref().unwrap());
                t.check_terminate(fname, ctx, vars)?;
                vars.pop();
                ctx.pop_var();
                Ok(())
            }
            Term::Pi(x, s, t) => {
                s.check_terminate(fname, ctx, vars)?;
                vars.push(ctx.depth());
                ctx.push_var(x.as_ref().unwrap(), s);
                t.check_terminate(fname, ctx, vars)?;
                vars.pop();
                ctx.pop_var();
                Ok(())
            }
            Term::App(..) | Term::Def(..) => {
                let caller = self.caller();
                let args = self.call_args();
                if let Term::Def(name) = caller {
                    if name == fname {
                        if args.len() == vars.len() {
                            let vars: Vec<_> = vars.into_iter().map(|d|Term::Var(*d).eval(ctx)).collect();
                            
                            for (arg, var) in args.iter().zip(vars.iter()) {
                                if is_subterm(arg.as_ref().unwrap(), var, true) {
                                    return Ok(());
                                }
                            }
                            
                            return Err(Error::new(
                                ErrorKind::TerminCheck,
                                format!("{:?} should be an strict subterm for {:?}",
                                        args.iter().map(|arg|arg.as_ref().unwrap().print(ctx)).collect::<Vec<_>>(),
                                        vars.iter().map(|var|var.print(ctx)).collect::<Vec<_>>()
                                )
                            ));
                        } else {
                            return Err(Error::new(
                                ErrorKind::TerminCheck,
                                "partial application of recursive definition is not allowed".into()
                            ));
                        }
                    }
                }
                if let Term::Def(..) = caller {
                    
                } else {
                    caller.check_terminate(fname, ctx, vars)?;
                }
                for callee in args {
                    callee.unwrap().check_terminate(fname, ctx, vars)?;
                }
                Ok(())
            }
            Term::Case(s, cases) => {
                let splitted_var = if let Term::Var(d) = s.deref() {
                    Some(*d)
                } else {
                    None
                };
                s.check_terminate(fname, ctx, vars)?;
                for (cname, cargs, t) in cases {
                    let (_, T) = s.clone().infer_type(ctx)?;
                    let (mut new_ctx, _, t) = ctx.split_check(splitted_var, &T, cname, cargs, t.clone())?;
                    t.check_terminate(fname, &mut new_ctx, vars)?;
                }
                Ok(())
            }
        }
    }
}

fn is_subterm(s: &Term, t: &Term, strict: bool) -> bool {
    if s == t && !strict {
        return true;
    }
    
    if let Term::App(..) = t {
        if let Term::Def(..) = t.caller() {
            for arg in t.call_args() {
                if is_subterm(s, arg.unwrap(), false) {
                    return true;
                }
            }
        }
    }
    false
}