use super::*;

impl Term {
    pub fn eval(self, ctx: &mut CheckingContext) -> Term {
        match self {
            Term::Var(d) if !ctx.is_free_var(d) => {
                ctx.lookup_var_term(d).clone().eval(ctx)
            }
            Term::Pi(x, A, B) => {
                let A = A.eval(ctx);
                ctx.push_var(x.as_ref().unwrap(), &Term::Unknown);
                let B = B.eval(ctx);
                ctx.pop_var();
                Term::Pi(x, Box::new(A), Box::new(B))
            },
            Term::App(s, t) => {
                let s = s.eval(ctx);
                match s {
                    Term::Lam(_, _, s) => s.subst(ctx.depth(), &t.unwrap()).eval(ctx),
                    s => Term::App(Box::new(s), t.map(|t|Box::new(t.eval(ctx))))
                }
            }
            Term::Def(name) => {
                if let Some(t) = ctx.lookup_def_term(&name) {
                    t.clone().lift(ctx.depth())
                } else {
                    Term::Def(name)
                }
            }
            Term::Case(s, cases) => {
                let s = s.eval(ctx);
                if let Term::Def(cname) = s.caller() {
                    for (cname1, vars, mut t) in cases {
                        if &cname1 == cname {
                            let d = ctx.depth();
                            let args = s.call_args();
                            for var in &vars {
                                ctx.push_var(var.as_ref().unwrap(), &Term::Unknown);
                            }
                            for (i, arg) in args.iter().enumerate().rev() {
                                t = t.subst(i+d, arg.as_ref().unwrap());
                            }
                            for _ in &vars {
                                ctx.pop_var();
                            }
                            return t;
                        }
                    }
                    panic!()
                } else {
                    Term::Case(Box::new(s), cases)
                }
            }
            Term::Unknown => panic!(),
            t => t
        }
    }
}