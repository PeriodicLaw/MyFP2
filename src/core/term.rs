use std::ops::Deref;

#[derive(Clone, PartialEq, Debug)]
pub enum Var {
    Named(String),
    Anon
}

#[derive(Clone, PartialEq, Debug)]
pub enum Arg<T> {
    Exp(T),
    Imp(T)
}

impl<T> Arg<T> {
    pub fn as_ref(&self) -> Arg<&T> {
        match self {
            Arg::Exp(a) => Arg::Exp(a),
            Arg::Imp(a) => Arg::Imp(a)
        }
    }
    
    pub fn unwrap(self) -> T {
        match self {
            Arg::Exp(a) => a,
            Arg::Imp(a) => a
        }
    }
    
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Arg<U> {
        match self {
            Arg::Exp(a) => Arg::Exp(f(a)),
            Arg::Imp(a) => Arg::Imp(f(a))
        }
    }
}

#[derive(Clone, PartialEq, Debug)]
pub enum Term {
    Var(usize),
    Lam(Arg<Var>, Option<Box<Term>>, Box<Term>),
    Pi(Arg<Var>, Box<Term>, Box<Term>),
    App(Box<Term>, Arg<Box<Term>>),
    Type,
    Def(String),
    Case(Box<Term>, Vec<(String, Vec<Arg<Var>>, Term)>),
    Unknown,
    Meta(usize)
}

impl Term {
    /// 检查变量是否在term中出现。
    pub fn occurs_var(&self, var: usize) -> bool {
        match self {
            Term::Var(v) => v == &var,
            Term::Lam(_, T, t) => {
                if let Some(ref T) = *T {
                    if T.occurs_var(var) {
                        return true;
                    }
                }
                t.occurs_var(var)
            }
            Term::Pi(_, A, B) => A.occurs_var(var) || B.occurs_var(var),
            Term::App(s, t) => s.occurs_var(var) || t.as_ref().unwrap().occurs_var(var),
            Term::Case(s, cases) => {
                if s.occurs_var(var) {
                    return true;
                }
                for (_, _, t) in cases {
                    if t.occurs_var(var) {
                        return true;
                    }
                }
                false
            }
            _ => false
        }
    }
    
    /// 将term中出现的`d`层自由变量替换为`t`，并消去这一层自由变量。
    /// 参数应保证`self`至少`d`层。
    pub fn subst(self, d: usize, t: &Term) -> Term {
        match self {
            Term::Var(d1) => if d1 == d {
                t.clone()
            } else if d1 < d {
                Term::Var(d1)
            } else {
                Term::Var(d1-1)
            }
            Term::Lam(x, S, s) => Term::Lam(x, S.map(|S|Box::new(S.subst(d, t))), Box::new(s.subst(d, t))),
            Term::Pi(x, A, B) => Term::Pi(x, Box::new(A.subst(d, t)), Box::new(B.subst(d, t))),
            Term::App(s1, s2) => Term::App(Box::new(s1.subst(d, t)), s2.map(|s2|Box::new(s2.subst(d, t)))),
            Term::Case(s, cases) => Term::Case(
                Box::new(s.subst(d, t)),
                cases.into_iter().map(|(cname, vars, s)|(cname, vars, s.subst(d, t))).collect()
            ),
            t => t
        }
    }
    
    /// 将term的层数提升`d`层。
    pub fn lift(self, d: usize) -> Term {
        self.lift_since(d, 0)
    }
    
    /// 将term中，大于等于`sd`层数的自由变量提升`d`层。
    pub fn lift_since(self, d: usize, sd: usize) -> Term {
        match self {
            Term::Var(d1) if d1 >= sd => Term::Var(d1+d),
            Term::Lam(x, S, s) => Term::Lam(x, S.map(|S|Box::new(S.lift(d))), Box::new(s.lift_since(d, sd))),
            Term::Pi(x, A, B) => Term::Pi(x, Box::new(A.lift_since(d, sd)), Box::new(B.lift_since(d, sd))),
            Term::App(s, t) => Term::App(Box::new(s.lift_since(d, sd)), t.map(|t|Box::new(t.lift_since(d, sd)))),
            Term::Case(t, cases) => Term::Case(
                Box::new(t.lift_since(d, sd)),
                cases.into_iter().map(|(cname, vars, t)|(cname, vars, t.lift_since(d, sd))).collect()
            ),
            t => t
        }
    }
    
    /// 函数类型的最后返回的类型
    pub fn return_type(&self) -> &Term {
        match self {
            Term::Pi(_, _, T) => T.return_type(),
            _ => self
        }
    }
    
    pub fn arg_types(&self) -> Vec<Arg<&Term>> {
        match self {
            Term::Pi(x, A, T) => {
                let mut v: Vec<Arg<&Term>> = vec![x.as_ref().map(|_|A.deref())];
                v.append(&mut T.arg_types());
                v
            }
            _ => Vec::new()
        }
    }
    
    /// 函数调用的调用函数
    pub fn caller(&self) -> &Term {
        match self {
            Term::App(t, _) => t.caller(),
            _ => self
        }
    }
    
    pub fn call_args(&self) -> Vec<Arg<&Term>> {
        match self {
            Term::App(s, t) => {
                let mut v = s.call_args();
                v.push(t.as_ref().map(|t|t.as_ref()));
                v
            }
            _ => Vec::new()
        }
    }
    
    pub fn call(caller: Term, callees: Vec<Arg<Term>>) -> Term {
        let mut t = caller;
        for callee in callees {
            t = Term::App(Box::new(t), callee.map(Box::new))
        }
        t
    }
}