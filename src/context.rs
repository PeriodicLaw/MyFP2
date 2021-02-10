use std::collections::{HashMap, HashSet};
use std::path::PathBuf;

// use crate::lang::term::Term;
use crate::core::term::Term;
use crate::parser::expr::Expr;
use crate::parser::stmt::SyntaxDef;
use std::cell::RefCell;

pub struct Context {
    pub library: HashMap<String, PathBuf>,
    pub imported: HashSet<Vec<String>>,
    pub syntax: Vec<(Vec<SyntaxDef>, Expr)>,
    
    pub datas: HashMap<String, (Term, HashMap<String, Term>)>,
    pub defs: HashMap<String, (Term, Term)>,
    pub temps: RefCell<HashMap<String, Term>>,
}

impl Context {
    pub fn new() -> Self {
        Self {
            library: HashMap::new(),
            imported: HashSet::new(),
            syntax: Vec::new(),
            datas: HashMap::new(),
            defs: HashMap::new(),
            temps: RefCell::new(HashMap::new()),
        }
    }
}