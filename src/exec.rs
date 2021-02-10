use std::path::Path;
use std::fs::read_to_string;

use crate::context::Context;
use crate::error::*;
use crate::core::*;
use crate::parser::gen::*;
use crate::parser::{lexer::TokenStream, stmt::*};
use std::collections::HashMap;

pub fn execute(stmt: Stmt, ctx: &mut Context) -> Result<()> {
    match stmt {
        Stmt::Import(path) => load_module(path, ctx),
        Stmt::Define(name, ty, expr) => {
            if ctx.defs.contains_key(&name) {
                return Err(Error::new(
                    ErrorKind::Context,
                    format!("name {} already exists", name)
                ))
            }
            
            let (t, T) = if let Some(ty) = ty {
                let mut lctx = CheckingContext::new(&ctx);
                let T = gen_term(ty, &mut Vec::new(), &mut lctx)?
                    .check_sort(&mut lctx)?
                    .resolve_meta(&lctx)?
                    .eval(&mut lctx);

                ctx.temps.borrow_mut().insert(name.clone(), T.clone());

                // let mut lctx = CheckingContext::new(&ctx);
                let t = gen_term(expr, &mut Vec::new(), &mut lctx)?;
                let t = t
                    .check_type(&T, &mut lctx)?
                    .resolve_meta(&lctx)?
                    .eval(&mut lctx);
                
                let T = T.resolve_meta(&lctx)?.eval(&mut lctx);
                t.check_terminate(&name, &mut lctx, &mut Vec::new())?;
                
                ctx.temps.borrow_mut().remove(&name);
                
                (t, T)
            } else {
                let mut lctx = CheckingContext::new(&ctx);
                let t = gen_term(expr, &mut Vec::new(), &mut lctx)?;
                let (t, T) = t.infer_type(&mut lctx)?;
                let t = t.resolve_meta(&lctx)?.eval(&mut lctx);
                let T = T.resolve_meta(&lctx)?.eval(&mut lctx);
                (t, T)
            };
            
            ctx.defs.insert(name, (T, t));
            Ok(())
        }
        Stmt::Eval(expr) => {
            let mut lctx = CheckingContext::new(&ctx);
            let t = gen_term(expr, &mut Vec::new(), &mut lctx)?;
            let (t, T) = t.infer_type(&mut lctx)?;
            let t = t.resolve_meta(&lctx)?.eval(&mut lctx);
            let T = T.resolve_meta(&lctx)?.eval(&mut lctx);
            println!("{} : {}", t.print(&mut lctx), T.print(&mut lctx));
            Ok(())
        }
        Stmt::Data(name, ty, cons) => {
            if ctx.datas.contains_key(&name) {
                return Err(Error::new(
                    ErrorKind::Context,
                    format!("name {} already exists", name)
                ))
            }
            let mut lctx = CheckingContext::new(&ctx);
            let T = gen_term(ty, &mut Vec::new(), &mut lctx)?
                .check_sort(&mut lctx)?
                .resolve_meta(&lctx)?
                .eval(&mut lctx);
            if T.return_type() != &Term::Type {
                return Err(Error::new(
                    ErrorKind::Typecheck,
                    format!("the return type of {} is not Type", name)
                ))
            }
            ctx.temps.borrow_mut().insert(name.clone(), T.clone());
            
            let mut cons_defs = HashMap::new();
            // let mut lctx = CheckingContext::new(&ctx);
            for (cname, ty) in cons {
                if cons_defs.contains_key(&cname) {
                    return Err(Error::new(
                        ErrorKind::Context,
                        format!("name {} already exists", cname)
                    ))
                }
                
                let T = gen_term(ty, &mut Vec::new(), &mut lctx)?
                    .check_sort(&mut lctx)?
                    .resolve_meta(&lctx)?
                    .eval(&mut lctx);
                
                if T.return_type().caller() != &Term::Def(name.clone()) {
                    return Err(Error::new(
                        ErrorKind::Typecheck,
                        format!("the return type of {} is not a type of {}", cname, name)
                    ))
                }
                cons_defs.insert(cname, T);
            }
            
            ctx.temps.borrow_mut().remove(&name);
            ctx.datas.insert(name, (T, cons_defs));
            
            Ok(())
        },
        Stmt::Syntax(syntax, expr) => Ok(ctx.syntax.push((syntax, expr))),
    }
}

pub fn load_file(path: &Path, ctx: &mut Context) -> Result<()> {
    let data = read_to_string(path).map_err(|err|{
        Error::new(
            ErrorKind::Other,
            format!("can not open '{}' ({})", path.to_str().unwrap(), err)
        )
    })?;
    
    let mut ts = TokenStream::new(data.chars());
    while ts.peek().is_some() {
        let stmt = parse_stmt(&mut ts)?;
        execute(stmt, ctx)?;
    }
    
    Ok(())
}

pub fn load_module(path: Vec<String>, ctx: &mut Context) -> Result<()> {
    if ctx.imported.contains(&path) {
        return Ok(());
    }
    
    let mut imports = path.iter();
    let libname = imports.next().unwrap();
    let mut libpath = ctx.library.get(libname)
        .ok_or(Error::new(
            ErrorKind::Context,
            format!("can not find library {:?}", libname)
        ))?.clone();
    for modname in imports {
        libpath.push(Path::new(&modname));
    }
    libpath.set_extension("myfp");
    load_file(&libpath, ctx)?;
    
    ctx.imported.insert(path);
    Ok(())
}