use crate::error::*;
use super::expr::*;
use super::lexer::*;

#[derive(Debug)]
pub enum SyntaxDef {
    Symbol(String),
    Name(String)
}

#[derive(Debug)]
pub enum Stmt {
    Import(Vec<String>),
    Define(String, Option<Expr>, Expr),
    Data(String, Expr, Vec<(String, Expr)>),
    Syntax(Vec<SyntaxDef>, Expr),
    Eval(Expr),
}

pub fn parse_stmt(ts: &mut TokenStream) -> Result<Stmt> {
    match ts.next() {
        Some(Token::Import) => match ts.next() {
            Some(Token::Word(path)) => {
                let path = path.split('.').map(str::to_string).collect();
                Ok(Stmt::Import(path))
            },
            _ => Err(Error::new(
                ErrorKind::Parser,
                format!("expected an import path")
            ))
        }
        
        Some(Token::Let) => {
            let name = ts.expect_word()?;
            let ty =
                if let Some(Token::Colon) = ts.peek() {
                    ts.consume();
                    Some(parse_expr(ts)?)
                } else {
                    None
                };
            ts.expect(Token::Equal)?;
            let expr = parse_expr(ts)?;
            Ok(Stmt::Define(name, ty, expr))
        }
        
        Some(Token::Data) => {
            let name = ts.expect_word()?;
            ts.expect(Token::Colon)?;
            let ty = parse_expr(ts)?;
            ts.expect(Token::Where)?;
            
            let mut cons = Vec::new();
            while let Some(Token::Split) = ts.peek() {
                ts.consume();
                let consname = ts.expect_word()?;
                ts.expect(Token::Colon)?;
                let consty = parse_expr(ts)?;
                cons.push((consname, consty));
            }
            Ok(Stmt::Data(name, ty, cons))
        }
        
        Some(Token::Syntax) => {
            let mut syntaxdef = Vec::new();
            loop {
                match ts.next() {
                    Some(Token::Symbol(sym)) => syntaxdef.push(SyntaxDef::Symbol(sym)),
                    Some(Token::Word(name)) => syntaxdef.push(SyntaxDef::Name(name)),
                    Some(Token::Equal) => break,
                    _ => return Err(Error::new(
                        ErrorKind::Parser,
                        "expected a symbol, an identifier name or =".into()
                    ))
                }
            }
            let expr = parse_expr(ts)?;
            Ok(Stmt::Syntax(syntaxdef, expr))
        }
        
        Some(Token::Eval) => Ok(Stmt::Eval(parse_expr(ts)?)),
        
        _ => Err(Error::new(
            ErrorKind::Parser,
            format!("expected a beginning of statement")
        ))
    }
}