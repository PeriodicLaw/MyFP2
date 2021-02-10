use std::collections::VecDeque;

use super::lexer::*;
use crate::error::*;

#[derive(Debug, Clone)]
pub enum SyntaxExpr {
    Symbol(String),
    Expr(Expr)
}

#[derive(Debug, Clone)]
pub enum Expr {
    Named(String),
    
    Func(Option<Vec<String>>, Box<Expr>, Box<Expr>),
    ImpFunc(Vec<String>, Box<Option<Expr>>, Box<Expr>),
    Type,
    
    Lam(String, Box<Option<Expr>>, Box<Expr>),
    ImpLam(String, Box<Option<Expr>>, Box<Expr>),
    App(Box<Expr>, Box<Expr>),
    ImpApp(Box<Expr>, Box<Expr>),
    
    Case(Box<Expr>, Vec<(String, Vec<(String, bool)>, Expr)>),
    Syntax(Vec<SyntaxExpr>),
    Unknown
}

pub fn parse_expr(ts: &mut TokenStream) -> Result<Expr> {
    match ts.peek() {
        Some(Token::Lambda) => {
            ts.consume();
            match ts.next() {
                Some(Token::LeftBrace) => {
                    let name = ts.expect_word()?;
                    let ty = 
                        if let Some(Token::Colon) = ts.peek() {
                            ts.consume();
                            Some(parse_expr(ts)?)
                        } else {
                            None
                        };
                    ts.expect(Token::RightBrace)?;
                    let expr = parse_expr(ts)?;
                    Ok(Expr::ImpLam(name, Box::new(ty), Box::new(expr)))
                }
                
                Some(Token::LeftParen) => {
                    let name = ts.expect_word()?;
                    ts.expect(Token::Colon)?;
                    let ty = parse_expr(ts)?;
                    ts.expect(Token::RightParen)?;
                    let expr = parse_expr(ts)?;
                    Ok(Expr::Lam(name, Box::new(Some(ty)), Box::new(expr)))
                }
                
                Some(Token::Word(name)) => {
                    let expr = parse_expr(ts)?;
                    Ok(Expr::Lam(name, Box::new(None), Box::new(expr)))
                }
                
                _ => Err(Error::new(
                    ErrorKind::Parser,
                    "expected '(', '{' or an identifier name for lambda expression".into()
                ))
            }
        }
        
        Some(Token::Case) => {
            ts.consume();
            let expr = parse_expr(ts)?;
            ts.expect(Token::Of)?;
            let mut cases = Vec::new();
            while let Some(Token::Split) = ts.peek() {
                ts.consume();
                let cons = ts.expect_word()?;
                let mut args = Vec::new();
                
                loop {
                    match ts.peek() {
                        Some(Token::Word(arg)) => {
                            args.push((arg.clone(), true));
                            ts.consume();
                        },
                        Some(Token::LeftBrace) => {
                            ts.consume();
                            args.push((ts.expect_word()?, false));
                            ts.expect(Token::RightBrace)?;
                        }
                        _ => break
                    }
                }
                
                ts.expect(Token::Arrow)?;
                let expr = parse_expr(ts)?;
                cases.push((cons, args, expr));
            }
            Ok(Expr::Case(Box::new(expr), cases))
        }
        
        // Some(Token::Let) => {
        //     ts.consume();
        //     let var = ts.expect_word()?;
        //     let ty =
        //         if let Some(Token::Colon) = ts.peek() {
        //             ts.consume();
        //             Some(parse_expr(ts)?)
        //         } else {
        //             None
        //         };
        //     ts.expect(Token::Equal)?;
        //     let expr = parse_expr(ts)?;
        //     ts.expect(Token::In)?;
        //     let expr1 = parse_expr(ts)?;
        //     Ok(Expr::LetDef(var, Box::new(ty), Box::new(expr), Box::new(expr1)))
        // }
        
        _ => parse_func(ts)
    }
}

pub fn parse_func(ts: &mut TokenStream) -> Result<Expr> {
    if let Some(Token::LeftParen) = ts.peek_nth(0) {
        if let Some(Token::Word(var)) = ts.peek_nth(1) {
            let mut vars = vec![var.clone()];
            let mut len = 1;
            while let Some(Token::Word(var)) = ts.peek_nth(len+1) {
                vars.push(var.clone());
                len += 1;
            }
            
            if let Some(Token::Colon) = ts.peek_nth(len+1) {
                for _ in 0..len+2 {
                    ts.consume();
                }
                
                let expr0 = parse_expr(ts)?;
                ts.expect(Token::RightParen)?;
                ts.expect(Token::To)?;
                let expr1 = parse_func(ts)?;
                return Ok(Expr::Func(Some(vars), Box::new(expr0), Box::new(expr1)));
            }
        }
    }
    
    if let Some(Token::LeftBrace) = ts.peek() {
        ts.consume();
        let mut vars = Vec::new();
        while let Some(Token::Word(var)) = ts.peek() {
            vars.push(var.clone());
            ts.consume();
        }
        
        let expr0 = if let Some(Token::Colon) = ts.peek() {
            ts.consume();
            Some(parse_expr(ts)?)
        } else {
            None
        };
        ts.expect(Token::RightBrace)?;
        ts.expect(Token::To)?;
        let expr1 = parse_func(ts)?;
        Ok(Expr::ImpFunc(vars, Box::new(expr0), Box::new(expr1)))
    } else {
        let expr0 = parse_syntax(ts)?;
        if let Some(Token::To) = ts.peek() {
            ts.consume();
            let expr1 = parse_func(ts)?;
            Ok(Expr::Func(None, Box::new(expr0), Box::new(expr1)))
        } else {
            Ok(expr0)
        }
    }
    
}

fn is_end_token(token: Option<&Token>) -> bool {
    match token {
        Some(token) => match token {
            Token::Word(_) | Token::LeftBrace | Token::LeftParen | Token::Type | Token::Question => false,
            _ => true
        }
        None => true
    }
}

pub fn parse_syntax(ts: &mut TokenStream) -> Result<Expr> {
    let mut syntax = Vec::new();
    loop {
        if let Some(Token::Symbol(sym)) = ts.peek() {
            syntax.push(SyntaxExpr::Symbol(sym.clone()));
            ts.consume();
        } else if is_end_token(ts.peek()) {
            break;
        } else {
            syntax.push(SyntaxExpr::Expr(parse_apply(ts)?));
        }
    }
    
    if syntax.len() == 0 {
        return Err(Error::new(
            ErrorKind::Parser,
            "expected an expression".into()
        ))
    }else if syntax.len() == 1 {
        if let Some(SyntaxExpr::Expr(_)) = syntax.get(0) {
            let expr = syntax.into_iter().next();
            match expr {
                Some(SyntaxExpr::Expr(expr)) => return Ok(expr),
                _ => unreachable!()
            }
        }
    }
    
    Ok(Expr::Syntax(syntax))
}

pub fn parse_apply(ts: &mut TokenStream) -> Result<Expr> {
    let mut expr0 = parse_atom(ts)?;
    
    let mut exprs = VecDeque::new();
    loop {
        if let Some(Token::LeftBrace) = ts.peek() {
            ts.consume();
            let expr = parse_expr(ts)?;
            ts.expect(Token::RightBrace)?;
            exprs.push_back((expr, true));
        } else if is_end_token(ts.peek()) {
            break;
        } else {
            exprs.push_back((parse_atom(ts)?, false));
        }
    }
    
    while let Some((expr1, imp)) = exprs.pop_front() {
        expr0 = if imp {
            Expr::ImpApp(Box::new(expr0), Box::new(expr1))
        } else {
            Expr::App(Box::new(expr0), Box::new(expr1))
        };
    }
    Ok(expr0)
}

pub fn parse_atom(ts: &mut TokenStream) -> Result<Expr> {
    match ts.next() {
        Some(Token::LeftParen) => {
            let expr = parse_expr(ts)?;
            ts.expect(Token::RightParen)?;
            Ok(expr)
        }
        
        Some(Token::Type) => Ok(Expr::Type),
        
        Some(Token::Word(name)) => Ok(Expr::Named(name)),
        
        Some(Token::Question) => Ok(Expr::Unknown),
        
        _ => Err(Error::new(
            ErrorKind::Parser,
            "expect (, Type or an identifier name".into()
        ))
    }
}