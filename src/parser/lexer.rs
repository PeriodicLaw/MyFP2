use std::iter::Peekable;
use std::str::Chars;
use itertools::{Itertools, PeekNth, peek_nth};
use crate::error::*;

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Word(String),
    Data, Where, Eval, Case, Of, Type, Import, Syntax, Let, In, // keywords
    Symbol(String),
    LeftParen, RightParen, LeftBrace, RightBrace, // ( ) { }
    Equal, Colon, To, Arrow, Split, Question, // = : -> => | ?
    Lambda, // \ lambda λ
}

struct TokenStreamInner<'a>(Peekable<Chars<'a>>);

impl Iterator for TokenStreamInner<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            self.0.peeking_take_while(|c|{c.is_whitespace()}).for_each(|_|{});
            if let Some('#') = self.0.peek() {
                self.0.peeking_take_while(|c|{c != &'\n'}).for_each(drop);
            } else {
                break;
            }
        }
        
        match self.0.peek() {
            Some(c) if c.is_alphabetic() => Some({
                let s: String = self.0.peeking_take_while(|c|{
                    !c.is_control() && !c.is_whitespace() && c != &'(' && c != &')' && c != &'{' && c != &'}' && c != &'#'
                }).collect();
                
                match s.as_str() {
                    "data" => Token::Data,
                    "where" => Token::Where,
                    "eval" => Token::Eval,
                    "case" => Token::Case,
                    "of" => Token::Of,
                    "Type" => Token::Type,
                    "import" => Token::Import,
                    "syntax" => Token::Syntax,
                    "let" => Token::Let,
                    "in" => Token::In,
                    "lambda" | "λ" => Token::Lambda,
                    _ => Token::Word(s)
                }
            }),
            
            Some('(') | Some(')') | Some('{') | Some('}') => Some({
                let c = self.0.next().unwrap();
                
                match c {
                    '(' => Token::LeftParen,
                    ')' => Token::RightParen,
                    '{' => Token::LeftBrace,
                    '}' => Token::RightBrace,
                    _ => unreachable!()
                }
            }),
            
            Some('#') => panic!(),
            
            Some(_) => Some({
                let s: String = self.0.peeking_take_while(|c|{
                    !c.is_control() && !c.is_whitespace() && c != &'#'
                        && c != &'(' && c != &'{' && c != &')' && c != &'}'
                }).collect();
                match s.as_str() {
                    "=" => Token::Equal,
                    ":" => Token::Colon,
                    "->" => Token::To,
                    "=>" => Token::Arrow,
                    "|" => Token::Split,
                    "\\" => Token::Lambda,
                    "?" => Token::Question,
                    _ => Token::Symbol(s)
                }
            }),
            
            None => None
        }
    }
}

pub struct TokenStream<'a>(PeekNth<TokenStreamInner<'a>>);

impl<'a> TokenStream<'a> {
    pub fn new(chars: Chars<'a>) -> Self {
        Self(peek_nth(TokenStreamInner(chars.peekable())))
    }
    
    pub fn peek(&mut self) -> Option<&Token> {
        self.0.peek()
    }
    
    pub fn peek_nth(&mut self, n: usize) -> Option<&Token> {
        self.0.peek_nth(n)
    }
    
    pub fn next(&mut self) -> Option<Token> {
        self.0.next()
    }
    
    pub fn consume(&mut self) {
        self.next();
    }
    
    pub fn expect(&mut self, token: Token) -> Result<()> {
        if let Some(t) = self.next() {
            if t == token {
                return Ok(())
            }
        }
        
        Err(Error::new(
            ErrorKind::Parser,
            format!("expected token {:?}", token)
        ))
    }
    
    pub fn expect_word(&mut self) -> Result<String> {
        if let Some(Token::Word(s)) = self.next() {
            return Ok(s)
        }
        
        Err(Error::new(
            ErrorKind::Parser,
            format!("expected a identifier name")
        ))
    }
}

impl Iterator for TokenStream<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        TokenStream::next(self)
    }
}