use std::fmt::Debug;

pub enum ErrorKind {
    Parser,
    Context,
    Typecheck,
    TerminCheck,
    Other
}

pub struct Error {
    kind: ErrorKind,
    info: String
}

pub type Result<T> = core::result::Result<T, Error>;

impl Error {
    pub fn new(kind: ErrorKind, info: String) -> Self {
        Self {kind, info}
    }
}

impl Debug for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            ErrorKind::Parser => write!(f, "(Parser Error) ")?,
            ErrorKind::Context => write!(f, "(Context Error) ")?,
            ErrorKind::Typecheck => write!(f, "(Type checking Error) ")?,
            ErrorKind::TerminCheck => write!(f, "(Termination Checking Error) ")?,
            
            ErrorKind::Other => {}
        }
        writeln!(f, "{}", self.info)?;
        
        Ok(())
    }
}