use std::fmt;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePos { pub line: u32, pub col: u32 }
impl fmt::Display for SourcePos { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}:{}", self.line, self.col) } }
#[derive(Debug, Clone, PartialEq)]
pub enum ReadError { Parse(String, SourcePos), UnexpectedEof(String, SourcePos), NotYetImplemented(String, SourcePos) }
impl ReadError {
    pub fn parse(s: impl Into<String>, pos: SourcePos) -> Self { ReadError::Parse(s.into(), pos) }
    pub fn unexpected_eof(s: impl Into<String>, pos: SourcePos) -> Self { ReadError::UnexpectedEof(s.into(), pos) }
    pub fn not_yet_implemented(s: impl Into<String>, pos: SourcePos) -> Self { ReadError::NotYetImplemented(s.into(), pos) }
}
impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self { ReadError::Parse(m,p) => write!(f, "parse error at {p}: {m}"), ReadError::UnexpectedEof(m,p) => write!(f, "unexpected EOF at {p}: {m}"), ReadError::NotYetImplemented(m,p) => write!(f, "not-yet-implemented at {p}: {m}") } }
}
impl std::error::Error for ReadError {}
