use std::fmt;
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePos { pub line: u32, pub col: u32 }
impl fmt::Display for SourcePos { fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result { write!(f, "{}:{}", self.line, self.col) } }
#[derive(Debug, Clone, PartialEq)]
pub enum ReadError {
    Parse { msg: String, pos: SourcePos }, UnexpectedEof { msg: String, pos: SourcePos }, NotYetImplemented { feature: String, pos: SourcePos },
}
impl ReadError {
    pub fn parse(s: impl Into<String>, pos: SourcePos) -> Self { ReadError::Parse { msg: s.into(), pos } }
    pub fn unexpected_eof(s: impl Into<String>, pos: SourcePos) -> Self { ReadError::UnexpectedEof { msg: s.into(), pos } }
    pub fn not_yet_implemented(s: impl Into<String>, pos: SourcePos) -> Self { ReadError::NotYetImplemented { feature: s.into(), pos } }
}
impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self { ReadError::Parse { msg, pos } => write!(f, "parse error at {pos}: {msg}"), ReadError::UnexpectedEof { msg, pos } => write!(f, "unexpected EOF at {pos}: {msg}"), ReadError::NotYetImplemented { feature, pos } => write!(f, "not-yet-implemented at {pos}: {feature}") } }
}
impl std::error::Error for ReadError {}
