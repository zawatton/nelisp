//! Reader-side error types, kept in the reader crate to avoid pulling
//! them into `eval/error.rs`.

use std::fmt;

/// Source position as 1-indexed line and UTF-8 byte column.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourcePos {
    pub line: u32,
    pub col: u32,
}

impl fmt::Display for SourcePos {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

/// Reader-side failure.
#[derive(Debug, Clone, PartialEq)]
pub enum ReadError {
    Parse { msg: String, pos: SourcePos },
    UnexpectedEof { msg: String, pos: SourcePos },
    NotYetImplemented { feature: String, pos: SourcePos },
}

macro_rules! readerr_ctor {
    ($fn:ident => $variant:ident, $field:ident) => {
        pub fn $fn(s: impl Into<String>, pos: SourcePos) -> Self {
            ReadError::$variant {
                $field: s.into(),
                pos,
            }
        }
    };
}

impl ReadError {
    readerr_ctor!(parse => Parse, msg);
    readerr_ctor!(unexpected_eof => UnexpectedEof, msg);
    readerr_ctor!(not_yet_implemented => NotYetImplemented, feature);
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (kind, body, pos) = match self {
            ReadError::Parse { msg, pos } => ("parse error", msg.as_str(), pos),
            ReadError::UnexpectedEof { msg, pos } => ("unexpected EOF", msg.as_str(), pos),
            ReadError::NotYetImplemented { feature, pos } => {
                ("not-yet-implemented", feature.as_str(), pos)
            }
        };
        write!(f, "{} at {}: {}", kind, pos, body)
    }
}

impl std::error::Error for ReadError {}
