//! Reader error type — single source of truth for what the lexer and
//! parser can fail with.
//!
//! The `NotYetImplemented` variant is *explicit* (not a silent skip)
//! per the prompt constraint "= explicit, not silent".  It carries
//! enough context for the parent agent / test to decide whether to
//! treat the source as a hard fail or as a feature the next sub-phase
//! must add.

use std::fmt;

/// Source position, 1-indexed line + column.  Tracked at the byte
/// level (= multibyte chars cost their UTF-8 byte width in `col`,
/// matching how Elisp's reader reports column on `.el` files for
/// error messages).
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

/// What went wrong while reading.  The variants are stable enough
/// that the next sub-phase (= Phase 7.5.4.2 evaluator) can pattern
/// match on them when it surfaces reader errors as Elisp `(error ...)`
/// signals.
#[derive(Debug, Clone, PartialEq)]
pub enum ReadError {
    /// Lexer-level malformed input (unknown escape, bad radix digit,
    /// stray byte).
    Lex { msg: String, pos: SourcePos },
    /// Parser-level structural failure (unbalanced paren, dot in the
    /// wrong position, trailing garbage).
    Parse { msg: String, pos: SourcePos },
    /// Stream ended while we still expected more input.
    UnexpectedEof { msg: String, pos: SourcePos },
    /// A construct the bootstrap subset deliberately defers.  Doc 44
    /// §3.2 lists backquote, char literal, sharpsign function-quote,
    /// byte-code literal, multibyte string handling.
    NotYetImplemented { feature: String, pos: SourcePos },
}

impl ReadError {
    pub fn lex(msg: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::Lex {
            msg: msg.into(),
            pos,
        }
    }

    pub fn parse(msg: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::Parse {
            msg: msg.into(),
            pos,
        }
    }

    pub fn unexpected_eof(msg: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::UnexpectedEof {
            msg: msg.into(),
            pos,
        }
    }

    pub fn not_yet_implemented(feature: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::NotYetImplemented {
            feature: feature.into(),
            pos,
        }
    }

    pub fn pos(&self) -> SourcePos {
        match self {
            ReadError::Lex { pos, .. }
            | ReadError::Parse { pos, .. }
            | ReadError::UnexpectedEof { pos, .. }
            | ReadError::NotYetImplemented { pos, .. } => *pos,
        }
    }
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ReadError::Lex { msg, pos } => write!(f, "lex error at {}: {}", pos, msg),
            ReadError::Parse { msg, pos } => write!(f, "parse error at {}: {}", pos, msg),
            ReadError::UnexpectedEof { msg, pos } => {
                write!(f, "unexpected EOF at {}: {}", pos, msg)
            }
            ReadError::NotYetImplemented { feature, pos } => {
                write!(
                    f,
                    "not-yet-implemented at {}: {} (deferred per Doc 44 §3.2)",
                    pos, feature
                )
            }
        }
    }
}

impl std::error::Error for ReadError {}
