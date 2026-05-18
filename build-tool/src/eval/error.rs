//! Evaluator error type mirrored by `lisp/nelisp-stdlib-error.el`.

use std::fmt;

use super::sexp::Sexp;

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

/// Reader-side failure surfaced through [`EvalError::Read`].
#[derive(Debug, Clone, PartialEq)]
pub enum ReadError {
    Lex { msg: String, pos: SourcePos },
    Parse { msg: String, pos: SourcePos },
    UnexpectedEof { msg: String, pos: SourcePos },
    NotYetImplemented { feature: String, pos: SourcePos },
}

impl ReadError {
    pub fn lex(msg: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::Lex { msg: msg.into(), pos }
    }
    pub fn parse(msg: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::Parse { msg: msg.into(), pos }
    }
    pub fn unexpected_eof(msg: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::UnexpectedEof { msg: msg.into(), pos }
    }
    pub fn not_yet_implemented(feature: impl Into<String>, pos: SourcePos) -> Self {
        ReadError::NotYetImplemented { feature: feature.into(), pos }
    }
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let (kind, body, pos) = match self {
            ReadError::Lex { msg, pos } => ("lex error", msg.as_str(), pos),
            ReadError::Parse { msg, pos } => ("parse error", msg.as_str(), pos),
            ReadError::UnexpectedEof { msg, pos } => ("unexpected EOF", msg.as_str(), pos),
            ReadError::NotYetImplemented { feature, pos } => ("not-yet-implemented", feature.as_str(), pos),
        };
        write!(f, "{} at {}: {}", kind, pos, body)
    }
}

impl std::error::Error for ReadError {}

/// Evaluator failure modes.
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// `void-variable`.
    UnboundVariable(String),
    /// `void-function`.
    UnboundFunction(String),
    /// `wrong-type-argument`.
    WrongType { expected: String, got: Sexp },
    /// `wrong-number-of-arguments`.
    WrongNumberOfArguments { function: String, expected: String, got: usize },
    /// `arith-error`.
    ArithError(String),
    /// User `(signal 'TAG DATA)`.
    UserError { tag: String, data: Sexp },
    /// `no-catch` for `(throw TAG VALUE)` off the catch stack.
    UncaughtThrow { tag: Sexp, value: Sexp },
    /// `setting-constant`.
    SettingConstant(String),
    /// `error` for deferred bootstrap constructs.
    NotImplemented(String),
    /// `invalid-read-syntax`.
    Read(ReadError),
    /// `quit`.
    Quit,
    /// `error` catch-all for evaluator bugs.
    Internal(String),
}

impl EvalError {
    /// Elisp symbol name a `condition-case` clause would catch on.
    pub fn error_tag(&self) -> &str {
        match self {
            EvalError::UnboundVariable(_) => "void-variable",
            EvalError::UnboundFunction(_) => "void-function",
            EvalError::WrongType { .. } => "wrong-type-argument",
            EvalError::WrongNumberOfArguments { .. } => "wrong-number-of-arguments",
            EvalError::ArithError(_) => "arith-error",
            EvalError::UserError { tag, .. } => tag.as_str(),
            EvalError::UncaughtThrow { .. } => "no-catch",
            EvalError::SettingConstant(_) => "setting-constant",
            EvalError::Read(_) => "invalid-read-syntax",
            EvalError::Quit => "quit",
            EvalError::NotImplemented(_) | EvalError::Internal(_) => "error",
        }
    }

    /// `(SYMBOL . DATA)' cons that `condition-case' binds the var to.
    pub fn signal_data(&self) -> Sexp {
        let tag = Sexp::Symbol(self.error_tag().to_string());
        let sym = |s: &str| Sexp::list_from(&[Sexp::Symbol(s.to_string())]);
        let str1 = |s: &str| Sexp::list_from(&[Sexp::Str(s.to_string())]);
        let data = match self {
            EvalError::UnboundVariable(n)
            | EvalError::UnboundFunction(n)
            | EvalError::SettingConstant(n) => sym(n),
            EvalError::WrongType { expected, got } => {
                Sexp::list_from(&[Sexp::Symbol(expected.clone()), got.clone()])
            }
            EvalError::WrongNumberOfArguments { function, got, .. } => {
                Sexp::list_from(&[Sexp::Symbol(function.clone()), Sexp::Int(*got as i64)])
            }
            EvalError::ArithError(m)
            | EvalError::NotImplemented(m)
            | EvalError::Internal(m) => str1(m),
            EvalError::UserError { data, .. } => data.clone(),
            EvalError::UncaughtThrow { tag: t, value } => {
                Sexp::list_from(&[t.clone(), value.clone()])
            }
            EvalError::Read(e) => str1(&e.to_string()),
            EvalError::Quit => Sexp::Nil,
        };
        Sexp::cons(tag, data)
    }
}

/// `condition-case` clause-tag match.
pub fn is_error_subtype(clause_tag: &str, actual_tag: &str) -> bool {
    clause_tag == actual_tag
        || clause_tag == "t"
        || (clause_tag == "error" && actual_tag != "quit")
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnboundVariable(n) => write!(f, "void-variable: {}", n),
            EvalError::UnboundFunction(n) => write!(f, "void-function: {}", n),
            EvalError::WrongType { expected, got } => {
                write!(f, "wrong-type-argument: ({} {})", expected, got)
            }
            EvalError::WrongNumberOfArguments { function, expected, got } => write!(
                f,
                "wrong-number-of-arguments: {} (expected {}, got {})",
                function, expected, got
            ),
            EvalError::ArithError(m) => write!(f, "arith-error: {}", m),
            EvalError::UserError { tag, data } => write!(f, "{}: {}", tag, data),
            EvalError::UncaughtThrow { tag, value } => {
                write!(f, "no-catch: ({} {})", tag, value)
            }
            EvalError::SettingConstant(n) => write!(f, "setting-constant: {}", n),
            EvalError::NotImplemented(m) => write!(f, "not-implemented: {}", m),
            EvalError::Read(e) => write!(f, "{}", e),
            EvalError::Quit => write!(f, "quit"),
            EvalError::Internal(m) => write!(f, "internal: {}", m),
        }
    }
}

impl std::error::Error for EvalError {}

impl From<ReadError> for EvalError {
    fn from(e: ReadError) -> Self {
        EvalError::Read(e)
    }
}
