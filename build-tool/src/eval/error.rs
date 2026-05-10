//! Evaluator error type (Doc 44 §3.3, Doc 86 §86.2 elisp-side mirror
//! in `lisp/nelisp-stdlib-error.el').  `condition-case' unwinds via
//! `Result<_, EvalError>'; [`EvalError::error_tag`] returns the Elisp
//! symbol a clause would catch on.  Read-side errors live here too
//! since the reader is dev-tooling and the production runtime must
//! reach `EvalError::Read'.

use std::fmt;

use super::sexp::Sexp;

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

/// Evaluator failure modes.  `condition-case' pattern-matches via
/// [`EvalError::error_tag`].  Conditions / parents / messages mirrored
/// in `lisp/nelisp-stdlib-error.el' (Doc 86 §86.2).
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// `void-variable' — `(symbol-value 'foo)' on never-bound symbol.
    UnboundVariable(String),
    /// `void-function' — `(funcall 'foo)' on never-defun'd symbol.
    UnboundFunction(String),
    /// `wrong-type-argument' — `(+ 'a 1)', `(car 1)', etc.
    WrongType { expected: String, got: Sexp },
    /// `wrong-number-of-arguments' — arity mismatch.
    WrongNumberOfArguments {
        function: String,
        expected: String,
        got: usize,
    },
    /// `arith-error' — `(/ 1 0)' and friends.
    ArithError(String),
    /// User `(signal 'TAG DATA)' for non-canonical tags.  `error_tag'
    /// returns `tag', `signal_data' returns `data' verbatim.
    UserError { tag: String, data: Sexp },
    /// `no-catch' — `(throw TAG VALUE)' off the top of the catch stack.
    UncaughtThrow { tag: Sexp, value: Sexp },
    /// `setting-constant' — `(setq nil 1)' / `(set 't 1)' / etc.
    SettingConstant(String),
    /// `error' — bootstrap reader / evaluator hits a deferred construct.
    NotImplemented(String),
    /// `invalid-read-syntax' — read-side error surfaced via
    /// `From<ReadError>'.
    Read(ReadError),
    /// `quit' — `C-g' / `(signal 'quit nil)'.  Distinct from `error':
    /// the universal `error' clause does NOT catch it (Doc 51 Track M).
    Quit,
    /// `error' (catch-all) — evaluator bug that should never fire on
    /// valid user input.  Kept as `Result::Err`, never `panic!'.
    Internal(String),
}

impl EvalError {
    /// Map the error to the Elisp symbol name a `condition-case` clause
    /// would catch on.  `error` is the universal parent that catches
    /// everything bar `quit` (see [`is_error_subtype`]).
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
            EvalError::NotImplemented(_) => "error",
            EvalError::Read(_) => "invalid-read-syntax",
            EvalError::Quit => "quit",
            EvalError::Internal(_) => "error",
        }
    }

    /// Build the `(SYMBOL . DATA)` cons that `condition-case` binds the
    /// caught variable to.  This mirrors GNU Emacs `signal-data`
    /// behaviour for the common error types.
    pub fn signal_data(&self) -> Sexp {
        let tag = Sexp::Symbol(self.error_tag().to_string());
        let data = match self {
            EvalError::UnboundVariable(name) => {
                Sexp::list_from(&[Sexp::Symbol(name.clone())])
            }
            EvalError::UnboundFunction(name) => {
                Sexp::list_from(&[Sexp::Symbol(name.clone())])
            }
            EvalError::WrongType { expected, got } => Sexp::list_from(&[
                Sexp::Symbol(expected.clone()),
                got.clone(),
            ]),
            EvalError::WrongNumberOfArguments {
                function, got, ..
            } => Sexp::list_from(&[
                Sexp::Symbol(function.clone()),
                Sexp::Int(*got as i64),
            ]),
            EvalError::ArithError(msg) => Sexp::list_from(&[Sexp::Str(msg.clone())]),
            EvalError::UserError { data, .. } => data.clone(),
            EvalError::UncaughtThrow { tag, value } => {
                Sexp::list_from(&[tag.clone(), value.clone()])
            }
            EvalError::SettingConstant(name) => {
                Sexp::list_from(&[Sexp::Symbol(name.clone())])
            }
            EvalError::NotImplemented(msg) => Sexp::list_from(&[Sexp::Str(msg.clone())]),
            EvalError::Read(e) => Sexp::list_from(&[Sexp::Str(e.to_string())]),
            EvalError::Quit => Sexp::Nil,
            EvalError::Internal(msg) => Sexp::list_from(&[Sexp::Str(msg.clone())]),
        };
        Sexp::cons(tag, data)
    }
}

/// Does `tag` from a `condition-case` clause catch an error whose
/// canonical tag is `actual`?  GNU Emacs has a real condition hierarchy
/// (see `error-conditions`), but for the bootstrap we only need the
/// universal `error` parent plus exact-match on the leaf tag.  The
/// `t` clause catches anything (including `quit`) per Elisp manual.
pub fn is_error_subtype(clause_tag: &str, actual_tag: &str) -> bool {
    if clause_tag == actual_tag {
        return true;
    }
    if clause_tag == "t" {
        return true;
    }
    // All non-`quit` errors are subtypes of `error`.
    if clause_tag == "error" && actual_tag != "quit" {
        return true;
    }
    false
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnboundVariable(name) => {
                write!(f, "void-variable: {}", name)
            }
            EvalError::UnboundFunction(name) => {
                write!(f, "void-function: {}", name)
            }
            EvalError::WrongType { expected, got } => {
                write!(f, "wrong-type-argument: ({} {})", expected, got)
            }
            EvalError::WrongNumberOfArguments {
                function,
                expected,
                got,
            } => write!(
                f,
                "wrong-number-of-arguments: {} (expected {}, got {})",
                function, expected, got
            ),
            EvalError::ArithError(msg) => write!(f, "arith-error: {}", msg),
            EvalError::UserError { tag, data } => {
                write!(f, "{}: {}", tag, data)
            }
            EvalError::UncaughtThrow { tag, value } => {
                write!(f, "no-catch: ({} {})", tag, value)
            }
            EvalError::SettingConstant(name) => {
                write!(f, "setting-constant: {}", name)
            }
            EvalError::NotImplemented(msg) => write!(f, "not-implemented: {}", msg),
            EvalError::Read(e) => write!(f, "{}", e),
            EvalError::Quit => write!(f, "quit"),
            EvalError::Internal(msg) => write!(f, "internal: {}", msg),
        }
    }
}

impl std::error::Error for EvalError {}

impl From<ReadError> for EvalError {
    fn from(e: ReadError) -> Self {
        EvalError::Read(e)
    }
}
