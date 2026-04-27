//! Evaluator error type — Phase 8.0.2 (Doc 44 §3.3 LOCKED).
//!
//! Contract per Doc 44 §4.3:
//!   - never panic on user-input error
//!   - `condition-case` is implemented as a `Result` unwind through
//!     a dedicated handler stack inside the evaluator (no Rust panic)
//!   - the variant set is *closed enough* that `condition-case` can
//!     pattern-match on the Elisp error symbol (= the [`error_tag`]
//!     accessor returns the canonical symbol name a `condition-case`
//!     clause would catch on)
//!
//! The variants are deliberately broad rather than fine-grained — the
//! bootstrap interpreter only needs the handful of error tags that the
//! NeLisp source actually `signal`s on the cold-init path; richer
//! variants can be folded in once the takeover bridge (Phase 8.0.3)
//! lands and the GNU Emacs error hierarchy needs to round-trip.

use std::fmt;

use crate::reader::ReadError;

use super::sexp::Sexp;

/// All the ways the evaluator can fail.  `condition-case` clauses
/// pattern-match on [`EvalError::error_tag`] which returns the Elisp
/// symbol the clause would name (e.g. `"void-variable"`,
/// `"wrong-type-argument"`, `"error"`).
#[derive(Debug, Clone, PartialEq)]
pub enum EvalError {
    /// `(symbol-value 'foo)` for a never-bound symbol → `void-variable`.
    UnboundVariable(String),
    /// `(funcall 'foo)` for a never-defun'd symbol → `void-function`.
    UnboundFunction(String),
    /// Argument did not match the operator's expected type
    /// (e.g. `(+ 'a 1)`, `(car 1)`) → `wrong-type-argument`.
    WrongType {
        /// Human-readable predicate name (`"numberp"`, `"listp"`, …).
        expected: String,
        /// The offending value.
        got: Sexp,
    },
    /// Argument count mismatch on a built-in or interpreted lambda
    /// → `wrong-number-of-arguments`.
    WrongNumberOfArguments {
        function: String,
        expected: String,
        got: usize,
    },
    /// `(/ 1 0)` and friends → `arith-error`.
    ArithError(String),
    /// `(signal 'my-tag DATA)` raised by user code.  The tag is
    /// preserved as a symbol name; the data is the cdr of the signal.
    /// `condition-case` matches against [`EvalError::error_tag`] which
    /// returns `tag.clone()`.
    UserError {
        /// Symbol name the user passed to `signal`.
        tag: String,
        /// Data list (already as a Lisp list, not raw arguments).
        data: Sexp,
    },
    /// `(throw TAG VALUE)` walked off the top of the catch stack.
    /// Surfaced as `(no-catch TAG VALUE)` to match Emacs.
    UncaughtThrow { tag: Sexp, value: Sexp },
    /// `setq`/`set` of a constant symbol such as `nil` or `t`.
    SettingConstant(String),
    /// Attempted to read a `#'foo`-style or `#[...]`-style construct
    /// that the bootstrap reader / evaluator does not yet understand.
    NotImplemented(String),
    /// Underlying read error surfaced through [`From<ReadError>`].
    Read(ReadError),
    /// Catch-all for evaluator bugs that should never fire on valid
    /// user input (we still keep it as `Result::Err`, never `panic!`).
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
