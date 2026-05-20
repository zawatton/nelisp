//! Evaluator error type mirrored by `lisp/nelisp-stdlib-error.el`.

use std::fmt;

use super::sexp::Sexp;

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
    WrongNumberOfArguments {
        function: String,
        expected: String,
        got: usize,
    },
    /// `arith-error`.
    ArithError(String),
    /// User `(signal 'TAG DATA)`.
    UserError { tag: String, data: Sexp },
    /// `setting-constant`.
    SettingConstant(String),
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
            EvalError::SettingConstant(_) => "setting-constant",
            EvalError::Quit => "quit",
            EvalError::Internal(_) => "error",
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
            EvalError::ArithError(m) | EvalError::Internal(m) => str1(m),
            EvalError::UserError { data, .. } => data.clone(),
            EvalError::Quit => Sexp::Nil,
        };
        Sexp::cons(tag, data)
    }
}

/// `condition-case` clause-tag match.
pub fn is_error_subtype(clause_tag: &str, actual_tag: &str) -> bool {
    clause_tag == actual_tag || clause_tag == "t" || (clause_tag == "error" && actual_tag != "quit")
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            EvalError::UnboundVariable(n) => write!(f, "void-variable: {}", n),
            EvalError::UnboundFunction(n) => write!(f, "void-function: {}", n),
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
            EvalError::ArithError(m) => write!(f, "arith-error: {}", m),
            EvalError::UserError { tag, data } => write!(f, "{}: {}", tag, data),
            EvalError::SettingConstant(n) => write!(f, "setting-constant: {}", n),
            EvalError::Quit => write!(f, "quit"),
            EvalError::Internal(m) => write!(f, "internal: {}", m),
        }
    }
}

impl std::error::Error for EvalError {}
