//! Re-export the reader's [`Sexp`] under `eval::` so the evaluator
//! sources can `use super::sexp::Sexp` without reaching into a
//! sibling module.  The evaluator does NOT widen the value enum yet
//! (lambdas are stored as `(closure ENV ARGS BODY...)` cons forms,
//! see [`super::env`]), so we deliberately keep a single `Sexp` type
//! across reader / evaluator boundary.

pub use crate::reader::Sexp;
