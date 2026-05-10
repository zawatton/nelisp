//! Doc 84 strategy.rs final residue — all Rust `bi_*' helpers ported
//! to elisp:
//!   - §84.1 (Float family, 8 fns)         → `lisp/nelisp-jit-strategy.el'
//!     on top of `nl-jit-call-float-{float,cmp}' (`jit/float.rs').
//!   - §84.2 (`bi_syscall_nr_resolve')     → `lisp/nelisp-syscall-table.el'
//!     auto-generated from `libc::SYS_*' via `build-tool/build.rs'.
//!   - §84.3 (6 Box accessors)             → `lisp/nelisp-jit-strategy.el'
//!     on top of `nl-jit-call-out-{1,1i,2i}' (`jit/box_accessor.rs').
//!
//! Module slated for deletion in §84.4 once mod.rs / builtins.rs cleanup
//! lands.  Kept as an empty placeholder for grep parity during staging.

#[allow(unused_imports)]
use crate::eval::error::EvalError;
#[allow(unused_imports)]
use crate::eval::sexp::Sexp;
