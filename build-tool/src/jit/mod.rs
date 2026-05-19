//! Hot-path primitive trampolines for the nelisp-cc / JIT bridge.
//!
//! Lowered primitives are reachable via two paths:
//!   1. `nl-jit-call-*' bridge primitives re-exported from [`bridge`];
//!      elisp wrappers in `lisp/nelisp-jit-strategy.el' invoke by name
//!      and `bridge::unified_fn_ptr' maps `nelisp_jit_*' to the trampoline.
//!   2. dlsym-resolved `nl_jit_*' trampolines that nelisp-cc compiled
//!      code calls directly via `nelisp-cc--dlsym-resolve' (`-rdynamic'
//!      in `.cargo/config.toml' exposes the `#[no_mangle]' symbols).

pub(super) const TRAMPOLINE_OK: i64 = 0;
pub(super) const TRAMPOLINE_ERR: i64 = 1;

/// Extract a string from `Sexp::Str`, `Symbol`, `MutStr`, `Nil`, or `T`.
/// Returns `None` for all other variants.
pub(super) fn read_sexp_str(v: &crate::eval::sexp::Sexp) -> Option<String> {
    use crate::eval::sexp::Sexp;
    match v {
        Sexp::Str(s) | Sexp::Symbol(s) => Some(s.clone()),
        Sexp::MutStr(rc) => Some(rc.value.clone()),
        Sexp::Nil => Some("nil".into()),
        Sexp::T => Some("t".into()),
        _ => None,
    }
}

mod box_accessor;
pub(super) mod bridge;
pub(crate) use bridge::{
    bi_nl_jit_call_float_cmp, bi_nl_jit_call_float_float, bi_nl_jit_call_float_unary,
    bi_nl_jit_call_format_float, bi_nl_jit_call_i64_i64, bi_nl_jit_call_out_1,
    bi_nl_jit_call_out_1i, bi_nl_jit_call_out_2, bi_nl_jit_call_out_2i, bi_nl_jit_call_ptr_ptr,
    bi_nl_jit_call_syscall,
};
mod hash;
mod regex;
mod strings;
mod syscall;
