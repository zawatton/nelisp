//! Phase 7.1.6.a (Doc 28 §3.6.a / Doc 81 §5.4) — dlsym bridge.
//!
//! This module exposes ONE primitive — `nelisp-cc--dlsym-resolve' — that
//! the elisp-side `nelisp-cc-runtime-resolve-symbol' contract delegates
//! to in order to turn a C symbol name (e.g. `nl_jit_cons_make') into a
//! runtime address suitable for CALL fixup patching.
//!
//! The shim is the +N Rust delta Doc 81 §0 deferred to Phase 7.1.6.a.
//! It is intentionally minimal:
//!
//!   - Single elisp-callable entry point (`nelisp-cc--dlsym-resolve').
//!   - Pure libc::dlsym(RTLD_DEFAULT, ...) with no caching, no PATH
//!     resolution, no signature marshalling.  All higher-level concerns
//!     (caching idempotence, ABI marshalling, link-table integration)
//!     stay in elisp on top of `nelisp-cc-runtime-resolve-symbol'.
//!   - Honour the `(STATUS . ADDR-OR-NIL)` return contract documented
//!     on `nelisp-cc-runtime-resolve-symbol' — the shim emits exactly
//!     `:resolved' / `:not-found' (the host stub `:host-stub' is the
//!     elisp-side default when this primitive is absent).
//!
//! Phase 7.1.6.a.2 (= cluster takeover deletion) builds on this shim by
//! adding `#[no_mangle]' to the cons trampolines + `-rdynamic' link so
//! the binary's dynamic symbol table actually exposes them.  Without
//! that follow-up the shim returns `:not-found' for the cons cluster
//! on a regular Rust binary (no dynamic export), which is *acceptable*
//! contract behaviour — the link pass treats `:not-found' as a deferred
//! fallback (per Doc 81 §6.3) rather than an error.

use super::error::EvalError;
use super::sexp::Sexp;
use std::ffi::CString;

/// `(nelisp-cc--dlsym-resolve SYMBOL-NAME) -> (STATUS . ADDR-OR-NIL)'.
///
/// SYMBOL-NAME is a string or symbol naming the C symbol to resolve.
/// Calls `dlsym(RTLD_DEFAULT, name)` (Linux/macOS) and returns:
///
///   - `(:resolved . ADDR)' — non-null pointer encoded as an unsigned i64.
///   - `(:not-found . nil)' — `dlsym' returned NULL (= symbol unexported
///                            or absent in the linked images).
///
/// This is the standalone NeLisp override Doc 81 §5.4 contract describes;
/// host Emacs falls back to the elisp-side `:host-stub' sentinel when
/// this primitive is not available.
#[cfg(unix)]
pub fn bi_dlsym_resolve(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity(
        "nelisp-cc--dlsym-resolve", args, 1, Some(1))?;
    let name: String = match &args[0] {
        Sexp::Str(s) => s.clone(),
        Sexp::Symbol(s) => s.clone(),
        other => return Err(EvalError::WrongType {
            expected: "symbol or string".into(),
            got: other.clone(),
        }),
    };
    let cstr = CString::new(name.as_bytes()).map_err(|_| EvalError::WrongType {
        expected: "C symbol name (no interior NUL)".into(),
        got: args[0].clone(),
    })?;
    // SAFETY: dlsym(3) is async-signal-safe and may return NULL for
    // unresolved names; we never dereference `addr' here.  RTLD_DEFAULT
    // searches the global symbol table built up by the dynamic linker
    // (the nelisp binary plus its loaded shared libraries).
    let addr = unsafe { libc::dlsym(libc::RTLD_DEFAULT, cstr.as_ptr()) };
    if addr.is_null() {
        Ok(Sexp::cons(Sexp::Symbol(":not-found".into()), Sexp::Nil))
    } else {
        Ok(Sexp::cons(
            Sexp::Symbol(":resolved".into()),
            Sexp::Int(addr as usize as i64),
        ))
    }
}

#[cfg(not(unix))]
pub fn bi_dlsym_resolve(args: &[Sexp]) -> Result<Sexp, EvalError> {
    crate::eval::builtins::require_arity(
        "nelisp-cc--dlsym-resolve", args, 1, Some(1))?;
    // Non-Unix host: emit `:not-found' uniformly so the elisp link pass
    // takes the deferred-fallback branch (Doc 81 §6.3).
    Ok(Sexp::cons(Sexp::Symbol(":not-found".into()), Sexp::Nil))
}
