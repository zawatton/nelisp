//! `dlsym` bridge — single primitive `nelisp-cc--dlsym-resolve` that
//! turns a C symbol name into a runtime address for CALL fixup
//! patching.  Pure `libc::dlsym(RTLD_DEFAULT, ...)` with no caching /
//! PATH resolution / signature marshalling — those live elisp-side
//! on top of `nelisp-cc-runtime-resolve-symbol`.  Returns the
//! `(STATUS . ADDR-OR-NIL)` cons: `:resolved` / `:not-found`.

use super::error::EvalError;
use super::sexp::Sexp;
use std::ffi::CString;

/// `(nelisp-cc--dlsym-resolve SYMBOL-NAME) -> (STATUS . ADDR-OR-NIL)`.
/// SYMBOL-NAME is a string or symbol naming the C symbol to resolve.
/// Returns `(:resolved . ADDR)` (ADDR = unsigned i64) on success,
/// `(:not-found . nil)` if `dlsym` returned NULL.
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
    // Non-Unix host: emit `:not-found' uniformly so the elisp link
    // pass takes the deferred-fallback branch.
    Ok(Sexp::cons(Sexp::Symbol(":not-found".into()), Sexp::Nil))
}
