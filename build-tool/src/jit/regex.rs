//! `string-match-p` trampoline — body migrated to Phase 47 elisp.
//!
//! See `lisp/nelisp-cc-jit-regex.el` for the elisp implementation.
//! The `#[no_mangle] extern "C" fn nl_jit_string_match_p` is now
//! supplied by the compiled `.o` archive linked in via
//! `build-tool/src/jit/bridge.rs`.
