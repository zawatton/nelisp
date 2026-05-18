//! Predicate trampolines — all bodies deleted; Phase 47 elisp owns them.
//!
//! `nl_jit_sxhash'  → `lisp/nelisp-cc-jit-sxhash.el'  (Doc 120 §120.A.3)
//! `nl_jit_type_of' → `lisp/nelisp-cc-jit-type-of.el' (Doc 120 §120.A.2)
//!
//! The symbols are resolved at link time from the Phase 47 `.o' archive via
//! the `extern "C"' declarations + `_ELISP_ARCHIVE_ANCHOR' in `bridge.rs'.
