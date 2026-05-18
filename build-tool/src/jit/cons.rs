//! Cons trampolines.  `car' / `cdr' / `setcar' / `setcdr' bodies live
//! in `lisp/nelisp-cc-jit-cons.el' (Phase 47-compiled `.o' in
//! `libnelisp_elisp_spike.a'); `bridge::cons_link'-style resolution
//! routes the externs via `unified_fn_ptr' on linux-x86_64 (= the
//! crate's only supported target per `lib.rs:30').
//!
//! `nl_jit_cons_make' moved to Phase 47 elisp (lisp/nelisp-cc-jit-cons-make.el)
//! using the `cons-make' opcode: alloc NlConsBox, raw 32-byte copy from
//! *a / *b into box->car / box->cdr, write Sexp::Cons(box) to *out,
//! return i64 = 0.
//! `nl_cons_{car,cdr}_ptr' are narrow helpers
//! reached from the elisp bodies via `extern-call' (= `*box_ptr()' field
//! address for `nl_sexp_clone_into' to copy refcount-safely).
//!
//! All five `jit/cons.rs' trampolines have been moved to Phase 47 elisp:
//!
//!   `nl_jit_cons_make'    -> lisp/nelisp-cc-jit-cons-make.el
//!   `nl_jit_cons_car'     -> lisp/nelisp-cc-jit-cons.el
//!   `nl_jit_cons_cdr'     -> lisp/nelisp-cc-jit-cons.el
//!   `nl_jit_cons_setcar'  -> lisp/nelisp-cc-jit-cons.el
//!   `nl_jit_cons_setcdr'  -> lisp/nelisp-cc-jit-cons.el
//!   `nl_cons_car_ptr'     -> lisp/nelisp-cc-jit-cons-car-ptr.el
//!   `nl_cons_cdr_ptr'     -> lisp/nelisp-cc-jit-cons-cdr-ptr.el
