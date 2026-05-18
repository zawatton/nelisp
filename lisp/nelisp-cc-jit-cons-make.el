;;; nelisp-cc-jit-cons-make.el --- Phase 47 nl_jit_cons_make swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the last Rust trampoline in
;; `build-tool/src/jit/cons.rs': `nl_jit_cons_make'.
;;
;; The Rust body was:
;;
;;   pub unsafe extern "C" fn nl_jit_cons_make(
;;       a: *const Sexp, b: *const Sexp, out: *mut Sexp,
;;   ) -> i64 {
;;       let car_owned = (*a).clone();
;;       let cdr_owned = (*b).clone();
;;       crate::elisp_cc_spike::cons_construct(
;;           &car_owned as *const Sexp, &cdr_owned as *const Sexp, out);
;;       std::mem::forget(car_owned);
;;       std::mem::forget(cdr_owned);
;;       TRAMPOLINE_OK   // = 0
;;   }
;;
;; The clone + mem::forget pattern in the Rust body was a workaround
;; to pass borrowed pointers to `nelisp_cons_construct' (which takes
;; `*const Sexp').  Since `bi_nl_jit_call_out_2' in bridge.rs already
;; passes `&args[1] as *const Sexp' and `&args[2] as *const Sexp'
;; (= stable stack pointers), we can forward those directly to the
;; Phase 47 `cons-make' opcode which does the same raw 32-byte copy
;; from `*car_ptr' / `*cdr_ptr' into a fresh `NlConsBox'.
;;
;; ABI: `(*const Sexp, *const Sexp, *mut Sexp) -> i64'
;;   a   — *const Sexp pointing at the car value.
;;   b   — *const Sexp pointing at the cdr value.
;;   out — *mut Sexp result slot.
;;   Returns: i64 = 0 (TRAMPOLINE_OK).
;;
;; `cons-make' opcode semantics (= `emit-cons-make' in
;; `nelisp-phase47-compiler.el'):
;;   1. Evaluates car-ptr / cdr-ptr / slot expressions (= our three args).
;;   2. Calls `nl_alloc_consbox()' for a fresh `NlConsBox' (rc=1, nil/nil).
;;   3. Raw-copies 32 bytes from `*car-ptr' into `box->car' and 32 bytes
;;      from `*cdr-ptr' into `box->cdr' via `movdqu' pairs.
;;   4. Writes `SEXP_TAG_CONS' (= 7) + the box pointer into `*out'.
;;   5. Returns the slot pointer in rax.
;;
;; The `(and (cons-make …) 0)' idiom discards the non-zero slot-pointer
;; return from `cons-make' and returns the required i64 = 0, following
;; the same pattern used by `nl_jit_cons_car' / `nl_jit_cons_cdr' in
;; `nelisp-cc-jit-cons.el'.
;;
;; MVP refcount note: `cons-make' byte-copies the 32-byte Sexp payloads
;; without `nl_rc_inc' for nested boxed variants — same constraint as
;; the previous Rust trampoline (which also used a raw copy via
;; `cons_construct').  A future §101.D.2 stage can make both paths
;; refcount-aware if profiling justifies it.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; `build-tool/build.rs' source list.  The emitted `nl_jit_cons_make'
;; STT_FUNC symbol is resolved by `build-tool/src/jit/bridge.rs::alias'
;; under the existing `"nelisp_jit_cons"' arm (no alias change needed).

;;; Code:

(defconst nelisp-cc-jit-cons-make--source
  '(defun nl_jit_cons_make (a b out)
     ;; a:   *const Sexp — pointer to the car value.
     ;; b:   *const Sexp — pointer to the cdr value.
     ;; out: *mut Sexp   — caller-owned result slot.
     ;; Returns: i64 = 0 (TRAMPOLINE_OK always).
     ;;
     ;; `cons-make' allocates a fresh NlConsBox and raw-copies 32 bytes
     ;; from *a into box->car and 32 bytes from *b into box->cdr, then
     ;; writes Sexp::Cons(box) into *out.  The `(and … 0)' idiom drops
     ;; the non-zero slot-pointer return from `cons-make' and returns
     ;; the required i64 = 0.
     (and (cons-make a b out) 0))
  "Phase 47 source for `nl_jit_cons_make' (jit/cons.rs → elisp).

Replaces the last Rust trampoline in `jit/cons.rs'.  Uses the
`cons-make' Phase 47 opcode to allocate a fresh `NlConsBox', raw-copy
32 bytes from each input pointer into the box fields, and write
`Sexp::Cons(box)' into the output slot.  Returns i64 = 0
(TRAMPOLINE_OK) via the `(and SIDE-EFFECT 0)' idiom.")

(provide 'nelisp-cc-jit-cons-make)

;;; nelisp-cc-jit-cons-make.el ends here
