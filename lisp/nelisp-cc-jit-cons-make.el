;;; nelisp-cc-jit-cons-make.el --- Doc 120.E nl_jit_cons_make pure-elisp swap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120.E — replaces the last remaining trampoline in
;; `build-tool/src/jit/cons.rs' (`nl_jit_cons_make') with a Phase
;; 47 elisp object.  The four sibling trampolines (`car' / `cdr' /
;; `setcar' / `setcdr') were already migrated in §120.C; the
;; constructor was deferred because it required a way to materialize
;; refcount-correct clones of `*car-ptr' / `*cdr-ptr' into the
;; freshly-allocated `NlConsBox' before tagging the result as
;; `Sexp::Cons(_)' in the caller's out slot.
;;
;; The §120.C comment "no local-slot allocation primitive yet" blocker
;; is resolved by adding a fused AOT grammar op,
;; `(cons-make-with-clone CAR-PTR CDR-PTR SLOT)', whose emitter handles
;; the full sequence in one composed value form: `nl_alloc_consbox' →
;; `nl_sexp_clone_into' twice (car / cdr) → tag byte + payload pointer
;; into SLOT.  All intermediate values live in registers + the stack
;; spill area that the emitter manages directly, so the .o source itself
;; never needs to bind the box pointer to a let variable.
;;
;; Function contract:
;;   a    *const Sexp — caller-owned car value, will be deep-cloned.
;;   b    *const Sexp — caller-owned cdr value, will be deep-cloned.
;;   out  *mut Sexp 32-byte slot — receives `Sexp::Cons(box)' after
;;                                 the cloned pair is installed.
;;   returns          i64 = 0 (TRAMPOLINE_OK).
;;
;; AOT ops consumed:
;;   `(cons-make-with-clone CAR-PTR CDR-PTR SLOT)' — fused box-alloc +
;;        two deep clones + tag/payload write.  Doc 120.E grammar
;;        extension shipped alongside this .o file.
;;   `(and EXPR ... INT)' — short-circuit value chain returning INT
;;        when every EXPR was non-Nil (= existing convention used by
;;        the §120.A / §120.C trampolines).
;;
;; Net Rust impact: deleting `build-tool/src/jit/cons.rs:9-20'
;; (= `nl_jit_cons_make' body, 12 LOC plus the surrounding `unsafe extern'
;; declaration and `mem::forget' pair, ~24 LOC of file content).  The
;; deletion satisfies the "Rust LOC never increases" hard rule:
;; G3 ships only elisp source plus a one-line `cc_wrap!' entry in
;; `lib.rs' (no new Rust extern declarations — `nl_alloc_consbox' and
;; `nl_sexp_clone_into' already exist for §120.C).

;;; Code:

(defconst nelisp-cc-jit-cons-make--source
  '(defun nelisp_jit_cons_make (a b out)
     ;; a:   *const Sexp — caller-owned car.
     ;; b:   *const Sexp — caller-owned cdr.
     ;; out: *mut Sexp   — destination Sexp slot (uninit on entry).
     ;;
     ;; The fused `cons-make-with-clone' grammar op (Doc 120.E) does
     ;; everything we need in one composed value form:
     ;;   1. call `nl_alloc_consbox' to allocate a fresh `NlConsBox'
     ;;      (car/cdr = Nil, refcount = 1),
     ;;   2. `nl_sexp_clone_into(*a, &box->car)' (= refcount-aware deep
     ;;      clone, safe to overwrite the pre-initialised Nil slot),
     ;;   3. `nl_sexp_clone_into(*b, &box->cdr)',
     ;;   4. write `Sexp::Cons(box)' into `*out' (tag byte + payload
     ;;      pointer).
     ;; `(and ... 0)' threads the side-effecting constructor through
     ;; to a stable `0' return value (= `TRAMPOLINE_OK', per the §120.A
     ;; / §120.C convention).  The constructor returns `out' in rax so
     ;; the `and' chain is non-Nil on success and short-circuits to the
     ;; final `0' that the bridge inspects.
     (and (cons-make-with-clone a b out) 0))
  "AOT source for the Doc 120.E `nl_jit_cons_make' swap.

Three-argument function — AOT's SysV AMD64 prologue spills
the three `*const Sexp' / `*mut Sexp' arguments into rbp-relative
slots 0..2.  The body binds `box' to a fresh `NlConsBox*' (= the
return value of `nl_alloc_consbox' which gives back a 72-byte
heap allocation with car/cdr = Nil and refcount = 1) and threads
the clone + tag-write + payload-write sequence through a top-level
`seq'.

Clone discipline: `nl_sexp_clone_into' performs the variant-aware
deep clone (= `(*src).clone()` then `core::ptr::write(dst, cloned)'
in `eval/sexp.rs:109') matching Rust's `Sexp::clone' for every
variant — `String::clone' for Symbol/Str, refcount bump for the
NlBox variants, plain copy for Int / Nil / T / Float.  Writing
directly into the box fields (rather than via `cons-make') avoids
needing a stack-local Sexp slot to hold an intermediate clone.

Refcount accounting: the fresh box has refcount = 1 (matches the
single `Sexp::Cons(box)' we install in `*out').  No additional
inc/dec is required at this layer; downstream consumers of `*out'
manage that via existing `Sexp::Drop' / `Sexp::Clone' paths.

Net Rust impact: -24 LOC after `jit/cons.rs' is deleted in the
follow-up commit (= same change set that wires `nelisp_jit_cons_make'
through `lib.rs' `cc_wrap!' and replaces the `bridge.rs' entry).")

(provide 'nelisp-cc-jit-cons-make)

;;; nelisp-cc-jit-cons-make.el ends here
