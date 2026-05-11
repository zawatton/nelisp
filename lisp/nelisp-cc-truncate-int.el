;;; nelisp-cc-truncate-int.el --- Doc 100 §100.C bi_truncate Int swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 100 v2 §100.C — the first real swap of a Rust algorithm line
;; into an elisp `.o' linked into `bin/nelisp'.  The Rust builtin
;; `bi_truncate''s Int arm (= `Sexp::Int(n) => Ok(Sexp::Int(*n))') is
;; rewritten to delegate to the function below; the algorithmic body
;; (= read tag byte + read i64 payload + write same payload into a
;; caller-owned slot) lives nowhere except this elisp source.
;;
;; Function contract:
;;   arg0:        *const Sexp pointing at a Sexp::Int(n) variant.
;;   result-slot: *mut Sexp pointing at a 32-byte caller-owned slot.
;;   returns:     result-slot (= same pointer, for ergonomics).
;;
;; The Rust side enforces the input's `Sexp::Int' tag *before* calling;
;; this body assumes the precondition (= no `sexp-tag' check needed
;; before `sexp-int-unwrap').  Doc 101+ swaps that route through here
;; for non-Int variants must add the tag check themselves.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' lists this feature
;; in its manifest; `build-tool/build.rs::link_elisp_cc_spike' compiles
;; the source into `nelisp_truncate_int.o' and archives it into the
;; static lib cargo links against the crate.

;;; Code:

(defconst nelisp-cc-truncate-int--source
  '(defun nelisp_truncate_int (arg0 result-slot)
     (sexp-int-make result-slot (sexp-int-unwrap arg0)))
  "Phase 47 source for the §100.C `bi_truncate' Int swap.

Two-argument function — Phase 47's SysV AMD64 prologue spills the
first arg (`arg0' = `*const Sexp') into the rbp-relative slot 0 and
the second arg (`result-slot' = `*mut Sexp') into slot 1.  The body
is one composed value form: read the i64 payload of `*arg0' via
`sexp-int-unwrap' and write a fresh `Sexp::Int' with the same
payload into `result-slot' via `sexp-int-make'.

`sexp-int-make' returns the slot pointer in rax, which is also the
function's return value.  The Rust caller already holds the same
pointer (= the &mut into the stack-local slot), so the returned
value is discarded by the shim in `build-tool/src/eval/builtins.rs'.

No allocation happens in this `.o'.  Memory accesses are all
register-indirect with immediate `disp8' (= offset 8), so the
emitted `.text' contains no relocations.")

(provide 'nelisp-cc-truncate-int)

;;; nelisp-cc-truncate-int.el ends here
