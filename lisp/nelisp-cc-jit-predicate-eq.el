;;; nelisp-cc-jit-predicate-eq.el --- Doc 120 §120.A predicate-eq swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.A — Phase-47-compiled replacement for the Rust
;; `nl_jit_predicate_eq' trampoline in `build-tool/src/jit/predicate.rs'.
;; Same `(*const Sexp, *const Sexp) -> i64' contract:
;;
;;   1. Same-ref short-circuit (`a == b' → 1 regardless of variant).
;;   2. Tag-byte equality test (= different variants → 0 without
;;      entering the slow path).
;;   3. Int payload fast path (= matching `Sexp::Int' tags → compare
;;      i64 payloads at offset 8 directly).
;;   4. `nl_sexp_eq' slow path (= variant-specific equality through the
;;      `#[no_mangle] extern "C"' wrapper in `special_forms.rs').
;;
;; Phase 47 grammar pieces used:
;;   `(= a b)'          — i64 / ptr equality via `cmp + sete + movzx'.
;;   `(sexp-tag PTR)'   — read tag byte at offset 0 via `movzx'.
;;   `(sexp-int-unwrap PTR)' — read i64 payload at offset 8.
;;   `(extern-call ...)' — call into Rust extern for slow path.
;;
;; The slow-path `nl_sexp_eq' itself ports the rest of `sexp_eq's
;; variant-specific arms (Symbol-by-name via `nelisp_eq_symbol' /
;; Cons-by-Rc-ptr-eq / Str-by-content / Float-by-bits / ...); the
;; Phase 47 grammar does not yet expose `Rc::ptr_eq' for boxed
;; variants so the slow path stays in Rust for now.
;;
;; Build wiring: `scripts/compile-elisp-objects.el' manifest entry +
;; `build-tool/build.rs::link_elisp_cc_spike' source list.  The
;; emitted `nelisp_jit_predicate_eq' STT_FUNC symbol is resolved by
;; `build-tool/src/jit/bridge.rs::unified_fn_ptr' under the
;; `"nelisp_jit_eq_inline"' arm (= reached from
;; `lisp/nelisp-jit-strategy.el's `(nl-jit-call-ptr-ptr
;; "nelisp_jit_eq_inline" a b)').

;;; Code:

(defconst nelisp-cc-jit-predicate-eq--source
  '(defun nelisp_jit_predicate_eq (a b)
     ;; a, b: *const Sexp (= passed in rdi / rsi).
     ;; Returns: i64 = 1 iff `(eq a b)' else 0.
     (if (= a b)
         1
       (if (= (sexp-tag a) (sexp-tag b))
           (if (= (sexp-tag a) 2)
               (if (= (sexp-int-unwrap a) (sexp-int-unwrap b)) 1 0)
             (extern-call nl_sexp_eq a b))
         0)))
  "Phase 47 source for the §120.A `nl_jit_predicate_eq' swap.

Three fast paths (= same-ref short-circuit / tag-byte mismatch / Int
payload compare) inline; the slow variant-specific equality reaches
the `nl_sexp_eq' Rust extern via `extern-call'.  The tag-byte
constant `2' is `SEXP_TAG_INT' from `build-tool/src/eval/sexp.rs'
pinned by the §62.5 ABI assert tests.")

(provide 'nelisp-cc-jit-predicate-eq)

;;; nelisp-cc-jit-predicate-eq.el ends here
