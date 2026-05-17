;;; nelisp-cc-jit-length.el --- Doc 120 §120.D length swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 120 §120.D — Phase-47-compiled replacement for the Rust
;; `nl_jit_access_length' trampoline in `build-tool/src/jit/access.rs'.
;; Same `(*const Sexp, *mut Sexp) -> i64' contract:
;;
;;   1. Nil arm:    `*out = Sexp::Int(0)' → TRAMPOLINE_OK (= 0).
;;   2. Vector arm: `*out = Sexp::Int(vec.value.len)' → OK.
;;   3. Str arm:    delegate to the narrow Rust extern
;;      `nl_jit_access_length_str_inner' which performs the
;;      `s.chars().count()' codepoint walk.  Phase 47 currently has
;;      no `(str-char-count H)' grammar op (= the existing `str-
;;      len' reads `String::len' = byte count, not codepoint count).
;;      See Doc 122 §122.A `mut-str-char-count' blocker cluster.
;;      Cannot degrade to ERR fallthrough: strategy.el's `string'
;;      arm routes `Sexp::Str' through `nelisp--mut-str-len' which
;;      errors on non-MutStr inputs (wrong-type 'mut-string).
;;   4. Anything else: ERR → strategy.el dispatches.
;;
;; The tag-byte constants `0' / `8' / `5' are `nelisp-sexp--tag-{nil,
;; vector,str}' from `lisp/nelisp-sexp-layout.el', pinned by the
;; §100.B ABI assert tests.

;;; Code:

(defconst nelisp-cc-jit-length--source
  '(defun nelisp_jit_length (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK (Nil / Vector / Str), 1 on ERR.
     (if (= (sexp-tag arg) 0)
         (and (sexp-int-make out 0) 0)
       (if (= (sexp-tag arg) 8)
           (and (sexp-int-make out (vector-len arg)) 0)
         (if (= (sexp-tag arg) 5)
             (extern-call nl_jit_access_length_str_inner arg out)
           1))))
  "Phase 47 source for the §120.D `nl_jit_access_length' swap.

Three guard arms: (1) Nil → write Int(0) inline, (2) Vector →
inline `vector-len' + `sexp-int-make' compose, (3) Str →
delegate to narrow `nl_jit_access_length_str_inner' Rust extern
(= UTF-8 codepoint count, kept in Rust until Phase 47 grows a
`str-char-count' grammar op).  Other variants return ERR;
strategy.el's `condition-case' dispatcher routes by `type-of'.

The `(and SIDE-EFFECT 0)' idiom returns 0 in rax after the
side-effect write — same convention as §120.A `nl_jit_ref_eq'
and §120.B `nl_jit_record_*' bodies.")

(provide 'nelisp-cc-jit-length)

;;; nelisp-cc-jit-length.el ends here
