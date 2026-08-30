;;; nelisp-cc-jit-mut-str-set-codepoint.el --- Doc 120.B mut-str-set-codepoint trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT definition of the `nl_jit_mut_str_set_codepoint' trampoline.
;;
;; Function contract (`(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64'):
;;   arg: *const Sexp  — MutStr/UnibyteMutStr to mutate in-place (tag 6/15).
;;   idx: i64          — char index (must be >= 0 and < char count).
;;   val: *const Sexp  — Sexp::Int holding the replacement codepoint.
;;   out: *mut Sexp    — receives Sexp::Int(cp) on success.
;;   returns: i64 = 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on error.
;;
;; Implementation:
;;   1. Guard: idx < 0 → ERR.
;;   2. Guard: sexp-tag arg is 6 or 15.  Else ERR.
;;   3. Guard: sexp-tag val == 2 (Sexp::Int).  Else ERR.
;;   4. Extract codepoint i64 from val at offset 8 via `ptr-read-u64'.
;;   5. Delegate to the separately AOT-lowered
;;      `nl_mut_str_set_codepoint_raw' symbol, which:
;;       - Enforces Doc 200's Emacs 31.1 fixed-width mutation rule,
;;       - Checks idx < character count,
;;       - Writes the one permitted byte without changing tag or byte length,
;;       - Writes `Sexp::Int(val_cp)' to `*out'.
;;
;; Tag constants:
;;   6 = `nelisp-sexp--tag-mut-str'; 15 = UnibyteMutStr.
;;   2 = `nelisp-sexp--tag-int'      (nelisp-sexp-layout.el)
;; `nelisp-sexp--offset-int-payload' = 8 (sexp-layout.el) — Int i64.
;;
;; Linker wiring keeps this AOT object in the runtime archive so the
;; `nl-jit-call-out-2i' dispatch in `nelisp-jit-strategy.el' can resolve it.

;;; Code:

(defconst nelisp-cc-jit-mut-str-set-codepoint--source
  '(defun nl_jit_mut_str_set_codepoint (arg idx val out)
     ;; arg: *const Sexp (MutStr).  idx: i64 (char index).
     ;; val: *const Sexp (Int codepoint).  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK, 1 on ERR.
     (if (< idx 0)
         1
       (if (or (= (sexp-tag arg) 6) (= (sexp-tag arg) 15))
           (if (= (sexp-tag val) 2)
               (extern-call nl_mut_str_set_codepoint_raw
                            arg
                            idx
                            (ptr-read-u64 val 8)
                            out)
             1)
         1)))
  "AOT source for the `nl_jit_mut_str_set_codepoint' swap (Doc 120.B residual).

Three guard layers: idx >= 0; sexp-tag arg is 6 (MutStr) or 15
(UnibyteMutStr); sexp-tag val == 2 (Int).  Extracts the codepoint i64 from
*val at offset 8 and delegates to the AOT-lowered
`nl_mut_str_set_codepoint_raw', which enforces the Emacs 31.1 fixed-width
rule and writes exactly one byte.  No mutation changes the tag or byte length.

The lower writes an Int-shaped Sexp carrying VAL-CP to *out on success.")

(provide 'nelisp-cc-jit-mut-str-set-codepoint)

;;; nelisp-cc-jit-mut-str-set-codepoint.el ends here
