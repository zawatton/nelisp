;;; nelisp-cc-jit-mut-str-set-codepoint.el --- Doc 120.B mut-str-set-codepoint trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT replacement for the Rust `nl_jit_mut_str_set_codepoint'
;; trampoline in `build-tool/src/jit/box_accessor.rs' (deleted via this file).
;;
;; Function contract (`(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64'):
;;   arg: *const Sexp  — MutStr to mutate in-place (tag must = 6).
;;   idx: i64          — char index (must be >= 0 and < char count).
;;   val: *const Sexp  — Sexp::Int holding the replacement codepoint.
;;   out: *mut Sexp    — receives Sexp::Int(cp) on success.
;;   returns: i64 = 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on error.
;;
;; Implementation:
;;   1. Guard: idx < 0 → ERR.
;;   2. Guard: sexp-tag arg == 6 (Sexp::MutStr).  Else ERR.
;;   3. Guard: sexp-tag val == 2 (Sexp::Int).  Else ERR.
;;   4. Extract codepoint i64 from val at offset 8 via `ptr-read-u64'.
;;   5. Delegate to `nl_mut_str_set_codepoint_raw' Rust extern (defined
;;      in `build-tool/src/eval/nlstr.rs') which:
;;       - Validates `char::from_u32(val_cp)' (surrogate / OOB → ERR),
;;       - Checks idx < char_count,
;;       - Rebuilds the String with the replacement codepoint via
;;         `rc.set_value(new_str)' (in-place, no grammar op available),
;;       - Writes `Sexp::Int(val_cp)' to `*out'.
;;
;; Tag constants:
;;   6 = `nelisp-sexp--tag-mut-str'  (nelisp-sexp-layout.el)
;;   2 = `nelisp-sexp--tag-int'      (nelisp-sexp-layout.el)
;; `nelisp-sexp--offset-int-payload' = 8 (sexp-layout.el) — Int i64.
;;
;; Linker wiring: `bridge.rs' declares `fn nl_jit_mut_str_set_codepoint()'
;; in the `extern "C"' block and adds it to `_ELISP_ARCHIVE_ANCHOR' so the
;; linker pulls this .o into the binary and `dlsym(RTLD_DEFAULT, ...)'
;; resolves the symbol for `nl-jit-call-out-2i' dispatch from
;; `nelisp-jit-strategy.el'.

;;; Code:

(defconst nelisp-cc-jit-mut-str-set-codepoint--source
  '(defun nl_jit_mut_str_set_codepoint (arg idx val out)
     ;; arg: *const Sexp (MutStr).  idx: i64 (char index).
     ;; val: *const Sexp (Int codepoint).  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK, 1 on ERR.
     (if (< idx 0)
         1
       (if (= (sexp-tag arg) 6)
           (if (= (sexp-tag val) 2)
               (extern-call nl_mut_str_set_codepoint_raw
                            arg
                            idx
                            (ptr-read-u64 val 8)
                            out)
             1)
         1)))
  "AOT source for the `nl_jit_mut_str_set_codepoint' swap (Doc 120.B residual).

Three guard layers: idx >= 0; sexp-tag arg == 6 (MutStr); sexp-tag val == 2
(Int).  Extracts the codepoint i64 from *val at offset 8 via `ptr-read-u64'
and delegates to `nl_mut_str_set_codepoint_raw' Rust extern which
rebuilds the String with the replacement codepoint via `NlStrRef::set_value'
(no AOT grammar op for in-place String content replace exists yet;
pending `mut-str-set-codepoint-at' from Doc 122 §1.2).

The extern writes `Sexp::Int(val_cp)' to *out on success, matching the
original Rust body (`*out = (*val).clone()' where *val is Sexp::Int).

Linker: `bridge.rs::_ELISP_ARCHIVE_ANCHOR' includes `nl_jit_mut_str_set_codepoint'
so `dlsym(RTLD_DEFAULT, ...)' resolves at runtime for `nl-jit-call-out-2i'
dispatch in `nelisp-jit-strategy.el'.")

(provide 'nelisp-cc-jit-mut-str-set-codepoint)

;;; nelisp-cc-jit-mut-str-set-codepoint.el ends here
