;;; nelisp-cc-jit-char-table-aref.el --- Doc 120.B char-table-aref trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the Rust `nl_jit_char_table_aref' trampoline
;; in `build-tool/src/jit/box_accessor.rs' (deleted via this file).
;;
;; Function contract (`(*const Sexp, i64, *mut Sexp) -> i64'):
;;   arg: *const Sexp  — input Sexp, must be CharTable (tag = 9).
;;   idx: i64          — character code to look up.
;;   out: *mut Sexp    — result slot for the mapped Sexp.
;;   returns: i64 = 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on error.
;;
;; Implementation:
;;   1. Guard: sexp-tag arg == 9 (Sexp::CharTable).  Else ERR.
;;   2. Delegate to `nl_char_table_get_raw' Rust extern (defined in
;;      `build-tool/src/eval/nlchartable.rs') which calls
;;      `char_table_get(r, idx)' — handles parent-table lookup and
;;      default_val fallback internally.  Writes result to *out.
;;   3. The outer guard tag-check is expressed in Phase 47 elisp so
;;      that the public `nl_jit_char_table_aref' symbol is provided
;;      by this .o, not by a Rust `#[no_mangle]' body.
;;
;; Tag constant: 9 = `nelisp-sexp--tag-char-table' (nelisp-sexp-layout.el).
;;
;; Linker wiring: `bridge.rs' declares `fn nl_jit_char_table_aref()' in
;; the `extern "C"' block and adds it to `_ELISP_ARCHIVE_ANCHOR' so the
;; linker pulls this .o into the binary and `dlsym(RTLD_DEFAULT, ...)'
;; resolves the symbol for `nl-jit-call-out-1i' dispatch from
;; `nelisp-jit-strategy.el'.

;;; Code:

(defconst nelisp-cc-jit-char-table-aref--source
  '(defun nl_jit_char_table_aref (arg idx out)
     ;; arg: *const Sexp.  idx: i64 (char code).  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK, 1 on ERR.
     ;;
     ;; sexp-tag = 9 guards Sexp::CharTable; the raw lookup (parent
     ;; recursion + default_val) is handled by the Rust extern
     ;; `nl_char_table_get_raw' which cannot yet be expressed in
     ;; Phase 47 grammar (no Vec<(i64,Sexp)> iteration op).
     (if (= (sexp-tag arg) 9)
         (extern-call nl_char_table_get_raw arg idx out)
       1))
  "Phase 47 source for the `nl_jit_char_table_aref' swap (Doc 120.B residual).

Guard: sexp-tag == 9 (CharTable); delegates to `nl_char_table_get_raw'
Rust extern for the actual `char_table_get' lookup (parent recursion +
default_val semantics).  Returns the extern's i64 result directly.

Linker: `bridge.rs::_ELISP_ARCHIVE_ANCHOR' includes `nl_jit_char_table_aref'
so `dlsym(RTLD_DEFAULT, ...)' resolves at runtime for `nl-jit-call-out-1i'
dispatch in `nelisp-jit-strategy.el'.")

(provide 'nelisp-cc-jit-char-table-aref)

;;; nelisp-cc-jit-char-table-aref.el ends here
