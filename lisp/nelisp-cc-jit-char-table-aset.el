;;; nelisp-cc-jit-char-table-aset.el --- Doc 120.B char-table-aset trampoline  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 47 replacement for the Rust `nl_jit_char_table_aset' trampoline
;; in `build-tool/src/jit/box_accessor.rs' (deleted via this file).
;;
;; Function contract (`(*const Sexp, i64, *const Sexp, *mut Sexp) -> i64'):
;;   arg: *const Sexp  — CharTable to mutate (tag must = 9).
;;   idx: i64          — character code (key) to set.
;;   val: *const Sexp  — value Sexp to store.
;;   out: *mut Sexp    — receives a clone of *val on success.
;;   returns: i64 = 0 (TRAMPOLINE_OK) on success, 1 (TRAMPOLINE_ERR) on error.
;;
;; Implementation:
;;   1. Guard: sexp-tag arg == 9 (Sexp::CharTable).  Else ERR.
;;   2. Delegate to `nl_char_table_set_raw' Rust extern (defined in
;;      `build-tool/src/eval/nlchartable.rs') which calls
;;      `char_table_set_one(inner, idx, (*val).clone())' via
;;      `with_inner_mut' and echoes `*val' into `*out'.
;;
;; Tag constant: 9 = `nelisp-sexp--tag-char-table' (nelisp-sexp-layout.el).
;;
;; Linker wiring: `bridge.rs' declares `fn nl_jit_char_table_aset()' in
;; the `extern "C"' block and adds it to `_ELISP_ARCHIVE_ANCHOR' so the
;; linker pulls this .o into the binary and `dlsym(RTLD_DEFAULT, ...)'
;; resolves the symbol for `nl-jit-call-out-2i' dispatch from
;; `nelisp-jit-strategy.el'.

;;; Code:

(defconst nelisp-cc-jit-char-table-aset--source
  '(defun nl_jit_char_table_aset (arg idx val out)
     ;; arg: *const Sexp.  idx: i64.  val: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK, 1 on ERR.
     ;;
     ;; sexp-tag = 9 guards Sexp::CharTable; mutation (entries Vec update
     ;; + val echo to *out) is handled by `nl_char_table_set_raw' Rust
     ;; extern which cannot be expressed in Phase 47 grammar.
     (if (= (sexp-tag arg) 9)
         (extern-call nl_char_table_set_raw arg idx val out)
       1))
  "Phase 47 source for the `nl_jit_char_table_aset' swap (Doc 120.B residual).

Guard: sexp-tag == 9 (CharTable); delegates to `nl_char_table_set_raw'
Rust extern for the actual `char_table_set_one' mutation + val echo.
Returns the extern's i64 result directly.

Linker: `bridge.rs::_ELISP_ARCHIVE_ANCHOR' includes `nl_jit_char_table_aset'
so `dlsym(RTLD_DEFAULT, ...)' resolves at runtime for `nl-jit-call-out-2i'
dispatch in `nelisp-jit-strategy.el'.")

(provide 'nelisp-cc-jit-char-table-aset)

;;; nelisp-cc-jit-char-table-aset.el ends here
