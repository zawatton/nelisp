;;; nelisp-cc-sexp-write-str.el --- Doc 122 §122.A grammar op probes  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.A introduces two new AOT grammar ops:
;;
;;   (sexp-write-str SLOT BYTES-PTR LEN)
;;     — Call `nl_alloc_str(bytes_ptr, len, slot)' which copies LEN
;;       bytes from BYTES-PTR into a fresh `String', wraps it as
;;       `Sexp::Str(s)', and writes that 40-byte Sexp value into the
;;       caller-owned SLOT.  Returns SLOT in rax.
;;
;;   (sexp-write-symbol SLOT BYTES-PTR LEN)
;;     — Same shape with `nl_alloc_symbol' instead — produces
;;       `Sexp::Symbol(s)' (no intern-table consult — see Doc 122 §5).
;;
;; This file packages each op as a standalone AOT-compiled
;; `defun' so the `tests/elisp_cc_sexp_write_str_probe.rs' integration
;; test can probe the round-trip end-to-end without any user-visible
;; swap.  Pattern mirrors `nelisp-cc-cell-ops.el' for §111.D's
;; cell-make / cell-value sibling probes.
;;
;; The two `defun's match the extern declarations in
;; `build-tool/src/lib.rs::elisp_cc_spike' and the manifest entries in
;; `scripts/compile-elisp-objects.el'.

;;; Code:

(defconst nelisp-cc-sexp-write-str--str-source
  '(defun nelisp_sexp_write_str (slot bytes-ptr len)
     ;; slot:       *mut Sexp — 40-byte slot to receive `Sexp::Str(_)'.
     ;; bytes-ptr:  *const u8 — start of the UTF-8 byte range.
     ;; len:        i64       — byte count (>= 0).
     ;;
     ;; The op evaluates the three args, marshals them to rdi/rsi/rdx
     ;; per SysV AMD64, and calls the `nl_alloc_str' extern which
     ;; copies LEN bytes from BYTES-PTR into a fresh String and writes
     ;; `Sexp::Str(s)' into `*slot'.  The extern returns SLOT in rax;
     ;; this op's body is just the grammar form (no extra ops needed).
     (sexp-write-str slot bytes-ptr len))
  "AOT source for the Doc 122 §122.A `sexp-write-str' op probe.")

(defconst nelisp-cc-sexp-write-str--symbol-source
  '(defun nelisp_sexp_write_symbol (slot bytes-ptr len)
     ;; slot:       *mut Sexp — 40-byte slot to receive `Sexp::Symbol(_)'.
     ;; bytes-ptr:  *const u8 — start of the UTF-8 byte range.
     ;; len:        i64       — byte count (>= 0).
     ;;
     ;; Same shape as `nelisp_sexp_write_str' but produces a
     ;; `Sexp::Symbol(_)' value via the `nl_alloc_symbol' extern.
     ;; Does NOT consult any intern table — caller is responsible for
     ;; symbol identity when needed (Doc 122 §5 open question).
     (sexp-write-symbol slot bytes-ptr len))
  "AOT source for the Doc 122 §122.A `sexp-write-symbol' op probe.")

(defconst nelisp-cc-jit-intern--source
  '(defun nl_jit_intern (arg out)
     ;; arg: *const Sexp.  out: *mut Sexp.
     ;; Returns: i64 = 0 on OK (= Str / MutStr -> Symbol), 1 on ERR.
     (if (= (sexp-tag arg) 5)
         (and (sexp-write-symbol out (str-bytes-ptr arg) (str-len arg)) 0)
       (if (= (sexp-tag arg) 6)
           (and (sexp-write-symbol out (str-bytes-ptr arg) (str-len arg)) 0)
         1)))
  "AOT source for the `nl_jit_intern' trampoline.

Reuses the existing `sexp-write-symbol' allocator op plus the
`str-bytes-ptr' / `str-len' string-view ops.  Keeps the original
contract exactly: `Str' and `MutStr' succeed, everything else
returns TRAMPOLINE_ERR.")

(provide 'nelisp-cc-sexp-write-str)

;;; nelisp-cc-sexp-write-str.el ends here
