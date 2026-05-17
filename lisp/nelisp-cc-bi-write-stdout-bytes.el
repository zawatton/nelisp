;;; nelisp-cc-bi-write-stdout-bytes.el --- Doc 117 §117.B I/O syscall sweep (write-stdout-bytes)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.B / Doc 122 §122.H — moves the byte-write algorithmic
;; body of the `(nelisp--write-stdout-bytes STR)' builtin from Rust
;; (`build-tool/src/eval/builtins.rs::bi_write_stdout_bytes') into a
;; Phase 47 elisp object.  The Rust shim keeps:
;;
;;   * arity validation                      (1 arg)
;;   * `WrongType' dispatch                  (`Sexp::Str' / `Sexp::MutStr')
;;   * the trailing flush                    (= behaviour parity with the
;;                                              pre-swap `out.flush()')
;;   * `Sexp' return value                   (`args[0].clone()')
;;
;; The elisp body's job is the *string-body write* — issue a single
;; `write(1, bytes, len)' syscall against fd 1 (stdout).  No newline,
;; no buffering, no flushing, no error inspection beyond the i64
;; return — the Rust shim layer handles the best-effort flush
;; semantics matching `write_all' + `out.flush()'.
;;
;; Twin of §117.B `write-stderr-line' — same grammar shape modulo the
;; fd literal (1 instead of 2) and the trailing newline (none here,
;; this builtin is the `princ' building block).
;;
;; Phase 47 ops consumed:
;;   §122.H  `str-bytes-ptr'  — `*const u8' data pointer of a string-y
;;                              Sexp (via Rust `nl_str_bytes_ptr' extern).
;;   §101.C  `str-len'        — `String::len' byte count (= `mov rax,
;;                              qword ptr [rdi + 24]' inline read).
;;   §100.A  `extern-call'    — 3-arg libc call to `write(1, ...)`.
;;
;; Function contract:
;;   str-ptr: *const Sexp pointing at a string-y `Sexp' (Str / Symbol /
;;            MutStr).  The Rust shim already validated the tag.
;;   returns: i64 — the libc `write(2)' return value.  `>= 0' = bytes
;;            written, `-1' = errno set.  The Rust shim maps this to a
;;            best-effort `Internal' error if negative (= mirrors the
;;            pre-swap `map_err' on `write_all + flush' I/O errors),
;;            else discards the i64 and yields `args[0].clone()'.
;;
;; Per Doc 117 §4.3: pure migration — no behaviour change.  The Rust
;; shim preserves arity + WrongType + flush; only the algorithmic
;; `out.write_all(s.as_bytes())' moves into elisp.

;;; Code:

(defconst nelisp-cc-bi-write-stdout-bytes--source
  '(defun nelisp_bi_write_stdout_bytes (str-ptr)
     ;; str-ptr: *const Sexp — caller-validated `Sexp::Str' /
     ;; `Sexp::Symbol' / `Sexp::MutStr'.
     ;;
     ;; Single libc `write(1, bytes_ptr, len)' syscall: fd 1 = stdout,
     ;; bytes_ptr via §122.H `str-bytes-ptr' (= Rust `nl_str_bytes_ptr'
     ;; extern), len via §101.C `str-len' (= inline `mov rax, [rdi+24]'
     ;; read of the `String' header's `length' field).
     ;;
     ;; Returns the `write(2)' i64 (= bytes written, or -1 on error).
     (extern-call write 1 (str-bytes-ptr str-ptr) (str-len str-ptr)))
  "Phase 47 source for the Doc 117 §117.B / Doc 122 §122.H
`(nelisp--write-stdout-bytes STR)' algorithmic body swap.

Single-arg function — Phase 47's SysV AMD64 prologue spills
`str-ptr' (= `*const Sexp' to a `Sexp::Str' / `Sexp::Symbol' /
`Sexp::MutStr') into the rbp-relative slot 0.  The body is one
composed value form: a 3-arg `extern-call' to libc `write' with

  arg0 = 1                          (fd, stdout)
  arg1 = (str-bytes-ptr str-ptr)    `*const u8' data pointer
  arg2 = (str-len str-ptr)          byte count (= `String::len')

`extern-call' marshals (1, *const u8, i64) into (rdi, rsi, rdx) per
SysV AMD64 and emits `call write@PLT'.  The libc `write(2)' return
value lands in rax which becomes the function return; the Rust
shim inspects it and dispatches to `EvalError::Internal' on a
negative result.

Twin of `nelisp-cc-bi-write-stderr-line.el' modulo (fd, no
trailing newline).  No allocation, no Rust helpers beyond
`nl_str_bytes_ptr' (Doc 122 §122.H) + the libc `write' PLT entry.")

(provide 'nelisp-cc-bi-write-stdout-bytes)

;;; nelisp-cc-bi-write-stdout-bytes.el ends here
