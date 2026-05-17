;;; nelisp-cc-bi-read-stdin-bytes.el --- Doc 117 §117.B I/O syscall sweep (read-stdin-bytes)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.B — moves the byte-read algorithmic body of the
;; `(read-stdin-bytes LIMIT)' builtin from Rust
;; (`build-tool/src/eval/builtins.rs::bi_read_stdin_bytes') into a
;; Phase 47 elisp object.  The Rust shim keeps:
;;
;;   * arity validation              (1 arg)
;;   * positive-integer dispatch     (`Sexp::Int(n)' with `n > 0')
;;   * destination buffer allocation (`Vec<u8>` of size limit)
;;   * EOF → `Sexp::Nil' / err → `EvalError::Internal' branch
;;   * `String::from_utf8_lossy' wrap to `Sexp::Str' on the read bytes
;;
;; The elisp body's job is the *syscall itself* — issue a single
;; `read(0, buf, limit)' libc call against fd 0 (stdin) and return
;; the i64 byte count (or -1 on error).  Rust handles before / after.
;;
;; This swap is symmetric with the §117.B write-side handlers but
;; flipped — for write-stdout/stderr the elisp body has the bytes
;; (`str-bytes-ptr' / `str-len') and writes them out; for
;; read-stdin the buffer is caller-owned (Rust allocates), the elisp
;; body receives `(buf_ptr, limit)' as scalar args and returns the
;; byte count for Rust to truncate + wrap.
;;
;; Why split this way: `from_utf8_lossy' requires a fallible UTF-8
;; check that Phase 47's `sexp-write-str' grammar op (= §122.A,
;; `nl_alloc_str' via `from_utf8_unchecked') can NOT express today.
;; A future §122.X `sexp-write-str-lossy' op would let the entire
;; body migrate; pending that, the syscall itself is the highest-
;; leverage piece to migrate (= matches the §117.B Tier B sweep
;; criterion of "the libc / syscall line, not the validation
;; bookkeeping").
;;
;; Phase 47 ops consumed:
;;   §100.A  `extern-call'    — 3-arg libc call to `read(0, ...)`.
;;
;; Function contract:
;;   buf-ptr: *mut u8 — Rust-owned destination buffer of `limit'
;;            initialised bytes (`Vec<u8>` zero-init).
;;   limit:   i64    — > 0, fits in usize.
;;   returns: i64    — libc `read(2)' return value.
;;                       `> 0' = bytes received (== n).
;;                       `0'   = EOF (peer closed stdin).
;;                       `-1'  = errno set (Rust shim → Internal err).
;;
;; Per Doc 117 §4.3: pure migration — no behaviour change visible to
;; the caller.

;;; Code:

(defconst nelisp-cc-bi-read-stdin-bytes--source
  '(defun nelisp_bi_read_stdin_bytes (buf-ptr limit)
     ;; buf-ptr: *mut u8 — Rust-owned, zero-initialised, `limit' bytes.
     ;; limit:   i64    — > 0, caller already validated.
     ;;
     ;; Single libc `read(0, buf_ptr, limit)' syscall: fd 0 = stdin,
     ;; buf_ptr passed through as the destination, limit clamped to the
     ;; size of the destination buffer.
     ;;
     ;; Returns the `read(2)' i64 (= bytes read, 0 = EOF, -1 = errno).
     (extern-call read 0 buf-ptr limit))
  "Phase 47 source for the Doc 117 §117.B `(read-stdin-bytes LIMIT)' swap.

Two-arg function — Phase 47's SysV AMD64 prologue spills `buf-ptr'
(= `*mut u8' destination, Rust-allocated `Vec<u8>`'s `as_mut_ptr()')
into the rbp-relative slot 0 and `limit' (= i64) into slot 1.  The
body is one composed value form: a 3-arg `extern-call' to libc
`read' with

  arg0 = 0                          (fd, stdin)
  arg1 = buf-ptr                    `*mut u8' destination
  arg2 = limit                      byte count cap (= LIMIT i64)

`extern-call' marshals (0, *mut u8, i64) into (rdi, rsi, rdx) per
SysV AMD64 and emits `call read@PLT'.  The libc `read(2)' return
value lands in rax which becomes the function return.  The Rust
shim inspects it: 0 → `Sexp::Nil', >0 → truncate buf + `from_utf8_
lossy' → `Sexp::Str', <0 → `EvalError::Internal'.

Twin of §122.H `nelisp-cc-bi-write-stderr-line' modulo direction.
The shape (= one `extern-call' to a libc fd-syscall) is identical;
only (fd, op) differ.

No allocation, no Rust helpers beyond the libc `read' PLT entry.")

(provide 'nelisp-cc-bi-read-stdin-bytes)

;;; nelisp-cc-bi-read-stdin-bytes.el ends here
