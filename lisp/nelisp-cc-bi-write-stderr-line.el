;;; nelisp-cc-bi-write-stderr-line.el --- Doc 117 §117.B I/O syscall sweep (write-stderr-line)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.B / Doc 122 §122.H — moves the byte-write algorithmic
;; body of the `(nelisp--write-stderr-line STR)' builtin from Rust
;; (`build-tool/src/eval/builtins.rs::bi_write_stderr_line') into a
;; AOT elisp object.  The Rust shim keeps:
;;
;;   * arity validation                      (1 arg)
;;   * `WrongType' dispatch                  (`Sexp::Str' / `Sexp::MutStr')
;;   * the trailing newline + final flush    (= behaviour parity with the
;;                                              pre-swap `writeln!')
;;   * `Sexp::T' / `args[0].clone()' return  (behaviour parity)
;;
;; The elisp body's job is the *string-body write* — issue a single
;; `write(2, bytes, len)' syscall against fd 2 (stderr).  No buffering,
;; no flushing, no error inspection beyond the i64 return — the Rust
;; shim layer handles trailing-newline emission and best-effort flush
;; semantics matching `writeln!' + `err.flush()'.
;;
;; This is the first I/O syscall sweep that consumes the Doc 122 §122.H
;; `str-bytes-ptr' grammar op (= outward `Sexp::Str' → `*const u8'
;; dispatch via the Rust `nl_str_bytes_ptr' extern).  AOT's
;; pre-§122.H `str-bytes' op is layout-coupled and only worked on the
;; inline-String `Sexp::Str' / `Sexp::Symbol' variants; the `str-bytes-
;; ptr' op covers `Sexp::MutStr' too which makes it usable for the full
;; Tier B sweep without a per-variant tag-check upstream.
;;
;; AOT ops consumed:
;;   §122.H  `str-bytes-ptr'  — `*const u8' data pointer of a string-y
;;                              Sexp (via Rust `nl_str_bytes_ptr' extern).
;;   §101.C  `str-len'        — `String::len' byte count (= `mov rax,
;;                              qword ptr [rdi + 24]' inline read).
;;   §100.A  `extern-call'    — 3-arg libc call to `write(2, ...)`.
;;
;; Function contract:
;;   str-ptr: *const Sexp pointing at a string-y `Sexp' (Str / Symbol /
;;            MutStr).  The Rust shim already validated the tag.
;;   returns: i64 — the libc `write(2)' return value.  `>= 0' = bytes
;;            written, `-1' = errno set (= the Rust shim discards both
;;            since the pre-swap body used `let _ = writeln!()' so EIO /
;;            EPIPE were already silently dropped).
;;
;; Per Doc 117 §4.3: pure migration — no behaviour change.  The Rust
;; shim preserves arity + WrongType + flush + newline; only the
;; algorithmic `out.write_all(s.as_bytes())' moves into elisp.

;;; Code:

(defconst nelisp-cc-bi-write-stderr-line--source
  '(defun nelisp_bi_write_stderr_line (str-ptr)
     ;; str-ptr: *const Sexp — caller-validated `Sexp::Str' /
     ;; `Sexp::Symbol' / `Sexp::MutStr'.
     ;;
     ;; Single libc `write(2, bytes_ptr, len)' syscall: fd 2 = stderr,
     ;; bytes_ptr via §122.H `str-bytes-ptr' (= Rust `nl_str_bytes_ptr'
     ;; extern), len via §101.C `str-len' (= inline `mov rax, [rdi+24]'
     ;; read of the `String' header's `length' field).
     ;;
     ;; Returns the `write(2)' i64 (= bytes written, or -1 on error).
     ;; The Rust shim discards this value — the pre-swap `writeln!'
     ;; body silently ignored errors via `let _ = ...' so the new
     ;; dispatch keeps the same observable error-suppression.
     (extern-call write 2 (str-bytes-ptr str-ptr) (str-len str-ptr)))
  "AOT source for the Doc 117 §117.B / Doc 122 §122.H
`(nelisp--write-stderr-line STR)' algorithmic body swap.

Single-arg function — AOT's SysV AMD64 prologue spills
`str-ptr' (= `*const Sexp' to a `Sexp::Str' / `Sexp::Symbol' /
`Sexp::MutStr') into the rbp-relative slot 0.  The body is one
composed value form: a 3-arg `extern-call' to libc `write' with

  arg0 = 2                          (fd, stderr)
  arg1 = (str-bytes-ptr str-ptr)    `*const u8' data pointer
  arg2 = (str-len str-ptr)          byte count (= `String::len')

`extern-call' marshals (2, *const u8, i64) into (rdi, rsi, rdx) per
SysV AMD64 and emits `call write@PLT'.  The libc `write(2)' return
value lands in rax which becomes the function return (= the Rust
shim discards it, matching the pre-swap `let _ = writeln!()'
error-suppression).

The trailing newline + best-effort flush stay in the Rust shim so
the elisp body is exactly the algorithmic core (= the bytes the
user payload contributes).  This split mirrors `bi_set_quit_flag'
(Doc 117 §117.B): elisp owns the kernel operation, Rust owns the
caller-side bookkeeping that cannot be expressed inside AOT's
current I/O grammar surface.

No allocation, no Rust helpers beyond `nl_str_bytes_ptr' (Doc 122
§122.H) + the libc `write' PLT entry.")

(provide 'nelisp-cc-bi-write-stderr-line)

;;; nelisp-cc-bi-write-stderr-line.el ends here
