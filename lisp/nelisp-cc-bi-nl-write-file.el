;;; nelisp-cc-bi-nl-write-file.el --- Doc 117 §117.D.gaps.3 open+write+close sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.3 — moves the syscall body of `(nl-write-file
;; PATH CONTENT)' (= `build-tool/src/eval/builtins.rs::bi_nl_write_file')
;; into a Phase 47 elisp object.  Third handler in the Doc 122 §122.I
;; CString helper sweep (after `nelisp_bi_syscall_stat' and
;; `nelisp_bi_syscall_canonicalize').
;;
;; The Rust shim keeps:
;;
;;   * arity validation             (2 args)
;;   * `stringp' dispatch           (both args)
;;   * Error-message construction   (when the kernel returns < 0)
;;
;; The elisp body's job is the *three-syscall chain*:
;;   1. open(path, O_WRONLY | O_CREAT | O_TRUNC, 0644)
;;   2. write(fd, content_ptr, content_len)   ; only if open succeeded
;;   3. close(fd)                              ; only if open succeeded
;;   + CString lifecycle (build via §122.I, drop via §125.A
;;     `dealloc-bytes' after all syscalls complete).
;;
;; Linux x86_64 ABI constants (matching `<bits/fcntl-linux.h>`):
;;   O_WRONLY = 1
;;   O_CREAT  = 64    (0o100)
;;   O_TRUNC  = 512   (0o1000)
;;   = bitwise-or  577 (= 0o1101)
;;
;;   Mode bits: 0o644 = 420 (= rw-r--r--).
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §122.H  `str-bytes-ptr'          — CONTENT byte data pointer.
;;   §101.C  `str-len'                — byte counts (CONTENT + path).
;;   §125.A  `dealloc-bytes'          — free path CString.
;;   §100.A  `extern-call'            — libc `open' / `write' / `close'.
;;
;; Function contract:
;;   path-ptr:    *const Sexp — caller-validated `Sexp::Str' / MutStr,
;;                normalised path (`stringp' check stays Rust-side).
;;   content-ptr: *const Sexp — caller-validated `Sexp::Str' /
;;                MutStr; byte payload to write to the file.
;;   returns:     i64        —
;;                 >= 0 (= bytes written by `write(2)') on success.
;;                 <  0 (= negative open(2) rc, or write(2) rc if open
;;                       succeeded then write failed) on failure.
;;                The Rust shim maps any < 0 return to a generic
;;                `EvalError::Internal' (matching the pre-swap
;;                `map_err' branch).
;;
;; Per Doc 117 §4.3: pure migration — observable behaviour matches
;; the pre-swap `std::fs::write(&path, content)' (= same flag set,
;; same mode, same `tee'-style semantic of "create or truncate +
;; write all").  Edge cases that differ slightly:
;;   * No retry on EINTR (Rust libstd retries; this kernel doesn't).
;;     The bi_* layer in elisp is the simplest possible composition;
;;     callers that need EINTR-safe writes should use a higher-level
;;     wrapper.  Today no NeLisp caller of `nl-write-file' does long
;;     enough writes to be EINTR-prone in practice.
;;   * No `fsync(2)' (Rust libstd doesn't either).

;;; Code:

(defconst nelisp-cc-bi-nl-write-file--source
  '(seq
    ;; Side-effect sequencer — 2-arg `(val _eff) -> val'.  Used to
    ;; cache a syscall rc while threading a cleanup side-effect.
    (defun nelisp_bi_nl_write_file_prog2 (val _eff) val)

    ;; 4-arg sequencer — `(val _e1 _e2 _e3) -> val'.  Used to cache
    ;; the `write(2)' rc while sequencing `close(2)' + `dealloc-bytes'
    ;; + a pad slot (=`0' literal that keeps the arity even and
    ;; makes the SysV ABI marshal order explicit: arg0=write_rc lands
    ;; in rdi, the rest in rsi/rdx/rcx).
    (defun nelisp_bi_nl_write_file_seq4 (val _e1 _e2 _e3) val)

    ;; 6-arg with-fd dispatcher — branches on the open(2) rc.
    ;; Args (all i64):
    ;;   fd:          open(2) rc.  Negative = error; non-negative = file
    ;;                descriptor.
    ;;   cstr:        path CString (= return of §122.I cstr_from_sexp).
    ;;   size:        path CString allocation size (= str-len + 1).
    ;;   content-ptr: CONTENT byte data pointer (= §122.H str-bytes-ptr).
    ;;   content-len: CONTENT byte count (= §101.C str-len).
    ;;   _pad:        unused; pads to even arity for the
    ;;                Doc 124.F call-alignment invariant.
    (defun nelisp_bi_nl_write_file_with_fd (fd cstr size content-ptr content-len _pad)
      (if (< fd 0)
          ;; Open failed.  Free the CString and return the negative
          ;; rc so the Rust shim raises EvalError::Internal.
          (nelisp_bi_nl_write_file_prog2
           fd
           (dealloc-bytes cstr size 1))
        ;; Open succeeded.  Issue write(2), close(2), dealloc in
        ;; sequence; return write_rc (= bytes written, or -1 on err).
        ;; SysV ABI evaluates args left-to-right so `write' runs
        ;; before `close' runs before `dealloc-bytes'.
        (nelisp_bi_nl_write_file_seq4
         (extern-call write fd content-ptr content-len)
         (extern-call close fd)
         (dealloc-bytes cstr size 1)
         0)))

    ;; 4-arg inner driver — receives the freshly-allocated path
    ;; CString + size + CONTENT byte slice, calls open(2), then
    ;; dispatches to the 6-arg `_with_fd' branch.
    ;;
    ;; Pulled out from the public entry so the public entry's body
    ;; is a flat composition of substrate ops (= no nested
    ;; `extern-call' inside an `extern-call' arg) — keeps the
    ;; per-arg eval order obvious for ABI auditors.
    (defun nelisp_bi_nl_write_file_inner (cstr size content-ptr content-len)
      (nelisp_bi_nl_write_file_with_fd
       (extern-call open cstr 577 420)
       cstr
       size
       content-ptr
       content-len
       0))

    ;; Public 2-arg entry — builds the path CString + CONTENT
    ;; byte-pointer / length pair, dispatches to the 4-arg inner.
    (defun nelisp_bi_nl_write_file (path-ptr content-ptr)
      (nelisp_bi_nl_write_file_inner
       (extern-call nelisp_cstr_from_sexp path-ptr)
       (+ (str-len path-ptr) 1)
       (str-bytes-ptr content-ptr)
       (str-len content-ptr))))
  "Phase 47 source for the Doc 117 §117.D.gaps.3 `(nl-write-file
PATH CONTENT)' libc syscall body swap.

Five-entry `(seq DEFUN ...)' manifest:
- `nelisp_bi_nl_write_file_prog2 (val _eff) -> val' — 2-arg cache.
- `nelisp_bi_nl_write_file_seq4 (val _e1 _e2 _e3) -> val' — 4-arg
  cache with 3-effect tail (write rc + close eff + dealloc eff +
  pad).
- `nelisp_bi_nl_write_file_with_fd (fd cstr size cptr clen _pad)' —
  6-arg branch on open(2) rc.
- `nelisp_bi_nl_write_file_inner (cstr size cptr clen)' — 4-arg
  driver that calls open(2) + dispatches.
- `nelisp_bi_nl_write_file (path-ptr content-ptr)' — 2-arg public
  entry.

Composes existing Phase 47 grammar (no new opcode):
- §122.I `nelisp_cstr_from_sexp' — path CString construction (cross-
  `.o' `extern-call').
- §125.A `dealloc-bytes' — CString free.
- §122.H `str-bytes-ptr' — CONTENT byte data pointer.
- §101.C `str-len' — byte counts.
- §100.A `extern-call' to libc `open' / `write' / `close'.

Linux-x86_64 only — same arch gate as the §122.I substrate.")

(provide 'nelisp-cc-bi-nl-write-file)

;;; nelisp-cc-bi-nl-write-file.el ends here
