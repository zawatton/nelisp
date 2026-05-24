;;; nelisp-cc-bi-getenv.el --- Wave A25.1 getenv() Phase 47 helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Wave A25.1 (Phase 47 self-application foundation) — pure-elisp
;; `(nelisp_bi_getenv NAME)' helper that calls libc `getenv(3)' against
;; a Sexp::Str / Sexp::Symbol argument and returns either Sexp::Str of
;; the environment value or Sexp::Nil if the variable is unset.
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build NAME CString.
;;   §125.A  `dealloc-bytes'          — free NAME CString after getenv.
;;   §101.C  `str-len'                — for the dealloc size arg.
;;   §100.A  `extern-call'            — libc `getenv' call.
;;   §122.E  `ptr-read-u8'            — walk getenv result bytes to
;;                                       compute strlen (no Phase 47
;;                                       `strlen' grammar today).
;;   §122.A  `sexp-write-str'         — allocate fresh Sexp::Str(value).
;;   §122.A  `sexp-write-nil'         — Sexp::Nil for missing variables.
;;
;; libc `getenv(3)' semantics:
;;   * Returns *const c_char (NUL-terminated value) on hit, or NULL when
;;     the environment variable is not set.
;;   * Returned pointer is borrowed (= points into the process's
;;     `environ' array).  Caller MUST NOT free it.  Phase 47 elisp
;;     copies the bytes into a fresh Sexp::Str owned by NeLisp's
;;     allocator (= `sexp-write-str' internally `alloc-bytes' + copies).
;;
;; Function contract:
;;   name-ptr:    *const Sexp — caller-validated `Sexp::Str' /
;;                `Sexp::Symbol' carrying the environment variable name
;;                (= no embedded NUL; Rust shim guards if needed).
;;   result-slot: *mut Sexp — caller-owned 32-byte slot, receives
;;                Sexp::Str(value) on hit or Sexp::Nil on miss.
;;   returns:     result-slot pointer in rax.
;;
;; Linux-x86_64 only — same arch gate as the §122.I parent + the
;; existing libc-syscall siblings (`nelisp_bi_syscall_stat' etc.).
;; Composes only existing Phase 47 grammar — no new opcode.

;;; Code:

(defconst nelisp-cc-bi-getenv--source
  '(seq
    ;; Side-effect sequencer — 3-arg `(val _e1 _e2) -> val'.  Used to
    ;; thread the dealloc cleanup behind the result-slot return.
    (defun nelisp_bi_getenv_prog3 (val _e1 _e2) val)

    ;; Tail-recursive byte walker that computes the length of a libc
    ;; NUL-terminated C string at `(ptr + i)'.  Stops when the byte at
    ;; index `i' equals zero, returns `i' (= the count of non-NUL bytes
    ;; preceding the terminator).  Even arity (2) — no Doc 124.F
    ;; alignment workaround.  Walks one byte at a time via §122.E
    ;; `ptr-read-u8'.
    (defun nelisp_bi_getenv_cstrlen (ptr i)
      (if (= (ptr-read-u8 ptr i) 0)
          i
        (nelisp_bi_getenv_cstrlen ptr (+ i 1))))

    ;; 4-arg inner driver — given the libc-borrowed value pointer
    ;; (= getenv return, nonzero), the path CString + its size + the
    ;; caller's result-slot, copies the libc bytes into a fresh
    ;; Sexp::Str (via §122.A `sexp-write-str') and frees the input
    ;; CString.  Returns the result-slot.
    ;;
    ;; Even arity (4) — no rsp-alignment workaround needed.  SysV
    ;; AMD64 marshals (rdi=val-ptr, rsi=cstr, rdx=size, rcx=result-slot).
    (defun nelisp_bi_getenv_with_value (val-ptr cstr size-plus-one result-slot)
      (nelisp_bi_getenv_prog3
       (sexp-write-str result-slot val-ptr
                       (nelisp_bi_getenv_cstrlen val-ptr 0))
       (dealloc-bytes cstr size-plus-one 1)
       0))

    ;; 4-arg inner driver — given the libc-borrowed pointer (may be 0
    ;; = NULL = variable unset), the path CString + its size + the
    ;; caller's result-slot, dispatches: NULL → Sexp::Nil + cleanup;
    ;; non-NULL → `_with_value' branch.  The dispatch is a single
    ;; `if (= val-ptr 0)' compile-time-stable comparison.
    (defun nelisp_bi_getenv_dispatch (val-ptr cstr size-plus-one result-slot)
      (if (= val-ptr 0)
          ;; Variable unset (getenv returned NULL).  Write Sexp::Nil
          ;; into the result-slot, free the input CString, return
          ;; the slot pointer.
          (nelisp_bi_getenv_prog3
           (sexp-write-nil result-slot)
           (dealloc-bytes cstr size-plus-one 1)
           0)
        ;; Variable set — copy libc bytes into fresh Sexp::Str.
        (nelisp_bi_getenv_with_value val-ptr cstr size-plus-one result-slot)))

    ;; 4-arg inner driver — receives the freshly-allocated NAME
    ;; CString + its size + the caller's result-slot, calls libc
    ;; `getenv(cstr)' and dispatches on the returned pointer.
    ;;
    ;; Pulled out from the public entry so the public entry's body
    ;; remains a flat composition (= no nested `extern-call' inside
    ;; an `extern-call' arg) — keeps eval order obvious.
    (defun nelisp_bi_getenv_inner (cstr size-plus-one result-slot _pad)
      (nelisp_bi_getenv_dispatch
       (extern-call getenv cstr)
       cstr
       size-plus-one
       result-slot))

    ;; Public 2-arg entry — builds the NAME CString and dispatches
    ;; to the 4-arg inner driver.  Even arity (2) — no rsp-alignment
    ;; workaround needed for the outer caller's dispatch into this
    ;; entry.
    (defun nelisp_bi_getenv (name-ptr result-slot)
      (nelisp_bi_getenv_inner
       (extern-call nelisp_cstr_from_sexp name-ptr)
       (+ (str-len name-ptr) 1)
       result-slot
       0)))
  "Phase 47 source for the Wave A25.1 `(nelisp_bi_getenv NAME
RESULT-SLOT)' helper.

Six-entry `(seq DEFUN ...)' manifest:
- `nelisp_bi_getenv_prog3 (val _e1 _e2) -> val' — 3-arg side-effect
  sequencer (= result + dealloc effect + pad).
- `nelisp_bi_getenv_cstrlen (ptr i) -> n' — tail-recursive byte walker
  for libc C-string length computation (Phase 47 has no `strlen'
  grammar op).
- `nelisp_bi_getenv_with_value (val-ptr cstr size result-slot) ->
  result-slot' — non-NULL branch: copy libc bytes into fresh Sexp::Str
  via §122.A `sexp-write-str' + free input CString.
- `nelisp_bi_getenv_dispatch (val-ptr cstr size result-slot) ->
  result-slot' — NULL-check branch on getenv return.
- `nelisp_bi_getenv_inner (cstr size result-slot _pad) -> result-slot' —
  calls libc `getenv' and chains into dispatch.
- `nelisp_bi_getenv (name-ptr result-slot) -> result-slot' — public
  2-arg entry; allocates name CString + dispatches to inner.

Composes only existing Phase 47 grammar — no new opcode:
- §122.I `nelisp_cstr_from_sexp' — NAME CString construction.
- §125.A `dealloc-bytes' — NAME CString lifecycle.
- §101.C `str-len' — byte count for dealloc size.
- §100.A `extern-call' to libc `getenv'.
- §122.E `ptr-read-u8' — walk libc result bytes to compute strlen.
- §122.A `sexp-write-str' — allocate fresh Sexp::Str(value).
- §122.A `sexp-write-nil' — Sexp::Nil for unset variables.

Linux-x86_64 only — same arch gate as the §122.I parent.  libc
`getenv(3)' is portable but the Phase 47 grammar substrate is
x86_64-only until the rest of the aarch64 sweep ships.")

(provide 'nelisp-cc-bi-getenv)

;;; nelisp-cc-bi-getenv.el ends here
