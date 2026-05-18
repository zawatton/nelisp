;;; nelisp-cc-bi-syscall-canonicalize.el --- Doc 117 §117.D.gaps.3 realpath() sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.3 — moves the syscall body of
;; `(nelisp--syscall-canonicalize PATH)' (= `build-tool/src/eval/
;; builtins.rs::bi_syscall_canonicalize') into a Phase 47 elisp
;; object.  Second handler in the Doc 122 §122.I CString helper
;; sweep (after `nelisp_bi_syscall_stat').
;;
;; The Rust shim keeps:
;;
;;   * arity validation                     (1 arg)
;;   * `default-directory' path resolution  (`resolve_existing_path')
;;   * Resolved-path output buffer alloc    (PATH_MAX = 4096 bytes,
;;                                            Rust-owned `Vec<u8>`)
;;   * NULL-check + CStr→Sexp::Str wrap     (Rust reads the C string
;;                                            from the result buffer
;;                                            via `CStr::from_ptr')
;;
;; The elisp body's job is the *syscall itself* — build the libc
;; CString from the normalised path Sexp, call `realpath(2)' with
;; the Rust-owned 4096-byte result buffer as `resolved_path' arg,
;; free the input CString, and return the libc rc (= result buffer
;; pointer on success or 0/NULL on error).
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §125.A  `dealloc-bytes'          — free CString after syscall.
;;   §101.C  `str-len'                — for the dealloc size arg.
;;   §100.A  `extern-call'            — 2-arg libc call to `realpath'.
;;
;; Function contract:
;;   path-ptr: *const Sexp — caller-validated `Sexp::Str' pointing at
;;             a normalised path (= Rust's `resolve_existing_path'
;;             already folded `default-directory' in).
;;   result-buf: *mut u8  — Rust-owned 4096-byte (PATH_MAX) buffer,
;;             zeroed `Vec<u8>'.
;;   returns:  i64        — libc `realpath(3)' return:
;;             > 0 = result-buf address (success, NUL-terminated
;;                   resolved path in buffer).
;;             0   = NULL (error / not-found / EACCES).
;;
;; Per Doc 117 §4.3: pure migration — observable behaviour matches
;; the pre-swap `std::fs::canonicalize' modulo the canonicalisation
;; engine (`realpath' vs Rust libstd) which both delegate to the
;; kernel's `pathname_lookup' for the resolution work.

;;; Code:

(defconst nelisp-cc-bi-syscall-canonicalize--source
  '(seq
    ;; Side-effect sequencer — see `nelisp_bi_syscall_stat_prog2'
    ;; (same shape).  Caches the realpath rc and drops the
    ;; dealloc-bytes = 1 sentinel.
    (defun nelisp_bi_syscall_canonicalize_prog2 (val _eff) val)

    ;; 3-arg inner driver — receives the result buffer, the freshly-
    ;; allocated path CString, and the dealloc-size (= str-len + 1).
    ;; Calls `realpath(cstr, result_buf)' libc, frees the CString,
    ;; returns the syscall rc.
    (defun nelisp_bi_syscall_canonicalize_inner (cstr result-buf size-plus-one)
      (nelisp_bi_syscall_canonicalize_prog2
       (extern-call realpath cstr result-buf)
       (dealloc-bytes cstr size-plus-one 1)))

    ;; Public 2-arg entry — builds the path CString and dispatches
    ;; to the inner driver.
    (defun nelisp_bi_syscall_canonicalize (path-ptr result-buf)
      (nelisp_bi_syscall_canonicalize_inner
       (extern-call nelisp_cstr_from_sexp path-ptr)
       result-buf
       (+ (str-len path-ptr) 1))))
  "Phase 47 source for the Doc 117 §117.D.gaps.3 `(nelisp--syscall-
canonicalize PATH)' libc syscall body swap.

Composes only existing Phase 47 grammar (same set as the §117.D.3
sibling `nelisp-cc-bi-syscall-stat'): §122.I cstr-helpers + §125.A
dealloc-bytes + §101.C str-len + §100.A extern-call to libc
`realpath'.

The Rust shim retains the result buffer alloc (PATH_MAX = 4096) +
NULL-check + CStr→Sexp::Str wrap.  The elisp body owns the input
CString lifecycle + the libc syscall edge.

Three-entry `(seq DEFUN ...)' manifest — identical shape to
`nelisp-cc-bi-syscall-stat' modulo the libc symbol name (= `realpath'
instead of `stat').

Linux-x86_64 only — same arch gate as the §122.I substrate.")

(provide 'nelisp-cc-bi-syscall-canonicalize)

;;; nelisp-cc-bi-syscall-canonicalize.el ends here
