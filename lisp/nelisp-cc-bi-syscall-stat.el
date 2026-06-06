;;; nelisp-cc-bi-syscall-stat.el --- Doc 117 §117.D.gaps.3 stat() sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.3 — moves the syscall body of
;; `(nelisp--syscall-stat PATH)' (= `build-tool/src/eval/builtins.rs::
;; bi_syscall_stat') into a AOT elisp object.  First file-I/O
;; sweep enabled by the Doc 122 §122.I `nelisp_cstr_from_sexp' helper.
;;
;; The Rust shim keeps:
;;
;;   * arity validation                     (1 arg)
;;   * `default-directory' path resolution  (`resolve_existing_path')
;;   * `struct stat' buffer allocation      (144 bytes, Rust-owned)
;;   * mode-field tag dispatch              (S_IFDIR / S_IFREG / else)
;;
;; The elisp body's job is the *syscall itself* — build the libc
;; CString from the normalised path Sexp, call `stat(2)' against the
;; Rust-owned 144-byte stat buffer, free the CString, and return the
;; libc rc (= 0 on success, -1 on error).  No buffer reads — the Rust
;; shim inspects the populated stat buffer's mode field.
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §125.A  `dealloc-bytes'          — free CString after syscall.
;;   §101.C  `str-len'                — for the dealloc size arg.
;;   §100.A  `extern-call'            — 2-arg libc call to `stat'.
;;
;; Function contract:
;;   path-ptr: *const Sexp — caller-validated `Sexp::Str' pointing at
;;             a normalised path (= Rust's `resolve_existing_path'
;;             already folded `default-directory' in).
;;   statbuf:  *mut u8    — Rust-owned 144-byte `struct stat' buffer
;;             (= zeroed `Vec<u8>` of size 144 = sizeof(struct stat)
;;             on Linux x86_64 glibc).
;;   returns:  i64        — libc `stat(2)' return: 0 = success
;;             (statbuf populated), -1 = errno set (Rust shim maps
;;             both -1 and exotic file types to `'absent).
;;
;; Per Doc 117 §4.3: pure migration — observable behaviour matches
;; the pre-swap `std::fs::metadata(&p).map(|m| m.file_type())' shape
;; modulo the `m.is_dir() / m.is_file()' check moving from libstd's
;; FileType to a direct mode-field mask in Rust.

;;; Code:

(defconst nelisp-cc-bi-syscall-stat--source
  '(seq
    ;; Side-effect sequencer — returns first arg (= the syscall rc),
    ;; discards second (= the dealloc-bytes = 1 sentinel).  Mirrors
    ;; the §124.G NlBox-drop pattern: compute value, then free,
    ;; then return the cached value.  4-arg helper isn't required
    ;; here; 2-arg suffices because the dealloc-size is threaded
    ;; through the outer `inner' driver.
    (defun nelisp_bi_syscall_stat_prog2 (val _eff) val)

    ;; 3-arg inner driver — receives the result buffer, the freshly-
    ;; allocated path CString, and the dealloc-size (= str-len + 1).
    ;; Calls `stat(cstr, statbuf)' libc syscall, frees the CString,
    ;; returns the syscall rc.
    ;;
    ;; Arg classes (all i64 / pointer): rdi=cstr, rsi=statbuf,
    ;; rdx=size_plus_one.  Even-arity (3 args) — Doc 124.F alignment
    ;; workaround not required (= `nelisp_cstr_helpers_prog2' is
    ;; the deepest call site this dispatches into, at 2-arg shape
    ;; with SysV rsp already at -16 from the prologue's `sub rsp, 8'
    ;; equivalent).
    (defun nelisp_bi_syscall_stat_inner (cstr statbuf size-plus-one)
      (nelisp_bi_syscall_stat_prog2
       (extern-call stat cstr statbuf)
       (dealloc-bytes cstr size-plus-one 1)))

    ;; Public 2-arg entry — builds the path CString and dispatches
    ;; to the inner driver.  Arity 2 (even) — no rsp-alignment
    ;; workaround needed.
    (defun nelisp_bi_syscall_stat (path-ptr statbuf)
      (nelisp_bi_syscall_stat_inner
       (extern-call nelisp_cstr_from_sexp path-ptr)
       statbuf
       (+ (str-len path-ptr) 1))))
  "AOT source for the Doc 117 §117.D.gaps.3 `(nelisp--syscall-
stat PATH)' libc syscall body swap.

Composes only existing AOT grammar:
- §122.I `nelisp_cstr_from_sexp' (cross-`.o' `extern-call') — path
  CString construction.
- §125.A `dealloc-bytes' — CString free after the libc consumer
  finishes.
- §101.C `str-len' — feeds the dealloc-size arg.
- §100.A `extern-call' to libc `stat' — the actual syscall.

The Rust shim retains the `struct stat' buffer alloc + mode-field
inspection + symbol-tag mapping (`'absent / `'file / `'directory).
The elisp body owns the CString lifecycle + the libc syscall edge.

Three-entry `(seq DEFUN ...)' manifest mirrors §122.I's shape:
- `nelisp_bi_syscall_stat_prog2 (val _eff) -> val' — sequencer.
- `nelisp_bi_syscall_stat_inner (cstr buf size) -> i64' — syscall
  + dealloc driver.
- `nelisp_bi_syscall_stat (path-ptr statbuf) -> i64' — public
  2-arg entry.

Linux-x86_64 only — `nelisp_cstr_from_sexp' substrate (§122.I) is
x86_64-gated today; aarch64 lands with the AOT aarch64 sweep.")

(provide 'nelisp-cc-bi-syscall-stat)

;;; nelisp-cc-bi-syscall-stat.el ends here
