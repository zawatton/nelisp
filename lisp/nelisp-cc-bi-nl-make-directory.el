;;; nelisp-cc-bi-nl-make-directory.el --- Doc 117 §117.D.gaps.3 mkdir() sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.3 — moves the syscall body of `(nl-make-
;; directory PATH)' (= `build-tool/src/eval/builtins.rs::
;; bi_nl_make_directory') into a Phase 47 elisp object.  Fourth
;; handler in the Doc 122 §122.I CString helper sweep (after
;; `nelisp_bi_syscall_stat', `_canonicalize', and `nelisp_bi_nl_write_file').
;;
;; The Rust shim keeps:
;;
;;   * arity validation             (1-2 args; 2nd ignored)
;;   * `stringp' dispatch           (1st arg)
;;   * Error-message construction   (when the kernel returns < 0)
;;
;; The elisp body's job is the *syscall itself*:
;;   1. nelisp_cstr_from_sexp(path)      — alloc CString.
;;   2. mkdir(cstr, 0o755)               — single mkdir(2) syscall.
;;   3. dealloc-bytes(cstr, size, 1)     — free CString.
;;
;; Per `nl-make-directory' contract: this is the non-recursive flavour.
;; Pre-swap `std::fs::create_dir_all' was the recursive form, but
;; (a) the builtin has zero elisp callers across the tree (the elisp
;; emacs-compat layer at `src/nelisp-emacs-compat-fileio.el:340'
;; dispatches via `nl-syscall-mkdir' instead), and (b) the §117.D.gaps.3
;; sweep criterion is "single libc syscall body lift".  A future
;; user that needs recursive semantics should go through a higher-
;; level wrapper that loops the path components — out of scope for
;; this kernel.
;;
;; Linux x86_64 ABI:
;;   mkdir(2) signature: int mkdir(const char *pathname, mode_t mode)
;;   mode 0o755 = 493 = rwxr-xr-x.
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §125.A  `dealloc-bytes'          — free CString after syscall.
;;   §101.C  `str-len'                — for the dealloc size arg.
;;   §100.A  `extern-call'            — 2-arg libc call to `mkdir'.
;;
;; Function contract:
;;   path-ptr: *const Sexp — caller-validated `Sexp::Str' / MutStr,
;;             path to create.
;;   returns:  i64        — libc `mkdir(2)' return:
;;                         0  = success.
;;                        -1 = errno set (e.g. EEXIST, ENOENT, EACCES);
;;                              the Rust shim maps it to EvalError::
;;                              Internal.
;;
;; Per Doc 117 §4.3: pure migration modulo the documented recursive →
;; non-recursive semantic narrowing (no callers depend on the recursive
;; form, see commentary above).

;;; Code:

(defconst nelisp-cc-bi-nl-make-directory--source
  '(seq
    ;; Side-effect sequencer — 2-arg `(val _eff) -> val', caches the
    ;; mkdir rc while threading the dealloc-bytes side-effect.
    ;; Identical shape to `nelisp_bi_syscall_stat_prog2'.
    (defun nelisp_bi_nl_make_directory_prog2 (val _eff) val)

    ;; 3-arg inner driver — receives the freshly-allocated path CString
    ;; + dealloc-size, issues `mkdir(cstr, 0o755)' libc, frees the
    ;; CString, returns the syscall rc.
    ;;
    ;; Arg classes (all i64 / pointer): rdi=cstr, rsi=size-plus-one,
    ;; rdx=mode.  Even arity (3 args + 0 frame slots) — Doc 124.F
    ;; alignment workaround not required.
    (defun nelisp_bi_nl_make_directory_inner (cstr size-plus-one mode)
      (nelisp_bi_nl_make_directory_prog2
       (extern-call mkdir cstr mode)
       (dealloc-bytes cstr size-plus-one 1)))

    ;; Public 1-arg entry — builds the path CString and dispatches
    ;; to the inner driver with mode = 0o755 (= 493).
    ;;
    ;; Arity 1 is odd; Doc 124.F requires the rsp-alignment workaround.
    ;; But the single internal call lands through Phase 47's standard
    ;; prologue which already maintains `rsp % 16 == 0' at the
    ;; call boundary (= same pattern as `nelisp_cstr_from_sexp' itself,
    ;; which is also 1-arg).
    (defun nelisp_bi_nl_make_directory (path-ptr)
      (nelisp_bi_nl_make_directory_inner
       (extern-call nelisp_cstr_from_sexp path-ptr)
       (+ (str-len path-ptr) 1)
       493)))
  "Phase 47 source for the Doc 117 §117.D.gaps.3 `(nl-make-directory
PATH)' libc syscall body swap.

Composes only existing Phase 47 grammar (same set as the §117.D.3
siblings `nelisp-cc-bi-syscall-stat' / `_canonicalize'): §122.I
cstr-helpers + §125.A dealloc-bytes + §101.C str-len + §100.A
extern-call to libc `mkdir'.

The Rust shim retains arg validation + the negative-rc → Internal-err
mapping.  The elisp body owns the CString lifecycle + the libc syscall
edge.

Three-entry `(seq DEFUN ...)' manifest — identical shape to
`nelisp-cc-bi-syscall-stat' modulo the libc symbol (`mkdir' vs `stat')
+ the mode arg (= 0o755 literal).

Linux-x86_64 only — same arch gate as the §122.I substrate.")

(provide 'nelisp-cc-bi-nl-make-directory)

;;; nelisp-cc-bi-nl-make-directory.el ends here
