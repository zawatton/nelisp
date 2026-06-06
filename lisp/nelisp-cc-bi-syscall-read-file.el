;;; nelisp-cc-bi-syscall-read-file.el --- Doc 117 §117.D.gaps.3 open+read+close sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.3 — moves the syscall body of `(nelisp--
;; syscall-read-file PATH)' (= `build-tool/src/eval/builtins.rs::
;; bi_syscall_read_file') into a AOT elisp object.  Fifth handler
;; in the Doc 122 §122.I CString helper sweep (after stat / canonicalize
;; / nl-write-file / nl-make-directory).
;;
;; The Rust shim keeps:
;;
;;   * arity validation              (1 arg)
;;   * `default-directory' resolution (`resolve_existing_path')
;;   * `stat(2)' size determination  (file-size → buffer size)
;;   * Destination buffer alloc      (`Vec<u8>` of size n_bytes)
;;   * `String::from_utf8_lossy' wrap to `Sexp::Str' on the read bytes
;;   * Negative-rc → `Sexp::Nil' branch (pre-swap returned Nil on err)
;;
;; The elisp body's job is the *three-syscall chain*:
;;   1. nelisp_cstr_from_sexp(path)          — alloc path CString.
;;   2. open(cstr, O_RDONLY)                 — open the file.
;;   3. (only if open succeeded:)
;;      read(fd, buf_ptr, size)              — slurp `size' bytes.
;;      close(fd)                             — release fd.
;;   4. dealloc-bytes(cstr, size_plus_one, 1) — free path CString.
;;
;; Why the buffer size is a separate Rust-side stat instead of an
;; in-elisp `fstat': the AOT grammar has no `i64'-class first-arg
;; conversion for `struct stat *' (= 144 bytes), so caller-side
;; allocation is the simpler split.  A future §122.X stat-via-fd
;; grammar would let the size determination migrate too.
;;
;; Linux x86_64 ABI:
;;   open(2) signature: int open(const char *path, int flags)
;;   O_RDONLY = 0.  (No mode arg when O_CREAT not set, but glibc's
;;   varargs prototype tolerates a 3rd arg; we pass 0 for clarity.)
;;   read(2):  ssize_t read(int fd, void *buf, size_t count)
;;   close(2): int close(int fd)
;;
;; Note: short reads on regular files only happen at EOF.  Since the
;; Rust shim sized the buffer via `stat' to exactly the file's size,
;; a single `read' returns all bytes for typical files; the
;; bytes-actually-read i64 is propagated up so Rust can truncate (=
;; matches the `Vec::truncate(rc as usize)' pattern from
;; `bi_read_stdin_bytes' twin).
;;
;; Substrate composition:
;;
;;   §122.I  `nelisp_cstr_from_sexp'  — build path CString.
;;   §125.A  `dealloc-bytes'          — free CString after syscalls.
;;   §101.C  `str-len'                — for the dealloc size arg.
;;   §100.A  `extern-call'            — libc `open' / `read' / `close'.
;;
;; Function contract:
;;   path-ptr: *const Sexp — caller-validated `Sexp::Str' / MutStr,
;;             normalised path.
;;   buf-ptr:  *mut u8     — Rust-owned destination buffer, `size'
;;             bytes initialised (Vec<u8> zero-init).
;;   size:     i64         — buffer cap from Rust's `stat()' call
;;             (= file size in bytes; passed as the read(2) count arg).
;;   returns:  i64         —
;;             >= 0 (= bytes read by `read(2)') on success.
;;             <  0 (= negative open(2) rc, or negative read(2) rc if
;;                   open succeeded then read failed) on failure.
;;             The Rust shim maps any < 0 return to `Sexp::Nil'
;;             (matching the pre-swap `Err(_) => Ok(Sexp::Nil)' arm).
;;
;; Per Doc 117 §4.3: pure migration — observable behaviour matches
;; the pre-swap `std::fs::read_to_string(&p)' shape modulo the
;; in-Rust path (`read_to_string' calls `read_to_end' which loops
;; on short reads; our single-`read' is sufficient for regular
;; files where the pre-stat'd size matches the kernel's view).
;; Edge cases:
;;   * No retry on EINTR (Rust libstd retries; this kernel doesn't).
;;     No NeLisp caller is EINTR-prone in practice.
;;   * Size mismatch between `stat' and `read' (= file grew/shrunk
;;     between the two syscalls) → caller gets `min(stat_size,
;;     actual_bytes)' (= what `read' returned).  Pre-swap had the
;;     same TOCTOU race surface area.

;;; Code:

(defconst nelisp-cc-bi-syscall-read-file--source
  '(seq
    ;; 2-arg side-effect sequencer — `(val _eff) -> val'.  Used for
    ;; the inner `read + dealloc' branch's CString-only-after-syscall
    ;; cleanup.
    (defun nelisp_bi_syscall_read_file_prog2 (val _eff) val)

    ;; 4-arg side-effect sequencer — `(val _e1 _e2 _e3) -> val'.
    ;; Caches the `read(2)' rc while sequencing `close(2)' +
    ;; `dealloc-bytes' + a pad slot (= keeps arity even per Doc
    ;; 124.F call-alignment invariant).  SysV ABI evaluates args
    ;; left-to-right so `read' runs before `close' runs before
    ;; `dealloc-bytes'.
    (defun nelisp_bi_syscall_read_file_seq4 (val _e1 _e2 _e3) val)

    ;; 6-arg with-fd dispatcher — branches on the open(2) rc.
    ;; Args (all i64 / pointer):
    ;;   fd:           open(2) rc.  Negative = error; non-negative = fd.
    ;;   cstr:         path CString (= return of §122.I cstr_from_sexp).
    ;;   cstr-size:    path CString allocation size (= str-len + 1).
    ;;   buf-ptr:      destination buffer (= Rust Vec<u8> as_mut_ptr).
    ;;   read-size:    bytes-to-read cap (= Rust-side stat'd file size).
    ;;   _pad:         unused; pads to even arity for Doc 124.F.
    (defun nelisp_bi_syscall_read_file_with_fd (fd cstr cstr-size buf-ptr read-size _pad)
      (if (< fd 0)
          ;; Open failed.  Free the CString and return the negative
          ;; rc so the Rust shim returns `Sexp::Nil'.
          (nelisp_bi_syscall_read_file_prog2
           fd
           (dealloc-bytes cstr cstr-size 1))
        ;; Open succeeded.  Chain read(2) + close(2) + dealloc;
        ;; return the read(2) rc (= bytes read, or -1 on err).
        (nelisp_bi_syscall_read_file_seq4
         (extern-call read fd buf-ptr read-size)
         (extern-call close fd)
         (dealloc-bytes cstr cstr-size 1)
         0)))

    ;; 4-arg inner driver — receives the freshly-allocated path
    ;; CString + size, plus the Rust-owned destination buffer + cap,
    ;; calls open(2), then dispatches to the 6-arg `_with_fd' branch.
    ;;
    ;; Pulled out from the public entry so the body is a flat
    ;; composition of substrate ops (= no nested `extern-call'
    ;; inside an `extern-call' arg) — matches the `nelisp_bi_nl_
    ;; write_file_inner' pattern bit-for-bit.
    (defun nelisp_bi_syscall_read_file_inner (cstr cstr-size buf-ptr read-size)
      (nelisp_bi_syscall_read_file_with_fd
       (extern-call open cstr 0 0)
       cstr
       cstr-size
       buf-ptr
       read-size
       0))

    ;; Public 3-arg entry — builds the path CString and dispatches
    ;; to the 4-arg inner.  Arity 3 (odd); the single internal
    ;; call lands through AOT's standard prologue which keeps
    ;; `rsp % 16 == 0' at the call boundary (same pattern as
    ;; `nelisp_bi_syscall_stat' which is also 2-arg + 1-arg
    ;; mixture in its `(seq ...)' chain).
    (defun nelisp_bi_syscall_read_file (path-ptr buf-ptr read-size)
      (nelisp_bi_syscall_read_file_inner
       (extern-call nelisp_cstr_from_sexp path-ptr)
       (+ (str-len path-ptr) 1)
       buf-ptr
       read-size)))
  "AOT source for the Doc 117 §117.D.gaps.3 `(nelisp--syscall-
read-file PATH)' libc syscall body swap.

Five-entry `(seq DEFUN ...)' manifest — symmetric mirror of
`nelisp-cc-bi-nl-write-file' (open/read/close vs open/write/close):
- `nelisp_bi_syscall_read_file_prog2 (val _eff) -> val' — 2-arg cache.
- `nelisp_bi_syscall_read_file_seq4 (val _e1 _e2 _e3) -> val' — 4-arg
  cache with 3-effect tail (read rc + close eff + dealloc eff + pad).
- `nelisp_bi_syscall_read_file_with_fd (fd cstr csize buf rsize _pad)' —
  6-arg branch on open(2) rc.
- `nelisp_bi_syscall_read_file_inner (cstr csize buf rsize)' — 4-arg
  driver that calls open(2) + dispatches.
- `nelisp_bi_syscall_read_file (path-ptr buf-ptr read-size)' — 3-arg
  public entry.

Composes existing AOT grammar (no new opcode):
- §122.I `nelisp_cstr_from_sexp' — path CString construction.
- §125.A `dealloc-bytes' — CString free.
- §101.C `str-len' — path byte count.
- §100.A `extern-call' to libc `open' / `read' / `close'.

Linux-x86_64 only — same arch gate as the §122.I substrate.")

(provide 'nelisp-cc-bi-syscall-read-file)

;;; nelisp-cc-bi-syscall-read-file.el ends here
