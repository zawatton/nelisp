;;; nelisp-cc-cstr-helpers.el --- Doc 122 §122.I CString construction  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 122 §122.I — CString grammar.  Pure-elisp helper that turns a
;; `Sexp::Str' / `Sexp::Symbol' / `Sexp::MutStr' payload into a fresh
;; NUL-terminated heap byte buffer (= what libc APIs like `open(2)',
;; `stat(2)', `mkdir(2)' expect for their `const char *path' argument).
;;
;; Substrate gating for Doc 117 §117.D.gaps.3 (= the file-I/O sweep:
;; `bi_open' / `bi_stat' / `bi_mkdir' / `bi_unlink' / `bi_rename' / …
;; ~12 handlers that each need to materialise a NUL-terminated path
;; from an elisp Sexp::Str argument).  Today the Rust shim allocates
;; via `CString::new(s)?`; with §122.I shipping, the entire path can
;; live in Phase 47 elisp through `extern-call' + raw byte ops.
;;
;; Surface:
;;
;;   (nelisp_cstr_from_sexp STR-PTR) -> BUF-PTR (i64)
;;     Allocates a fresh heap byte buffer of size (str-len + 1) bytes,
;;     copies the UTF-8 payload bytes from STR-PTR, appends a trailing
;;     `\0' (= the NUL terminator).  Returns the buffer pointer
;;     cast to i64.
;;
;;     Caller owns the buffer and MUST eventually call
;;     `(dealloc-bytes BUF-PTR (+ (str-len STR-PTR) 1) 1)' — or the
;;     convenience wrapper `nelisp_cstr_drop' below.  Layout passed
;;     to `dealloc-bytes' must match the original `(alloc-bytes (+ n
;;     1) 1)' call's `(size, align)' pair.  Align is 1 because byte
;;     buffers have no alignment requirement (libc string APIs only
;;     require `char *', not aligned multi-byte access).
;;
;;     STR-PTR must point at a `Sexp::Str' / `Sexp::Symbol' /
;;     `Sexp::MutStr' (= the three arms whose `String' header lives
;;     at the standard 24-byte offset that `str-len' + `str-byte-at'
;;     read).  Any other tag is undefined behaviour (= no runtime
;;     check; caller gates).
;;
;; Composition (no new opcode — pure §122 grammar):
;;   §101.C  `str-len' + `str-byte-at' — byte iteration.
;;   §122.E  `ptr-write-u8'            — buf[i] = byte writes.
;;   §125.A  `alloc-bytes'             — fresh heap allocation.
;;   §125.A  `dealloc-bytes'           — paired free (caller; only
;;                                       used by the optional
;;                                       `nelisp_cstr_drop' wrapper).
;;
;; Recursion shape: a tail-recursive inner loop threads `(buf src i
;; n)' through itself until `i = n', then writes the terminating NUL
;; via `(ptr-write-u8 buf n 0)' and returns `buf' (= the public
;; return value).  Mirrors the §115.7 `nelisp_fnv1a_step' shape
;; exactly, modulo (a) 4 args instead of 4 (still even — Doc 124.F
;; alignment fix not required) and (b) returning the pointer instead
;; of a hash accumulator.
;;
;; Caller pattern (= what the §117.D Tier C sweep targets):
;;
;;   (let* ((path-ptr (nelisp_cstr_from_sexp arg))
;;          (rc       (extern-call "open" path-ptr O_RDONLY))
;;          (_unused  (dealloc-bytes path-ptr
;;                                   (+ (str-len arg) 1) 1)))
;;     rc)
;;
;; The `(+ (str-len arg) 1)' on the dealloc must use the *original*
;; argument's length (or a saved-aside copy from the alloc time),
;; not a re-fetched length from a different pointer — `Sexp::Str's
;; payload is immutable so re-reading `str-len' on the same arg
;; works in practice, but capturing the alloc-time length is the
;; future-proof pattern.  `nelisp_cstr_drop' below codifies the
;; recommended capture pattern.
;;
;; ABI / arity notes:
;;   - `nelisp_cstr_copy_loop' is 4-arg = even = no Doc 124.F-blocker
;;     rsp-alignment workaround needed.  The inner intra-text
;;     recursive call (loop step) lands at rsp ≡ 0 mod 16 naturally.
;;   - `nelisp_cstr_from_sexp_inner' is 2-arg = even = no workaround.
;;   - `nelisp_cstr_from_sexp' is 1-arg (odd).  Its single internal
;;     call to `nelisp_cstr_from_sexp_inner' lands through the
;;     compiler's `(needs-align)' branch (= `sub rsp, 8' /
;;     `add rsp, 8' bracket around the `call' opcode), same code
;;     path that `nelisp_fnv1a' (also 1-arg) uses to dispatch into
;;     its 4-arg inner helper without crashing.
;;
;; Linux-x86_64 only for the same reason as the rest of the §122
;; family — the underlying §101.C / §122.E / §125.A grammar ops only
;; have x86_64 emit paths today; aarch64 lands with the rest of the
;; Phase 47 aarch64 sweep.

;;; Code:

(defconst nelisp-cc-cstr-helpers--source
  '(seq
    ;; Side-effect sequencer mirroring §124.G/H `nelisp_nl*_drop_prog2'
    ;; and §116.A `nelisp_reader_prog2': evaluate both args left-to-
    ;; right (= SysV ABI arg marshal order is well-defined) and
    ;; return the second arg.  Used to thread a `ptr-write-u8' side
    ;; effect through a value form that yields `buf' as the public
    ;; return.
    (defun nelisp_cstr_helpers_prog2 (_eff val) val)

    ;; Inner tail-recursive byte copy loop.
    ;;
    ;;   buf:  *mut u8 — destination heap buffer (allocated by the
    ;;         outer `nelisp_cstr_from_sexp_inner' via `alloc-bytes').
    ;;   src:  *const Sexp — source string (Str / Symbol / MutStr).
    ;;   i:    i64 — current byte index (0-indexed).
    ;;   n:    i64 — total byte count (= `str-len' on src).
    ;;
    ;; Returns `buf' unchanged (= the freshly-allocated pointer)
    ;; so the outer `nelisp_cstr_from_sexp' can return it as the
    ;; public buffer-pointer result without a separate `let'
    ;; binding (= Phase 47's `let' only accepts compile-time
    ;; constant bindings, so runtime intermediate values must
    ;; thread through call args).
    ;;
    ;; Termination: `(< i n)' false → write the NUL terminator at
    ;; offset `n' (= the final byte of the (n+1)-byte allocation)
    ;; and return `buf'.  The `nelisp_cstr_helpers_prog2' sequencer
    ;; orders the write-NUL side effect before the return-`buf'.
    (defun nelisp_cstr_copy_loop (buf src i n)
      (if (< i n)
          (nelisp_cstr_helpers_prog2
           (ptr-write-u8 buf i (str-byte-at src i))
           (nelisp_cstr_copy_loop buf src (+ i 1) n))
        (nelisp_cstr_helpers_prog2
         (ptr-write-u8 buf n 0)
         buf)))

    ;; 2-arg inner driver: takes the (cached) byte count `n' so
    ;; we don't re-evaluate `(str-len str-ptr)' twice (once to size
    ;; the allocation, once as the loop bound).  Returns the
    ;; allocated buffer pointer.
    ;;
    ;; Allocation layout: `(alloc-bytes (+ n 1) 1)' — `n + 1' bytes
    ;; (= payload + trailing NUL) at alignment 1 (= byte buffers
    ;; have no alignment requirement; libc string APIs only require
    ;; `char *', not aligned multi-byte access).
    (defun nelisp_cstr_from_sexp_inner (str-ptr n)
      (nelisp_cstr_copy_loop (alloc-bytes (+ n 1) 1) str-ptr 0 n))

    ;; Public entry — 1-arg.  Threads the byte count through
    ;; `nelisp_cstr_from_sexp_inner' so the allocation and the
    ;; loop bound share one `str-len' evaluation.
    ;;
    ;; Arity = 1 (odd).  The single intra-text call to the 2-arg
    ;; `nelisp_cstr_from_sexp_inner' below lands through the
    ;; compiler's `needs-align' branch (= `sub rsp, 8' around
    ;; the `call' opcode), same code path as `nelisp_fnv1a's
    ;; 1-arg dispatch into its 4-arg inner step.
    (defun nelisp_cstr_from_sexp (str-ptr)
      (nelisp_cstr_from_sexp_inner str-ptr (str-len str-ptr)))

    ;; Convenience wrapper — pairs `nelisp_cstr_from_sexp' with the
    ;; matching `dealloc-bytes' call.  Callers who tracked the
    ;; original `(str-len arg)' separately should call
    ;; `(dealloc-bytes BUF-PTR (+ saved-len 1) 1)' directly;
    ;; this 2-arg helper exists for the common case where the
    ;; caller has already lost the original length and is willing
    ;; to re-derive `(+ LEN 1)' from the (caller-supplied) total
    ;; alloc-size value.
    ;;
    ;; ARGS:
    ;;   buf-ptr:  *mut u8 — pointer returned by
    ;;             `nelisp_cstr_from_sexp'.  Null = no-op.
    ;;   size:     i64    — must equal the original
    ;;             `(+ (str-len ARG) 1)' value passed (implicitly)
    ;;             to `alloc-bytes' inside `nelisp_cstr_from_sexp'.
    ;;             Mismatch is UB per `std::alloc::dealloc'.
    ;;
    ;; Returns the `dealloc-bytes' = 1 sentinel for `and'-chain
    ;; composition with sibling syscall side effects.
    (defun nelisp_cstr_drop (buf-ptr size)
      (dealloc-bytes buf-ptr size 1)))
  "Phase 47 source for the Doc 122 §122.I CString construction
helpers.

Three-entry `(seq DEFUN ...)' manifest:
- `nelisp_cstr_helpers_prog2 (_eff val) -> val' — side-effect
  sequencer, identical pattern to §116.A / §124.A `*_prog2'.
- `nelisp_cstr_copy_loop (buf src i n) -> buf' — tail-recursive
  byte copy + trailing NUL write.  4-arg (even) — no Doc 124.F
  alignment workaround needed.
- `nelisp_cstr_from_sexp_inner (str-ptr n) -> buf' — 2-arg driver
  threading the byte count from the public entry into the
  alloc + loop pipeline.
- `nelisp_cstr_from_sexp (str-ptr) -> buf' — public 1-arg entry.

Plus an optional sibling for the dealloc side:
- `nelisp_cstr_drop (buf-ptr size) -> 1' — convenience wrapper
  around `(dealloc-bytes BUF-PTR SIZE 1)'.

Composes only existing Phase 47 grammar ops — no new opcode added.
ABI deps: §101.C (str-len / str-byte-at), §122.E (ptr-write-u8),
§125.A (alloc-bytes / dealloc-bytes).

Unlocks Doc 117 §117.D.gaps.3 Tier C — the file-I/O sweep can now
materialise libc `const char *path' arguments from Sexp::Str
inputs entirely in Phase 47 elisp via `extern-call' + this helper,
deleting the Rust `CString::new(s)?' shims in
`build-tool/src/eval/builtins.rs::bi_open' / `bi_stat' / `bi_mkdir' /
the ~12 sibling I/O handlers.")

(provide 'nelisp-cc-cstr-helpers)

;;; nelisp-cc-cstr-helpers.el ends here
