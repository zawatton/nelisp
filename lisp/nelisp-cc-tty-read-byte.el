;;; nelisp-cc-tty-read-byte.el --- nl_tty_read_byte Phase 47 swap  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 133 cutover — re-provides `nl_tty_read_byte' as a Phase
;; 47-compiled elisp .o after commit fa8932eb deleted the Rust
;; `#[no_mangle] pub unsafe extern "C" fn nl_tty_read_byte() -> i64'
;; body from `build-tool/src/eval/mod.rs'.
;;
;; Recovered signature (from fa8932eb^):
;;
;;   #[no_mangle]
;;   pub unsafe extern "C" fn nl_tty_read_byte() -> i64 {
;;       let mut b = [0u8; 1];
;;       let n = unsafe { libc::read(0, b.as_mut_ptr() as *mut libc::c_void, 1) };
;;       if n == 1 { b[0] as i64 } else { -1 }
;;   }
;;
;; Phase 47 implementation:
;;
;;   * No-arg C-ABI entry, returns i64.
;;   * Buffer strategy: heap (`alloc-bytes 1 1') — Phase 47 has no
;;     `alloca' / stack-local-array primitive, so we fall back to a
;;     1-byte heap allocation.  A global static would require a Rust
;;     `#[no_mangle]' pointer-getter (= Rust LOC addition, rejected).
;;     `alloc-bytes' + `dealloc-bytes' are the canonical Phase 47
;;     heap primitives (§125.A); the allocation is freed before every
;;     return path, so there is NO leak.
;;   * `extern-call read' issues `call libc::read@PLT' (= SysV AMD64:
;;     rdi=0 [fd], rsi=buf [*mut u8], rdx=1 [count]).
;;   * `ptr-read-u8 buf 0' reads the received byte (0..255) from the
;;     heap slot after a successful read.
;;   * Return: byte value (0..255) on success; -1 on EOF/error.
;;
;; Helpers:
;;   nl_tty_rb_prog1 (val _eff) -> val
;;     — 2-arg sequencer that returns the first arg and evaluates the
;;       second as a side-effect.  Used to pair `ptr-read-u8' (= the
;;       byte value to return) with `dealloc-bytes' (= the cleanup)
;;       without losing the value.  The `and'-chain alternative fails
;;       when byte == 0 (falsy in Elisp).
;;   nl_tty_rb_dispatch (buf n) -> i64
;;     — branches on the `read' return count: n==1 → free + return
;;       byte; else → free + return -1.
;;
;; Phase 47 ops consumed:
;;   §125.A  `alloc-bytes' / `dealloc-bytes' — 1-byte heap buf.
;;   §100.A  `extern-call'                   — libc `read(0, buf, 1)'.
;;   §101.C  `ptr-read-u8'                   — byte extraction.
;;   `='  `if'  literal `1'  literal `-1'    — dispatch / return.
;;
;; C-ABI contract (SysV AMD64):
;;   no params
;;   return: i64 in rax — byte value (0..255) on success, -1 otherwise.
;;
;; Linux-x86_64 only — `extern-call' / `alloc-bytes' share the same
;; arch gate as §122.I and §125.A parents.

;;; Code:

(defconst nelisp-cc-tty-read-byte--source
  '(seq
    ;; ------------------------------------------------------------------
    ;; nl_tty_rb_prog1: evaluate both args left-to-right, return the
    ;; first.  Used to capture the byte value before calling
    ;; `dealloc-bytes' (the dealloc is the side-effect; the byte is the
    ;; value we want to return).  Byte 0 is a valid return value so the
    ;; `and'-chain alternative would incorrectly short-circuit.
    (defun nl_tty_rb_prog1 (val _eff) val)

    ;; ------------------------------------------------------------------
    ;; nl_tty_rb_dispatch: branch on read(2) return count.
    ;; buf: *mut u8 — the 1-byte heap buffer allocated by nl_tty_read_byte.
    ;; n:   i64    — return value of read(0, buf, 1).
    ;; Returns the byte (0..255) on success (n==1), or -1 on EOF/error.
    ;; Frees `buf' via `dealloc-bytes 1 1' on both paths.
    (defun nl_tty_rb_dispatch (buf n)
      (if (= n 1)
          ;; Success — read the byte then free the buffer.
          (nl_tty_rb_prog1 (ptr-read-u8 buf 0)
                           (dealloc-bytes buf 1 1))
        ;; EOF / error — free the buffer and return -1.
        ;; The -1 literal is in the else-branch; its type is inferred
        ;; from the then-branch (i64 from ptr-read-u8), so no sys:cast
        ;; is needed here (Phase 47 propagates the then-type to else).
        (nl_tty_rb_prog1 -1 (dealloc-bytes buf 1 1))))

    ;; ------------------------------------------------------------------
    ;; nl_tty_read_byte: public C-ABI entry — no parameters.
    ;; Allocates a 1-byte heap buffer, calls read(0, buf, 1), dispatches
    ;; on the return count, frees the buffer, and returns the byte or -1.
    ;;
    ;; Matches the Rust body deleted by fa8932eb:
    ;;   let mut b = [0u8; 1];
    ;;   let n = libc::read(0, b.as_mut_ptr(), 1);
    ;;   if n == 1 { b[0] as i64 } else { -1 }
    (defun nl_tty_read_byte ()
      (let ((buf (alloc-bytes 1 1)))
        (nl_tty_rb_dispatch buf (extern-call read 0 buf 1)))))
  "Phase 47 source for the `nl_tty_read_byte' Doc 133 cutover.

Three-entry `(seq DEFUN ...)' manifest:
- `nl_tty_rb_prog1 (val _eff) -> val' — prog1 sequencer; returns
  the first arg (the byte value or -1) after evaluating the second
  arg (the dealloc-bytes call) as a side-effect.
- `nl_tty_rb_dispatch (buf n) -> i64' — branches on the read(2)
  return count; frees the 1-byte buffer on both paths.
- `nl_tty_read_byte () -> i64' — public no-arg C-ABI entry;
  allocates a 1-byte heap buffer with alloc-bytes, calls
  `extern-call read 0 buf 1', dispatches via nl_tty_rb_dispatch.

Buffer strategy: heap (alloc-bytes 1 1 / dealloc-bytes 1 1).
Phase 47 has no stack-alloca primitive; a Rust static-pointer getter
would add Rust LOC (rejected per hard rule).  The 1-byte allocation
is freed before every return path — NO persistent leak.

Phase 47 ops consumed:
  `alloc-bytes 1 1'   — allocate 1 byte, align 1.
  `extern-call read'  — call libc read(0, buf, 1).
  `ptr-read-u8 buf 0' — extract the received byte.
  `dealloc-bytes'     — free the 1-byte buffer.
  `='  `if'  `-1'  `let' — dispatch and control flow.

Net Rust delta: zero.  Resolves the `nl_tty_read_byte' undefined
symbol introduced by the fa8932eb deletion (= 1 of the 19 remaining
undefined symbols in the linker error list).")

(provide 'nelisp-cc-tty-read-byte)

;;; nelisp-cc-tty-read-byte.el ends here
