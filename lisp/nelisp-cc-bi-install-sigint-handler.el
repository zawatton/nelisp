;;; nelisp-cc-bi-install-sigint-handler.el --- Doc 117 §117.D.gaps.4 sigaction(SIGINT) sweep  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117 §117.D.gaps.4 — moves the sigaction-struct construction +
;; libc `sigaction(SIGINT, &sa, NULL)' dispatch body of `(install-
;; sigint-handler)' (= `build-tool/src/eval/builtins.rs::
;; bi_install_sigint_handler' → `eval/quit.rs::install_sigint_handler')
;; into a Phase 47 elisp object.  First handler in the Doc 122 §122.J
;; (struct-by-value) + §122.K (libc constants) sweep.
;;
;; Residual Rust:
;;
;;   * The signal-handler fn itself (`nl_sigint_handler_impl' in
;;     `eval/quit.rs') must stay Rust — signal handlers must be
;;     async-signal-safe, and elisp emit needs the runtime allocator
;;     which is async-signal-unsafe.  Its address is surfaced via the
;;     `nl_sigint_handler_addr' extern (= returns `*const () as i64')
;;     so the elisp body can write it into the `sa_sigaction' slot.
;;
;;   * Idempotency gate (`SIGINT_INSTALLED' static, AtomicI64) stays
;;     Rust — the `install_sigint_handler' shim early-returns when
;;     the flag is already 1.  The elisp body flips the flag via
;;     `ptr-write-u64' after `sigaction(2)' returns.
;;
;; The elisp body's job is the *struct construction + libc call*:
;;
;;   1. struct-make 'sigaction 152 8        — alloc sigaction buffer.
;;   2. Zero `sa_mask' (128 bytes) at offsets 8..135 (= 16 × u64) and
;;      sa_restorer at offset 144 (= 8 bytes).
;;   3. Write `sa_sigaction' (= nl_sigint_handler_addr) at offset 0.
;;   4. Write `sa_flags' (= SA_RESTART = 0x10000000) at offset 136 (u32).
;;   5. extern-call sigaction SIGINT &sa NULL.
;;   6. dealloc-bytes buf 152 8.
;;   7. ptr-write-u64 sigint_installed_ptr 0 1.
;;
;; Linux x86_64 ABI:
;;   sigaction(2) signature: int sigaction(int signum,
;;     const struct sigaction *act, struct sigaction *oldact);
;;
;; struct sigaction layout (linux glibc x86_64, sizeof = 152):
;;   offset   0: sa_handler / sa_sigaction (8 bytes, fn pointer)
;;   offset   8: sa_mask                   (128 bytes, sigset_t)
;;   offset 136: sa_flags                  (4 bytes, int)
;;   offset 140: <padding>                 (4 bytes)
;;   offset 144: sa_restorer               (8 bytes, fn pointer)
;;
;; Substrate composition:
;;
;;   §122.J  `struct-make' / `struct-field-set' — sigaction buffer +
;;                                                 u32/u64 field writes.
;;   §122.E  `ptr-write-u64'                    — sa_mask zeroing + flag.
;;   §122.K  libc constants `nelisp-cc--SIGINT' / `--SA_RESTART'.
;;   §125.A  `dealloc-bytes'                    — free sigaction buffer.
;;   §100.A  `extern-call'                      — `sigaction(2)' libc +
;;                                                 `nl_sigint_handler_addr'
;;                                                 + `nl_sigint_installed_ptr'.
;;
;; Sequencing model: Phase 47 evaluates function arguments left-to-
;; right per SysV AMD64.  Each sequencer (`prog2', `seq4') is just a
;; trivial `(val _eff...) -> val' function — the side effects in the
;; `_eff' arms run because their arg slot is evaluated before the
;; function body executes.  We use a nested call chain where each
;; outer call's first arg is the inner step's result, so the
;; evaluation order is: alloc -> zero -> write-fields -> sigaction ->
;; cleanup.
;;
;; Function contract (`nelisp_bi_install_sigint_handler'):
;;   args:    none (0-arg).
;;   returns: i64 — libc `sigaction(2)' rc (= 0 on success, -1 on err).
;;            The Rust shim discards the return — pre-swap
;;            `libc::sigaction(...)' also ignored the rc.
;;
;; Per Doc 117 §4.3: pure migration — no behaviour change.  The Rust
;; shim preserves the idempotency gate + the t/nil return type; only
;; the sigaction-struct construction + libc dispatch moves into elisp.

;;; Code:

(require 'nelisp-cc-libc-constants)

(defconst nelisp-cc-bi-install-sigint-handler--source
  `(seq
    ;; Side-effect sequencer — 2-arg `(val _eff) -> val'.  Evaluates
    ;; both args left-to-right per SysV; val computed first, eff
    ;; computed second.  Identical shape to
    ;; `nelisp_bi_nl_make_directory_prog2'.
    (defun nelisp_bi_install_sigint_handler_prog2 (val _eff) val)

    ;; 4-arg sequencer — `(val _e1 _e2 _e3) -> val'.  Used to sequence
    ;; multiple side effects after a value.  Args eval left-to-right.
    (defun nelisp_bi_install_sigint_handler_seq4 (val _e1 _e2 _e3) val)

    ;; Zero the 128-byte `sa_mask' field + 8-byte `sa_restorer' via
    ;; 17 × `ptr-write-u64' calls.  Returns BUF for forward chaining.
    ;;
    ;; The 17 ptr-write-u64 side effects run because the seq4 args
    ;; are evaluated before the function body returns BUF (the first
    ;; arg = the returned value).  Order: arg0 (= buf load, no eff),
    ;; arg1, arg2, arg3 (each = a nested seq4 holding 4 writes), then
    ;; return val = buf.  Each inner seq4 args also eval L-to-R.
    ;;
    ;; Arity = 1 (odd).  Single internal call lands through standard
    ;; Phase 47 prologue.
    (defun nelisp_bi_install_sigint_handler_zero_mask (buf)
      (nelisp_bi_install_sigint_handler_seq4
       buf
       ;; Group A: offsets 8, 16, 24, 32 (= first 4 u64's of sa_mask).
       (nelisp_bi_install_sigint_handler_seq4
        (ptr-write-u64 buf 8 0)
        (ptr-write-u64 buf 16 0)
        (ptr-write-u64 buf 24 0)
        (ptr-write-u64 buf 32 0))
       ;; Group B: offsets 40, 48, 56, 64.
       (nelisp_bi_install_sigint_handler_seq4
        (ptr-write-u64 buf 40 0)
        (ptr-write-u64 buf 48 0)
        (ptr-write-u64 buf 56 0)
        (ptr-write-u64 buf 64 0))
       ;; Group C: offsets 72, 80, 88, 96.
       (nelisp_bi_install_sigint_handler_seq4
        (ptr-write-u64 buf 72 0)
        (ptr-write-u64 buf 80 0)
        (ptr-write-u64 buf 88 0)
        (ptr-write-u64 buf 96 0))))

    ;; Continue zeroing offsets 104..128 + 144 (sa_restorer).  We
    ;; need a second function because zero_mask above is already at
    ;; arity 1 with a fanned 4-arg seq4 + 3 nested 4-arg seq4's; the
    ;; remaining 5 writes (104, 112, 120, 128, 144) need their own
    ;; pass.  Returns BUF for forward chaining.
    (defun nelisp_bi_install_sigint_handler_zero_mask_tail (buf)
      (nelisp_bi_install_sigint_handler_seq4
       buf
       (ptr-write-u64 buf 104 0)
       (ptr-write-u64 buf 112 0)
       (nelisp_bi_install_sigint_handler_seq4
        0
        (ptr-write-u64 buf 120 0)
        (ptr-write-u64 buf 128 0)
        ;; sa_restorer at offset 144 (= 8 bytes).  Linux glibc no
        ;; longer requires this slot but `man 2 sigaction' warns
        ;; against leaving it uninitialised, so we zero it.
        (ptr-write-u64 buf 144 0))))

    ;; Write sa_sigaction (offset 0) + sa_flags (offset 136).  Returns
    ;; BUF for forward chaining.  Args eval L-to-R so: buf-load (no
    ;; eff), ptr-write-u64 (handler), ptr-write-u32 (flags), pad,
    ;; then val=buf returned.
    ;;
    ;; Arity = 2 (even).
    (defun nelisp_bi_install_sigint_handler_write_fields (buf handler-addr)
      (nelisp_bi_install_sigint_handler_seq4
       buf
       (ptr-write-u64 buf 0 handler-addr)
       (ptr-write-u32 buf 136 ,nelisp-cc--SA_RESTART)
       0))

    ;; Cleanup after sigaction(2): free the buffer, set the installed
    ;; flag.  Returns RC for forward chaining.
    ;;
    ;; Arity = 3 (odd).
    (defun nelisp_bi_install_sigint_handler_cleanup (rc buf installed-ptr)
      (nelisp_bi_install_sigint_handler_seq4
       rc
       (dealloc-bytes buf 152 8)
       (ptr-write-u64 installed-ptr 0 1)
       0))

    ;; Inner driver — given a fully populated buf, call sigaction(2)
    ;; then cleanup.  Returns the sigaction(2) rc.
    ;;
    ;; Arity = 2 (even).  Args eval L-to-R: buf-load, then
    ;; installed-ptr-load (= the extern-call below); then the body
    ;; runs cleanup(sigaction(SIGINT,buf,0), buf, installed-ptr).
    ;;
    ;; sigaction(2) signature: int sigaction(int signum,
    ;;                            const struct sigaction *act,
    ;;                            struct sigaction *oldact).
    ;; Args via SysV AMD64: rdi=SIGINT, rsi=buf, rdx=NULL (0).
    (defun nelisp_bi_install_sigint_handler_call (buf installed-ptr)
      (nelisp_bi_install_sigint_handler_cleanup
       (extern-call sigaction ,nelisp-cc--SIGINT buf 0)
       buf
       installed-ptr))

    ;; Two-step zero_mask wrapper — call zero_mask_tail on the result
    ;; of zero_mask so both passes run.  Arity = 1.
    (defun nelisp_bi_install_sigint_handler_zero (buf)
      (nelisp_bi_install_sigint_handler_zero_mask_tail
       (nelisp_bi_install_sigint_handler_zero_mask buf)))

    ;; Public 0-arg entry — allocates the sigaction buffer, zeroes
    ;; sa_mask + sa_restorer, writes sa_sigaction + sa_flags, calls
    ;; sigaction(2), frees the buffer, sets the installed flag.
    ;; Returns the sigaction(2) rc (= 0 on success, -1 on err — Rust
    ;; shim discards).
    ;;
    ;; Arity = 0 (even).  Composition order (innermost evaluated
    ;; first per SysV L-to-R):
    ;;   1. (struct-make 'sigaction 152 8)        => buf
    ;;   2. (zero buf)                             => buf (after writes)
    ;;   3. (write-fields buf (nl_sigint_handler_addr)) => buf (after writes)
    ;;   4. (call buf (nl_sigint_installed_ptr))  => rc (after sigaction
    ;;                                                + dealloc + flag set)
    (defun nelisp_bi_install_sigint_handler ()
      (nelisp_bi_install_sigint_handler_call
       (nelisp_bi_install_sigint_handler_write_fields
        (nelisp_bi_install_sigint_handler_zero
         (struct-make 'sigaction 152 8))
        (extern-call nl_sigint_handler_addr))
       (extern-call nl_sigint_installed_ptr))))
  "Phase 47 source for the Doc 117 §117.D.gaps.4 `(install-sigint-
handler)' sigaction-struct construction + libc dispatch swap.

Eight-entry `(seq DEFUN ...)' manifest:
- `nelisp_bi_install_sigint_handler_prog2 (val _eff) -> val' — cache.
- `nelisp_bi_install_sigint_handler_seq4 (val _e1 _e2 _e3) -> val'.
- `nelisp_bi_install_sigint_handler_zero_mask (buf)' — first 12 ×
  ptr-write-u64 over offsets 8..96.
- `nelisp_bi_install_sigint_handler_zero_mask_tail (buf)' — last 5 ×
  ptr-write-u64 over offsets 104, 112, 120, 128, 144.
- `nelisp_bi_install_sigint_handler_write_fields (buf handler-addr)' —
  ptr-write-u64 (sa_sigaction @0) + ptr-write-u32 (sa_flags @136).
- `nelisp_bi_install_sigint_handler_cleanup (rc buf installed-ptr)' —
  dealloc + flag set; returns rc.
- `nelisp_bi_install_sigint_handler_call (buf installed-ptr)' —
  sigaction(2) + cleanup; returns rc.
- `nelisp_bi_install_sigint_handler_zero (buf)' — chain both zero
  passes; returns buf.
- `nelisp_bi_install_sigint_handler ()' — public 0-arg entry.

Composes only existing Phase 47 grammar (§122.J struct-by-value +
§122.E ptr-{read,write}-uN + §122.K libc constants + §125.A
dealloc-bytes + §100.A extern-call).  No new opcode.

Linux-x86_64 only — same arch gate as the §122.J substrate.")

(provide 'nelisp-cc-bi-install-sigint-handler)

;;; nelisp-cc-bi-install-sigint-handler.el ends here
