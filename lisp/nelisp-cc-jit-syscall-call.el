;;; nelisp-cc-jit-syscall-call.el --- AOT nl_jit_syscall_call + nl_jit_syscall_supported_p  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; AOT migration of `build-tool/src/jit/syscall.rs':
;;   `nl_jit_syscall_call'        — raw Linux x86_64 SYSCALL trampoline.
;;   `nl_jit_syscall_supported_p' — constant 1 sentinel.
;;
;; Both functions are reached via `dlsym(RTLD_DEFAULT, name)' through
;; the `nl_jit_alias' / `unified_fn_ptr' bridge path in
;; `build-tool/src/jit/bridge.rs':
;;   "nelisp_jit_syscall"            → nl_jit_syscall_call
;;   "nelisp_jit_syscall_supported_p" → nl_jit_syscall_supported_p
;;
;; Function contracts:
;;
;;   nl_jit_syscall_call (nr a0 a1 a2 a3 a4) -> i64
;;     Performs the Linux x86_64 SYSCALL instruction with nr as the
;;     syscall number and a0-a4 as arguments (a5 is implicitly 0 —
;;     see limitation note below).  Returns the kernel's raw result:
;;     non-negative = success value, negative = -errno on error.
;;     No libc errno normalisation is needed: the kernel already
;;     returns -errno directly (unlike libc::syscall which returns -1
;;     and sets errno).
;;
;;     Callers see the same -errno convention as the pre-swap Rust
;;     `nl_jit_syscall_call', which used `libc::syscall' and then
;;     mapped `r == -1' → `-*__errno_location()'.  Both paths produce
;;     -errno.  The raw-SYSCALL path is correct and more direct.
;;
;;   nl_jit_syscall_supported_p () -> i64
;;     Returns 1 (= syscalls are supported; Linux x86_64 always true).
;;     Called via `nl-jit-call-i64-i64' with two dummy i64 args that
;;     the 0-parameter SysV prologue silently ignores.
;;
;; Limitation — a5 is hardcoded to 0:
;;   `nl_jit_syscall_call' has 6 SysV AMD64 register-passed params
;;   (nr, a0-a4).  The 7th C argument (a5) would arrive on the stack
;;   at [rbp+16] after prologue; AOT has no grammar op to read
;;   that slot yet (7-arg defun support is deferred to a future Doc).
;;   The `nelisp--syscall' wrapper in `nelisp-jit-strategy.el' passes
;;   a5=0 by default; the only callers that use non-zero a5 are mmap
;;   calls with a non-zero file offset.  Anonymous mmap (offset=0) is
;;   unaffected.
;;
;; AOT grammar ops consumed:
;;   `syscall-direct NR A0 A1 A2 A3 A4 A5' — new op added alongside
;;        this swap; emits Linux SYSCALL with the correct kernel ABI
;;        register layout (rax=nr, rdi=a0, rsi=a1, rdx=a2, r10=a3,
;;        r8=a4, r9=a5).  x86_64 only, matching the Rust source's
;;        `cfg(target_os = "linux", target_arch = "x86_64")' gate.
;;
;; Build wiring:
;;   `scripts/compile-elisp-objects.el' manifest entry →
;;     `nl_jit_syscall_call.o' (Linux x86_64 only).
;;   `build-tool/build.rs' manifest_sources list.
;;   `build-tool/src/jit/syscall.rs' deleted (full file).
;;   `build-tool/src/jit/mod.rs': `mod syscall;' removed.
;;
;; Net Rust delta: -57 LOC (full file delete).

;;; Code:

(defconst nelisp-cc-jit-syscall-call--source
  '(seq
    ;; ---- nl_jit_syscall_call (6-arg, a5 = 0) --------------------------------
    ;;
    ;; SysV AMD64 parameter registers (1-6): rdi rsi rdx rcx r8 r9.
    ;; AOT prologue spills each to [rbp-8*(slot+1)]:
    ;;   nr → slot 0  a0 → slot 1  a1 → slot 2
    ;;   a2 → slot 3  a3 → slot 4  a4 → slot 5
    ;;
    ;; `(syscall-direct nr a0 a1 a2 a3 a4 0)' maps:
    ;;   rax=nr  rdi=a0  rsi=a1  rdx=a2  r10=a3  r8=a4  r9=0
    ;; then executes SYSCALL.  Result in rax = kernel return value.
    (defun nl_jit_syscall_call (nr a0 a1 a2 a3 a4)
      (syscall-direct nr a0 a1 a2 a3 a4 0))

    ;; ---- nl_jit_syscall_supported_p (0-arg, constant 1) ---------------------
    ;;
    ;; Called via `nl-jit-call-i64-i64' with two dummy args (rdi, rsi)
    ;; which the 0-param prologue leaves unspilled.  Returns 1.
    (defun nl_jit_syscall_supported_p ()
      1))
  "AOT source for the `jit/syscall.rs' swap.

Two-entry `(seq DEFUN DEFUN)' manifest:
- `nl_jit_syscall_call (nr a0 a1 a2 a3 a4) -> i64'
  Raw Linux SYSCALL trampoline via `(syscall-direct NR A0..A4 0)'.
  a5 is hardcoded 0 (7-arg defun support deferred).
- `nl_jit_syscall_supported_p () -> i64'
  Constant 1; AOT 0-arg defun idiom (no prologue spills).

Linux x86_64 only — same gate as the Rust source this replaces.")

(provide 'nelisp-cc-jit-syscall-call)

;;; nelisp-cc-jit-syscall-call.el ends here
