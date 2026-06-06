;;; nelisp-cc-bi-syscall-resolve-nr.el --- Doc 117.D bi_syscall name->nr AOT swap -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 117.D — moves the 25-entry symbol-name → SYS_X table
;; dispatch out of `build-tool/src/eval/builtins.rs::bi_syscall'
;; (the Linux-x86_64 dispatcher's biggest chunk, ~16 LOC of inline
;; `match arm => libc::SYS_X' pairs) and into a AOT elisp
;; object.  Unlocks Rust net-negative for the `nelisp--syscall'
;; primitive: after this shim ships, `bi_syscall' becomes a thin
;; arity-check + `nl_jit_syscall_call' invocation that delegates
;; the symbol→nr lookup to this `.o' via the new (G1)
;; `(symbol-name-eq SYM_PTR "literal")' grammar op.
;;
;; Function contract:
;;   sym-ptr  *const Sexp pointing at a Sexp::Symbol value (= caller-
;;            validated before the call; non-Symbol input is ruled
;;            out by the Rust shim's arity / type checks).
;;   returns  i64 = Linux x86_64 syscall number on a hit, -1 sentinel
;;            on miss (= caller signals UserError "unknown syscall").
;;
;; AOT ops consumed:
;;   `(symbol-name-eq SYM_PTR "literal")' — G1 op (= tag-byte check +
;;        length-byte check + inline byte loop against compile-time
;;        bytes).  Returns 0 / 1.
;;   `(if (= EXPR 1) THEN ELSE)' — short-circuit dispatch chain.
;;   integer literals — fold into `mov rax, imm32' / `mov eax, imm32'.
;;
;; The 25-entry chain is auto-built at .o-source compile time from
;; `nelisp-cc-bi-syscall-resolve-nr--linux-x86_64-table' (= the
;; source-of-truth list, kept in sync with the Rust SYS_X arms in
;; `bi_syscall' and the auto-generated `lisp/nelisp-syscall-table.el'
;; per Doc 84 §84.2).  Keeping the list inline here (rather than
;; require-ing the auto-gen file which uses `setq' + no `provide')
;; keeps the .o self-contained.  Maintainer rule: if you add an arm
;; to the Rust `bi_syscall' table OR to `emit_linux_table' in
;; `build-tool/build.rs', add the matching entry here.
;;
;; Linux x86_64 only.  Same arch gate as the underlying
;; `jit/syscall.rs' (= `cfg(target_os = "linux", target_arch =
;; "x86_64")') and the existing §117.B file-I/O sweep.

;;; Code:

(eval-when-compile (require 'cl-lib))

(defconst nelisp-cc-bi-syscall-resolve-nr--linux-x86_64-table
  '(("read"               . 0)
    ("write"              . 1)
    ("close"              . 3)
    ("openat"             . 257)
    ("exit_group"         . 231)
    ("lseek"              . 8)
    ("dup2"               . 33)
    ("getpid"             . 39)
    ("kill"               . 62)
    ("mmap"               . 9)
    ("mprotect"           . 10)
    ("munmap"             . 11)
    ("fcntl"              . 72)
    ("fork"               . 57)
    ("socket"             . 41)
    ("listen"             . 50)
    ("wait4"              . 61)
    ("getppid"            . 110)
    ("setpgid"            . 109)
    ("pidfd_open"         . 434)
    ("pidfd_send_signal"  . 424)
    ("inotify_init1"      . 294)
    ("inotify_rm_watch"   . 255)
    ("eventfd2"           . 290)
    ("timerfd_create"     . 283))
  "Source-of-truth alist of (NAME-STRING . LINUX-X86_64-SYS-NR).
Mirrors `build-tool/build.rs' `emit_linux_table' + the Rust
`bi_syscall' arms.")

(defun nelisp-cc-bi-syscall-resolve-nr--build-chain (entries)
  "Build the nested `if'-chain for ENTRIES (alist of name . nr).
Terminator returns -1 = `unknown syscall' sentinel."
  (if (null entries)
      -1
    (let* ((pair (car entries))
           (name (car pair))
           (nr (cdr pair)))
      `(if (= (symbol-name-eq sym-ptr ,name) 1)
           ,nr
         ,(nelisp-cc-bi-syscall-resolve-nr--build-chain (cdr entries))))))

(defconst nelisp-cc-bi-syscall-resolve-nr--source
  `(defun nelisp_bi_syscall_resolve_nr (sym-ptr)
     ,(nelisp-cc-bi-syscall-resolve-nr--build-chain
       nelisp-cc-bi-syscall-resolve-nr--linux-x86_64-table))
  "AOT source for the Doc 117.D `bi_syscall' symbol→nr swap.

One-argument function — AOT's SysV AMD64 prologue spills
`sym-ptr' (= `*const Sexp' Sexp::Symbol) into rbp-relative slot 0.
The body is a 25-deep nested `if' chain; each arm calls G1's
`(symbol-name-eq sym-ptr LITERAL)' (= tag-byte / len-byte guard
+ inline byte loop, ~50 bytes of code per literal depending on
length) and returns the matching Linux x86_64 syscall number via
`mov eax, imm32; ret' on a hit.  The terminator falls through to
`mov rax, -1; ret' for unknown names (= the Rust caller turns
this sentinel into an `EvalError').

No PLT calls inside the function body — `symbol-name-eq' is
fully inline (no shared helper) and the integer literals
(0..434) fit in `imm32' (Linux x86_64 SYS_X numbers cap at the
mid-400s for the entries we cover, well under INT32_MAX).

Net Rust impact: removes the 16-LOC `Sexp::Symbol(s) => ... s.as_str() ...'
arm and the inline 25-entry `match` from `build-tool/src/eval/builtins.rs::bi_syscall'
(L304-331 in the pre-Doc 117.D source).  The Rust shim shrinks
to: arity-check + Int passthrough + Symbol → call this `.o' +
Int args[1..7] decode + `nl_jit_syscall_call' invocation.")

(provide 'nelisp-cc-bi-syscall-resolve-nr)

;;; nelisp-cc-bi-syscall-resolve-nr.el ends here
