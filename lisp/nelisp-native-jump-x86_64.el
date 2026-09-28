;;; nelisp-native-jump-x86_64.el --- private x86-64 jump pair -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Emit a provider-owned returns-twice jump pair for x86-64 SysV.
;; The opaque buffer is 64 bytes or larger, with offsets 0..56 holding
;; RBX, RBP, R12-R15, post-call RSP, and return RIP.  This private layout
;; must never be passed to glibc or GNU Emacs setjmp/longjmp.

;;; Code:

(require 'nelisp-asm-x86_64)

(defconst nelisp-native-jump-x86_64-buffer-size 64
  "Minimum private buffer size in bytes.")

(defun nelisp-native-jump-x86_64--emit-jmp-reg (buf reg)
  "Emit an indirect near jump through REG into BUF."
  (let ((ext (nelisp-asm-x86_64--reg-ext reg))
        (low (nelisp-asm-x86_64--reg-low3 reg)))
    (if (zerop ext)
        (nelisp-asm-x86_64--append-bytes
         buf (unibyte-string #xFF (nelisp-asm-x86_64--modrm 3 4 low)))
      (nelisp-asm-x86_64--append-bytes
       buf (unibyte-string (nelisp-asm-x86_64--rex 0 0 0 1)
                           #xFF (nelisp-asm-x86_64--modrm 3 4 low))))))

(defun nelisp-native-jump-x86_64-emit-setjmp (buf label)
  "Emit a private setjmp entry at LABEL into BUF.
RDI points to a writable buffer of at least
`nelisp-native-jump-x86_64-buffer-size' bytes.  Initial return is 0."
  (nelisp-asm-x86_64-define-label buf label)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 0 'rbx)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 8 'rbp)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 16 'r12)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 24 'r13)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 32 'r14)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 40 'r15)
  (nelisp-asm-x86_64-mov-reg-reg buf 'rax 'rsp)
  (nelisp-asm-x86_64-add-imm32 buf 'rax 8)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 48 'rax)
  (nelisp-asm-x86_64-mov-reg-mem-rsp-disp buf 'rax 0)
  (nelisp-asm-x86_64-mov-mem-reg-disp8 buf 'rdi 56 'rax)
  (nelisp-asm-x86_64-mov-imm32 buf 'rax 0)
  (nelisp-asm-x86_64-ret buf))

(defun nelisp-native-jump-x86_64-emit-longjmp (buf label)
  "Emit a private longjmp entry at LABEL into BUF.
RDI points to the matching buffer and RSI supplies the return value;
zero is normalized to one.  The transfer never returns normally."
  (let ((nonzero (make-symbol "nelisp-jump-nonzero")))
    (nelisp-asm-x86_64-define-label buf label)
    ;; `longjmp' accepts C int.  Move only ESI so upper argument bits
    ;; cannot affect zero normalization and EAX has the ABI return width.
    (nelisp-asm-x86_64--append-bytes buf (unibyte-string #x89 #xF0))
    (nelisp-asm-x86_64-cmp-imm32 buf 'rax 0)
    (nelisp-asm-x86_64-jnz-rel32 buf nonzero)
    (nelisp-asm-x86_64-mov-imm32 buf 'rax 1)
    (nelisp-asm-x86_64-define-label buf nonzero)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'r11 'rdi 56)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'r10 'rdi 48)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'rbx 'rdi 0)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'rbp 'rdi 8)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'r12 'rdi 16)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'r13 'rdi 24)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'r14 'rdi 32)
    (nelisp-asm-x86_64-mov-reg-mem-disp8 buf 'r15 'rdi 40)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rsp 'r10)
    (nelisp-native-jump-x86_64--emit-jmp-reg buf 'r11)))

(provide 'nelisp-native-jump-x86_64)

;;; nelisp-native-jump-x86_64.el ends here
