;;; nelisp-asm-x86_64-test.el --- ERT tests for §92.a  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 92 §92.a — pure-elisp ert tests for the freestanding x86_64
;; macro assembler (`lisp/nelisp-asm-x86_64.el').  Verifies:
;;   1. byte/int helpers (REX, ModR/M, imm32, imm64),
;;   2. each instruction emitter against Intel SDM byte tables,
;;   3. label / fixup machinery (forward + backward rel32 resolve),
;;   4. relocation marker recording (= Doc 93 handoff stub),
;;   5. objdump cross-check for the canonical `exit(0)' shape
;;      (skipped if host has no `objdump').

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-asm-x86_64)

;; Doc 93 §93.a/b handshake helpers — declared (not required at load
;; time) so this file byte-compiles cleanly even if the static linker
;; module is reorganized later.  Tests `require' them lazily under
;; `skip-unless'.
(declare-function nelisp-link-symtab-make "nelisp-static-linker")
(declare-function nelisp-link-symtab-add "nelisp-static-linker" (st sym))
(declare-function nelisp-link-symbol
                  "nelisp-static-linker" (name value &rest rest))
(declare-function nelisp-link-apply-relocs
                  "nelisp-static-linker" (bytes relocs symtab section-va))

;; ---- helpers ----

(defun nelisp-asm-x86_64-test--emit (thunk)
  "Run THUNK on a fresh buffer, return (BYTES . BUF)."
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (funcall thunk b)
    (cons (nelisp-asm-x86_64-buffer-bytes b) b)))

(defun nelisp-asm-x86_64-test--bytes (thunk)
  "Run THUNK on a fresh buffer, return its accumulated bytes."
  (car (nelisp-asm-x86_64-test--emit thunk)))

(defun nelisp-asm-x86_64-test--ub (&rest bs)
  "Construct a unibyte-string from the integer args BS."
  (apply #'unibyte-string bs))

;; ---- L0 helpers ----

(ert-deftest nelisp-asm-x86_64-rex-all-zero ()
  (should (equal (nelisp-asm-x86_64--rex 0 0 0 0) #x40)))

(ert-deftest nelisp-asm-x86_64-rex-w-set ()
  (should (equal (nelisp-asm-x86_64--rex 1 0 0 0) #x48)))

(ert-deftest nelisp-asm-x86_64-rex-all-bits ()
  (should (equal (nelisp-asm-x86_64--rex 1 1 1 1) #x4F)))

(ert-deftest nelisp-asm-x86_64-modrm-mod3-reg-reg ()
  ;; mod=11, reg=0 (rax), rm=0 (rax) -> 0xC0
  (should (equal (nelisp-asm-x86_64--modrm 3 0 0) #xC0))
  ;; mod=11, reg=7 (rdi), rm=0 (rax) -> 0xF8
  (should (equal (nelisp-asm-x86_64--modrm 3 7 0) #xF8)))

(ert-deftest nelisp-asm-x86_64-imm32-roundtrip-zero ()
  (should (equal (nelisp-asm-x86_64--imm32-bytes 0)
                 (nelisp-asm-x86_64-test--ub 0 0 0 0))))

(ert-deftest nelisp-asm-x86_64-imm32-roundtrip-60 ()
  (should (equal (nelisp-asm-x86_64--imm32-bytes 60)
                 (nelisp-asm-x86_64-test--ub #x3C 0 0 0))))

(ert-deftest nelisp-asm-x86_64-imm32-negative ()
  ;; -1 = 0xFFFFFFFF when masked
  (should (equal (nelisp-asm-x86_64--imm32-bytes -1)
                 (nelisp-asm-x86_64-test--ub #xFF #xFF #xFF #xFF))))

(ert-deftest nelisp-asm-x86_64-imm32-out-of-range ()
  (should-error (nelisp-asm-x86_64--imm32-bytes (ash 1 33))
                :type 'nelisp-asm-x86_64-error))

(ert-deftest nelisp-asm-x86_64-imm64-roundtrip-0x42 ()
  (should (equal (nelisp-asm-x86_64--imm64-bytes #x42)
                 (nelisp-asm-x86_64-test--ub #x42 0 0 0 0 0 0 0))))

(ert-deftest nelisp-asm-x86_64-imm64-roundtrip-large ()
  ;; 0xDEADBEEFCAFEBABE
  (should (equal (nelisp-asm-x86_64--imm64-bytes #xDEADBEEFCAFEBABE)
                 (nelisp-asm-x86_64-test--ub #xBE #xBA #xFE #xCA
                                             #xEF #xBE #xAD #xDE))))

;; ---- buffer abstraction ----

(ert-deftest nelisp-asm-x86_64-buffer-empty ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should (equal (nelisp-asm-x86_64-buffer-bytes b) ""))
    (should (= (nelisp-asm-x86_64-buffer-pos b) 0))))

(ert-deftest nelisp-asm-x86_64-buffer-pos-after-ret ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-ret b)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 1))
    (should (equal (nelisp-asm-x86_64-buffer-bytes b)
                   (nelisp-asm-x86_64-test--ub #xC3)))))

(ert-deftest nelisp-asm-x86_64-buffer-pos-after-syscall ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-syscall b)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 2))))

;; ---- single-byte / opcode-only instructions ----

(ert-deftest nelisp-asm-x86_64-ret-encoding ()
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-ret b)))
                 (nelisp-asm-x86_64-test--ub #xC3))))

(ert-deftest nelisp-asm-x86_64-syscall-encoding ()
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-syscall b)))
                 (nelisp-asm-x86_64-test--ub #x0F #x05))))

(ert-deftest nelisp-asm-x86_64-nop-encoding ()
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-nop b)))
                 (nelisp-asm-x86_64-test--ub #x90))))

(ert-deftest nelisp-asm-x86_64-int3-encoding ()
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-int3 b)))
                 (nelisp-asm-x86_64-test--ub #xCC))))

(ert-deftest nelisp-asm-x86_64-hlt-encoding ()
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-hlt b)))
                 (nelisp-asm-x86_64-test--ub #xF4))))

;; ---- MOV imm32 ----

(ert-deftest nelisp-asm-x86_64-mov-rax-imm32-60 ()
  ;; `mov rax, 60' = `mov rax, $0x3c'
  ;; REX.W=0x48, opcode 0xC7, ModR/M=0xC0 (mod=11,/0,rm=rax=0), imm32 little-endian
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-imm32 b 'rax 60)))
                 (nelisp-asm-x86_64-test--ub
                  #x48 #xC7 #xC0 #x3C #x00 #x00 #x00))))

(ert-deftest nelisp-asm-x86_64-mov-rdi-imm32-0 ()
  ;; `mov rdi, 0' = 48 c7 c7 00 00 00 00
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-imm32 b 'rdi 0)))
                 (nelisp-asm-x86_64-test--ub
                  #x48 #xC7 #xC7 #x00 #x00 #x00 #x00))))

(ert-deftest nelisp-asm-x86_64-mov-r8-imm32 ()
  ;; `mov r8, 1' -> REX.WB=0x49, opcode 0xC7, ModR/M=0xC0 (rm=r8.low3=0)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-imm32 b 'r8 1)))
                 (nelisp-asm-x86_64-test--ub
                  #x49 #xC7 #xC0 #x01 #x00 #x00 #x00))))

(ert-deftest nelisp-asm-x86_64-mov-r15-imm32 ()
  ;; `mov r15, 0' -> REX.WB=0x49, opcode 0xC7, ModR/M=0xC7 (rm=r15.low3=7)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-imm32 b 'r15 0)))
                 (nelisp-asm-x86_64-test--ub
                  #x49 #xC7 #xC7 #x00 #x00 #x00 #x00))))

;; ---- MOV imm64 ----

(ert-deftest nelisp-asm-x86_64-mov-rax-imm64 ()
  ;; `mov rax, 0x42' (long form) -> 48 b8 42 00 00 00 00 00 00 00
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-imm64 b 'rax #x42)))
                 (nelisp-asm-x86_64-test--ub
                  #x48 #xB8 #x42 0 0 0 0 0 0 0))))

(ert-deftest nelisp-asm-x86_64-mov-r9-imm64-large ()
  ;; `mov r9, 0xCAFEBABEDEADBEEF' -> REX.WB=0x49, opcode 0xB9 (= 0xB8 + low3(r9)=1)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b)
                    (nelisp-asm-x86_64-mov-imm64 b 'r9
                                                 #xCAFEBABEDEADBEEF)))
                 (nelisp-asm-x86_64-test--ub
                  #x49 #xB9 #xEF #xBE #xAD #xDE #xBE #xBA #xFE #xCA))))

;; ---- MOV reg-reg ----

(ert-deftest nelisp-asm-x86_64-mov-rax-rax ()
  ;; `mov rax, rax' MR form -> 48 89 c0
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-reg-reg b 'rax 'rax)))
                 (nelisp-asm-x86_64-test--ub #x48 #x89 #xC0))))

(ert-deftest nelisp-asm-x86_64-mov-rdi-rax ()
  ;; `mov rdi, rax' MR -> 48 89 c7 (reg=rax=0, rm=rdi=7)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-reg-reg b 'rdi 'rax)))
                 (nelisp-asm-x86_64-test--ub #x48 #x89 #xC7))))

(ert-deftest nelisp-asm-x86_64-mov-r8-r15 ()
  ;; `mov r8, r15' MR -> REX.WRB=0x4D, 0x89, ModR/M=0xF8 (reg=r15.low3=7, rm=r8.low3=0)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-mov-reg-reg b 'r8 'r15)))
                 (nelisp-asm-x86_64-test--ub #x4D #x89 #xF8))))

;; ---- ADD / SUB / CMP ----

(ert-deftest nelisp-asm-x86_64-add-rax-rcx ()
  ;; `add rax, rcx' MR opcode 0x01 -> 48 01 c8 (reg=rcx=1, rm=rax=0)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-add-reg-reg b 'rax 'rcx)))
                 (nelisp-asm-x86_64-test--ub #x48 #x01 #xC8))))

(ert-deftest nelisp-asm-x86_64-sub-rsp-rbp ()
  ;; `sub rsp, rbp' MR 0x29 -> 48 29 ec (reg=rbp=5, rm=rsp=4)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-sub-reg-reg b 'rsp 'rbp)))
                 (nelisp-asm-x86_64-test--ub #x48 #x29 #xEC))))

;; ---- Doc 100 §100.D bitwise + shift helpers ----

(ert-deftest nelisp-asm-x86_64-or-rax-r10 ()
  ;; `or rax, r10' MR opcode 0x09, src=r10 sets REX.R, rm=rax=0,
  ;; reg=r10.low3=2 -> 4C 09 D0
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-or-reg-reg b 'rax 'r10)))
                 (nelisp-asm-x86_64-test--ub #x4C #x09 #xD0))))

(ert-deftest nelisp-asm-x86_64-and-rax-r10 ()
  ;; `and rax, r10' MR 0x21 -> 4C 21 D0
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-and-reg-reg b 'rax 'r10)))
                 (nelisp-asm-x86_64-test--ub #x4C #x21 #xD0))))

(ert-deftest nelisp-asm-x86_64-xor-rax-r10 ()
  ;; `xor rax, r10' MR 0x31 -> 4C 31 D0
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-xor-reg-reg b 'rax 'r10)))
                 (nelisp-asm-x86_64-test--ub #x4C #x31 #xD0))))

(ert-deftest nelisp-asm-x86_64-shl-rax-cl ()
  ;; `shl rax, cl' = 48 D3 E0 (REX.W + D3 /4 ModR/M)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-shl-rax-cl b)))
                 (nelisp-asm-x86_64-test--ub #x48 #xD3 #xE0))))

(ert-deftest nelisp-asm-x86_64-sar-rax-cl ()
  ;; `sar rax, cl' = 48 D3 F8 (REX.W + D3 /7 ModR/M)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-sar-rax-cl b)))
                 (nelisp-asm-x86_64-test--ub #x48 #xD3 #xF8))))

(ert-deftest nelisp-asm-x86_64-add-imm32-rax-1 ()
  ;; `add rax, 1' = 48 81 c0 01 00 00 00
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-add-imm32 b 'rax 1)))
                 (nelisp-asm-x86_64-test--ub
                  #x48 #x81 #xC0 #x01 #x00 #x00 #x00))))

(ert-deftest nelisp-asm-x86_64-sub-imm32-rsp-32 ()
  ;; `sub rsp, 32' = 48 81 ec 20 00 00 00 (/5, rm=rsp=4)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-sub-imm32 b 'rsp 32)))
                 (nelisp-asm-x86_64-test--ub
                  #x48 #x81 #xEC #x20 #x00 #x00 #x00))))

(ert-deftest nelisp-asm-x86_64-cmp-imm32-rax-0 ()
  ;; `cmp rax, 0' = 48 81 f8 00 00 00 00 (/7, rm=rax=0)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-cmp-imm32 b 'rax 0)))
                 (nelisp-asm-x86_64-test--ub
                  #x48 #x81 #xF8 #x00 #x00 #x00 #x00))))

;; ---- PUSH / POP ----

(ert-deftest nelisp-asm-x86_64-push-rbp ()
  ;; `push rbp' = 55 (= 0x50 + 5)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-push b 'rbp)))
                 (nelisp-asm-x86_64-test--ub #x55))))

(ert-deftest nelisp-asm-x86_64-pop-rax ()
  ;; `pop rax' = 58
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-pop b 'rax)))
                 (nelisp-asm-x86_64-test--ub #x58))))

(ert-deftest nelisp-asm-x86_64-push-r12 ()
  ;; `push r12' = 41 54 (REX.B + 0x50 + 4)
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-push b 'r12)))
                 (nelisp-asm-x86_64-test--ub #x41 #x54))))

(ert-deftest nelisp-asm-x86_64-pop-r15 ()
  ;; `pop r15' = 41 5F
  (should (equal (nelisp-asm-x86_64-test--bytes
                  (lambda (b) (nelisp-asm-x86_64-pop b 'r15)))
                 (nelisp-asm-x86_64-test--ub #x41 #x5F))))

;; ---- label + fixup (= rel32 resolution) ----

(ert-deftest nelisp-asm-x86_64-call-to-immediately-following-label ()
  ;; call foo; foo: ret  -> call rel32 = 0
  (let* ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-call-rel32 b 'foo)
    (nelisp-asm-x86_64-define-label b 'foo)
    (nelisp-asm-x86_64-ret b)
    (let ((bytes (nelisp-asm-x86_64-resolve-fixups b)))
      ;; E8 00 00 00 00 (call rel32=0) + C3 (ret) = 6 bytes
      (should (equal bytes
                     (nelisp-asm-x86_64-test--ub
                      #xE8 #x00 #x00 #x00 #x00 #xC3))))))

(ert-deftest nelisp-asm-x86_64-jmp-forward ()
  ;; jmp foo; nop; foo: ret  -> jmp rel32 = 1 (= one nop between)
  (let* ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-jmp-rel32 b 'foo)
    (nelisp-asm-x86_64-nop b)
    (nelisp-asm-x86_64-define-label b 'foo)
    (nelisp-asm-x86_64-ret b)
    (let ((bytes (nelisp-asm-x86_64-resolve-fixups b)))
      (should (equal bytes
                     (nelisp-asm-x86_64-test--ub
                      #xE9 #x01 #x00 #x00 #x00 #x90 #xC3))))))

(ert-deftest nelisp-asm-x86_64-jmp-backward ()
  ;; foo: nop; jmp foo -> jmp rel32 = -6 (= 0xFFFFFFFA LE)
  ;; layout: pos 0 -> nop (1B); pos 1 -> E9 + 4B placeholder; pos 6 end
  ;; rel = label_pos(0) - (slot(2) + 4) = -6
  (let* ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-define-label b 'foo)
    (nelisp-asm-x86_64-nop b)
    (nelisp-asm-x86_64-jmp-rel32 b 'foo)
    (let ((bytes (nelisp-asm-x86_64-resolve-fixups b)))
      (should (equal bytes
                     (nelisp-asm-x86_64-test--ub
                      #x90 #xE9 #xFA #xFF #xFF #xFF))))))

(ert-deftest nelisp-asm-x86_64-duplicate-label-signals ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-define-label b 'foo)
    (should-error (nelisp-asm-x86_64-define-label b 'foo)
                  :type 'nelisp-asm-x86_64-error)))

(ert-deftest nelisp-asm-x86_64-unresolved-label-signals ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-jmp-rel32 b 'nowhere)
    (should-error (nelisp-asm-x86_64-resolve-fixups b)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- relocation marker recording (= §92.a (6)) ----

(ert-deftest nelisp-asm-x86_64-emit-reloc-pc32 ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    ;; Pretend we emitted 4 placeholder bytes for a PC32 site at pos 0
    (nelisp-asm-x86_64-emit-reloc b 'pc32 'printf -4)
    (let ((r (car (nelisp-asm-x86_64-buffer-relocs b))))
      (should (eq (plist-get r :type) 'pc32))
      (should (eq (plist-get r :sym) 'printf))
      (should (= (plist-get r :offset) 0))
      (should (= (plist-get r :addend) -4)))))

(ert-deftest nelisp-asm-x86_64-emit-reloc-abs64-after-mov-imm64 ()
  ;; mov rax, 0  + abs64 reloc against `global_x' patching the imm64 slot
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-mov-imm64 b 'rax 0)
    ;; Slot for the imm64 starts at offset 2 (REX + opcode = 2 bytes).
    ;; But for spike we just record at current pos for simplicity.
    (nelisp-asm-x86_64-emit-reloc b 'abs64 'global_x 0)
    (should (= (length (nelisp-asm-x86_64-buffer-relocs b)) 1))
    (let ((r (car (nelisp-asm-x86_64-buffer-relocs b))))
      (should (eq (plist-get r :type) 'abs64))
      (should (= (plist-get r :offset) 10)))))

(ert-deftest nelisp-asm-x86_64-emit-reloc-unknown-type-signals ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-emit-reloc b 'absurd 'foo)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §92.c reloc shape canonicalization ----

(ert-deftest nelisp-asm-x86_64-emit-reloc-default-section-text ()
  ;; §92.a back-compat: no :section keyword -> defaults to `text'
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-reloc b 'pc32 "printf" -4)
    (let ((r (car (nelisp-asm-x86_64-buffer-relocs b))))
      (should (eq (plist-get r :section) 'text))
      (should (equal (plist-get r :symbol) "printf"))
      (should (equal (plist-get r :sym) "printf"))
      (should (eq (plist-get r :type) 'pc32))
      (should (= (plist-get r :addend) -4)))))

(ert-deftest nelisp-asm-x86_64-emit-reloc-section-rodata ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-reloc b 'abs64 "msg" 0 :section 'rodata)
    (let ((r (car (nelisp-asm-x86_64-buffer-relocs b))))
      (should (eq (plist-get r :section) 'rodata))
      (should (eq (plist-get r :type) 'abs64)))))

(ert-deftest nelisp-asm-x86_64-emit-reloc-section-data ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-reloc b 'pc32 "gv" 0 :section 'data)
    (let ((r (car (nelisp-asm-x86_64-buffer-relocs b))))
      (should (eq (plist-get r :section) 'data)))))

(ert-deftest nelisp-asm-x86_64-extract-relocs-canonical-shape ()
  ;; Verify §93.a-compatible shape: :offset / :type / :symbol /
  ;; :addend / :section, without internal :sym alias.
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-reloc b 'pc32 "printf" -4)
    (let* ((relocs (nelisp-asm-x86_64-extract-relocs b))
           (r (car relocs)))
      (should (= (length relocs) 1))
      (should (= (plist-get r :offset) 0))
      (should (eq (plist-get r :type) 'pc32))
      (should (equal (plist-get r :symbol) "printf"))
      (should (= (plist-get r :addend) -4))
      (should (eq (plist-get r :section) 'text))
      ;; :sym alias should be stripped from the extractor output.
      (should-not (plist-member r :sym)))))

(ert-deftest nelisp-asm-x86_64-extract-relocs-order-preserved ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-reloc b 'pc32 "a" 0)
    (nelisp-asm-x86_64-emit-reloc b 'abs64 "b" 0)
    (nelisp-asm-x86_64-emit-reloc b 'plt32 "c" 0)
    (let ((relocs (nelisp-asm-x86_64-extract-relocs b)))
      (should (= (length relocs) 3))
      (should (equal (plist-get (nth 0 relocs) :symbol) "a"))
      (should (equal (plist-get (nth 1 relocs) :symbol) "b"))
      (should (equal (plist-get (nth 2 relocs) :symbol) "c")))))

(ert-deftest nelisp-asm-x86_64-reloc-pc32-here-emits-zeros ()
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (off (nelisp-asm-x86_64-reloc-pc32-here b "callee" -4)))
    (should (= off 0))
    (should (equal (nelisp-asm-x86_64-buffer-bytes b)
                   (nelisp-asm-x86_64-test--ub 0 0 0 0)))
    (should (= (nelisp-asm-x86_64-buffer-pos b) 4))
    (let ((r (car (nelisp-asm-x86_64-extract-relocs b))))
      (should (= (plist-get r :offset) 0))
      (should (eq (plist-get r :type) 'pc32))
      (should (equal (plist-get r :symbol) "callee"))
      (should (= (plist-get r :addend) -4)))))

(ert-deftest nelisp-asm-x86_64-reloc-pc32-here-with-prefix ()
  ;; CALL opcode + pc32 placeholder
  (let* ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-bytes b (unibyte-string #xE8))
    (let ((off (nelisp-asm-x86_64-reloc-pc32-here b "main" -4)))
      (should (= off 1))
      (should (equal (nelisp-asm-x86_64-buffer-bytes b)
                     (nelisp-asm-x86_64-test--ub #xE8 0 0 0 0)))
      (let ((r (car (nelisp-asm-x86_64-extract-relocs b))))
        (should (= (plist-get r :offset) 1))
        (should (= (plist-get r :addend) -4))))))

(ert-deftest nelisp-asm-x86_64-reloc-abs64-here-emits-zeros ()
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (off (nelisp-asm-x86_64-reloc-abs64-here b "gv" 0)))
    (should (= off 0))
    (should (= (length (nelisp-asm-x86_64-buffer-bytes b)) 8))
    (should (equal (nelisp-asm-x86_64-buffer-bytes b)
                   (nelisp-asm-x86_64-test--ub 0 0 0 0 0 0 0 0)))
    (let ((r (car (nelisp-asm-x86_64-extract-relocs b))))
      (should (eq (plist-get r :type) 'abs64))
      (should (= (plist-get r :offset) 0)))))

(ert-deftest nelisp-asm-x86_64-reloc-plt32-here-emits-zeros ()
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (off (nelisp-asm-x86_64-reloc-plt32-here b "puts" -4)))
    (should (= off 0))
    (should (= (length (nelisp-asm-x86_64-buffer-bytes b)) 4))
    (let ((r (car (nelisp-asm-x86_64-extract-relocs b))))
      (should (eq (plist-get r :type) 'plt32))
      (should (= (plist-get r :addend) -4)))))

(ert-deftest nelisp-asm-x86_64-reloc-helpers-respect-section-arg ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-reloc-pc32-here b "ref" 0 'rodata)
    (let ((r (car (nelisp-asm-x86_64-extract-relocs b))))
      (should (eq (plist-get r :section) 'rodata)))))

(ert-deftest nelisp-asm-x86_64-buffer-to-unit-shape ()
  ;; Tiny snippet: mov rax, 60 ; ret  (no relocs, one label)
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-define-label b '_start)
    (nelisp-asm-x86_64-mov-imm32 b 'rax 60)
    (nelisp-asm-x86_64-ret b)
    (let* ((unit (nelisp-asm-x86_64-buffer-to-unit b "tiny.o"))
           (sections (plist-get unit :sections))
           (syms (plist-get unit :symbols)))
      (should (equal (plist-get unit :name) "tiny.o"))
      (should (= (length sections) 1))
      (should (eq (car (car sections)) 'text))
      (should (= (length (cdr (assq 'text sections))) 8))
      (should (= (length syms) 1))
      (let ((s (car syms)))
        (should (equal (plist-get s :name) "_start"))
        (should (= (plist-get s :value) 0))
        (should (eq (plist-get s :section) 'text))
        (should (eq (plist-get s :bind) 'global)))
      (should (null (plist-get unit :relocs))))))

(ert-deftest nelisp-asm-x86_64-buffer-to-unit-with-rodata ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-ret b)
    (let* ((unit (nelisp-asm-x86_64-buffer-to-unit
                  b "u" :rodata (unibyte-string ?h ?i)))
           (sections (plist-get unit :sections)))
      (should (= (length sections) 2))
      (should (equal (cdr (assq 'text sections))
                     (unibyte-string #xC3)))
      (should (equal (cdr (assq 'rodata sections))
                     (unibyte-string ?h ?i))))))

(ert-deftest nelisp-asm-x86_64-buffer-to-unit-with-reloc ()
  ;; CALL placeholder + pc32 reloc — verify reloc is wired through.
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-bytes b (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-pc32-here b "callee" -4)
    (let* ((unit (nelisp-asm-x86_64-buffer-to-unit b "caller.o"))
           (relocs (plist-get unit :relocs)))
      (should (= (length relocs) 1))
      (let ((r (car relocs)))
        (should (= (plist-get r :offset) 1))
        (should (eq (plist-get r :type) 'pc32))
        (should (equal (plist-get r :symbol) "callee"))
        (should (= (plist-get r :addend) -4))
        (should (eq (plist-get r :section) 'text))))))

;; ---- §92.c ↔ §93.a/b handshake micro-integration ----

(ert-deftest nelisp-asm-x86_64-93-handshake-pc32 ()
  "End-to-end: emit pc32 reloc via §92.c, patch via §93.a apply-reloc."
  (skip-unless (or (require 'nelisp-static-linker nil 'noerror)
                   (locate-library "nelisp-static-linker")))
  (require 'nelisp-static-linker)
  ;; Caller buffer: CALL placeholder targeting `callee'.
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (_ (nelisp-asm-x86_64-emit-bytes b (unibyte-string #xE8)))
         (_off (nelisp-asm-x86_64-reloc-pc32-here b "callee" -4))
         (relocs (nelisp-asm-x86_64-extract-relocs b))
         (bytes (nelisp-asm-x86_64-buffer-bytes b))
         (symtab (nelisp-link-symtab-make)))
    ;; Define `callee' at VA 0x1000 + 0x40 (= section-va + offset).
    (nelisp-link-symtab-add
     symtab (nelisp-link-symbol "callee" #x1040))
    (let* ((patched (nelisp-link-apply-relocs
                     bytes relocs symtab #x1000)))
      ;; CALL + rel32(callee - (call-site + 5)).
      ;; call site = 0x1000 (CALL opcode) + rel32 patches at 0x1001..0x1004
      ;; P = section-va + offset + 4 = 0x1000 + 1 + 4 = 0x1005
      ;; rel = (S + A) - P = (0x1040 + (-4)) - 0x1005 = 0x37
      (should (= (length patched) 5))
      (should (= (aref patched 0) #xE8))
      (should (= (aref patched 1) #x37))
      (should (= (aref patched 2) #x00))
      (should (= (aref patched 3) #x00))
      (should (= (aref patched 4) #x00)))))

(ert-deftest nelisp-asm-x86_64-93-handshake-via-buffer-to-unit ()
  "End-to-end: §92.c buffer-to-unit ↔ §93.a apply-relocs."
  (skip-unless (or (require 'nelisp-static-linker nil 'noerror)
                   (locate-library "nelisp-static-linker")))
  (require 'nelisp-static-linker)
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-emit-bytes b (unibyte-string #xE8))
    (nelisp-asm-x86_64-reloc-pc32-here b "callee" -4)
    (let* ((unit (nelisp-asm-x86_64-buffer-to-unit b "u.o"))
           (text (cdr (assq 'text (plist-get unit :sections))))
           (relocs (plist-get unit :relocs))
           (symtab (nelisp-link-symtab-make)))
      (nelisp-link-symtab-add
       symtab (nelisp-link-symbol "callee" #x2040))
      (let ((patched (nelisp-link-apply-relocs
                      text relocs symtab #x2000)))
        ;; rel = (0x2040 + -4) - (0x2000 + 1 + 4) = 0x37
        (should (= (aref patched 1) #x37))))))

;; ---- composite: minimal exit(0) ----

(ert-deftest nelisp-asm-x86_64-exit0-shape ()
  ;; mov rax, 60 ; mov rdi, 0 ; syscall  (== _exit(0) on Linux x86_64)
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-mov-imm32 b 'rax 60)
    (nelisp-asm-x86_64-mov-imm32 b 'rdi 0)
    (nelisp-asm-x86_64-syscall b)
    (should (equal (nelisp-asm-x86_64-buffer-bytes b)
                   (nelisp-asm-x86_64-test--ub
                    #x48 #xC7 #xC0 #x3C #x00 #x00 #x00
                    #x48 #xC7 #xC7 #x00 #x00 #x00 #x00
                    #x0F #x05)))
    (should (= (nelisp-asm-x86_64-buffer-pos b) 16))))

;; ---- objdump cross-check ----

(ert-deftest nelisp-asm-x86_64-objdump-roundtrip-exit0 ()
  (skip-unless (executable-find "objdump"))
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (tmp (make-temp-file "nelisp-asm-x86_64-" nil ".bin")))
    (unwind-protect
        (progn
          (nelisp-asm-x86_64-mov-imm32 b 'rax 60)
          (nelisp-asm-x86_64-mov-imm32 b 'rdi 0)
          (nelisp-asm-x86_64-syscall b)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (nelisp-asm-x86_64-buffer-bytes b) nil tmp
                          nil 'silent))
          (let ((out (with-temp-buffer
                       (call-process "objdump" nil t nil
                                     "-D" "-b" "binary"
                                     "-m" "i386:x86-64" tmp)
                       (buffer-string))))
            ;; Look for the canonical disasm fragments.  AT&T syntax
            ;; (gas default): `mov $0x3c,%eax', `mov $0x0,%edi',
            ;; `syscall'.  objdump elides REX.W when imm32 fits.
            (should (string-match-p "mov[[:space:]]+\\$0x3c" out))
            (should (string-match-p "mov[[:space:]]+\\$0x0" out))
            (should (string-match-p "syscall" out))))
      (when (file-exists-p tmp) (delete-file tmp)))))

;; ---- §92.d chunk-build invariants + perf gate ----

(ert-deftest nelisp-asm-x86_64-92d-chunks-field-exists-after-emit ()
  ;; §92.d invariant: emitter must push onto :chunks (= reverse-order
  ;; list) and bump :length, instead of concatenating onto :bytes.
  (let* ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-nop b)
    (nelisp-asm-x86_64-ret b)
    (let* ((plist (aref b 0))
           (chunks (plist-get plist :chunks))
           (len (plist-get plist :length)))
      (should (listp chunks))
      (should (= (length chunks) 2))
      ;; Most-recent push at head -> ret (0xC3) first, nop (0x90) second.
      (should (equal (car chunks) (unibyte-string #xC3)))
      (should (equal (cadr chunks) (unibyte-string #x90)))
      (should (= len 2)))))

(ert-deftest nelisp-asm-x86_64-92d-buffer-bytes-finalize-matches ()
  ;; §92.d invariant: finalize via `apply concat nreverse' yields the
  ;; same byte sequence as the pre-optimization concat-on-each-emit
  ;; pattern.  Cross-check with `exit(0)' shape (= existing fixture).
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-mov-imm32 b 'rax 60)
    (nelisp-asm-x86_64-mov-imm32 b 'rdi 0)
    (nelisp-asm-x86_64-syscall b)
    (should (equal (nelisp-asm-x86_64-buffer-bytes b)
                   (nelisp-asm-x86_64-test--ub
                    #x48 #xC7 #xC0 #x3C #x00 #x00 #x00
                    #x48 #xC7 #xC7 #x00 #x00 #x00 #x00
                    #x0F #x05)))))

(ert-deftest nelisp-asm-x86_64-92d-buffer-bytes-idempotent ()
  ;; §92.d: `buffer-bytes' must be idempotent — repeated calls return
  ;; the same string and do not destructively reverse :chunks.
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-nop b)
    (nelisp-asm-x86_64-nop b)
    (nelisp-asm-x86_64-ret b)
    (let ((first  (nelisp-asm-x86_64-buffer-bytes b))
          (second (nelisp-asm-x86_64-buffer-bytes b))
          (third  (nelisp-asm-x86_64-buffer-bytes b)))
      (should (equal first second))
      (should (equal second third))
      (should (equal first (nelisp-asm-x86_64-test--ub
                            #x90 #x90 #xC3))))))

(ert-deftest nelisp-asm-x86_64-92d-resolve-fixups-collapses-chunks ()
  ;; §92.d: `resolve-fixups' materializes + patches + stores back as a
  ;; single chunk; subsequent `buffer-bytes' returns the patched form.
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-call-rel32 b 'foo)
    (nelisp-asm-x86_64-define-label b 'foo)
    (nelisp-asm-x86_64-ret b)
    (let ((patched (nelisp-asm-x86_64-resolve-fixups b)))
      (should (equal patched
                     (nelisp-asm-x86_64-test--ub
                      #xE8 #x00 #x00 #x00 #x00 #xC3)))
      ;; After resolve, chunks list is collapsed to a single chunk.
      (let* ((plist (aref b 0))
             (chunks (plist-get plist :chunks)))
        (should (= (length chunks) 1))
        (should (equal (car chunks) patched)))
      ;; And `buffer-bytes' returns the patched form.
      (should (equal (nelisp-asm-x86_64-buffer-bytes b) patched)))))

(ert-deftest nelisp-asm-x86_64-92d-benchmark-emit-100kb ()
  ;; §92.d perf gate: 100 KB of synthetic NOP emit completes well
  ;; under 2 sec on commodity hardware via chunk-build (= O(N) total).
  ;; Pre-optimization `(concat old bs)` per byte was O(N²) and would
  ;; take >30 sec for 100 KB.
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (nbytes (* 100 1024))
         (start (current-time)))
    (nelisp-asm-x86_64-benchmark-emit b nbytes)
    (let* ((bytes (nelisp-asm-x86_64-buffer-bytes b))
           (elapsed (float-time (time-subtract (current-time) start))))
      (should (= (length bytes) nbytes))
      ;; First byte should be NOP (0x90).
      (should (= (aref bytes 0) #x90))
      (should (= (aref bytes (1- nbytes)) #x90))
      (should (< elapsed 2.0)))))

(ert-deftest nelisp-asm-x86_64-92d-benchmark-emit-1mb ()
  ;; §92.d perf gate: 1 MB of synthetic NOP emit completes in < 5 sec.
  ;; Mirrors Doc 91 §91.d's 1 MB benchmark target.
  (let* ((b (nelisp-asm-x86_64-make-buffer))
         (nbytes (* 1024 1024))
         (start (current-time)))
    (nelisp-asm-x86_64-benchmark-emit b nbytes)
    (let* ((bytes (nelisp-asm-x86_64-buffer-bytes b))
           (elapsed (float-time (time-subtract (current-time) start))))
      (should (= (length bytes) nbytes))
      (should (= (nelisp-asm-x86_64-buffer-pos b) nbytes))
      (should (< elapsed 5.0)))))

(ert-deftest nelisp-asm-x86_64-92d-buffer-pos-cached-o1 ()
  ;; §92.d: `buffer-pos' must read from cached :length field (= O(1))
  ;; — never traverse :chunks to recompute total bytes.
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should (= (nelisp-asm-x86_64-buffer-pos b) 0))
    (nelisp-asm-x86_64-nop b)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 1))
    (dotimes (_ 100) (nelisp-asm-x86_64-nop b))
    (should (= (nelisp-asm-x86_64-buffer-pos b) 101))
    (nelisp-asm-x86_64-mov-imm64 b 'rax #xDEADBEEF)
    ;; mov-imm64 = REX.W + opcode + imm64 = 10 bytes.
    (should (= (nelisp-asm-x86_64-buffer-pos b) 111))))

;; ---- Doc 101 §101.B Wave 5 — Win64 ABI tests ----

(ert-deftest nelisp-asm-x86_64-abi-arg-regs-sysv ()
  ;; SysV arg regs: RDI RSI RDX RCX R8 R9.
  (should (equal (nelisp-asm-x86_64-abi-arg-regs 'sysv)
                 '(rdi rsi rdx rcx r8 r9))))

(ert-deftest nelisp-asm-x86_64-abi-arg-regs-win64 ()
  ;; Win64 arg regs: RCX RDX R8 R9 (4 slots only).
  (let ((regs (nelisp-asm-x86_64-abi-arg-regs 'win64)))
    (should (equal regs '(rcx rdx r8 r9)))
    (should (= (length regs) 4))))

(ert-deftest nelisp-asm-x86_64-abi-callee-saved-sysv ()
  ;; SysV callee-saved: RBP RBX R12-R15 (6 regs).
  (let ((saved (nelisp-asm-x86_64-abi-callee-saved 'sysv)))
    (should (equal saved '(rbp rbx r12 r13 r14 r15)))
    (should (= (length saved) 6))))

(ert-deftest nelisp-asm-x86_64-abi-callee-saved-win64 ()
  ;; Win64 callee-saved GP regs: RBP RBX RDI RSI R12-R15 (8 regs).
  ;; RDI and RSI are caller-saved in SysV but callee-saved in Win64.
  (let ((saved (nelisp-asm-x86_64-abi-callee-saved 'win64)))
    (should (equal saved '(rbp rbx rdi rsi r12 r13 r14 r15)))
    (should (= (length saved) 8))
    ;; RDI and RSI must be present in Win64 but are absent from SysV.
    (should (memq 'rdi saved))
    (should (memq 'rsi saved))
    (should (not (memq 'rdi (nelisp-asm-x86_64-abi-callee-saved 'sysv))))
    (should (not (memq 'rsi (nelisp-asm-x86_64-abi-callee-saved 'sysv))))))

(ert-deftest nelisp-asm-x86_64-abi-shadow-space-sysv ()
  ;; SysV has no shadow space requirement.
  (should (= (nelisp-asm-x86_64-abi-shadow-space 'sysv) 0)))

(ert-deftest nelisp-asm-x86_64-abi-shadow-space-win64 ()
  ;; Win64 mandates 32 bytes of shadow space on the caller's stack.
  (should (= (nelisp-asm-x86_64-abi-shadow-space 'win64) 32)))

(ert-deftest nelisp-asm-x86_64-abi-unknown-signals-error ()
  ;; Unknown ABI signals `nelisp-asm-x86_64-error'.
  (should-error (nelisp-asm-x86_64-abi-arg-regs 'mips)
                :type 'nelisp-asm-x86_64-error)
  (should-error (nelisp-asm-x86_64-abi-callee-saved 'mips)
                :type 'nelisp-asm-x86_64-error)
  (should-error (nelisp-asm-x86_64-abi-shadow-space 'mips)
                :type 'nelisp-asm-x86_64-error))

(ert-deftest nelisp-asm-x86_64-make-buffer-win64-abi ()
  ;; make-buffer accepts 'win64 and stores it in the :abi field.
  (let ((buf (nelisp-asm-x86_64-make-buffer 'win64)))
    (should (eq (nelisp-asm-x86_64-buffer-abi buf) 'win64))
    (should (= (nelisp-asm-x86_64-buffer-pos buf) 0))))

(ert-deftest nelisp-asm-x86_64-make-buffer-sysv-default ()
  ;; Default ABI is 'sysv.
  (let ((buf (nelisp-asm-x86_64-make-buffer)))
    (should (eq (nelisp-asm-x86_64-buffer-abi buf) 'sysv))))

(ert-deftest nelisp-asm-x86_64-make-buffer-explicit-sysv ()
  ;; Explicit 'sysv accepted.
  (let ((buf (nelisp-asm-x86_64-make-buffer 'sysv)))
    (should (eq (nelisp-asm-x86_64-buffer-abi buf) 'sysv))))

(ert-deftest nelisp-asm-x86_64-win64-buffer-emit-bytes ()
  ;; Win64 buffer can emit bytes identically to a SysV buffer — the
  ;; ABI field affects caller-convention logic only, not raw encoding.
  (let ((buf (nelisp-asm-x86_64-make-buffer 'win64)))
    ;; push rbp = 0x55, mov rbp,rsp = REX.W 89 E5
    (nelisp-asm-x86_64-push buf 'rbp)
    (nelisp-asm-x86_64-mov-reg-reg buf 'rbp 'rsp)
    (nelisp-asm-x86_64-ret buf)
    (let ((bytes (nelisp-asm-x86_64-buffer-bytes buf)))
      (should (= (aref bytes 0) #x55))        ; push rbp
      (should (= (aref bytes 1) #x48))        ; REX.W
      (should (= (aref bytes 2) #x89))        ; mov r/m, r
      (should (= (aref bytes (1- (length bytes))) #xC3))))) ; ret

(provide 'nelisp-asm-x86_64-test)

;;; nelisp-asm-x86_64-test.el ends here
