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

(provide 'nelisp-asm-x86_64-test)

;;; nelisp-asm-x86_64-test.el ends here
