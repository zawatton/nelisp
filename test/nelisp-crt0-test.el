;;; nelisp-crt0-test.el --- ERT tests for crt0 §94.a  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 94 §94.a — pure-elisp ert tests for the `nelisp-crt0' module.
;; Covers (1) the raw byte-pattern of each stub against hand-encoded
;; Intel SDM expectations, (2) length invariants, (3) the rel32
;; encoder, (4) the integrated hello-world orchestrator that runs the
;; emitted binary and asserts stdout + exit code.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-crt0)

;; ---------------------------------------------------------------- helpers

(defun nelisp-crt0-test--read-file-bytes (path)
  "Return raw unibyte bytes of PATH."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (let ((coding-system-for-read 'no-conversion))
      (insert-file-contents-literally path))
    (buffer-substring-no-properties (point-min) (point-max))))

(defun nelisp-crt0-test--x86_64-host-p ()
  "Return non-nil when running on x86_64 Linux (= can exec the binary)."
  (and (memq system-type '(gnu/linux gnu))
       (string-match-p "x86_64\\|amd64"
                       (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))

;; ---------------------------------------------------------------- §1 encoders

(ert-deftest nelisp-crt0-encode-rel32-zero ()
  "rel32 of 0 emits four null bytes."
  (let ((b (nelisp-crt0--encode-rel32 0)))
    (should (= (length b) 4))
    (should (= (aref b 0) 0))
    (should (= (aref b 1) 0))
    (should (= (aref b 2) 0))
    (should (= (aref b 3) 0))))

(ert-deftest nelisp-crt0-encode-rel32-positive ()
  "rel32 of #x12345678 emits little-endian bytes 78 56 34 12."
  (let ((b (nelisp-crt0--encode-rel32 #x12345678)))
    (should (= (aref b 0) #x78))
    (should (= (aref b 1) #x56))
    (should (= (aref b 2) #x34))
    (should (= (aref b 3) #x12))))

(ert-deftest nelisp-crt0-encode-rel32-negative ()
  "rel32 of -4 wraps to 0xfffffffc in two's-complement little-endian."
  (let ((b (nelisp-crt0--encode-rel32 -4)))
    (should (= (aref b 0) #xfc))
    (should (= (aref b 1) #xff))
    (should (= (aref b 2) #xff))
    (should (= (aref b 3) #xff))))

;; ---------------------------------------------------------------- §2 entry stub

(ert-deftest nelisp-crt0-entry-bytes-length ()
  "The entry stub is exactly 23 bytes (= 8 prologue + 5 call + 10 epilogue)."
  (let ((b (nelisp-crt0-x86_64-entry-bytes)))
    (should (= (length b) 23))
    (should (= (length b) nelisp-crt0-x86_64-entry-size))))

(ert-deftest nelisp-crt0-entry-bytes-stack-align ()
  "Entry stub begins with `48 83 e4 f0' (= and rsp, -16)."
  (let ((b (nelisp-crt0-x86_64-entry-bytes)))
    (should (= (aref b 0) #x48))
    (should (= (aref b 1) #x83))
    (should (= (aref b 2) #xe4))
    (should (= (aref b 3) #xf0))))

(ert-deftest nelisp-crt0-entry-bytes-argc-argv ()
  "Bytes 4..7 unpack argc/argv: `pop rdi' (5f) + `mov rsi, rsp' (48 89 e6)."
  (let ((b (nelisp-crt0-x86_64-entry-bytes)))
    (should (= (aref b 4) #x5f))
    (should (= (aref b 5) #x48))
    (should (= (aref b 6) #x89))
    (should (= (aref b 7) #xe6))))

(ert-deftest nelisp-crt0-entry-bytes-call-opcode ()
  "Byte at offset 8 is the `call rel32' opcode `e8'."
  (let ((b (nelisp-crt0-x86_64-entry-bytes)))
    (should (= (aref b 8) #xe8))))

(ert-deftest nelisp-crt0-entry-bytes-call-placeholder ()
  "With no rel32 argument, bytes 9..12 are zero (= linker placeholder)."
  (let ((b (nelisp-crt0-x86_64-entry-bytes)))
    (should (= (aref b 9) 0))
    (should (= (aref b 10) 0))
    (should (= (aref b 11) 0))
    (should (= (aref b 12) 0))))

(ert-deftest nelisp-crt0-entry-bytes-call-resolved ()
  "Explicit rel32 lands at byte offsets 9..12 in little-endian order."
  (let ((b (nelisp-crt0-x86_64-entry-bytes #x42)))
    (should (= (aref b 9) #x42))
    (should (= (aref b 10) 0))
    (should (= (aref b 11) 0))
    (should (= (aref b 12) 0))))

(ert-deftest nelisp-crt0-entry-bytes-epilogue ()
  "Bytes 13..22 are mov rdi,rax + mov eax,60 + syscall."
  (let ((b (nelisp-crt0-x86_64-entry-bytes)))
    ;; mov rdi, rax = 48 89 c7
    (should (= (aref b 13) #x48))
    (should (= (aref b 14) #x89))
    (should (= (aref b 15) #xc7))
    ;; mov eax, 60 = b8 3c 00 00 00
    (should (= (aref b 16) #xb8))
    (should (= (aref b 17) #x3c))
    (should (= (aref b 18) 0))
    (should (= (aref b 19) 0))
    (should (= (aref b 20) 0))
    ;; syscall = 0f 05
    (should (= (aref b 21) #x0f))
    (should (= (aref b 22) #x05))))

;; ---------------------------------------------------------------- §3 write stub

(ert-deftest nelisp-crt0-write-stdout-length ()
  "The callable write-stdout stub is exactly 19 bytes."
  (let ((b (nelisp-crt0-x86_64-write-stdout-bytes)))
    (should (= (length b) 19))))

(ert-deftest nelisp-crt0-write-stdout-shape ()
  "Write stub: mov rdx,rsi; mov rsi,rdi; mov edi,1; mov eax,1; syscall; ret."
  (let ((b (nelisp-crt0-x86_64-write-stdout-bytes)))
    ;; 48 89 f2 = mov rdx, rsi
    (should (= (aref b 0) #x48))
    (should (= (aref b 1) #x89))
    (should (= (aref b 2) #xf2))
    ;; 48 89 fe = mov rsi, rdi
    (should (= (aref b 3) #x48))
    (should (= (aref b 4) #x89))
    (should (= (aref b 5) #xfe))
    ;; bf 01 00 00 00 = mov edi, 1
    (should (= (aref b 6) #xbf))
    (should (= (aref b 7) 1))
    (should (= (aref b 8) 0))
    (should (= (aref b 9) 0))
    (should (= (aref b 10) 0))
    ;; b8 01 00 00 00 = mov eax, 1 (= SYS_write)
    (should (= (aref b 11) #xb8))
    (should (= (aref b 12) 1))
    (should (= (aref b 13) 0))
    (should (= (aref b 14) 0))
    (should (= (aref b 15) 0))
    ;; 0f 05 = syscall
    (should (= (aref b 16) #x0f))
    (should (= (aref b 17) #x05))
    ;; c3 = ret
    (should (= (aref b 18) #xc3))))

;; ---------------------------------------------------------------- §4 exit stub

(ert-deftest nelisp-crt0-exit-bytes-length ()
  "The exit stub is exactly 7 bytes (= mov eax, 60 + syscall)."
  (let ((b (nelisp-crt0-x86_64-exit-bytes)))
    (should (= (length b) 7))))

(ert-deftest nelisp-crt0-exit-bytes-shape ()
  "Exit stub bytes: b8 3c 00 00 00 + 0f 05."
  (let ((b (nelisp-crt0-x86_64-exit-bytes)))
    (should (= (aref b 0) #xb8))
    (should (= (aref b 1) #x3c))
    (should (= (aref b 2) 0))
    (should (= (aref b 3) 0))
    (should (= (aref b 4) 0))
    (should (= (aref b 5) #x0f))
    (should (= (aref b 6) #x05))))

;; ---------------------------------------------------------------- §5 hello-world

(ert-deftest nelisp-crt0-hello-world-text-length ()
  "The hello-world _start routine is exactly 37 bytes."
  (let ((b (nelisp-crt0--hello-world-emit-text 6 #x400078 #x40009d)))
    (should (= (length b) 37))
    (should (= (length b) nelisp-crt0--hello-world-text-size))))

(ert-deftest nelisp-crt0-hello-world-text-rip-rel ()
  "Hello-world `lea rsi, [rip+rel]' rel32 = rodata-vaddr - (text-vaddr + 16)."
  (let* ((text-vaddr  #x400078)
         (rodata-vaddr (+ text-vaddr 37))   ; .rodata starts after .text
         (rel-expected (- rodata-vaddr (+ text-vaddr 16)))
         (b (nelisp-crt0--hello-world-emit-text 6 text-vaddr rodata-vaddr)))
    ;; LEA opcode prefix at offsets 9..11: 48 8d 35
    (should (= (aref b 9)  #x48))
    (should (= (aref b 10) #x8d))
    (should (= (aref b 11) #x35))
    ;; rel32 at 12..15, low byte first.
    (should (= (aref b 12) (logand rel-expected #xff)))
    (should (= (aref b 13) (logand (ash rel-expected -8) #xff)))
    (should (= (aref b 14) (logand (ash rel-expected -16) #xff)))
    (should (= (aref b 15) (logand (ash rel-expected -24) #xff)))))

(ert-deftest nelisp-crt0-hello-world-text-msg-len-imm ()
  "Hello-world embeds MSG-LEN as `mov edx, imm32' at offsets 16..20."
  (let ((b (nelisp-crt0--hello-world-emit-text 42 #x400078 #x4000a0)))
    (should (= (aref b 16) #xba))         ; mov edx, imm32 opcode
    (should (= (aref b 17) 42))
    (should (= (aref b 18) 0))
    (should (= (aref b 19) 0))
    (should (= (aref b 20) 0))))

(ert-deftest nelisp-crt0-hello-world-emit-file ()
  "`nelisp-crt0-emit-hello-world' writes an ELF file with the right magic."
  (let ((path (make-temp-file "nelisp-crt0-hello-emit-")))
    (unwind-protect
        (progn
          (nelisp-crt0-emit-hello-world path)
          (should (file-exists-p path))
          (let ((bytes (nelisp-crt0-test--read-file-bytes path)))
            ;; ELF magic.
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            ;; ELFCLASS64 + ELFDATA2LSB.
            (should (= (aref bytes 4) 2))
            (should (= (aref bytes 5) 1))
            ;; .text at file offset 0x78 starts with `and rsp, -16'.
            (should (= (aref bytes #x78) #x48))
            (should (= (aref bytes (+ #x78 1)) #x83))
            (should (= (aref bytes (+ #x78 2)) #xe4))
            (should (= (aref bytes (+ #x78 3)) #xf0))
            ;; .rodata payload `hello\n' starts at file offset 0x78 + 37.
            (should (equal
                     (substring bytes (+ #x78 37) (+ #x78 37 6))
                     (unibyte-string ?h ?e ?l ?l ?o ?\n)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-crt0-hello-world-exec-stdout-and-exit ()
  "End-to-end: exec the emitted hello-world binary, capture stdout + rc."
  (skip-unless (nelisp-crt0-test--x86_64-host-p))
  (let ((path   (make-temp-file "nelisp-crt0-hello-exec-"))
        (output (make-temp-file "nelisp-crt0-hello-out-")))
    (unwind-protect
        (progn
          (nelisp-crt0-emit-hello-world path)
          (let ((rc (call-process path nil (list :file output) nil)))
            (should (eq rc 0))
            (should
             (equal
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally output)
                (buffer-substring-no-properties (point-min) (point-max)))
              (unibyte-string ?h ?e ?l ?l ?o ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

(ert-deftest nelisp-crt0-hello-world-readelf-h ()
  "`readelf -h' reports EXEC + x86_64 + a valid entry point for the binary."
  (skip-unless (executable-find "readelf"))
  (let ((path (make-temp-file "nelisp-crt0-hello-readelf-")))
    (unwind-protect
        (progn
          (nelisp-crt0-emit-hello-world path)
          (let ((out (with-output-to-string
                       (with-current-buffer standard-output
                         (call-process "readelf" nil t nil "-h" path)))))
            (should (string-match-p "ELF64" out))
            (should (string-match-p "EXEC" out))
            (should (string-match-p "X86-64\\|x86-64\\|x86_64" out))
            ;; Entry point lives in the loaded segment (>= 0x400000).
            (should (string-match-p "Entry point address:[ \t]+0x40" out))))
      (ignore-errors (delete-file path)))))

;; ---------------------------------------------------------------- §6 §94.c full-entry stub

(ert-deftest nelisp-crt0-full-entry-bytes-length ()
  "The §94.c full-entry stub is exactly 44 bytes (= 26+5+13)."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    (should (= (length b) 44))
    (should (= (length b) nelisp-crt0-x86_64-full-entry-size))))

(ert-deftest nelisp-crt0-full-entry-argc-load ()
  "Full entry begins with `mov rdi, [rsp]' = 48 8b 3c 24 (= argc → rdi)."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    (should (= (aref b 0) #x48))
    (should (= (aref b 1) #x8b))
    (should (= (aref b 2) #x3c))
    (should (= (aref b 3) #x24))))

(ert-deftest nelisp-crt0-full-entry-argv-load ()
  "Bytes 4..8 emit `lea rsi, [rsp+8]' = 48 8d 74 24 08 (= argv → rsi)."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    (should (= (aref b 4) #x48))
    (should (= (aref b 5) #x8d))
    (should (= (aref b 6) #x74))
    (should (= (aref b 7) #x24))
    (should (= (aref b 8) #x08))))

(ert-deftest nelisp-crt0-full-entry-envp-compute ()
  "Bytes 9..18 compute envp via mov rcx,rdi + inc rcx + lea rdx,[rsi+rcx*8]."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    ;; mov rcx, rdi = 48 89 f9
    (should (= (aref b 9)  #x48))
    (should (= (aref b 10) #x89))
    (should (= (aref b 11) #xf9))
    ;; inc rcx = 48 ff c1
    (should (= (aref b 12) #x48))
    (should (= (aref b 13) #xff))
    (should (= (aref b 14) #xc1))
    ;; lea rdx, [rsi + rcx*8] = 48 8d 14 ce
    (should (= (aref b 15) #x48))
    (should (= (aref b 16) #x8d))
    (should (= (aref b 17) #x14))
    (should (= (aref b 18) #xce))))

(ert-deftest nelisp-crt0-full-entry-stack-align ()
  "Full entry includes `and rsp, -16' (= 48 83 e4 f0) for SysV alignment."
  (let* ((b (nelisp-crt0-x86_64-full-entry-bytes))
         ;; mov rbp, rsp at offsets 19..21, then and rsp,-16 at 22..25.
         (and-rsp (substring b 22 26)))
    (should (= (aref b 19) #x48))     ; mov rbp, rsp = 48 89 e5
    (should (= (aref b 20) #x89))
    (should (= (aref b 21) #xe5))
    (should (equal and-rsp
                   (unibyte-string #x48 #x83 #xe4 #xf0)))))

(ert-deftest nelisp-crt0-full-entry-call-opcode ()
  "Byte at offset 26 is the `call rel32' opcode `e8'."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    (should (= (aref b 26) #xe8))))

(ert-deftest nelisp-crt0-full-entry-call-rel32-resolved ()
  "Explicit CALL-REL32 lands at byte offsets 27..30 in little-endian."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes #x12345678)))
    (should (= (aref b 27) #x78))
    (should (= (aref b 28) #x56))
    (should (= (aref b 29) #x34))
    (should (= (aref b 30) #x12))))

(ert-deftest nelisp-crt0-full-entry-epilogue ()
  "Bytes 31..43 restore rsp + invoke SYS_exit with main's rax."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    ;; mov rsp, rbp = 48 89 ec
    (should (= (aref b 31) #x48))
    (should (= (aref b 32) #x89))
    (should (= (aref b 33) #xec))
    ;; mov rdi, rax = 48 89 c7
    (should (= (aref b 34) #x48))
    (should (= (aref b 35) #x89))
    (should (= (aref b 36) #xc7))
    ;; mov eax, 60 = b8 3c 00 00 00
    (should (= (aref b 37) #xb8))
    (should (= (aref b 38) #x3c))
    (should (= (aref b 39) 0))
    (should (= (aref b 40) 0))
    (should (= (aref b 41) 0))
    ;; syscall = 0f 05
    (should (= (aref b 42) #x0f))
    (should (= (aref b 43) #x05))))

(ert-deftest nelisp-crt0-full-entry-call-placeholder ()
  "Without CALL-REL32, bytes 27..30 are zero (= linker placeholder)."
  (let ((b (nelisp-crt0-x86_64-full-entry-bytes)))
    (should (= (aref b 27) 0))
    (should (= (aref b 28) 0))
    (should (= (aref b 29) 0))
    (should (= (aref b 30) 0))))

(ert-deftest nelisp-crt0-full-entry-preserves-old-api ()
  "§94.a `nelisp-crt0-x86_64-entry-bytes' (= 23 bytes) still works."
  (let ((old (nelisp-crt0-x86_64-entry-bytes))
        (new (nelisp-crt0-x86_64-full-entry-bytes)))
    (should (= (length old) 23))
    (should (= (length new) 44))
    ;; They are distinct byte strings (= no accidental aliasing).
    (should-not (equal old new))))

;; ---------------------------------------------------------------- §7 find-auxv

(ert-deftest nelisp-crt0-find-auxv-length ()
  "The §94.c `find_auxv' stub is exactly 16 bytes."
  (let ((b (nelisp-crt0-x86_64-find-auxv-bytes)))
    (should (= (length b) 16))))

(ert-deftest nelisp-crt0-find-auxv-shape ()
  "find_auxv: mov rax,rdi; loop: mov rcx,[rax]; add rax,8; test; jne; ret."
  (let ((b (nelisp-crt0-x86_64-find-auxv-bytes)))
    ;; 48 89 f8 = mov rax, rdi
    (should (= (aref b 0) #x48))
    (should (= (aref b 1) #x89))
    (should (= (aref b 2) #xf8))
    ;; 48 8b 08 = mov rcx, [rax]
    (should (= (aref b 3) #x48))
    (should (= (aref b 4) #x8b))
    (should (= (aref b 5) #x08))
    ;; 48 83 c0 08 = add rax, 8
    (should (= (aref b 6) #x48))
    (should (= (aref b 7) #x83))
    (should (= (aref b 8) #xc0))
    (should (= (aref b 9) #x08))
    ;; 48 85 c9 = test rcx, rcx
    (should (= (aref b 10) #x48))
    (should (= (aref b 11) #x85))
    (should (= (aref b 12) #xc9))
    ;; 75 f4 = jne -12
    (should (= (aref b 13) #x75))
    (should (= (aref b 14) #xf4))
    ;; c3 = ret
    (should (= (aref b 15) #xc3))))

;; ---------------------------------------------------------------- §8 getauxval

(ert-deftest nelisp-crt0-getauxval-length ()
  "The §94.c `getauxval' stub is exactly 27 bytes."
  (let ((b (nelisp-crt0-x86_64-getauxval-bytes)))
    (should (= (length b) 27))))

(ert-deftest nelisp-crt0-getauxval-shape ()
  "getauxval: loop probes (type, value) pairs; returns value or 0."
  (let ((b (nelisp-crt0-x86_64-getauxval-bytes)))
    ;; 48 8b 07 = mov rax, [rdi]
    (should (= (aref b 0) #x48))
    (should (= (aref b 1) #x8b))
    (should (= (aref b 2) #x07))
    ;; 48 85 c0 = test rax, rax
    (should (= (aref b 3) #x48))
    (should (= (aref b 4) #x85))
    (should (= (aref b 5) #xc0))
    ;; 74 10 = je LNOT
    (should (= (aref b 6) #x74))
    (should (= (aref b 7) #x10))
    ;; 48 39 f0 = cmp rax, rsi
    (should (= (aref b 8) #x48))
    (should (= (aref b 9) #x39))
    (should (= (aref b 10) #xf0))
    ;; 74 06 = je LFOUND
    (should (= (aref b 11) #x74))
    (should (= (aref b 12) #x06))
    ;; 48 83 c7 10 = add rdi, 16
    (should (= (aref b 13) #x48))
    (should (= (aref b 14) #x83))
    (should (= (aref b 15) #xc7))
    (should (= (aref b 16) #x10))
    ;; eb ed = jmp loop
    (should (= (aref b 17) #xeb))
    (should (= (aref b 18) #xed))
    ;; 48 8b 47 08 = mov rax, [rdi+8]
    (should (= (aref b 19) #x48))
    (should (= (aref b 20) #x8b))
    (should (= (aref b 21) #x47))
    (should (= (aref b 22) #x08))
    ;; c3 = ret
    (should (= (aref b 23) #xc3))
    ;; 31 c0 = xor eax, eax
    (should (= (aref b 24) #x31))
    (should (= (aref b 25) #xc0))
    ;; c3 = ret
    (should (= (aref b 26) #xc3))))

;; ---------------------------------------------------------------- §9 hello-argc

(ert-deftest nelisp-crt0-hello-argc-text-length ()
  "The §94.c hello-with-argc _start routine is exactly 108 bytes."
  (let ((b (nelisp-crt0--hello-argc-emit-text #x400078 #x4000e4)))
    (should (= (length b) 108))
    (should (= (length b) nelisp-crt0--hello-argc-text-size))))

(ert-deftest nelisp-crt0-hello-argc-rip-rel ()
  "hello-argc `lea rsi, [rip+rel]' rel32 = rodata-vaddr - (text-vaddr + 16)."
  (let* ((text-vaddr  #x400078)
         (rodata-vaddr (+ text-vaddr 108))    ; .rodata after .text
         (rel-expected (- rodata-vaddr (+ text-vaddr 16)))
         (b (nelisp-crt0--hello-argc-emit-text text-vaddr rodata-vaddr)))
    ;; mov rbx,[rsp] at 0..3, mov edi,1 at 4..8, lea rsi at 9..15.
    (should (= (aref b 9)  #x48))
    (should (= (aref b 10) #x8d))
    (should (= (aref b 11) #x35))
    (should (= (aref b 12) (logand rel-expected #xff)))
    (should (= (aref b 13) (logand (ash rel-expected -8) #xff)))
    (should (= (aref b 14) (logand (ash rel-expected -16) #xff)))
    (should (= (aref b 15) (logand (ash rel-expected -24) #xff)))))

(ert-deftest nelisp-crt0-hello-argc-emit-file ()
  "`nelisp-crt0-emit-hello-with-argc' writes a valid ELF file."
  (let ((path (make-temp-file "nelisp-crt0-argc-emit-")))
    (unwind-protect
        (progn
          (nelisp-crt0-emit-hello-with-argc path)
          (should (file-exists-p path))
          (let ((bytes (nelisp-crt0-test--read-file-bytes path)))
            ;; ELF magic.
            (should (equal (substring bytes 0 4)
                           (unibyte-string #x7F #x45 #x4C #x46)))
            ;; ELFCLASS64 + ELFDATA2LSB.
            (should (= (aref bytes 4) 2))
            (should (= (aref bytes 5) 1))
            ;; .text at 0x78 starts with `mov rbx, [rsp]'.
            (should (= (aref bytes #x78) #x48))
            (should (= (aref bytes (+ #x78 1)) #x8b))
            (should (= (aref bytes (+ #x78 2)) #x1c))
            (should (= (aref bytes (+ #x78 3)) #x24))
            ;; .rodata `argc=' starts at file offset 0x78 + 108.
            (should (equal
                     (substring bytes (+ #x78 108) (+ #x78 108 5))
                     (unibyte-string ?a ?r ?g ?c ?=)))))
      (ignore-errors (delete-file path)))))

(ert-deftest nelisp-crt0-hello-argc-exec-no-args ()
  "End-to-end: exec hello-with-argc with no args → stdout = `argc=1\\n'."
  (skip-unless (nelisp-crt0-test--x86_64-host-p))
  (let ((path   (make-temp-file "nelisp-crt0-argc-exec-1-"))
        (output (make-temp-file "nelisp-crt0-argc-out-1-")))
    (unwind-protect
        (progn
          (nelisp-crt0-emit-hello-with-argc path)
          (let ((rc (call-process path nil (list :file output) nil)))
            (should (eq rc 0))
            (should
             (equal
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally output)
                (buffer-substring-no-properties (point-min) (point-max)))
              (unibyte-string ?a ?r ?g ?c ?= ?1 ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

(ert-deftest nelisp-crt0-hello-argc-exec-three-args ()
  "End-to-end: exec hello-with-argc with 3 args → stdout = `argc=4\\n'."
  (skip-unless (nelisp-crt0-test--x86_64-host-p))
  (let ((path   (make-temp-file "nelisp-crt0-argc-exec-4-"))
        (output (make-temp-file "nelisp-crt0-argc-out-4-")))
    (unwind-protect
        (progn
          (nelisp-crt0-emit-hello-with-argc path)
          (let ((rc (call-process path nil (list :file output) nil
                                  "arg1" "arg2" "arg3")))
            (should (eq rc 0))
            (should
             (equal
              (with-temp-buffer
                (set-buffer-multibyte nil)
                (insert-file-contents-literally output)
                (buffer-substring-no-properties (point-min) (point-max)))
              (unibyte-string ?a ?r ?g ?c ?= ?4 ?\n)))))
      (ignore-errors (delete-file path))
      (ignore-errors (delete-file output)))))

(provide 'nelisp-crt0-test)

;;; nelisp-crt0-test.el ends here
