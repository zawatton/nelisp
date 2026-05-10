;;; nelisp-asm-arm64-test.el --- ERT tests for §92.b  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 92 §92.b — pure-elisp ert tests for the freestanding arm64
;; macro assembler (`lisp/nelisp-asm-arm64.el').  Verifies:
;;   1. word + register helpers,
;;   2. each instruction emitter against ARM ARM bit-field tables,
;;   3. label / fixup machinery (forward + backward imm26 resolve),
;;   4. relocation marker recording (= Doc 93 handoff stub),
;;   5. objdump cross-check for the canonical `exit(0)' shape
;;      (skipped if host has no aarch64-capable `objdump').

;;; Code:

(require 'ert)
(require 'cl-lib)

(let* ((this (or load-file-name buffer-file-name))
       (test-dir (and this (file-name-directory this)))
       (lisp-dir (and test-dir
                      (expand-file-name "../lisp" test-dir))))
  (when (and lisp-dir (file-directory-p lisp-dir))
    (add-to-list 'load-path lisp-dir)))

(require 'nelisp-asm-arm64)

;; ---- helpers ----

(defun nelisp-asm-arm64-test--bytes (thunk)
  "Run THUNK on a fresh buffer, return its accumulated bytes."
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (funcall thunk b)
    (nelisp-asm-arm64-buffer-bytes b)))

(defun nelisp-asm-arm64-test--ub (&rest bs)
  "Construct a unibyte-string from the integer args BS."
  (apply #'unibyte-string bs))

(defun nelisp-asm-arm64-test--word (word)
  "Return WORD as a unibyte-string in 4 LE bytes."
  (nelisp-asm-arm64-test--ub
   (logand word #xFF)
   (logand (ash word  -8) #xFF)
   (logand (ash word -16) #xFF)
   (logand (ash word -24) #xFF)))

;; ---- L0 helpers ----

(ert-deftest nelisp-asm-arm64-word-bytes-zero ()
  (should (equal (nelisp-asm-arm64--word-bytes 0)
                 (nelisp-asm-arm64-test--ub 0 0 0 0))))

(ert-deftest nelisp-asm-arm64-word-bytes-ret-constant ()
  ;; RET (X30) = 0xD65F03C0 -> LE bytes C0 03 5F D6
  (should (equal (nelisp-asm-arm64--word-bytes #xD65F03C0)
                 (nelisp-asm-arm64-test--ub #xC0 #x03 #x5F #xD6))))

(ert-deftest nelisp-asm-arm64-reg-num-x0 ()
  (should (= (nelisp-asm-arm64--reg-num 'x0) 0)))

(ert-deftest nelisp-asm-arm64-reg-num-x30 ()
  (should (= (nelisp-asm-arm64--reg-num 'x30) 30)))

(ert-deftest nelisp-asm-arm64-reg-num-sp-xzr-aliases ()
  (should (= (nelisp-asm-arm64--reg-num 'sp) 31))
  (should (= (nelisp-asm-arm64--reg-num 'xzr) 31))
  (should (= (nelisp-asm-arm64--reg-num 'wzr) 31)))

(ert-deftest nelisp-asm-arm64-reg-num-unknown-signals ()
  (should-error (nelisp-asm-arm64--reg-num 'zoo)
                :type 'nelisp-asm-arm64-error))

;; ---- buffer abstraction ----

(ert-deftest nelisp-asm-arm64-buffer-empty ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should (equal (nelisp-asm-arm64-buffer-bytes b) ""))
    (should (= (nelisp-asm-arm64-buffer-pos b) 0))))

(ert-deftest nelisp-asm-arm64-buffer-pos-after-nop ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-nop b)
    (should (= (nelisp-asm-arm64-buffer-pos b) 4))))

;; ---- single-word opcodes ----

(ert-deftest nelisp-asm-arm64-nop-encoding ()
  ;; NOP = 0xD503201F  -> LE bytes 1F 20 03 D5
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-nop b)))
                 (nelisp-asm-arm64-test--ub #x1F #x20 #x03 #xD5))))

(ert-deftest nelisp-asm-arm64-ret-default-encoding ()
  ;; RET = RET X30 = 0xD65F03C0 -> LE bytes C0 03 5F D6
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-ret b)))
                 (nelisp-asm-arm64-test--ub #xC0 #x03 #x5F #xD6))))

(ert-deftest nelisp-asm-arm64-ret-explicit-reg ()
  ;; RET X16 -> 0xD65F0000 | (16 << 5) = 0xD65F0200 -> LE 00 02 5F D6
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-ret b 'x16)))
                 (nelisp-asm-arm64-test--ub #x00 #x02 #x5F #xD6))))

(ert-deftest nelisp-asm-arm64-svc-zero-encoding ()
  ;; SVC #0 = 0xD4000001 -> LE bytes 01 00 00 D4
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-svc b 0)))
                 (nelisp-asm-arm64-test--ub #x01 #x00 #x00 #xD4))))

(ert-deftest nelisp-asm-arm64-svc-imm-nonzero ()
  ;; SVC #0x10 = 0xD4000001 | (0x10 << 5) = 0xD4000201
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-svc b #x10)))
                 (nelisp-asm-arm64-test--word #xD4000201))))

(ert-deftest nelisp-asm-arm64-brk-zero-encoding ()
  ;; BRK #0 = 0xD4200000 -> LE bytes 00 00 20 D4
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-brk b 0)))
                 (nelisp-asm-arm64-test--ub #x00 #x00 #x20 #xD4))))

(ert-deftest nelisp-asm-arm64-svc-out-of-range-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-svc b #x10000)
                  :type 'nelisp-asm-arm64-error)))

;; ---- MOVZ / MOVK ----

(ert-deftest nelisp-asm-arm64-mov-imm-z-x8-93 ()
  ;; MOVZ x8, #93 = 0xD2800000 | (93 << 5) | 8
  ;;             = 0xD2800000 | 0xBA0 | 8 = 0xD2800BA8
  ;; -> LE bytes A8 0B 80 D2
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-mov-imm-z b 'x8 93)))
                 (nelisp-asm-arm64-test--ub #xA8 #x0B #x80 #xD2))))

(ert-deftest nelisp-asm-arm64-mov-imm-z-x0-zero ()
  ;; MOVZ x0, #0 = 0xD2800000 -> LE 00 00 80 D2
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-mov-imm-z b 'x0 0)))
                 (nelisp-asm-arm64-test--ub #x00 #x00 #x80 #xD2))))

(ert-deftest nelisp-asm-arm64-mov-imm-z-out-of-range ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-mov-imm-z b 'x0 #x10000)
                  :type 'nelisp-asm-arm64-error)))

(ert-deftest nelisp-asm-arm64-mov-imm-k-x0-lsl16 ()
  ;; MOVK x0, #0xABCD, LSL #16 = 0xF2800000 | (1<<21) | (0xABCD<<5) | 0
  ;;   = 0xF2200000 + 0x15799A0
  ;;   = 0xF2A00000 | (0xABCD<<5)
  ;;   Compute concretely: 0xABCD<<5 = 0x1579A0. Plus (1<<21)=0x200000.
  ;;   Plus 0xF2800000 = 0xF2A00000. OR with 0x1579A0 = 0xF2B579A0.
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-mov-imm-k b 'x0 #xABCD 16)))
                 (nelisp-asm-arm64-test--word #xF2B579A0))))

(ert-deftest nelisp-asm-arm64-mov-imm-k-bad-lsl-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-mov-imm-k b 'x0 0 17)
                  :type 'nelisp-asm-arm64-error)))

;; ---- MOV imm64 chain ----

(ert-deftest nelisp-asm-arm64-mov-imm64-small ()
  ;; mov-imm64 x0, #0x42 -> single MOVZ x0, #0x42
  ;; MOVZ x0, #0x42 = 0xD2800000 | (0x42<<5) | 0 = 0xD2800840
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-mov-imm64 b 'x0 #x42)))
                 (nelisp-asm-arm64-test--word #xD2800840))))

(ert-deftest nelisp-asm-arm64-mov-imm64-zero ()
  ;; mov-imm64 x0, 0 -> single MOVZ x0, #0 (all-zero short circuit)
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-mov-imm64 b 'x0 0)))
                 (nelisp-asm-arm64-test--word #xD2800000))))

(ert-deftest nelisp-asm-arm64-mov-imm64-shift16-only ()
  ;; mov-imm64 x0, (#xABCD << 16) -> single MOVZ x0, #0xABCD, LSL #16
  ;; (= because lower 16 bits = 0).  hw=1.
  ;; word = 0xD2800000 | (1<<21) | (0xABCD<<5) | 0 = 0xD2A00000 + 0x1579A0
  ;;      = 0xD2B579A0
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-mov-imm64
                     b 'x0 (ash #xABCD 16))))
                 (nelisp-asm-arm64-test--word #xD2B579A0))))

(ert-deftest nelisp-asm-arm64-mov-imm64-full-64bit ()
  ;; mov-imm64 x0, #0x12345678ABCDEF00 -> MOVZ + 3 MOVK
  ;; slices: s0=0xEF00, s1=0xABCD, s2=0x5678, s3=0x1234
  ;; (i) MOVZ x0, #0xEF00      = 0xD2800000 | (0xEF00<<5) | 0
  ;;                            = 0xD2800000 | 0x1DE000 = 0xD29DE000
  ;; (ii) MOVK x0, #0xABCD,16 = 0xF2800000 | (1<<21) | (0xABCD<<5) | 0
  ;;                            = 0xF2A00000 | 0x1579A0 = 0xF2B579A0
  ;; (iii)MOVK x0, #0x5678,32 = 0xF2800000 | (2<<21) | (0x5678<<5) | 0
  ;;                            = 0xF2C00000 | 0xACF00 = 0xF2CACF00
  ;; (iv) MOVK x0, #0x1234,48 = 0xF2800000 | (3<<21) | (0x1234<<5) | 0
  ;;                            = 0xF2E00000 | 0x24680 = 0xF2E24680
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-mov-imm64
                     b 'x0 #x12345678ABCDEF00)))
                 (concat (nelisp-asm-arm64-test--word #xD29DE000)
                         (nelisp-asm-arm64-test--word #xF2B579A0)
                         (nelisp-asm-arm64-test--word #xF2CACF00)
                         (nelisp-asm-arm64-test--word #xF2E24680)))))

;; ---- MOV reg-reg ----

(ert-deftest nelisp-asm-arm64-mov-reg-reg-x0-x1 ()
  ;; MOV x0, x1 = ORR x0, xzr, x1
  ;; = 0xAA0003E0 | (1 << 16) | 0 = 0xAA0103E0
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-mov-reg-reg b 'x0 'x1)))
                 (nelisp-asm-arm64-test--word #xAA0103E0))))

(ert-deftest nelisp-asm-arm64-mov-reg-reg-x29-x30 ()
  ;; MOV x29, x30 = 0xAA0003E0 | (30 << 16) | 29
  ;; = 0xAA0003E0 | 0x1E0000 | 0x1D = 0xAA1E03FD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-mov-reg-reg b 'x29 'x30)))
                 (nelisp-asm-arm64-test--word #xAA1E03FD))))

;; ---- ADD / SUB / CMP imm12 ----

(ert-deftest nelisp-asm-arm64-add-imm-sp-sp-16 ()
  ;; ADD sp, sp, #16 = 0x91000000 | (16<<10) | (31<<5) | 31
  ;; = 0x91000000 | 0x4000 | 0x3E0 | 0x1F = 0x910043FF
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-add-imm b 'sp 'sp 16)))
                 (nelisp-asm-arm64-test--word #x910043FF))))

(ert-deftest nelisp-asm-arm64-sub-imm-sp-sp-32 ()
  ;; SUB sp, sp, #32 = 0xD1000000 | (32<<10) | (31<<5) | 31
  ;; = 0xD1000000 | 0x8000 | 0x3E0 | 0x1F = 0xD10083FF
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-sub-imm b 'sp 'sp 32)))
                 (nelisp-asm-arm64-test--word #xD10083FF))))

(ert-deftest nelisp-asm-arm64-add-imm-x0-x1-1 ()
  ;; ADD x0, x1, #1 = 0x91000000 | (1<<10) | (1<<5) | 0
  ;; = 0x91000000 | 0x400 | 0x20 | 0 = 0x91000420
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-add-imm b 'x0 'x1 1)))
                 (nelisp-asm-arm64-test--word #x91000420))))

(ert-deftest nelisp-asm-arm64-cmp-imm-x0-0 ()
  ;; CMP x0, #0 = SUBS xzr, x0, #0
  ;; = 0xF100001F | (0<<10) | (0<<5) = 0xF100001F
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cmp-imm b 'x0 0)))
                 (nelisp-asm-arm64-test--word #xF100001F))))

(ert-deftest nelisp-asm-arm64-cmp-imm-x1-0xff ()
  ;; CMP x1, #0xFF = 0xF100001F | (0xFF<<10) | (1<<5)
  ;; = 0xF100001F | 0x3FC00 | 0x20 = 0xF103FC3F
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cmp-imm b 'x1 #xFF)))
                 (nelisp-asm-arm64-test--word #xF103FC3F))))

(ert-deftest nelisp-asm-arm64-imm12-out-of-range-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-add-imm b 'x0 'x0 #x1000)
                  :type 'nelisp-asm-arm64-error)))

;; ---- label + fixup (= imm26 resolution) ----

(ert-deftest nelisp-asm-arm64-bl-to-immediately-following-label ()
  ;; bl foo; foo: ret  -> imm26 = (4 - 0) >> 2 = 1
  ;; word = 0x94000000 | 1 = 0x94000001  -> LE 01 00 00 94
  ;; followed by ret: C0 03 5F D6
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-bl b 'foo)
    (nelisp-asm-arm64-define-label b 'foo)
    (nelisp-asm-arm64-ret b)
    (let ((bytes (nelisp-asm-arm64-resolve-fixups b)))
      (should (equal bytes
                     (concat (nelisp-asm-arm64-test--word #x94000001)
                             (nelisp-asm-arm64-test--word #xD65F03C0)))))))

(ert-deftest nelisp-asm-arm64-b-forward ()
  ;; b foo; nop; foo: ret -> imm26 = (8 - 0) >> 2 = 2
  ;; word = 0x14000000 | 2 = 0x14000002
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-b b 'foo)
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-define-label b 'foo)
    (nelisp-asm-arm64-ret b)
    (let ((bytes (nelisp-asm-arm64-resolve-fixups b)))
      (should (equal bytes
                     (concat (nelisp-asm-arm64-test--word #x14000002)
                             (nelisp-asm-arm64-test--word #xD503201F)
                             (nelisp-asm-arm64-test--word #xD65F03C0)))))))

(ert-deftest nelisp-asm-arm64-b-backward ()
  ;; foo: nop; b foo -> imm26 = (0 - 4) >> 2 = -1
  ;; -1 in 26-bit two's-complement = 0x3FFFFFF
  ;; word = 0x14000000 | 0x3FFFFFF = 0x17FFFFFF
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-define-label b 'foo)
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-b b 'foo)
    (let ((bytes (nelisp-asm-arm64-resolve-fixups b)))
      (should (equal bytes
                     (concat (nelisp-asm-arm64-test--word #xD503201F)
                             (nelisp-asm-arm64-test--word #x17FFFFFF)))))))

(ert-deftest nelisp-asm-arm64-duplicate-label-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-define-label b 'foo)
    (should-error (nelisp-asm-arm64-define-label b 'foo)
                  :type 'nelisp-asm-arm64-error)))

(ert-deftest nelisp-asm-arm64-unresolved-label-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-b b 'nowhere)
    (should-error (nelisp-asm-arm64-resolve-fixups b)
                  :type 'nelisp-asm-arm64-error)))

(ert-deftest nelisp-asm-arm64-fixup-unknown-type-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    ;; Need a slot to put the fixup at — emit a NOP then try to record
    ;; an invalid-typed fixup at slot 0.
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-define-label b 'foo)
    (should-error (nelisp-asm-arm64-emit-fixup b 0 'foo 'absurd)
                  :type 'nelisp-asm-arm64-error)))

;; ---- relocation marker recording (= §92.b (4)) ----

(ert-deftest nelisp-asm-arm64-emit-reloc-b26-pc ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    ;; Pretend we emitted a BL placeholder at pos 0.
    (nelisp-asm-arm64-emit-reloc b 'b26-pc 'printf 0)
    (let ((r (car (nelisp-asm-arm64-buffer-relocs b))))
      (should (eq (plist-get r :type) 'b26-pc))
      (should (eq (plist-get r :sym) 'printf))
      (should (= (plist-get r :offset) 0))
      (should (= (plist-get r :addend) 0)))))

(ert-deftest nelisp-asm-arm64-emit-reloc-abs64-after-movz-chain ()
  ;; Emit a 4-instr MOVZ/MOVK chain for a 64-bit immediate slot, then
  ;; record an abs64 reloc at the next byte.  Slot is just informative
  ;; for the spike — Doc 93 patches the imm16 fields per slice.
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-mov-imm64 b 'x0 #xDEADBEEFCAFEBABE)
    (nelisp-asm-arm64-emit-reloc b 'abs64 'global_x 0)
    (should (= (length (nelisp-asm-arm64-buffer-relocs b)) 1))
    (let ((r (car (nelisp-asm-arm64-buffer-relocs b))))
      (should (eq (plist-get r :type) 'abs64))
      (should (= (plist-get r :offset) 16)))))

(ert-deftest nelisp-asm-arm64-emit-reloc-unknown-type-signals ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-emit-reloc b 'absurd 'foo)
                  :type 'nelisp-asm-arm64-error)))

;; ---- composite: minimal exit(0) ----

(ert-deftest nelisp-asm-arm64-exit0-shape ()
  ;; Linux arm64 exit(0):
  ;;   mov x8, #93   (= __NR_exit syscall number)
  ;;   mov x0, #0
  ;;   svc #0
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-mov-imm-z b 'x8 93)
    (nelisp-asm-arm64-mov-imm-z b 'x0 0)
    (nelisp-asm-arm64-svc b 0)
    (should (equal (nelisp-asm-arm64-buffer-bytes b)
                   (concat
                    (nelisp-asm-arm64-test--ub #xA8 #x0B #x80 #xD2)
                    (nelisp-asm-arm64-test--ub #x00 #x00 #x80 #xD2)
                    (nelisp-asm-arm64-test--ub #x01 #x00 #x00 #xD4))))
    (should (= (nelisp-asm-arm64-buffer-pos b) 12))))

;; ---- objdump cross-check (optional) ----

(defun nelisp-asm-arm64-test--objdump-aarch64-supported-p ()
  "Return non-nil iff host `objdump' can decode aarch64 binaries.
Some Debian / NixOS hosts ship a binutils without aarch64 BFD —
exec a probe and check exit status."
  (and (executable-find "objdump")
       (zerop
        (call-process "objdump" nil nil nil "-m" "aarch64"
                      "-D" "-b" "binary" "/dev/null"))))

(ert-deftest nelisp-asm-arm64-objdump-roundtrip-exit0 ()
  (skip-unless (nelisp-asm-arm64-test--objdump-aarch64-supported-p))
  (let* ((b (nelisp-asm-arm64-make-buffer))
         (tmp (make-temp-file "nelisp-asm-arm64-" nil ".bin")))
    (unwind-protect
        (progn
          (nelisp-asm-arm64-mov-imm-z b 'x8 93)
          (nelisp-asm-arm64-mov-imm-z b 'x0 0)
          (nelisp-asm-arm64-svc b 0)
          (let ((coding-system-for-write 'no-conversion))
            (write-region (nelisp-asm-arm64-buffer-bytes b) nil tmp
                          nil 'silent))
          (let ((out (with-temp-buffer
                       (call-process "objdump" nil t nil
                                     "-D" "-b" "binary"
                                     "-m" "aarch64" tmp)
                       (buffer-string))))
            ;; Look for the canonical disasm fragments.
            (should (string-match-p "mov[[:space:]]+x8,[[:space:]]*#0x5d"
                                    out))
            (should (string-match-p "mov[[:space:]]+x0,[[:space:]]*#0"
                                    out))
            (should (string-match-p "svc[[:space:]]+#0" out))))
      (when (file-exists-p tmp) (delete-file tmp)))))

(provide 'nelisp-asm-arm64-test)

;;; nelisp-asm-arm64-test.el ends here
