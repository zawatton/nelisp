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

(ert-deftest nelisp-asm-arm64-blr-x0 ()
  ;; BLR X0 = 0xD63F0000 (Doc 133 Phase 0 indirect call)
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-blr b 'x0)))
                 (nelisp-asm-arm64-test--word #xD63F0000))))

(ert-deftest nelisp-asm-arm64-blr-x16 ()
  ;; BLR X16 = 0xD63F0000 | (16 << 5) = 0xD63F0200
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-blr b 'x16)))
                 (nelisp-asm-arm64-test--word #xD63F0200))))

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

;; ---- Doc 100 §100.D Stage 2 reg-reg / cmp / cset / bitwise / shift ----

(ert-deftest nelisp-asm-arm64-add-reg-reg-x0-x0-x1 ()
  ;; ADD x0, x0, x1 = 0x8B000000 | (1<<16) | (0<<5) | 0 = 0x8B010000
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-add-reg-reg b 'x0 'x0 'x1)))
                 (nelisp-asm-arm64-test--word #x8B010000))))

(ert-deftest nelisp-asm-arm64-add-reg-reg-x10-x2-x7 ()
  ;; ADD x10, x2, x7 = 0x8B000000 | (7<<16) | (2<<5) | 10 = 0x8B07004A
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-add-reg-reg b 'x10 'x2 'x7)))
                 (nelisp-asm-arm64-test--word #x8B07004A))))

(ert-deftest nelisp-asm-arm64-sub-reg-reg-x0-x1-x0 ()
  ;; SUB x0, x1, x0 = 0xCB000000 | (0<<16) | (1<<5) | 0 = 0xCB000020
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-sub-reg-reg b 'x0 'x1 'x0)))
                 (nelisp-asm-arm64-test--word #xCB000020))))

(ert-deftest nelisp-asm-arm64-sub-reg-reg-x29-x29-x10 ()
  ;; SUB x29, x29, x10 = 0xCB000000 | (10<<16) | (29<<5) | 29 = 0xCB0A03BD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-sub-reg-reg b 'x29 'x29 'x10)))
                 (nelisp-asm-arm64-test--word #xCB0A03BD))))

(ert-deftest nelisp-asm-arm64-mul-reg-reg-x0-x0-x1 ()
  ;; MUL x0, x0, x1 = 0x9B007C00 | (1<<16) | (0<<5) | 0 = 0x9B017C00
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-mul-reg-reg b 'x0 'x0 'x1)))
                 (nelisp-asm-arm64-test--word #x9B017C00))))

(ert-deftest nelisp-asm-arm64-mul-reg-reg-x10-x2-x7 ()
  ;; MUL x10, x2, x7 = 0x9B007C00 | (7<<16) | (2<<5) | 10 = 0x9B077C4A
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-mul-reg-reg b 'x10 'x2 'x7)))
                 (nelisp-asm-arm64-test--word #x9B077C4A))))

(ert-deftest nelisp-asm-arm64-and-reg-reg-x0-x1-x2 ()
  ;; AND x0, x1, x2 = 0x8A000000 | (2<<16) | (1<<5) | 0 = 0x8A020020
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-and-reg-reg b 'x0 'x1 'x2)))
                 (nelisp-asm-arm64-test--word #x8A020020))))

(ert-deftest nelisp-asm-arm64-and-reg-reg-x29-x29-x10 ()
  ;; AND x29, x29, x10 = 0x8A000000 | (10<<16) | (29<<5) | 29 = 0x8A0A03BD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-and-reg-reg b 'x29 'x29 'x10)))
                 (nelisp-asm-arm64-test--word #x8A0A03BD))))

(ert-deftest nelisp-asm-arm64-orr-reg-reg-x0-x0-x1 ()
  ;; ORR x0, x0, x1 = 0xAA000000 | (1<<16) | (0<<5) | 0 = 0xAA010000
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-orr-reg-reg b 'x0 'x0 'x1)))
                 (nelisp-asm-arm64-test--word #xAA010000))))

(ert-deftest nelisp-asm-arm64-orr-reg-reg-x10-x2-x7 ()
  ;; ORR x10, x2, x7 = 0xAA000000 | (7<<16) | (2<<5) | 10 = 0xAA07004A
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-orr-reg-reg b 'x10 'x2 'x7)))
                 (nelisp-asm-arm64-test--word #xAA07004A))))

(ert-deftest nelisp-asm-arm64-eor-reg-reg-x0-x1-x2 ()
  ;; EOR x0, x1, x2 = 0xCA000000 | (2<<16) | (1<<5) | 0 = 0xCA020020
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-eor-reg-reg b 'x0 'x1 'x2)))
                 (nelisp-asm-arm64-test--word #xCA020020))))

(ert-deftest nelisp-asm-arm64-eor-reg-reg-x29-x29-x10 ()
  ;; EOR x29, x29, x10 = 0xCA000000 | (10<<16) | (29<<5) | 29 = 0xCA0A03BD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-eor-reg-reg b 'x29 'x29 'x10)))
                 (nelisp-asm-arm64-test--word #xCA0A03BD))))

(ert-deftest nelisp-asm-arm64-cmp-reg-reg-x0-x1 ()
  ;; CMP x0, x1 = 0xEB00001F | (1<<16) | (0<<5) = 0xEB01001F
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-cmp-reg-reg b 'x0 'x1)))
                 (nelisp-asm-arm64-test--word #xEB01001F))))

(ert-deftest nelisp-asm-arm64-cmp-reg-reg-x29-x10 ()
  ;; CMP x29, x10 = 0xEB00001F | (10<<16) | (29<<5) = 0xEB0A03BF
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-cmp-reg-reg b 'x29 'x10)))
                 (nelisp-asm-arm64-test--word #xEB0A03BF))))

(ert-deftest nelisp-asm-arm64-cset-x0-eq ()
  ;; CSET x0, eq = 0x9A9F07E0 | ((0 xor 1)<<12) | 0 = 0x9A9F17E0
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x0 'eq)))
                 (nelisp-asm-arm64-test--word #x9A9F17E0))))

(ert-deftest nelisp-asm-arm64-cset-x1-ne ()
  ;; CSET x1, ne = 0x9A9F07E0 | ((1 xor 1)<<12) | 1 = 0x9A9F07E1
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x1 'ne)))
                 (nelisp-asm-arm64-test--word #x9A9F07E1))))

(ert-deftest nelisp-asm-arm64-cset-x2-lt ()
  ;; CSET x2, lt = 0x9A9F07E0 | ((11 xor 1)<<12) | 2 = 0x9A9FA7E2
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x2 'lt)))
                 (nelisp-asm-arm64-test--word #x9A9FA7E2))))

(ert-deftest nelisp-asm-arm64-cset-x3-ge ()
  ;; CSET x3, ge = 0x9A9F07E0 | ((10 xor 1)<<12) | 3 = 0x9A9FB7E3
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x3 'ge)))
                 (nelisp-asm-arm64-test--word #x9A9FB7E3))))

(ert-deftest nelisp-asm-arm64-cset-x10-lo ()
  ;; CSET x10, lo = 0x9A9F07E0 | ((3 xor 1)<<12) | 10 = 0x9A9F27EA
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x10 'lo)))
                 (nelisp-asm-arm64-test--word #x9A9F27EA))))

(ert-deftest nelisp-asm-arm64-cset-x11-hs ()
  ;; CSET x11, hs = 0x9A9F07E0 | ((2 xor 1)<<12) | 11 = 0x9A9F37EB
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x11 'hs)))
                 (nelisp-asm-arm64-test--word #x9A9F37EB))))

(ert-deftest nelisp-asm-arm64-cset-x29-gt ()
  ;; CSET x29, gt = 0x9A9F07E0 | ((12 xor 1)<<12) | 29 = 0x9A9FD7FD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x29 'gt)))
                 (nelisp-asm-arm64-test--word #x9A9FD7FD))))

(ert-deftest nelisp-asm-arm64-cset-x7-le ()
  ;; CSET x7, le = 0x9A9F07E0 | ((13 xor 1)<<12) | 7 = 0x9A9FC7E7
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b) (nelisp-asm-arm64-cset b 'x7 'le)))
                 (nelisp-asm-arm64-test--word #x9A9FC7E7))))

(ert-deftest nelisp-asm-arm64-lslv-x0-x1-x2 ()
  ;; LSLV x0, x1, x2 = 0x9AC02000 | (2<<16) | (1<<5) | 0 = 0x9AC22020
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-lslv b 'x0 'x1 'x2)))
                 (nelisp-asm-arm64-test--word #x9AC22020))))

(ert-deftest nelisp-asm-arm64-lslv-x29-x29-x0 ()
  ;; LSLV x29, x29, x0 = 0x9AC02000 | (0<<16) | (29<<5) | 29 = 0x9AC023BD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-lslv b 'x29 'x29 'x0)))
                 (nelisp-asm-arm64-test--word #x9AC023BD))))

(ert-deftest nelisp-asm-arm64-asrv-x0-x1-x2 ()
  ;; ASRV x0, x1, x2 = 0x9AC02800 | (2<<16) | (1<<5) | 0 = 0x9AC22820
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-asrv b 'x0 'x1 'x2)))
                 (nelisp-asm-arm64-test--word #x9AC22820))))

(ert-deftest nelisp-asm-arm64-asrv-x29-x29-x0 ()
  ;; ASRV x29, x29, x0 = 0x9AC02800 | (0<<16) | (29<<5) | 29 = 0x9AC02BBD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-asrv b 'x29 'x29 'x0)))
                 (nelisp-asm-arm64-test--word #x9AC02BBD))))

(ert-deftest nelisp-asm-arm64-b-cond-eq-placeholder-and-fixup ()
  ;; B.eq foo emits the base word with imm19 = 0 and records a b19 fixup.
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-b-cond b 'eq 'foo)
    (should (equal (nelisp-asm-arm64-buffer-bytes b)
                   (nelisp-asm-arm64-test--word #x54000000)))
    (should (equal (nelisp-asm-arm64-buffer-fixups b)
                   '((0 foo b19))))))

(ert-deftest nelisp-asm-arm64-b-cond-lt-after-nop-placeholder-and-fixup ()
  ;; B.lt foo at slot 4 keeps imm19 clear in the placeholder and records b19.
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-b-cond b 'lt 'foo)
    (should (equal (nelisp-asm-arm64-buffer-bytes b)
                   (concat (nelisp-asm-arm64-test--word #xD503201F)
                           (nelisp-asm-arm64-test--word #x5400000B))))
    (should (equal (nelisp-asm-arm64-buffer-fixups b)
                   '((4 foo b19))))))

(ert-deftest nelisp-asm-arm64-str-pre-sp-16-x0 ()
  ;; STR x0, [sp, #-16]! = 0xF81F0FE0 | 0 = 0xF81F0FE0
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-str-pre-sp-16 b 'x0)))
                 (nelisp-asm-arm64-test--word #xF81F0FE0))))

(ert-deftest nelisp-asm-arm64-str-pre-sp-16-x10 ()
  ;; STR x10, [sp, #-16]! = 0xF81F0FE0 | 10 = 0xF81F0FEA
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-str-pre-sp-16 b 'x10)))
                 (nelisp-asm-arm64-test--word #xF81F0FEA))))

(ert-deftest nelisp-asm-arm64-str-pre-sp-16-x29 ()
  ;; STR x29, [sp, #-16]! = 0xF81F0FE0 | 29 = 0xF81F0FFD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-str-pre-sp-16 b 'x29)))
                 (nelisp-asm-arm64-test--word #xF81F0FFD))))

(ert-deftest nelisp-asm-arm64-ldr-post-sp-16-x0 ()
  ;; LDR x0, [sp], #16 = 0xF84107E0 | 0 = 0xF84107E0
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-ldr-post-sp-16 b 'x0)))
                 (nelisp-asm-arm64-test--word #xF84107E0))))

(ert-deftest nelisp-asm-arm64-ldr-post-sp-16-x10 ()
  ;; LDR x10, [sp], #16 = 0xF84107E0 | 10 = 0xF84107EA
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-ldr-post-sp-16 b 'x10)))
                 (nelisp-asm-arm64-test--word #xF84107EA))))

(ert-deftest nelisp-asm-arm64-ldr-post-sp-16-x29 ()
  ;; LDR x29, [sp], #16 = 0xF84107E0 | 29 = 0xF84107FD
  (should (equal (nelisp-asm-arm64-test--bytes
                  (lambda (b)
                    (nelisp-asm-arm64-ldr-post-sp-16 b 'x29)))
                 (nelisp-asm-arm64-test--word #xF84107FD))))

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

;; ---- §92.d-arm64 chunk-build invariants + perf gate ----

(ert-deftest nelisp-asm-arm64-92d-chunks-field-exists-after-emit ()
  ;; §92.d-arm64 invariant: emitter must push onto :chunks (=
  ;; reverse-order list) and bump :length, instead of concatenating
  ;; onto :bytes.  AArch64 instructions are fixed-width 4 bytes, so
  ;; one nop + one ret = 2 chunks, each length 4 = total :length 8.
  (let* ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-ret b)
    (let* ((plist (aref b 0))
           (chunks (plist-get plist :chunks))
           (len (plist-get plist :length)))
      (should (listp chunks))
      (should (= (length chunks) 2))
      ;; Most-recent push at head -> ret first, nop second.
      ;; RET (X30) = 0xD65F03C0 -> LE bytes C0 03 5F D6.
      (should (equal (car chunks)
                     (nelisp-asm-arm64-test--ub #xC0 #x03 #x5F #xD6)))
      ;; NOP = 0xD503201F -> LE bytes 1F 20 03 D5.
      (should (equal (cadr chunks)
                     (nelisp-asm-arm64-test--ub #x1F #x20 #x03 #xD5)))
      (should (= len 8)))))

(ert-deftest nelisp-asm-arm64-92d-buffer-bytes-finalize-matches ()
  ;; §92.d-arm64 invariant: finalize via `apply concat nreverse'
  ;; yields the same byte sequence as the pre-optimization
  ;; concat-on-each-emit pattern.  Cross-check with `exit(0)' shape.
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

(ert-deftest nelisp-asm-arm64-92d-buffer-bytes-idempotent ()
  ;; §92.d-arm64: `buffer-bytes' must be idempotent — repeated calls
  ;; return the same string and do not destructively reverse :chunks.
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-nop b)
    (nelisp-asm-arm64-ret b)
    (let ((first  (nelisp-asm-arm64-buffer-bytes b))
          (second (nelisp-asm-arm64-buffer-bytes b))
          (third  (nelisp-asm-arm64-buffer-bytes b)))
      (should (equal first second))
      (should (equal second third))
      (should (= (length first) 12))
      ;; First 4 bytes = NOP LE.
      (should (= (aref first 0) #x1F))
      (should (= (aref first 1) #x20))
      (should (= (aref first 2) #x03))
      (should (= (aref first 3) #xD5)))))

(ert-deftest nelisp-asm-arm64-92d-resolve-fixups-collapses-chunks ()
  ;; §92.d-arm64: `resolve-fixups' materializes + patches + stores
  ;; back as a single chunk; subsequent `buffer-bytes' returns the
  ;; patched form.  BL to immediately following label = imm26 = 1.
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (nelisp-asm-arm64-bl b 'foo)
    (nelisp-asm-arm64-define-label b 'foo)
    (nelisp-asm-arm64-ret b)
    (let ((patched (nelisp-asm-arm64-resolve-fixups b)))
      ;; BL with imm26 = 1 -> 0x94000001 -> LE 01 00 00 94.
      (should (equal patched
                     (concat
                      (nelisp-asm-arm64-test--ub #x01 #x00 #x00 #x94)
                      (nelisp-asm-arm64-test--ub #xC0 #x03 #x5F #xD6))))
      ;; After resolve, chunks list is collapsed to a single chunk.
      (let* ((plist (aref b 0))
             (chunks (plist-get plist :chunks)))
        (should (= (length chunks) 1))
        (should (equal (car chunks) patched)))
      ;; And `buffer-bytes' returns the patched form.
      (should (equal (nelisp-asm-arm64-buffer-bytes b) patched)))))

(ert-deftest nelisp-asm-arm64-92d-benchmark-emit-100kb ()
  ;; §92.d-arm64 perf gate: 100 KB of synthetic NOP emit completes
  ;; well under 2 sec on commodity hardware via chunk-build (= O(N)
  ;; total).  Pre-optimization `(concat old bs)` per byte was O(N²)
  ;; and would take >30 sec for 100 KB.
  (let* ((b (nelisp-asm-arm64-make-buffer))
         (nbytes (* 100 1024))
         (start (current-time)))
    (nelisp-asm-arm64-benchmark-emit b nbytes)
    (let* ((bytes (nelisp-asm-arm64-buffer-bytes b))
           (elapsed (float-time (time-subtract (current-time) start))))
      (should (= (length bytes) nbytes))
      ;; First 4 bytes of every chunk are NOP LE (1F 20 03 D5).
      (should (= (aref bytes 0) #x1F))
      (should (= (aref bytes 3) #xD5))
      (should (= (aref bytes (- nbytes 4)) #x1F))
      (should (= (aref bytes (- nbytes 1)) #xD5))
      (should (< elapsed 2.0)))))

(ert-deftest nelisp-asm-arm64-92d-benchmark-emit-1mb ()
  ;; §92.d-arm64 perf gate: 1 MB of synthetic NOP emit completes in
  ;; < 5 sec.  Mirrors §92.d x86_64's 1 MB benchmark target.
  (let* ((b (nelisp-asm-arm64-make-buffer))
         (nbytes (* 1024 1024))
         (start (current-time)))
    (nelisp-asm-arm64-benchmark-emit b nbytes)
    (let* ((bytes (nelisp-asm-arm64-buffer-bytes b))
           (elapsed (float-time (time-subtract (current-time) start))))
      (should (= (length bytes) nbytes))
      (should (= (nelisp-asm-arm64-buffer-pos b) nbytes))
      (should (< elapsed 5.0)))))

(ert-deftest nelisp-asm-arm64-92d-buffer-pos-cached-o1 ()
  ;; §92.d-arm64: `buffer-pos' must read from cached :length field
  ;; (= O(1)) — never traverse :chunks to recompute total bytes.
  ;; AArch64 instructions are 4 bytes each.
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should (= (nelisp-asm-arm64-buffer-pos b) 0))
    (nelisp-asm-arm64-nop b)
    (should (= (nelisp-asm-arm64-buffer-pos b) 4))
    (dotimes (_ 100) (nelisp-asm-arm64-nop b))
    (should (= (nelisp-asm-arm64-buffer-pos b) 404))
    ;; mov-imm64 with #xDEADBEEFCAFEBABE = 4 slices = 16 bytes.
    (nelisp-asm-arm64-mov-imm64 b 'x0 #xDEADBEEFCAFEBABE)
    (should (= (nelisp-asm-arm64-buffer-pos b) 420))))

(provide 'nelisp-asm-arm64-test)

;;; nelisp-asm-arm64-test.el ends here
