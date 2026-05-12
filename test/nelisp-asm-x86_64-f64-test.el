;;; nelisp-asm-x86_64-f64-test.el --- ERT tests for Doc 110 §110.A f64 ABI helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 110 §110.A.1 — pure-elisp ert tests for the SSE2 / xmm
;; encoding helpers added to `lisp/nelisp-asm-x86_64.el':
;;
;;   nelisp-asm-x86_64--xmm-reg            (= reg table)
;;   nelisp-asm-x86_64--xmm-reg-num        (= lookup)
;;   nelisp-asm-x86_64--xmm-reg-low3       (= low 3 bits for ModR/M)
;;   nelisp-asm-x86_64--xmm-reg-ext        (= REX.R/.B bit)
;;   nelisp-asm-x86_64-movsd-reg-reg       (= F2 0F 10 /r reg-reg)
;;   nelisp-asm-x86_64-movsd-xmm-mem-disp8 (= F2 0F 10 /r [base+disp8])
;;   nelisp-asm-x86_64-movsd-mem-disp8-xmm (= F2 0F 11 /r [base+disp8])
;;   nelisp-asm-x86_64-movsd-xmm-rip-disp32 (= F2 0F 10 /r [rip+disp32])
;;
;; All encoding values cross-checked against Intel SDM Vol 2A:
;; MOVSD entry (p. 4-159 in Order 325383) + §2.2 ModR/M / §2.2.1.6
;; RIP-relative + §2.5 REX prefix.

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

;; ---- helpers (= mirror nelisp-asm-x86_64-test--{emit,bytes,ub}) ----

(defun nelisp-asm-x86_64-f64-test--bytes (thunk)
  "Run THUNK on a fresh buffer, return its accumulated bytes."
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (funcall thunk b)
    (nelisp-asm-x86_64-buffer-bytes b)))

(defun nelisp-asm-x86_64-f64-test--ub (&rest bs)
  "Construct a unibyte-string from the integer args BS."
  (apply #'unibyte-string bs))

;; ---- §110.A.1 (1) xmm register table ----

(ert-deftest nelisp-asm-x86_64-f64/xmm-reg-num-xmm0 ()
  (should (= (nelisp-asm-x86_64--xmm-reg-num 'xmm0) 0)))

(ert-deftest nelisp-asm-x86_64-f64/xmm-reg-num-xmm7 ()
  (should (= (nelisp-asm-x86_64--xmm-reg-num 'xmm7) 7)))

(ert-deftest nelisp-asm-x86_64-f64/xmm-reg-num-xmm8 ()
  (should (= (nelisp-asm-x86_64--xmm-reg-num 'xmm8) 8)))

(ert-deftest nelisp-asm-x86_64-f64/xmm-reg-num-xmm15 ()
  (should (= (nelisp-asm-x86_64--xmm-reg-num 'xmm15) 15)))

(ert-deftest nelisp-asm-x86_64-f64/xmm-reg-num-unknown-errors ()
  (should-error (nelisp-asm-x86_64--xmm-reg-num 'xmm16)
                :type 'nelisp-asm-x86_64-error)
  ;; GP symbol must NOT resolve through the xmm table — preserves
  ;; the type-discipline boundary that motivates the separate alist.
  (should-error (nelisp-asm-x86_64--xmm-reg-num 'rax)
                :type 'nelisp-asm-x86_64-error))

(ert-deftest nelisp-asm-x86_64-f64/xmm-reg-low3-and-ext ()
  ;; xmm0-xmm7 → low3 = N, ext = 0
  (should (= (nelisp-asm-x86_64--xmm-reg-low3 'xmm0) 0))
  (should (= (nelisp-asm-x86_64--xmm-reg-low3 'xmm5) 5))
  (should (= (nelisp-asm-x86_64--xmm-reg-low3 'xmm7) 7))
  (should (= (nelisp-asm-x86_64--xmm-reg-ext  'xmm0) 0))
  (should (= (nelisp-asm-x86_64--xmm-reg-ext  'xmm7) 0))
  ;; xmm8-xmm15 → low3 = N - 8, ext = 1
  (should (= (nelisp-asm-x86_64--xmm-reg-low3 'xmm8)  0))
  (should (= (nelisp-asm-x86_64--xmm-reg-low3 'xmm15) 7))
  (should (= (nelisp-asm-x86_64--xmm-reg-ext  'xmm8)  1))
  (should (= (nelisp-asm-x86_64--xmm-reg-ext  'xmm15) 1)))

;; ---- §110.A.1 (2) MOVSD reg-reg ----

;; MOVSD xmm0, xmm1 — F2 0F 10 C1 (= mod=11, reg=000, rm=001)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm0-xmm1 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #xC1))))

;; MOVSD xmm1, xmm0 — F2 0F 10 C8 (= mod=11, reg=001, rm=000)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm1-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm1 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #xC8))))

;; MOVSD xmm0, xmm0 — F2 0F 10 C0 (= self-move; degenerate)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm0-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm0 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #xC0))))

;; MOVSD xmm7, xmm7 — F2 0F 10 FF (= max-low3 both sides)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm7-xmm7 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm7 'xmm7)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #xFF))))

;; MOVSD xmm8, xmm0 — F2 44 0F 10 C0 (= REX.R=1)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm8-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm8 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x44 #x0F #x10 #xC0))))

;; MOVSD xmm0, xmm8 — F2 41 0F 10 C0 (= REX.B=1)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm0-xmm8 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm0 'xmm8)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x41 #x0F #x10 #xC0))))

;; MOVSD xmm15, xmm15 — F2 45 0F 10 FF (= REX.R=1 + REX.B=1)
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-xmm15-xmm15 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm15 'xmm15)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x45 #x0F #x10 #xFF))))

;; Unknown reg signals
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-unknown-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-movsd-reg-reg b 'rax 'xmm0)
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-movsd-reg-reg b 'xmm0 'rax)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §110.A.1 (3) MOVSD load from [base+disp8] ----

;; MOVSD xmm0, [rbp - 8] — F2 0F 10 45 F8
;;   mod=01, reg=000 (xmm0), rm=101 (rbp) → 0x45
;;   disp8 = -8 = 0xF8
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-xmm0-rbp-minus-8 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'rbp -8)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #x45 #xF8))))

;; MOVSD xmm0, [rdi + 8] — F2 0F 10 47 08
;;   mod=01, reg=000, rm=111 (rdi) → 0x47, disp8 = 8
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-xmm0-rdi-plus-8 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'rdi 8)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #x47 #x08))))

;; MOVSD xmm1, [rdi + 16] — F2 0F 10 4F 10
;;   mod=01, reg=001 (xmm1), rm=111 → 0x4F, disp8 = 16
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-xmm1-rdi-plus-16 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm1 'rdi 16)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #x4F #x10))))

;; MOVSD xmm8, [rdi + 0] — F2 44 0F 10 47 00 (= REX.R for xmm8)
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-xmm8-rdi-plus-0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm8 'rdi 0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x44 #x0F #x10 #x47 #x00))))

;; MOVSD xmm0, [r9 + 0] — F2 41 0F 10 41 00 (= REX.B for r9)
;;   mod=01, reg=000, rm=001 (r9.low3) → 0x41, disp8 = 0
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-xmm0-r9-plus-0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'r9 0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x41 #x0F #x10 #x41 #x00))))

;; MOVSD xmm8, [r9 + 0x7F] — F2 45 0F 10 41 7F (= REX.R + REX.B)
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-xmm8-r9-plus-127 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm8 'r9 127)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x45 #x0F #x10 #x41 #x7F))))

;; rsp / r12 base must signal (= SIB-required encoding not modelled)
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-rsp-base-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'rsp 0)
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'r12 0)
                  :type 'nelisp-asm-x86_64-error)))

;; disp8 out of range signals
(ert-deftest nelisp-asm-x86_64-f64/movsd-load-disp8-overflow-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'rbp 128)
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'rbp -129)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §110.A.1 (4) MOVSD store to [base+disp8] ----

;; MOVSD [rbp - 8], xmm0 — F2 0F 11 45 F8 (= opcode 0x11 vs 0x10)
(ert-deftest nelisp-asm-x86_64-f64/movsd-store-rbp-minus-8-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-mem-disp8-xmm b 'rbp -8 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x11 #x45 #xF8))))

;; MOVSD [rdi + 16], xmm1 — F2 0F 11 4F 10
(ert-deftest nelisp-asm-x86_64-f64/movsd-store-rdi-plus-16-xmm1 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-mem-disp8-xmm b 'rdi 16 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x11 #x4F #x10))))

;; MOVSD [rdi + 0], xmm8 — F2 44 0F 11 47 00 (= REX.R)
(ert-deftest nelisp-asm-x86_64-f64/movsd-store-rdi-plus-0-xmm8 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-mem-disp8-xmm b 'rdi 0 'xmm8)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x44 #x0F #x11 #x47 #x00))))

;; Store with rsp base errors
(ert-deftest nelisp-asm-x86_64-f64/movsd-store-rsp-base-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-movsd-mem-disp8-xmm b 'rsp 0 'xmm0)
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-movsd-mem-disp8-xmm b 'r12 0 'xmm0)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §110.A.1 (5) MOVSD load from [rip+disp32] ----

;; MOVSD xmm0, [rip + 0] — F2 0F 10 05 00 00 00 00
;;   mod=00, reg=000 (xmm0), rm=101 (= RIP-relative in 64-bit mode)
;;   disp32 = 0
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-xmm0-disp-0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 0)))
           (nelisp-asm-x86_64-f64-test--ub
            #xF2 #x0F #x10 #x05  #x00 #x00 #x00 #x00))))

;; MOVSD xmm0, [rip + 64] — F2 0F 10 05 40 00 00 00
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-xmm0-disp-64 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 64)))
           (nelisp-asm-x86_64-f64-test--ub
            #xF2 #x0F #x10 #x05  #x40 #x00 #x00 #x00))))

;; MOVSD xmm0, [rip + 0x12345678] — little-endian disp
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-xmm0-disp-large ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 #x12345678)))
           (nelisp-asm-x86_64-f64-test--ub
            #xF2 #x0F #x10 #x05  #x78 #x56 #x34 #x12))))

;; MOVSD xmm0, [rip - 1] — signed disp32, -1 = 0xFFFFFFFF
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-xmm0-disp-negative ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 -1)))
           (nelisp-asm-x86_64-f64-test--ub
            #xF2 #x0F #x10 #x05  #xFF #xFF #xFF #xFF))))

;; MOVSD xmm8, [rip + 0] — F2 44 0F 10 05 00 00 00 00 (= REX.R)
;;   mod=00, reg=000 (xmm8.low3), rm=101 → 0x05
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-xmm8-disp-0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm8 0)))
           (nelisp-asm-x86_64-f64-test--ub
            #xF2 #x44 #x0F #x10 #x05  #x00 #x00 #x00 #x00))))

;; MOVSD xmm1, [rip + 0] — F2 0F 10 0D 00 00 00 00
;;   reg=001 (xmm1) → ModR/M = 0x0D
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-xmm1-disp-0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm1 0)))
           (nelisp-asm-x86_64-f64-test--ub
            #xF2 #x0F #x10 #x0D  #x00 #x00 #x00 #x00))))

;; disp32 max bounds
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-disp32-int32-min ()
  (let ((min32 (- (ash 1 31))))
    (should (equal
             (nelisp-asm-x86_64-f64-test--bytes
              (lambda (b)
                (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 min32)))
             (nelisp-asm-x86_64-f64-test--ub
              #xF2 #x0F #x10 #x05  #x00 #x00 #x00 #x80)))))

(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-disp32-int32-max ()
  (let ((max32 (1- (ash 1 31))))
    (should (equal
             (nelisp-asm-x86_64-f64-test--bytes
              (lambda (b)
                (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 max32)))
             (nelisp-asm-x86_64-f64-test--ub
              #xF2 #x0F #x10 #x05  #xFF #xFF #xFF #x7F)))))

;; disp32 overflow signals
(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-disp32-overflow-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 (ash 1 31))
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0
                                                          (- (1+ (ash 1 31))))
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §110.A.1 (6) buffer position invariants ----

;; Sanity: emitted byte length matches the documented per-form sizes.
(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-pos-4-no-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movsd-reg-reg b 'xmm0 'xmm1)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 4))))

(ert-deftest nelisp-asm-x86_64-f64/movsd-reg-reg-pos-5-with-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movsd-reg-reg b 'xmm8 'xmm0)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 5))))

(ert-deftest nelisp-asm-x86_64-f64/movsd-load-pos-5-no-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm0 'rdi 0)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 5))))

(ert-deftest nelisp-asm-x86_64-f64/movsd-load-pos-6-with-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movsd-xmm-mem-disp8 b 'xmm8 'rdi 0)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 6))))

(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-pos-8-no-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm0 0)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 8))))

(ert-deftest nelisp-asm-x86_64-f64/movsd-rip-pos-9-with-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movsd-xmm-rip-disp32 b 'xmm8 0)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 9))))

;; ---- §110.B (1) ADDSD reg-reg ----
;;
;; F2 0F 58 ModR/M — ModR/M same as MOVSD reg-reg, only opcode byte
;; differs.  Coverage focuses on (a) baseline reg+reg with no REX,
;; (b) commutation (dst ↔ src), (c) REX.R, (d) REX.B, (e) self-op
;; degenerate case.

;; ADDSD xmm0, xmm1 — F2 0F 58 C1
(ert-deftest nelisp-asm-x86_64-f64/addsd-xmm0-xmm1 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-addsd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x58 #xC1))))

;; ADDSD xmm1, xmm0 — F2 0F 58 C8
(ert-deftest nelisp-asm-x86_64-f64/addsd-xmm1-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-addsd-reg-reg b 'xmm1 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x58 #xC8))))

;; ADDSD xmm0, xmm0 — F2 0F 58 C0 (= self-op)
(ert-deftest nelisp-asm-x86_64-f64/addsd-xmm0-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-addsd-reg-reg b 'xmm0 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x58 #xC0))))

;; ADDSD xmm8, xmm0 — F2 44 0F 58 C0 (= REX.R)
(ert-deftest nelisp-asm-x86_64-f64/addsd-xmm8-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-addsd-reg-reg b 'xmm8 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x44 #x0F #x58 #xC0))))

;; ADDSD xmm0, xmm8 — F2 41 0F 58 C0 (= REX.B)
(ert-deftest nelisp-asm-x86_64-f64/addsd-xmm0-xmm8 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-addsd-reg-reg b 'xmm0 'xmm8)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x41 #x0F #x58 #xC0))))

;; ---- §110.B (2) SUBSD reg-reg (= opcode 0x5C) ----

;; SUBSD xmm0, xmm1 — F2 0F 5C C1
(ert-deftest nelisp-asm-x86_64-f64/subsd-xmm0-xmm1 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-subsd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x5C #xC1))))

;; SUBSD xmm15, xmm15 — F2 45 0F 5C FF (= REX.R + REX.B)
(ert-deftest nelisp-asm-x86_64-f64/subsd-xmm15-xmm15 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-subsd-reg-reg b 'xmm15 'xmm15)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x45 #x0F #x5C #xFF))))

;; ---- §110.B (3) MULSD reg-reg (= opcode 0x59) ----

;; MULSD xmm0, xmm1 — F2 0F 59 C1
(ert-deftest nelisp-asm-x86_64-f64/mulsd-xmm0-xmm1 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-mulsd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x59 #xC1))))

;; MULSD xmm8, xmm15 — F2 45 0F 59 C7 (= REX.R + REX.B,
;;   reg=000 (xmm8.low3), rm=111 (xmm15.low3) → 0xC7)
(ert-deftest nelisp-asm-x86_64-f64/mulsd-xmm8-xmm15 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-mulsd-reg-reg b 'xmm8 'xmm15)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x45 #x0F #x59 #xC7))))

;; ---- §110.B (4) DIVSD reg-reg (= opcode 0x5E) ----

;; DIVSD xmm0, xmm1 — F2 0F 5E C1
(ert-deftest nelisp-asm-x86_64-f64/divsd-xmm0-xmm1 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-divsd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x5E #xC1))))

;; DIVSD xmm7, xmm7 — F2 0F 5E FF (= max-low3 self-op, no REX)
(ert-deftest nelisp-asm-x86_64-f64/divsd-xmm7-xmm7 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-divsd-reg-reg b 'xmm7 'xmm7)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x5E #xFF))))

;; ---- §110.B (5) byte-length invariants for arith ops ----

(ert-deftest nelisp-asm-x86_64-f64/addsd-pos-4-no-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-addsd-reg-reg b 'xmm0 'xmm1)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 4))))

(ert-deftest nelisp-asm-x86_64-f64/addsd-pos-5-with-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-addsd-reg-reg b 'xmm8 'xmm0)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 5))))

;; ---- §110.B (6) Unknown-reg signal coverage for arith ops ----

(ert-deftest nelisp-asm-x86_64-f64/addsd-unknown-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-addsd-reg-reg b 'rax 'xmm0)
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-addsd-reg-reg b 'xmm0 'rax)
                  :type 'nelisp-asm-x86_64-error)))

(ert-deftest nelisp-asm-x86_64-f64/divsd-unknown-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-divsd-reg-reg b 'xmm0 'r8)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §110.B (7) opcode discrimination cross-check ----
;;
;; Sanity: with the same xmm-xmm operands, the 4 arith ops produce
;; bytes that differ in exactly one position (= the opcode at
;; offset 2).  Catches accidental opcode collisions if the shared
;; skeleton ever regresses.

(ert-deftest nelisp-asm-x86_64-f64/arith-opcode-discrimination ()
  (let* ((bs (lambda (op)
               (nelisp-asm-x86_64-f64-test--bytes
                (lambda (b)
                  (funcall op b 'xmm0 'xmm1)))))
         (add (funcall bs #'nelisp-asm-x86_64-addsd-reg-reg))
         (sub (funcall bs #'nelisp-asm-x86_64-subsd-reg-reg))
         (mul (funcall bs #'nelisp-asm-x86_64-mulsd-reg-reg))
         (div (funcall bs #'nelisp-asm-x86_64-divsd-reg-reg)))
    ;; All 4 ops emit 4 bytes for xmm0/xmm1.
    (should (= (length add) 4))
    (should (= (length sub) 4))
    (should (= (length mul) 4))
    (should (= (length div) 4))
    ;; Byte 0..1 (= F2 0F prefix), byte 3 (= ModR/M) identical.
    (dolist (x (list sub mul div))
      (should (= (aref add 0) (aref x 0)))
      (should (= (aref add 1) (aref x 1)))
      (should (= (aref add 3) (aref x 3))))
    ;; Byte 2 (= opcode) is the discriminator.
    (should (= (aref add 2) #x58))
    (should (= (aref sub 2) #x5C))
    (should (= (aref mul 2) #x59))
    (should (= (aref div 2) #x5E))
    ;; All opcodes are distinct.
    (let ((ops (list (aref add 2) (aref sub 2)
                     (aref mul 2) (aref div 2))))
      (should (= (length (delete-dups (copy-sequence ops)))
                 4)))))

;; ---- §110.B (8) MOVSD vs ADDSD share skeleton — encoding fidelity ----
;;
;; The §110.A.1 refactor extracted `--emit-sse2-scalar-double-rr'
;; from `movsd-reg-reg' and now ADDSD-family also uses it.  Confirm
;; MOVSD opcode = 0x10 remains stable post-refactor (= ensures the
;; refactor didn't accidentally rebind the opcode in the shared
;; emitter).

(ert-deftest nelisp-asm-x86_64-f64/movsd-after-refactor-stable ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-movsd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #xF2 #x0F #x10 #xC1))))

(provide 'nelisp-asm-x86_64-f64-test)

;;; nelisp-asm-x86_64-f64-test.el ends here
