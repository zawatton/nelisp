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

;; ---- §110.C.1 (1) UCOMISD reg-reg ----
;;
;; UCOMISD switches the SSE2 prefix from F2 (= scalar double for
;; MOVSD/ADDSD family) to 66 (= packed-double encoding family).
;; The shared `--emit-sse2-rr' skeleton parametrises the prefix
;; so the same ModR/M + REX policy applies.

;; UCOMISD xmm0, xmm1 — 66 0F 2E C1
;; Prefix discrimination from MOVSD/ADDSD: 0x66 (packed-double) vs
;; the 0xF2 (scalar-double) shared by the arith family.  Confirms
;; the parametrised `--emit-sse2-rr' skeleton doesn't silently
;; revert to the F2 default for the comparison opcode.
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-xmm0-xmm1-prefix-66 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm1)))
           (nelisp-asm-x86_64-f64-test--ub #x66 #x0F #x2E #xC1))))

;; And explicitly assert NOT-F2 to pin the prefix discriminator:
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-prefix-not-f2 ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--bytes
                (lambda (b)
                  (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm1)))))
    (should (= (aref bytes 0) #x66))
    (should-not (= (aref bytes 0) #xF2))))

;; UCOMISD xmm1, xmm0 — 66 0F 2E C8
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-xmm1-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm1 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #x66 #x0F #x2E #xC8))))

;; UCOMISD xmm8, xmm0 — 66 44 0F 2E C0 (= REX.R)
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-xmm8-xmm0 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm8 'xmm0)))
           (nelisp-asm-x86_64-f64-test--ub #x66 #x44 #x0F #x2E #xC0))))

;; UCOMISD xmm0, xmm8 — 66 41 0F 2E C0 (= REX.B)
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-xmm0-xmm8 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm8)))
           (nelisp-asm-x86_64-f64-test--ub #x66 #x41 #x0F #x2E #xC0))))

;; UCOMISD xmm15, xmm15 — 66 45 0F 2E FF
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-xmm15-xmm15 ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b)
              (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm15 'xmm15)))
           (nelisp-asm-x86_64-f64-test--ub #x66 #x45 #x0F #x2E #xFF))))

;; ucomisd length invariants (4 bytes no REX, 5 with)
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-pos-4-no-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm1)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 4))))

(ert-deftest nelisp-asm-x86_64-f64/ucomisd-pos-5-with-rex ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm8)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 5))))

;; Unknown reg signals
(ert-deftest nelisp-asm-x86_64-f64/ucomisd-unknown-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-ucomisd-reg-reg b 'rax 'xmm0)
                  :type 'nelisp-asm-x86_64-error)
    (should-error (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'rax)
                  :type 'nelisp-asm-x86_64-error)))

;; ---- §110.C.1 (2) SETcc AL ----
;;
;; Each cc produces `0F 9X C0' where X is the cc-specific low
;; nibble.  ModR/M = C0 fixes the target as AL (= ModR/M.rm = 0
;; with mod=11; the /0 in the SDM table is don't-care for SETcc
;; since the opcode encodes the condition itself).

;; SETB AL — 0F 92 C0 (= CF=1, a < b unsigned)
(ert-deftest nelisp-asm-x86_64-f64/setb-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setb)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x92 #xC0))))

;; SETAE AL — 0F 93 C0
(ert-deftest nelisp-asm-x86_64-f64/setae-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setae)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x93 #xC0))))

;; SETE AL — 0F 94 C0
(ert-deftest nelisp-asm-x86_64-f64/sete-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'sete)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x94 #xC0))))

;; SETNE AL — 0F 95 C0
(ert-deftest nelisp-asm-x86_64-f64/setne-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setne)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x95 #xC0))))

;; SETBE AL — 0F 96 C0
(ert-deftest nelisp-asm-x86_64-f64/setbe-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setbe)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x96 #xC0))))

;; SETA AL — 0F 97 C0
(ert-deftest nelisp-asm-x86_64-f64/seta-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'seta)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x97 #xC0))))

;; SETP AL — 0F 9A C0 (= unordered helper)
(ert-deftest nelisp-asm-x86_64-f64/setp-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setp)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x9A #xC0))))

;; SETNP AL — 0F 9B C0
(ert-deftest nelisp-asm-x86_64-f64/setnp-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setnp)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x9B #xC0))))

;; Signed variants (= unused by f64 path but encoded for completeness)
(ert-deftest nelisp-asm-x86_64-f64/setl-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setl)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x9C #xC0))))

(ert-deftest nelisp-asm-x86_64-f64/setge-al ()
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-setcc-al b 'setge)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #x9D #xC0))))

;; Unknown cc signals
(ert-deftest nelisp-asm-x86_64-f64/setcc-unknown-errors ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (should-error (nelisp-asm-x86_64-setcc-al b 'set-bogus)
                  :type 'nelisp-asm-x86_64-error)))

;; All 12 cc keys produce 3-byte encodings
(ert-deftest nelisp-asm-x86_64-f64/setcc-all-3-bytes ()
  (dolist (cc '(setb setae sete setne setbe seta
                     setp setnp setl setge setle setg))
    (let ((b (nelisp-asm-x86_64-make-buffer)))
      (nelisp-asm-x86_64-setcc-al b cc)
      (should (= (nelisp-asm-x86_64-buffer-pos b) 3)))))

;; ---- §110.C.1 (3) MOVZX EAX, AL (= existing helper, doc-110 reuse) ----
;;
;; The pre-existing `movzx-eax-al' helper (line 1091) is reused for
;; f64 cmp result widening.  Encoding is `0F B6 C0' (3 bytes, no
;; REX) — exploits the AMD64 rule that 32-bit writes implicitly
;; zero-extend into the full 64-bit register, so EAX-extension
;; automatically clears RAX[63:32].  Shorter + cheaper than the
;; explicit `REX.W + MOVZX rax, al' encoding (4 bytes).

(ert-deftest nelisp-asm-x86_64-f64/movzx-eax-al-reuse ()
  ;; Reaffirm the existing helper produces the expected 3-byte
  ;; encoding (= Doc 110 §110.C will call it through the same
  ;; entry point).
  (should (equal
           (nelisp-asm-x86_64-f64-test--bytes
            (lambda (b) (nelisp-asm-x86_64-movzx-eax-al b)))
           (nelisp-asm-x86_64-f64-test--ub #x0F #xB6 #xC0))))

(ert-deftest nelisp-asm-x86_64-f64/movzx-eax-al-pos-3 ()
  (let ((b (nelisp-asm-x86_64-make-buffer)))
    (nelisp-asm-x86_64-movzx-eax-al b)
    (should (= (nelisp-asm-x86_64-buffer-pos b) 3))))

;; ---- §110.C.1 (4) end-to-end compare sequence byte-check ----
;;
;; The §110.E float.rs swap will emit the canonical 3-instruction
;; sequence for each compare op:
;;   UCOMISD xmm0, xmm1   ; flags ← cmp(a, b)
;;   SETcc al             ; al ← (flags match cc) ? 1 : 0
;;   MOVZX eax, al        ; rax ← zext(al)  (= 32-bit zext implicitly
;;                         ;  clears RAX[63:32])
;;
;; For `(a < b)' (= SETB), the byte sequence is:
;;   66 0F 2E C1   ; UCOMISD xmm0, xmm1   (4 bytes)
;;   0F 92 C0      ; SETB al              (3 bytes)
;;   0F B6 C0      ; MOVZX eax, al        (3 bytes)
;; = 10 bytes total.  This ert pins the canonical byte layout so a
;; future swap that depends on it can't silently drift.

(ert-deftest nelisp-asm-x86_64-f64/cmp-lt-sequence-bytes ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--bytes
                (lambda (b)
                  (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm1)
                  (nelisp-asm-x86_64-setcc-al        b 'setb)
                  (nelisp-asm-x86_64-movzx-eax-al    b)))))
    (should (= (length bytes) 10))
    (should (equal bytes
                   (nelisp-asm-x86_64-f64-test--ub
                    #x66 #x0F #x2E #xC1     ; UCOMISD xmm0, xmm1
                    #x0F #x92 #xC0          ; SETB al
                    #x0F #xB6 #xC0)))))     ; MOVZX eax, al

;; Same shape with SETA (= a > b ordered)
(ert-deftest nelisp-asm-x86_64-f64/cmp-gt-sequence-bytes ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--bytes
                (lambda (b)
                  (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm1)
                  (nelisp-asm-x86_64-setcc-al        b 'seta)
                  (nelisp-asm-x86_64-movzx-eax-al    b)))))
    (should (= (length bytes) 10))
    (should (equal bytes
                   (nelisp-asm-x86_64-f64-test--ub
                    #x66 #x0F #x2E #xC1
                    #x0F #x97 #xC0
                    #x0F #xB6 #xC0)))))

;; NaN-aware sequence preview for `(a == b)' Rust semantics:
;;   UCOMISD xmm0, xmm1   ; flags
;;   SETE al              ; al = ZF
;;   SETNP cl             ; (= not implemented; §110.C compiler emit
;;                          will need an AL/CL combo via a NaN mask)
;; For the asm layer we only ensure each individual primitive emits
;; correctly; the masking logic lives in the compiler stage.

(ert-deftest nelisp-asm-x86_64-f64/setnp-sequence-bytes ()
  ;; UCOMISD + SETNP al sequence — gives 1 when ordered (no NaN).
  ;; Used in NaN-aware compare to AND with the primary cc result.
  (let ((bytes (nelisp-asm-x86_64-f64-test--bytes
                (lambda (b)
                  (nelisp-asm-x86_64-ucomisd-reg-reg b 'xmm0 'xmm1)
                  (nelisp-asm-x86_64-setcc-al        b 'setnp)))))
    (should (= (length bytes) 7))
    (should (equal bytes
                   (nelisp-asm-x86_64-f64-test--ub
                    #x66 #x0F #x2E #xC1
                    #x0F #x9B #xC0)))))

;; ---- §110.E.1 (1) Phase 47 compiler f64 integration smoke ----
;;
;; End-to-end compile of `(defun fn ((a :type f64) (b :type f64))
;;   (f64-add a b))' via parse-stmt + emit-defun.  Pin canonical
;; byte layout so the integration's two-pass invariant is locked
;; in before §110.E.2 ships the float.rs swap proper.

(require 'nelisp-phase47-compiler)

(defun nelisp-asm-x86_64-f64-test--compile-defun (sexp)
  "Compile SEXP (= a `(defun ...)' form) and return its emitted bytes.
Uses the Phase 47 compiler entry points directly (= no ELF
wrapping) so byte-level assertions can pin the prologue / body /
epilogue layout."
  (let* ((nelisp-phase47-compiler--arch 'x86_64)
         (nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse-stmt sexp nil nil nil))
         (buf (nelisp-asm-x86_64-make-buffer)))
    (nelisp-phase47-compiler--emit-defun ir buf)
    (nelisp-asm-x86_64-buffer-bytes buf)))

;; Parser accepts annotated f64 params and tags FENV cells.
(ert-deftest nelisp-asm-x86_64-f64/parser-accepts-typed-params ()
  (let ((ir (nelisp-phase47-compiler--parse-stmt
             '(defun fn ((a :type f64) (b :type f64)) (f64-add a b))
             nil nil nil)))
    (should (eq (plist-get ir :kind) 'defun))
    (should (eq (plist-get ir :param-class) 'f64))
    (should (equal (plist-get ir :params) '(a b)))
    (should (equal (plist-get ir :param-regs) '(xmm0 xmm1)))))

;; Parser rejects mixed-class params.
(ert-deftest nelisp-asm-x86_64-f64/parser-rejects-mixed-class ()
  (should-error
   (nelisp-phase47-compiler--parse-stmt
    '(defun fn ((a :type f64) b) (f64-add a b))
    nil nil nil)
   :type 'nelisp-phase47-compiler-error))

;; Parser rejects unknown class (= only `f64' / `gp' supported).
(ert-deftest nelisp-asm-x86_64-f64/parser-rejects-unknown-class ()
  (should-error
   (nelisp-phase47-compiler--parse-stmt
    '(defun fn ((a :type wat)) (f64-add a a))
    nil nil nil)
   :type 'nelisp-phase47-compiler-error))

;; (f64-add) etc. parse to :kind f64-binop.
(ert-deftest nelisp-asm-x86_64-f64/parser-f64-binop-shape ()
  (let* ((ir (nelisp-phase47-compiler--parse-stmt
              '(defun fn ((a :type f64) (b :type f64)) (f64-add a b))
              nil nil nil))
         (body (plist-get ir :body)))
    (should (eq (plist-get body :kind) 'f64-binop))
    (should (eq (plist-get body :op) 'f64-add))))

;; Nested f64-binop rejected at emit time (= MVP scope).
(ert-deftest nelisp-asm-x86_64-f64/emit-rejects-nested-f64-binop ()
  ;; Parsing succeeds — nesting is a structural feature of the IR.
  ;; The rejection lands at emit-defun -> emit-f64-binop ->
  ;; emit-f64-leaf-into, which checks each leaf's shape.
  (should-error
   (nelisp-asm-x86_64-f64-test--compile-defun
    '(defun fn ((a :type f64) (b :type f64))
       (f64-add (f64-add a b) a)))
   :type 'nelisp-phase47-compiler-error))

;; Canonical byte layout for `(defun fn ((a :type f64) (b :type f64))
;;   (f64-add a b))'.
;;
;; Prologue (21 bytes):
;;   55                       ; push rbp
;;   48 89 E5                 ; mov rbp, rsp
;;   48 81 EC 10 00 00 00     ; sub rsp, 16
;;   F2 0F 11 45 F8           ; movsd [rbp - 8], xmm0   (= spill param 0)
;;   F2 0F 11 4D F0           ; movsd [rbp - 16], xmm1  (= spill param 1)
;; Body (14 bytes):
;;   F2 0F 10 4D F0           ; movsd xmm1, [rbp - 16]  (= eval B)
;;   F2 0F 10 45 F8           ; movsd xmm0, [rbp - 8]   (= eval A)
;;   F2 0F 58 C1              ; addsd xmm0, xmm1
;; Epilogue (5 bytes):
;;   48 89 EC                 ; mov rsp, rbp
;;   5D                       ; pop rbp
;;   C3                       ; ret
;; Total = 40 bytes.

(ert-deftest nelisp-asm-x86_64-f64/defun-f64-add-canonical-bytes ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-add a b)))))
    (should (= (length bytes) 40))
    (should (equal bytes
                   (nelisp-asm-x86_64-f64-test--ub
                    ;; Prologue
                    #x55                          ; push rbp
                    #x48 #x89 #xE5                ; mov rbp, rsp
                    #x48 #x81 #xEC #x10 #x00 #x00 #x00  ; sub rsp, 16
                    #xF2 #x0F #x11 #x45 #xF8      ; movsd [rbp-8], xmm0
                    #xF2 #x0F #x11 #x4D #xF0      ; movsd [rbp-16], xmm1
                    ;; Body: eval B → xmm1, eval A → xmm0, addsd
                    #xF2 #x0F #x10 #x4D #xF0      ; movsd xmm1, [rbp-16]
                    #xF2 #x0F #x10 #x45 #xF8      ; movsd xmm0, [rbp-8]
                    #xF2 #x0F #x58 #xC1           ; addsd xmm0, xmm1
                    ;; Epilogue
                    #x48 #x89 #xEC                ; mov rsp, rbp
                    #x5D                          ; pop rbp
                    #xC3)))))                     ; ret

;; (f64-sub / f64-mul / f64-div) — body byte differs at the ADDSD-
;; family opcode position; prologue + epilogue + leaf loads are
;; identical to add.
;;
;; Byte index 33 is the OPCODE position within the trailing 4-byte
;; ADDSD-family instruction:
;;   prologue (21 bytes) ends at index 20
;;   movsd xmm1, [...]   (5 bytes) → indices 21..25
;;   movsd xmm0, [...]   (5 bytes) → indices 26..30
;;   F2 0F OPCODE C1     (4 bytes) → indices 31..34
;;     byte 31 = F2 prefix
;;     byte 32 = 0F escape
;;     byte 33 = OPCODE  (58 = ADDSD, 5C = SUBSD, 59 = MULSD, 5E = DIVSD)
;;     byte 34 = ModR/M (C1 = mod=11 reg=000 [xmm0] rm=001 [xmm1])

(ert-deftest nelisp-asm-x86_64-f64/defun-f64-sub-body-opcode ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-sub a b)))))
    (should (= (length bytes) 40))
    (should (= (aref bytes 33) #x5C))))

(ert-deftest nelisp-asm-x86_64-f64/defun-f64-mul-body-opcode ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-mul a b)))))
    (should (= (length bytes) 40))
    (should (= (aref bytes 33) #x59))))

(ert-deftest nelisp-asm-x86_64-f64/defun-f64-div-body-opcode ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-div a b)))))
    (should (= (length bytes) 40))
    (should (= (aref bytes 33) #x5E))))

;; GP path unchanged — `(defun fn (a b) (+ a b))' still compiles to
;; the legacy `push rdi / push rsi / mov rax via spill / add via
;; r10' sequence with no f64 contamination.
(ert-deftest nelisp-asm-x86_64-f64/gp-defun-unchanged ()
  (let ((bytes (nelisp-asm-x86_64-f64-test--compile-defun
                '(defun fn (a b) (+ a b)))))
    ;; First byte after `push rbp; mov rbp, rsp' (= 4 bytes) is
    ;; the first param push: `push rdi' = 0x57.  Confirms the GP
    ;; arm of the param-class dispatch fires for bare-symbol
    ;; params.
    (should (= (aref bytes 0) #x55))   ; push rbp
    (should (= (aref bytes 4) #x57)))) ; push rdi (= GP param 0)

(provide 'nelisp-asm-x86_64-f64-test)

;;; nelisp-asm-x86_64-f64-test.el ends here
