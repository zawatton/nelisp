;;; nelisp-asm-arm64-f64-test.el --- ERT for Doc 110 §110.D aarch64 f64 helpers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 110 §110.D.1 — pure-elisp ert tests for the SIMD/FP scalar-
;; double encoding helpers added to `lisp/nelisp-asm-arm64.el':
;;
;;   --fp-reg                            (= D-register table)
;;   --fp-reg-num                        (= lookup)
;;   fadd-reg-reg / fsub / fmul / fdiv   (= FADD/FSUB/FMUL/FDIV)
;;   fabs-reg-reg                        (= FABS)
;;   fcmp-reg-reg                        (= FCMP)
;;   fmov-d-from-x                       (= FMOV Dd, Xn)
;;   stur-d-base-disp                    (= STUR Dt, [Xn, #imm9])
;;   ldur-d-base-disp                    (= LDUR Dt, [Xn, #imm9])
;;
;; All encoding values cross-checked against ARM ARM (DDI0487) for
;; the Data Processing — SIMD FP families.

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

(defun nelisp-asm-arm64-f64-test--word (thunk)
  "Run THUNK on a fresh buffer, return its first 4-byte word as integer."
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (funcall thunk b)
    (let* ((bytes (nelisp-asm-arm64-buffer-bytes b)))
      (logior (aref bytes 0)
              (ash (aref bytes 1) 8)
              (ash (aref bytes 2) 16)
              (ash (aref bytes 3) 24)))))

;; ---- §110.D.1 (1) FP register table ----

(ert-deftest nelisp-asm-arm64-f64/fp-reg-num-d0 ()
  (should (= (nelisp-asm-arm64--fp-reg-num 'd0) 0)))

(ert-deftest nelisp-asm-arm64-f64/fp-reg-num-d7 ()
  (should (= (nelisp-asm-arm64--fp-reg-num 'd7) 7)))

(ert-deftest nelisp-asm-arm64-f64/fp-reg-num-d31 ()
  (should (= (nelisp-asm-arm64--fp-reg-num 'd31) 31)))

(ert-deftest nelisp-asm-arm64-f64/fp-reg-num-unknown ()
  (should-error (nelisp-asm-arm64--fp-reg-num 'd32)
                :type 'nelisp-asm-arm64-error)
  ;; GP symbol must not resolve through fp table
  (should-error (nelisp-asm-arm64--fp-reg-num 'x0)
                :type 'nelisp-asm-arm64-error))

;; ---- §110.D.1 (2) FP arithmetic encodings ----

;; FADD D0, D1, D2 = 0x1E622820
;;   base 0x1E602800 | (m=2 << 16) | (n=1 << 5) | d=0
(ert-deftest nelisp-asm-arm64-f64/fadd-d0-d1-d2 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-fadd-reg-reg b 'd0 'd1 'd2)))
             #x1E622820)))

;; FADD D0, D0, D0 (self-op) = 0x1E602800
(ert-deftest nelisp-asm-arm64-f64/fadd-d0-d0-d0 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-fadd-reg-reg b 'd0 'd0 'd0)))
             #x1E602800)))

;; FSUB D0, D0, D1 = 0x1E613800
;;   base 0x1E603800 | (m=1 << 16) | (n=0 << 5) | d=0
(ert-deftest nelisp-asm-arm64-f64/fsub-d0-d0-d1 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-fsub-reg-reg b 'd0 'd0 'd1)))
             #x1E613800)))

;; FMUL D0, D0, D1 = 0x1E610800
(ert-deftest nelisp-asm-arm64-f64/fmul-d0-d0-d1 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-fmul-reg-reg b 'd0 'd0 'd1)))
             #x1E610800)))

;; FDIV D0, D0, D1 = 0x1E611800
(ert-deftest nelisp-asm-arm64-f64/fdiv-d0-d0-d1 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-fdiv-reg-reg b 'd0 'd0 'd1)))
             #x1E611800)))

;; FADD D31, D31, D31 = 0x1E7F2BFF (= max reg encoding)
;;   base 0x1E602800 | (31<<16) | (31<<5) | 31
;;   = 0x1E602800 | 0x1F0000 | 0x3E0 | 0x1F
;;   = 0x1E7F2BFF
(ert-deftest nelisp-asm-arm64-f64/fadd-d31-d31-d31 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-fadd-reg-reg b 'd31 'd31 'd31)))
             #x1E7F2BFF)))

;; ---- §110.D.1 (3) FABS encoding ----

;; FABS D0, D0 = 0x1E60C000
;; FABS D1, D0 = 0x1E60C001
(ert-deftest nelisp-asm-arm64-f64/fabs-d0-d0 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fabs-reg-reg b 'd0 'd0)))
             #x1E60C000)))

(ert-deftest nelisp-asm-arm64-f64/fabs-d1-d0 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fabs-reg-reg b 'd1 'd0)))
             #x1E60C001)))

;; FABS D0, D31 = 0x1E60C3E0  (= src=31, dst=0)
(ert-deftest nelisp-asm-arm64-f64/fabs-d0-d31 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fabs-reg-reg b 'd0 'd31)))
             #x1E60C3E0)))

;; ---- §110.D.1 (4) FCMP encoding ----

;; FCMP D0, D1 = 0x1E612000
;;   base 0x1E602000 | (m=1 << 16) | (n=0 << 5)
(ert-deftest nelisp-asm-arm64-f64/fcmp-d0-d1 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fcmp-reg-reg b 'd0 'd1)))
             #x1E612000)))

;; FCMP D1, D0 = 0x1E602020
(ert-deftest nelisp-asm-arm64-f64/fcmp-d1-d0 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fcmp-reg-reg b 'd1 'd0)))
             #x1E602020)))

;; ---- §110.D.1 (5) FMOV Dd, Xn ----

;; FMOV D0, X10 = 0x9E670140
;;   base 0x9E670000 | (n=10 << 5) | d=0
(ert-deftest nelisp-asm-arm64-f64/fmov-d0-x10 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fmov-d-from-x b 'd0 'x10)))
             #x9E670140)))

;; FMOV D1, X10 = 0x9E670141
(ert-deftest nelisp-asm-arm64-f64/fmov-d1-x10 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fmov-d-from-x b 'd1 'x10)))
             #x9E670141)))

;; FMOV D31, X30 = 0x9E6703DF
(ert-deftest nelisp-asm-arm64-f64/fmov-d31-x30 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b) (nelisp-asm-arm64-fmov-d-from-x b 'd31 'x30)))
             #x9E6703DF)))

;; ---- §110.D.1 (6) STUR / LDUR D ----

;; STUR D0, [X29, #-16] = 0xFC1F03A0
;;   imm9 = -16 = 0x1F0 (9-bit signed), n=29, t=0
(ert-deftest nelisp-asm-arm64-f64/stur-d0-x29-minus-16 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-stur-d-base-disp b 'd0 'x29 -16)))
             #xFC1F03A0)))

;; STUR D1, [X29, #-24] = 0xFC1E83A1
;;   imm9 = -24 = 0x1E8, n=29, t=1
(ert-deftest nelisp-asm-arm64-f64/stur-d1-x29-minus-24 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-stur-d-base-disp b 'd1 'x29 -24)))
             #xFC1E83A1)))

;; STUR D0, [X29, #0] = 0xFC0003A0
(ert-deftest nelisp-asm-arm64-f64/stur-d0-x29-0 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-stur-d-base-disp b 'd0 'x29 0)))
             #xFC0003A0)))

;; LDUR D0, [X29, #-8] = 0xFC5F83A0
;;   imm9 = -8 = 0x1F8, n=29, t=0
(ert-deftest nelisp-asm-arm64-f64/ldur-d0-x29-minus-8 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-ldur-d-base-disp b 'd0 'x29 -8)))
             #xFC5F83A0)))

;; LDUR D1, [X29, #-16] = 0xFC5F03A1
(ert-deftest nelisp-asm-arm64-f64/ldur-d1-x29-minus-16 ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-ldur-d-base-disp b 'd1 'x29 -16)))
             #xFC5F03A1)))

;; STUR D7 at the max +imm9 (= 255) — boundary check
(ert-deftest nelisp-asm-arm64-f64/stur-d-imm9-max ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-stur-d-base-disp b 'd7 'x29 255)))
             #xFC0FF3A7)))

;; STUR D0 at imm9 -256 (= min) — boundary check
(ert-deftest nelisp-asm-arm64-f64/stur-d-imm9-min ()
  (should (= (nelisp-asm-arm64-f64-test--word
              (lambda (b)
                (nelisp-asm-arm64-stur-d-base-disp b 'd0 'x29 -256)))
             #xFC1003A0)))

;; Out-of-range imm9 signals
(ert-deftest nelisp-asm-arm64-f64/stur-d-imm9-overflow ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-stur-d-base-disp b 'd0 'x29 256)
                  :type 'nelisp-asm-arm64-error)
    (should-error (nelisp-asm-arm64-stur-d-base-disp b 'd0 'x29 -257)
                  :type 'nelisp-asm-arm64-error)))

(ert-deftest nelisp-asm-arm64-f64/ldur-d-imm9-overflow ()
  (let ((b (nelisp-asm-arm64-make-buffer)))
    (should-error (nelisp-asm-arm64-ldur-d-base-disp b 'd0 'x29 256)
                  :type 'nelisp-asm-arm64-error)
    (should-error (nelisp-asm-arm64-ldur-d-base-disp b 'd0 'x29 -257)
                  :type 'nelisp-asm-arm64-error)))

;; ---- §110.D.1 (7) byte-length invariants ----

(ert-deftest nelisp-asm-arm64-f64/all-helpers-emit-4-bytes ()
  ;; Every AArch64 instruction is exactly 4 bytes.  Verify each
  ;; helper emits exactly 4 (= no buffer corruption).
  (dolist (thunk
           (list (lambda (b) (nelisp-asm-arm64-fadd-reg-reg b 'd0 'd0 'd0))
                 (lambda (b) (nelisp-asm-arm64-fsub-reg-reg b 'd0 'd0 'd0))
                 (lambda (b) (nelisp-asm-arm64-fmul-reg-reg b 'd0 'd0 'd0))
                 (lambda (b) (nelisp-asm-arm64-fdiv-reg-reg b 'd0 'd0 'd0))
                 (lambda (b) (nelisp-asm-arm64-fabs-reg-reg b 'd0 'd0))
                 (lambda (b) (nelisp-asm-arm64-fcmp-reg-reg b 'd0 'd1))
                 (lambda (b) (nelisp-asm-arm64-fmov-d-from-x b 'd0 'x10))
                 (lambda (b) (nelisp-asm-arm64-stur-d-base-disp b 'd0 'x29 -16))
                 (lambda (b) (nelisp-asm-arm64-ldur-d-base-disp b 'd0 'x29 -8))))
    (let ((b (nelisp-asm-arm64-make-buffer)))
      (funcall thunk b)
      (should (= (nelisp-asm-arm64-buffer-pos b) 4)))))

;; ---- §110.D.2 — Phase 47 compiler aarch64 f64 integration ----
;;
;; End-to-end compile + emit smoke for `(defun fn ((a :type f64)
;; (b :type f64)) (f64-add a b))' under AArch64 target.

(require 'nelisp-phase47-compiler)

(defun nelisp-asm-arm64-f64-test--compile-defun (sexp)
  "Compile SEXP via Phase 47 compiler targeting aarch64.
Returns the emitted bytes from `--emit-defun' for byte-level
assertions.  Sets `--arch' to `aarch64' and uses the arm64
assembler buffer."
  (let* ((nelisp-phase47-compiler--arch 'aarch64)
         (nelisp-phase47-compiler--label-counter 0)
         (ir (nelisp-phase47-compiler--parse-stmt sexp nil nil nil))
         (buf (nelisp-asm-arm64-make-buffer)))
    (nelisp-phase47-compiler--emit-defun ir buf)
    (nelisp-asm-arm64-buffer-bytes buf)))

;; Defun compiles without error (= no `:f64-defun-aarch64-not-yet').
(ert-deftest nelisp-asm-arm64-f64/defun-f64-add-compiles ()
  (let ((bytes (nelisp-asm-arm64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-add a b)))))
    (should (stringp bytes))
    (should (> (length bytes) 0))))

;; Sanity byte-length for f64-add aarch64 layout:
;;   Prologue:
;;     STR x30, [SP, #-16]!  (4)
;;     STR x29, [SP, #-16]!  (4)
;;     MOV x29, sp           (4)
;;     SUB sp, sp, #32       (4)
;;     STUR d0, [x29, #-16]  (4)
;;     STUR d1, [x29, #-32]  (4)  = 24 bytes
;;   Body:
;;     LDUR d1, [x29, #-32]  (4)
;;     LDUR d0, [x29, #-16]  (4)
;;     FADD d0, d0, d1       (4)  = 12 bytes
;;   Epilogue:
;;     MOV sp, x29           (4)
;;     LDR x29, [SP], #16    (4)
;;     LDR x30, [SP], #16    (4)
;;     RET                   (4)  = 16 bytes
;; Total = 52 bytes

(ert-deftest nelisp-asm-arm64-f64/defun-f64-add-byte-length ()
  (let ((bytes (nelisp-asm-arm64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-add a b)))))
    (should (= (length bytes) 52))))

;; Sanity: f64-sub / f64-mul / f64-div use the same prologue +
;; epilogue layout — only the FADD-family opcode differs.  All
;; should compile to 52 bytes.

(ert-deftest nelisp-asm-arm64-f64/defun-f64-binops-same-length ()
  (dolist (op '(f64-add f64-sub f64-mul f64-div))
    (let ((bytes (nelisp-asm-arm64-f64-test--compile-defun
                  `(defun fn ((a :type f64) (b :type f64))
                     (,op a b)))))
      (should (= (length bytes) 52)))))

;; f64-lt / f64-gt / f64-le / f64-ge use FCMP + CSET (= 8 bytes
;; body) instead of FADD (= 4 bytes).  Layout: prologue 24 +
;; (2 LDUR + 1 FCMP + 1 CSET = 16) + epilogue 16 = 56 bytes.

(ert-deftest nelisp-asm-arm64-f64/defun-f64-cmps-byte-length ()
  (dolist (op '(f64-lt f64-gt f64-le f64-ge))
    (let ((bytes (nelisp-asm-arm64-f64-test--compile-defun
                  `(defun fn ((a :type f64) (b :type f64))
                     (,op a b)))))
      (should (= (length bytes) 56)))))

;; f64-eq-eps aarch64 sequence (= body 44 bytes):
;;   2 LDUR (8) + FSUB (4) + FABS (4) +
;;   MOV-imm64 (= 4-instr MOVZ/MOVK chain for the 1e-15 bit
;;     pattern 0x3CD203AF9EE75616 — all 4 chunks non-zero
;;     so 16 bytes) +
;;   FMOV d1, x10 (4) + FCMP d1, d0 (4) + CSET x0, gt (4)
;; Total = 24 prologue + 44 body + 16 epilogue = 84 bytes

(ert-deftest nelisp-asm-arm64-f64/defun-f64-eq-eps-byte-length ()
  (let ((bytes (nelisp-asm-arm64-f64-test--compile-defun
                '(defun fn ((a :type f64) (b :type f64))
                   (f64-eq-eps a b)))))
    (should (= (length bytes) 84))))

(provide 'nelisp-asm-arm64-f64-test)

;;; nelisp-asm-arm64-f64-test.el ends here
