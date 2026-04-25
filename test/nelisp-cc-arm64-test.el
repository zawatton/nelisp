;;; nelisp-cc-arm64-test.el --- ERT for arm64 backend skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1.3 *skeleton* tests for `nelisp-cc-arm64'.  Doc 28 §5
;; commits four test classes for the native compiler; this file
;; populates the *backend encoding golden* class for arm64.  Coverage:
;;
;;   1. RET encodes to 0xD65F03C0 (constant — no operands)
;;   2. MOV X0, X1 encodes to 0xAA0103E0
;;   3. MOVZ X0, #42 encodes to 0xD2800540
;;   4. ADD X0, X1, X2 encodes to 0x8B020020
;;   5. buffer-emit-instruction expands a uint32 into LE 4 bytes
;;   6. forward branch via fixup resolves to the right imm26 offset
;;   7. virtual r0..r7 maps to physical x0..x7 (T4 default-int-regs)
;;   8. compile of `(lambda () nil)' emits MOVZ X0,#0 + RET
;;   9. compile of `(lambda (x) x)' emits a bare RET
;;     (param already in X0 via AAPCS64 ABI, no MOV needed)
;;  10. compile of `(lambda (x y) (+ x y))' emits a BL fixup tagged
;;      `callee:+' followed by the return sequence

;;; Code:

(require 'ert)
(require 'nelisp-cc)
(require 'nelisp-cc-arm64)

;;; (1) RET encoding -------------------------------------------------

(ert-deftest nelisp-cc-arm64-encode-ret-correct ()
  "RET (default Xn=X30) encodes to the canonical 0xD65F03C0."
  (should (= #xD65F03C0 (nelisp-cc-arm64--encode-ret)))
  ;; Explicit X30 must yield the same word.
  (should (= #xD65F03C0 (nelisp-cc-arm64--encode-ret 'x30))))

;;; (2) MOV (register) -----------------------------------------------

(ert-deftest nelisp-cc-arm64-encode-mov-reg-reg-x0-x1 ()
  "MOV X0, X1 encodes to 0xAA0103E0 (canonical alias for ORR)."
  (should (= #xAA0103E0 (nelisp-cc-arm64--encode-mov-reg-reg 'x0 'x1)))
  ;; Sanity: a different register pair changes only the X0/X1 fields.
  (should (= #xAA0503E3 (nelisp-cc-arm64--encode-mov-reg-reg 'x3 'x5))))

;;; (3) MOVZ immediate -----------------------------------------------

(ert-deftest nelisp-cc-arm64-encode-movz-imm-x0-42 ()
  "MOVZ X0, #42 encodes to 0xD2800540."
  (should (= #xD2800540 (nelisp-cc-arm64--encode-movz-imm 'x0 42)))
  ;; #0 with destination X1 yields D2800001.
  (should (= #xD2800001 (nelisp-cc-arm64--encode-movz-imm 'x1 0)))
  ;; LSL #16 shifts hw to 1.
  (should (= #xD2A00541 (nelisp-cc-arm64--encode-movz-imm 'x1 42 16))))

;;; (4) ADD -----------------------------------------------------------

(ert-deftest nelisp-cc-arm64-encode-add-x0-x1-x2 ()
  "ADD X0, X1, X2 (shifted register, shift=0) is 0x8B020020.
SUB and MUL shift only the opcode field and the embedded marker bits;
this test pins all three sibling helpers for symmetry."
  (should (= #x8B020020 (nelisp-cc-arm64--encode-add-reg-reg 'x0 'x1 'x2)))
  (should (= #xCB020020 (nelisp-cc-arm64--encode-sub-reg-reg 'x0 'x1 'x2)))
  (should (= #x9B027C20 (nelisp-cc-arm64--encode-mul-reg-reg 'x0 'x1 'x2))))

;;; (5) buffer LE 4-byte expansion ----------------------------------

(ert-deftest nelisp-cc-arm64-buffer-emit-instruction-uint32-little-endian ()
  "Emitting RET writes the 4 bytes [c0 03 5f d6] (little-endian)."
  (let ((buf (nelisp-cc-arm64--buffer-make)))
    (nelisp-cc-arm64--buffer-emit-instruction
     buf (nelisp-cc-arm64--encode-ret))
    (let ((bytes (nelisp-cc-arm64--buffer-finalize buf)))
      (should (equal bytes [#xC0 #x03 #x5F #xD6]))))
  ;; A second sanity case: MOVZ X0, #42 = 0xD2800540
  ;; -> [40 05 80 d2]
  (let ((buf (nelisp-cc-arm64--buffer-make)))
    (nelisp-cc-arm64--buffer-emit-instruction
     buf (nelisp-cc-arm64--encode-movz-imm 'x0 42))
    (let ((bytes (nelisp-cc-arm64--buffer-finalize buf)))
      (should (equal bytes [#x40 #x05 #x80 #xD2])))))

;;; (6) forward-reference label resolution ---------------------------

(ert-deftest nelisp-cc-arm64-buffer-define-and-resolve-label ()
  "Forward branch via `B' fixup resolves to the correct imm26 offset.
We emit a B referencing label LATER, then a NOP-equivalent (RET, used
only as a 4-byte spacer here), then bind LATER and finalize.  The
displacement is 8 bytes (one B + one RET = 8 bytes), so imm26 = 2 and
the encoded word should be 0x14000002."
  (let ((buf (nelisp-cc-arm64--buffer-make)))
    ;; Position 0: B <later>  (4 bytes, fixup)
    (nelisp-cc-arm64--buffer-emit-fixup
     buf
     (lambda (off) (nelisp-cc-arm64--encode-b off))
     'later)
    ;; Position 4: RET as a 4-byte spacer
    (nelisp-cc-arm64--buffer-emit-instruction
     buf (nelisp-cc-arm64--encode-ret))
    ;; Position 8: bind label LATER
    (nelisp-cc-arm64--buffer-define-label buf 'later)
    ;; Position 8: emit a final RET so the buffer has something at LATER.
    (nelisp-cc-arm64--buffer-emit-instruction
     buf (nelisp-cc-arm64--encode-ret))
    (let ((bytes (nelisp-cc-arm64--buffer-finalize buf)))
      ;; First instruction word should be B +8 = 0x14000002 (LE).
      (should (= 12 (length bytes)))
      (should (equal (aref bytes 0) #x02))
      (should (equal (aref bytes 1) #x00))
      (should (equal (aref bytes 2) #x00))
      (should (equal (aref bytes 3) #x14))
      ;; Second word: RET = c0 03 5f d6
      (should (equal (aref bytes 4) #xC0))
      (should (equal (aref bytes 5) #x03))
      (should (equal (aref bytes 6) #x5F))
      (should (equal (aref bytes 7) #xD6))
      ;; Third word: RET again
      (should (equal (aref bytes 8) #xC0))
      (should (equal (aref bytes 9) #x03))
      (should (equal (aref bytes 10) #x5F))
      (should (equal (aref bytes 11) #xD6)))))

;;; (7) virtual → physical mapping -----------------------------------

(ert-deftest nelisp-cc-arm64-virtual-to-physical-default-mapping ()
  "Default mapping projects T4 r0..r7 onto AAPCS64 x0..x7."
  ;; Sanity-check the constant table matches what `-virtual-reg' returns.
  (should (eq 'x0 (nelisp-cc-arm64--virtual-reg 'r0)))
  (should (eq 'x1 (nelisp-cc-arm64--virtual-reg 'r1)))
  (should (eq 'x7 (nelisp-cc-arm64--virtual-reg 'r7)))
  ;; A physical register passes through unchanged.
  (should (eq 'x0 (nelisp-cc-arm64--virtual-reg 'x0)))
  ;; Encoding accepts both names equivalently.
  (should (= (nelisp-cc-arm64--reg-encoding 'r0)
             (nelisp-cc-arm64--reg-encoding 'x0)))
  (should (= (nelisp-cc-arm64--reg-encoding 'r5)
             (nelisp-cc-arm64--reg-encoding 'x5))))

;;; (8) compile empty lambda -----------------------------------------

(ert-deftest nelisp-cc-arm64-compile-empty-lambda ()
  "`(lambda () nil)' lowers to MOVZ X0, #0 followed by RET.
SSA shape: entry block has [const(nil) -> v0] then [return v0].
The const is 16 bits (#0) so we hit the small-imm path.  v0 is
allocated x0 via the first virtual register r0 → x0 default mapping,
so the return sequence emits no extra MOV (already in X0)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda () nil)))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile fn alloc)))
    ;; 8 bytes: MOVZ X0,#0 (4) + RET (4)
    (should (= 8 (length bytes)))
    ;; MOVZ X0, #0 = 0xD2800000 little-endian = 00 00 80 d2
    (should (= #x00 (aref bytes 0)))
    (should (= #x00 (aref bytes 1)))
    (should (= #x80 (aref bytes 2)))
    (should (= #xD2 (aref bytes 3)))
    ;; RET = c0 03 5f d6
    (should (= #xC0 (aref bytes 4)))
    (should (= #x03 (aref bytes 5)))
    (should (= #x5F (aref bytes 6)))
    (should (= #xD6 (aref bytes 7)))))

;;; (9) compile identity lambda ---------------------------------------

(ert-deftest nelisp-cc-arm64-compile-identity-lambda ()
  "`(lambda (x) x)' should compile to a sequence ending in RET.
The exact MOV preamble depends on which register the linear-scan
allocator hands the parameter (it is the first SSA value, so r0=x0
in the default pool).  AAPCS64 already places arg #1 in X0, so the
emitted code degenerates to a single RET (no MOV needed)."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x) x)))
         (alloc (nelisp-cc--linear-scan fn))
         (bytes (nelisp-cc-arm64-compile fn alloc)))
    ;; Last 4 bytes must be the RET sequence c0 03 5f d6.
    (should (>= (length bytes) 4))
    (let ((n (length bytes)))
      (should (= #xC0 (aref bytes (- n 4))))
      (should (= #x03 (aref bytes (- n 3))))
      (should (= #x5F (aref bytes (- n 2))))
      (should (= #xD6 (aref bytes (- n 1)))))))

;;; (10) compile arithmetic call (BL fixup) --------------------------

(ert-deftest nelisp-cc-arm64-compile-arithmetic-via-call-stub ()
  "`(lambda (x y) (+ x y))' lowers to a BL fixup + RET.
The skeleton emits a single BL with a placeholder displacement (the
callee resolution is Phase 7.5 wiring).  The fixup label is
`callee:+'.  After finalize the BL word is whatever
`-encode-bl' produces for a *missing-binding* fallback — but
because no `callee:+' label is bound in this test, we expect
`finalize' to signal `nelisp-cc-arm64-error :unbound-label'.

This test exercises the fail-fast contract: emitting an unresolved
call without binding the callee label is a programmer error, not a
silent miscompile."
  (let* ((fn (nelisp-cc-build-ssa-from-ast '(lambda (x y) (+ x y))))
         (alloc (nelisp-cc--linear-scan fn))
         (err (should-error (nelisp-cc-arm64-compile fn alloc)
                            :type 'nelisp-cc-arm64-error)))
    ;; Error data should be (:unbound-label callee:+).
    (should (eq :unbound-label (car (cdr err))))
    (should (eq 'callee:+ (cadr (cdr err))))))

(provide 'nelisp-cc-arm64-test)
;;; nelisp-cc-arm64-test.el ends here
