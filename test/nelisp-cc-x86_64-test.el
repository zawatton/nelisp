;;; nelisp-cc-x86_64-test.el --- ERT for nelisp-cc-x86_64 Phase 7.1.2 skeleton  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Bytes-level golden tests for the Phase 7.1.2 x86_64 backend skeleton
;; in `src/nelisp-cc-x86_64.el'.  Doc 28 §5 4-test class "backend
;; encoding golden" specifies table-driven, fixed-IR / fixed-bytes
;; assertions; the skeleton lands the substrate so the follow-up agent
;; can extend the table without reorganising the harness.
;;
;; Coverage (8 tests):
;;
;;   Encoding (1-3):
;;     1. MOV rax, rdi    → 0x48 0x89 0xF8
;;     2. RET             → 0xC3
;;     3. MOV rax, 42     → 0x48 0xC7 0xC0 0x2A 0x00 0x00 0x00
;;
;;   Buffer + label resolution (4):
;;     4. forward JMP rel32 fixup resolves to the correct displacement
;;
;;   ABI table (5):
;;     5. virtual r0..r7 map to rdi/rsi/rdx/rcx/r8/r9/r10/r11
;;
;;   SSA → bytes round-trip (6-8):
;;     6. (lambda () nil)             → MOV rax, 0; RET prefix present
;;     7. (lambda (x) x)              → MOV rax, rdi; RET prefix present
;;        (the AST→SSA frontend lowers the body's bare-symbol reference
;;        to a variable load that the allocator binds to the parameter
;;        register)
;;     8. (lambda (x y) (+ x y))      → :call meta carries :unresolved t
;;        and is captured by `compile-with-meta' as a CALL fixup against
;;        the symbol `+'

;;; Code:

(require 'ert)
(require 'nelisp-cc)
(require 'nelisp-cc-x86_64)

;;; Helpers -----------------------------------------------------------

(defun nelisp-cc-x86_64-test--bytes (lst)
  "Convert LST (a list of integers) to a unibyte vector for comparison."
  (apply #'vector lst))

(defun nelisp-cc-x86_64-test--vec-prefix-p (vec prefix)
  "Return non-nil when VEC begins with the byte sequence PREFIX (a list)."
  (and (>= (length vec) (length prefix))
       (cl-loop for i from 0 below (length prefix)
                for want in prefix
                always (= (aref vec i) want))))

(defun nelisp-cc-x86_64-test--vec-contains-subseq-p (vec subseq)
  "Return non-nil when VEC contains the contiguous byte sequence SUBSEQ."
  (let ((n (length vec))
        (m (length subseq))
        (found nil))
    (cl-loop for start from 0 to (- n m)
             when (cl-loop for j from 0 below m
                           always (= (aref vec (+ start j))
                                     (nth j subseq)))
             do (setq found t) (cl-return))
    found))

;;; (1) MOV rax, rdi -------------------------------------------------

(ert-deftest nelisp-cc-x86_64-emit-mov-reg-reg-bytes-correct ()
  "MOV rax, rdi encodes to REX.W + 0x89 + ModR/M(11, rdi=7, rax=0).

Expected: 0x48 0x89 0xF8.

Walkthrough:
  REX.W only            = 0x48
  opcode (MOV r/m64,r64) = 0x89
  ModR/M(mod=11, reg=7=rdi, rm=0=rax)
    = (3<<6) | (7<<3) | 0
    = 0xF8"
  (let ((bs (nelisp-cc-x86_64--emit-mov-reg-reg 'rax 'rdi)))
    (should (equal bs (list #x48 #x89 #xF8))))
  ;; Also check a high-register variant: MOV r8, r9
  ;;   REX.W=1, REX.R=1 (r9), REX.B=1 (r8) → 0x4D
  ;;   opcode 0x89
  ;;   ModR/M(mod=11, reg=r9.low3=1, rm=r8.low3=0) = 0xC8
  (let ((bs (nelisp-cc-x86_64--emit-mov-reg-reg 'r8 'r9)))
    (should (equal bs (list #x4D #x89 #xC8)))))

;;; (2) RET → 0xC3 ---------------------------------------------------

(ert-deftest nelisp-cc-x86_64-emit-ret-byte-c3 ()
  "Near RET is the single byte 0xC3."
  (should (equal (nelisp-cc-x86_64--emit-ret) (list #xC3))))

;;; (3) MOV rax, 42 --------------------------------------------------

(ert-deftest nelisp-cc-x86_64-emit-mov-reg-imm32-encoding ()
  "MOV rax, 42 encodes to REX.W + 0xC7 + ModR/M(11, /0, rax) + imm32.

Expected: 0x48 0xC7 0xC0 0x2A 0x00 0x00 0x00.

Walkthrough:
  REX.W only            = 0x48
  opcode (MOV r/m64,imm32) = 0xC7
  ModR/M(mod=11, reg=0 (the /0 sub-opcode), rm=0=rax) = 0xC0
  imm32 little-endian: 42 = 0x2A → 0x2A 0x00 0x00 0x00"
  (let ((bs (nelisp-cc-x86_64--emit-mov-reg-imm32 'rax 42)))
    (should (equal bs (list #x48 #xC7 #xC0 #x2A #x00 #x00 #x00))))
  ;; Negative immediate sign-extends correctly.  -1 → 0xFFFFFFFF
  ;; little-endian = 0xFF 0xFF 0xFF 0xFF.
  (let ((bs (nelisp-cc-x86_64--emit-mov-reg-imm32 'rax -1)))
    (should (equal bs (list #x48 #xC7 #xC0 #xFF #xFF #xFF #xFF))))
  ;; Out-of-range imm raises encoding error.
  (should-error
   (nelisp-cc-x86_64--emit-mov-reg-imm32 'rax (ash 1 33))
   :type 'nelisp-cc-x86_64-encoding-error))

;;; (4) buffer + label fixup -----------------------------------------

(ert-deftest nelisp-cc-x86_64-buffer-define-and-resolve-label ()
  "A forward JMP fixup against a later-defined label resolves to the
correct rel32 displacement at finalize time.

Layout:
  offset 0: JMP rel32 (5 bytes, opcode 0xE9 + 4-byte placeholder)
  offset 5: NOP padding (1 byte 0x90)
  offset 6: target label `L_after'

JMP rel32 is measured from the byte *after* the displacement field
(i.e. from offset 5).  Distance from 5 → 6 = 1, so the patched
displacement should be 1 little-endian = 0x01 0x00 0x00 0x00."
  (let* ((buf (nelisp-cc-x86_64--buffer-make))
         ;; Emit JMP rel32 with a fixup against `L_after'.  rel32 is at
         ;; byte offset 1 within the 5-byte JMP instruction.
         (_ (nelisp-cc-x86_64--buffer-emit-fixup
             buf
             (nelisp-cc-x86_64--emit-jmp-rel32 0)
             'L_after
             1))
         ;; NOP (single-byte 0x90) so the label sits at offset 6.
         (_ (nelisp-cc-x86_64--buffer-emit-byte buf #x90))
         (_ (nelisp-cc-x86_64--buffer-define-label buf 'L_after))
         (vec (nelisp-cc-x86_64--buffer-finalize buf)))
    ;; 6-byte buffer: [0xE9 0x01 0x00 0x00 0x00 0x90]
    (should (= 6 (length vec)))
    (should (= #xE9 (aref vec 0)))
    (should (= #x01 (aref vec 1)))
    (should (= #x00 (aref vec 2)))
    (should (= #x00 (aref vec 3)))
    (should (= #x00 (aref vec 4)))
    (should (= #x90 (aref vec 5))))
  ;; Negative case: undefined label triggers
  ;; `nelisp-cc-x86_64-unresolved-label' at finalize.
  (let ((buf (nelisp-cc-x86_64--buffer-make)))
    (nelisp-cc-x86_64--buffer-emit-fixup
     buf (nelisp-cc-x86_64--emit-jmp-rel32 0) 'L_missing 1)
    (should-error
     (nelisp-cc-x86_64--buffer-finalize buf)
     :type 'nelisp-cc-x86_64-unresolved-label)))

;;; (5) virtual → physical mapping ----------------------------------

(ert-deftest nelisp-cc-x86_64-virtual-to-physical-default-mapping ()
  "T4 linear-scan virtual registers r0..r7 map to rdi/rsi/rdx/rcx/r8-r11.

Rationale: r0..r5 alias the six System V argument registers in
positional order so a 6-arg function never needs a register move on
function entry.  r6/r7 spill onto the two ABI-scratch registers
(r10/r11)."
  (should (eq 'rdi (nelisp-cc-x86_64-resolve-virtual 'r0)))
  (should (eq 'rsi (nelisp-cc-x86_64-resolve-virtual 'r1)))
  (should (eq 'rdx (nelisp-cc-x86_64-resolve-virtual 'r2)))
  (should (eq 'rcx (nelisp-cc-x86_64-resolve-virtual 'r3)))
  (should (eq 'r8  (nelisp-cc-x86_64-resolve-virtual 'r4)))
  (should (eq 'r9  (nelisp-cc-x86_64-resolve-virtual 'r5)))
  (should (eq 'r10 (nelisp-cc-x86_64-resolve-virtual 'r6)))
  (should (eq 'r11 (nelisp-cc-x86_64-resolve-virtual 'r7)))
  ;; ABI tables stay in sync.
  (should (= 6 (length nelisp-cc-x86_64--int-arg-regs)))
  (should (eq 'rdi (car nelisp-cc-x86_64--int-arg-regs)))
  (should (eq 'rax nelisp-cc-x86_64--return-reg))
  ;; Unknown virtual reg signals.
  (should-error
   (nelisp-cc-x86_64-resolve-virtual 'r99)
   :type 'nelisp-cc-x86_64-encoding-error))

;;; (6) (lambda () nil) → MOV r?, 0; ...; RET ------------------------

(ert-deftest nelisp-cc-x86_64-compile-empty-lambda-returns-rax-zero ()
  "Compiling `(lambda () nil)' produces bytes that include the literal
0 load (`MOV r?, 0`) and terminate with RET 0xC3.

The exact destination register depends on the linear-scan allocator's
choice for the single SSA value (the const-nil result).  In the
default 8-register pool this is r0 → rdi, so the SSA emit sequence
is:

  const     v0 = nil    → MOV rdi, 0          ; 0x48 0xC7 0xC7 0x00 0x00 0x00 0x00
  return v0             → MOV rax, rdi; RET   ; 0x48 0x89 0xF8 0xC3

The test asserts on the RET terminator and on the existence of *some*
MOV r?, 0 sequence with imm32=0 — exact destination register is left
to the allocator and may shift in future."
  (let* ((fn       (nelisp-cc-build-ssa-from-ast '(lambda () nil)))
         (alloc    (nelisp-cc--linear-scan fn))
         (vec      (nelisp-cc-x86_64-compile fn alloc)))
    ;; RET as the last byte.
    (should (> (length vec) 0))
    (should (= #xC3 (aref vec (1- (length vec)))))
    ;; Some MOV r?, 0 imm32=0 sequence appears in the prefix —
    ;; specifically a `0xC7 0xC?` followed by 4 zero bytes after a
    ;; REX.W byte.  We assert the simpler invariant: there is a 4-byte
    ;; run of 0x00 somewhere in vec (the imm32 part of MOV ?, 0).
    (let ((found nil))
      (cl-loop for i from 0 to (- (length vec) 4)
               when (and (= 0 (aref vec i))
                         (= 0 (aref vec (+ i 1)))
                         (= 0 (aref vec (+ i 2)))
                         (= 0 (aref vec (+ i 3))))
               do (setq found t) (cl-return))
      (should found))))

;;; (7) (lambda (x) x) → MOV rax, rdi; RET ---------------------------

(ert-deftest nelisp-cc-x86_64-compile-identity-lambda ()
  "Compiling `(lambda (x) x)' produces bytes containing the MOV rax, rdi
3-byte sequence (0x48 0x89 0xF8) followed eventually by RET (0xC3).

The frontend lowers the body's bare-symbol reference to the param
SSA value directly (scope binding hits — no `:load-var' is emitted).
The allocator gives the param value virtual register r0 = rdi.  The
return therefore moves rdi → rax and emits RET."
  (let* ((fn       (nelisp-cc-build-ssa-from-ast '(lambda (x) x)))
         (alloc    (nelisp-cc--linear-scan fn))
         (vec      (nelisp-cc-x86_64-compile fn alloc)))
    ;; MOV rax, rdi must appear somewhere.
    (should (nelisp-cc-x86_64-test--vec-contains-subseq-p
             vec (list #x48 #x89 #xF8)))
    ;; Last byte is RET.
    (should (= #xC3 (aref vec (1- (length vec)))))))

;;; (8) (+ x y) call site → unresolved fixup -------------------------

(ert-deftest nelisp-cc-x86_64-compile-arithmetic-via-call-stub ()
  "Compiling `(lambda (x y) (+ x y))' produces a CALL rel32 with a
placeholder 0 displacement and records a CALL-FIXUPS entry against the
symbol `+'.

Phase 7.5 will consume the fixup list to patch the rel32 against the
actual primitive address (resolved via `nelisp-defs-index').  The
skeleton stops at fixup recording and asserts the entry is shaped
correctly."
  (let* ((fn         (nelisp-cc-build-ssa-from-ast
                      '(lambda (x y) (+ x y))))
         (alloc      (nelisp-cc--linear-scan fn))
         (result     (nelisp-cc-x86_64-compile-with-meta fn alloc))
         (vec        (car result))
         (call-fixs  (cdr result)))
    ;; CALL opcode 0xE8 must appear somewhere in vec.
    (let ((seen-call nil))
      (cl-loop for i from 0 below (length vec)
               when (= (aref vec i) #xE8)
               do (setq seen-call t) (cl-return))
      (should seen-call))
    ;; The fixup list must contain at least one entry against `+'.
    (should (cl-some (lambda (cell) (eq (cdr cell) '+))
                     call-fixs))
    ;; Every fixup offset must be inside vec and the 4-byte rel32 there
    ;; must currently be all zeros (the placeholder).
    (dolist (cell call-fixs)
      (let ((off (car cell)))
        (should (and (<= 0 off) (<= (+ off 4) (length vec))))
        (should (= 0 (aref vec off)))
        (should (= 0 (aref vec (+ off 1))))
        (should (= 0 (aref vec (+ off 2))))
        (should (= 0 (aref vec (+ off 3))))))
    ;; RET still terminates the function.
    (should (= #xC3 (aref vec (1- (length vec)))))))

(provide 'nelisp-cc-x86_64-test)
;;; nelisp-cc-x86_64-test.el ends here
