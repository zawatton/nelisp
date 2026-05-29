;;; nelisp-sys-check-test.el --- ERT tests for nelisp-sys type checker -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Stage 131.2 gate: positive checks plus negative checks asserting on
;; stable diagnostic codes (E-SYS-TYPE-NNN).

;;; Code:

(require 'ert)
(require 'nelisp-sys-frontend)
(require 'nelisp-sys-check)

(defun nelisp-sys-check-test--code (forms)
  "Parse + type-check FORMS; return the first diagnostic code, or t if clean."
  (let ((diags (nelisp-sys-check-collect
                (nelisp-sys-frontend-parse-module forms))))
    (if diags (car (car diags)) t)))

;;; Positive.

(ert-deftest nelisp-sys-check-add-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun add ((a i32) (b i32)) i32
                     (:abi c :alloc none) (+ a b)))))))

(ert-deftest nelisp-sys-check-struct-distance-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defstruct point (:repr c) (x i32) (y i32))
                   (sys:defun distance2 ((p (ptr point))) i64
                     (:abi c :alloc none)
                     (let ((x i32 (sys:load-field p x))
                           (y i32 (sys:load-field p y)))
                       (+ (* (sys:cast i64 x) (sys:cast i64 x))
                          (* (sys:cast i64 y) (sys:cast i64 y))))))))))

(ert-deftest nelisp-sys-check-call-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun g ((a i32)) i32 () a)
                   (sys:defun f () i32 () (g 5)))))))

(ert-deftest nelisp-sys-check-control-flow-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun f ((a i32) (b i32)) i32 ()
                     (if (< a b) a b)))))))

(ert-deftest nelisp-sys-check-slice-and-ptr-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun len ((s (slice u8))) usize () (sys:slice-len s))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun deref ((p (ptr i32))) i32 () (sys:load p)))))))

(ert-deftest nelisp-sys-check-while-and-set-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun count ((n i32)) void ()
                     (let ((i i32 0))
                       (while (< i n) (set! i (+ i 1)))))))))) ; body void

;;; Negative — each asserts a specific stable code.

(ert-deftest nelisp-sys-check-unknown-variable ()
  (should (eq 'E-SYS-TYPE-001
              (nelisp-sys-check-test--code
               '((sys:defun f () i32 () x))))))

(ert-deftest nelisp-sys-check-let-type-mismatch ()
  (should (eq 'E-SYS-TYPE-002
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i64)) void () (let ((x i32 a)) (seq))))))))

(ert-deftest nelisp-sys-check-unknown-type ()
  (should (eq 'E-SYS-TYPE-003
              (nelisp-sys-check-test--code
               '((sys:defun f ((a (ptr nope))) void () (seq)))))))

(ert-deftest nelisp-sys-check-unknown-field ()
  (should (eq 'E-SYS-TYPE-004
              (nelisp-sys-check-test--code
               '((sys:defstruct p (:repr c) (x i32))
                 (sys:defun f ((q (ptr p))) i32 () (sys:load-field q y)))))))

(ert-deftest nelisp-sys-check-arity-mismatch ()
  (should (eq 'E-SYS-TYPE-005
              (nelisp-sys-check-test--code
               '((sys:defun g ((a i32)) i32 () a)
                 (sys:defun f () i32 () (g 1 2)))))))

(ert-deftest nelisp-sys-check-unknown-function ()
  (should (eq 'E-SYS-TYPE-006
              (nelisp-sys-check-test--code
               '((sys:defun f () i32 () (nope 1)))))))

(ert-deftest nelisp-sys-check-field-on-non-struct ()
  (should (eq 'E-SYS-TYPE-007
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i32)) i32 () (sys:load-field a x)))))))

(ert-deftest nelisp-sys-check-return-mismatch ()
  (should (eq 'E-SYS-TYPE-011
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i64)) i32 () a)))))) ; returns i64, declared i32

(ert-deftest nelisp-sys-check-slice-on-non-slice ()
  (should (eq 'E-SYS-TYPE-012
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i32)) usize () (sys:slice-len a)))))))

(ert-deftest nelisp-sys-check-deref-non-pointer ()
  (should (eq 'E-SYS-TYPE-014
              (nelisp-sys-check-test--code
               '((sys:defun f ((a i32)) i32 () (sys:load a)))))))

(ert-deftest nelisp-sys-check-duplicate-function ()
  (should (eq 'E-SYS-TYPE-015
              (nelisp-sys-check-test--code
               '((sys:defun f () void () (seq))
                 (sys:defun f () void () (seq)))))))

;;; char-table / mutable-string / float surfaces (positive).

(ert-deftest nelisp-sys-check-char-table-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun ctg ((tbl usize) (idx i64) (out usize)) i64 ()
                     (sys:char-table-get tbl idx out))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun cts ((tbl usize) (idx i64) (val usize) (out usize))
                               i64 ()
                     (sys:char-table-set! tbl idx val out)))))))

(ert-deftest nelisp-sys-check-mut-str-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun mk ((slot usize)) usize ()
                     (sys:mut-str-make-empty slot 0))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun build ((p usize) (cp i64)) i64 ()
                     (sys:mut-str-push-byte p 65)
                     (sys:mut-str-push-codepoint p cp)
                     (sys:mut-str-len p)))))))

(ert-deftest nelisp-sys-check-f64-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun fa ((a f64) (b f64)) f64 () (sys:f64+ a b))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun lt ((a f64) (b f64)) bool () (sys:f64< a b))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun i2f ((x i64)) f64 () (sys:i64->f64 x))))))
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun f2i ((x f64)) i64 () (sys:f64->i64 x)))))))

(ert-deftest nelisp-sys-check-str-to-float-clean ()
  (should (eq t (nelisp-sys-check-test--code
                 '((sys:defun s2f ((bytes usize) (len usize) (out usize)) i64 ()
                     (sys:str-to-float bytes len out)))))))

;;; Negative — float discipline + address typing.

(ert-deftest nelisp-sys-check-f64-arith-needs-float ()
  ;; sys:f64+ on integer operands is a float-typing error (E-SYS-TYPE-016).
  (should (eq 'E-SYS-TYPE-016
              (nelisp-sys-check-test--code
               '((sys:defun bad ((a i64) (b i64)) f64 () (sys:f64+ a b)))))))

(ert-deftest nelisp-sys-check-char-table-needs-int-addr ()
  ;; a float where a raw address is expected -> E-SYS-TYPE-013.
  (should (eq 'E-SYS-TYPE-013
              (nelisp-sys-check-test--code
               '((sys:defun bad ((tbl f64) (idx i64) (out usize)) i64 ()
                   (sys:char-table-get tbl idx out)))))))

(ert-deftest nelisp-sys-check-plain-arith-rejects-float ()
  ;; `+'/`<' on floats would silently lower to integer ops; the checker
  ;; routes them to the sys:f64 family with E-SYS-TYPE-008.
  (should (eq 'E-SYS-TYPE-008
              (nelisp-sys-check-test--code
               '((sys:defun bad ((a f64) (b f64)) f64 () (+ a b))))))
  (should (eq 'E-SYS-TYPE-008
              (nelisp-sys-check-test--code
               '((sys:defun bad ((a f64) (b f64)) bool () (< a b)))))))

;;; nelisp-sys-check-test.el ends here
