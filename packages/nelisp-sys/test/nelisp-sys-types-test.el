;;; nelisp-sys-types-test.el --- ERT tests for nelisp-sys-types -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Type predicates, validity, struct env, and Copy/move classification.

;;; Code:

(require 'ert)
(require 'nelisp-sys-types)

(ert-deftest nelisp-sys-types-scalar-predicates ()
  (should (nelisp-sys-type-scalar-p 'i32))
  (should (nelisp-sys-type-integer-p 'u64))
  (should (nelisp-sys-type-float-p 'f64))
  (should (nelisp-sys-type-bool-p 'bool))
  (should (nelisp-sys-type-void-p 'void))
  (should-not (nelisp-sys-type-scalar-p 'point))
  (should-not (nelisp-sys-type-scalar-p '(ptr i32))))

(ert-deftest nelisp-sys-types-signedness ()
  (should (nelisp-sys-type-signed-p 'i32))
  (should-not (nelisp-sys-type-signed-p 'u32))
  (should (nelisp-sys-type-signed-p 'isize))
  (should-not (nelisp-sys-type-signed-p 'usize))
  (should-error (nelisp-sys-type-signed-p 'bool) :type 'nelisp-sys-type-error))

(ert-deftest nelisp-sys-types-compound-predicates ()
  (should (nelisp-sys-type-pointer-p '(ptr i32)))
  (should (nelisp-sys-type-array-p '(array i32 4)))
  (should (nelisp-sys-type-struct-ref-p '(struct point)))
  (should (nelisp-sys-type-slice-p '(slice u8)))
  (should (nelisp-sys-type-slice-mut-p '(slice-mut u8)))
  (should-not (nelisp-sys-type-slice-mut-p '(slice u8)))
  (should (nelisp-sys-type-owned-p '(owned fd)))
  (should (nelisp-sys-type-ref-p '(& i32)))
  (should (nelisp-sys-type-mut-ref-p '(&mut i32)))
  (should-not (nelisp-sys-type-mut-ref-p '(& i32)))
  (should (eq 'i32 (nelisp-sys-type-element '(ptr i32)))))

(ert-deftest nelisp-sys-types-validity ()
  (should (nelisp-sys-type-valid-p 'i32))
  (should (nelisp-sys-type-valid-p '(ptr void)))
  (should (nelisp-sys-type-valid-p '(array (ptr i8) 8)))
  (should (nelisp-sys-type-valid-p '(slice-mut u8)))
  (should-not (nelisp-sys-type-valid-p '(array i32 -1)))
  (should (nelisp-sys-type-valid-p 'point))        ; bare named type (no env)
  (should (nelisp-sys-type-valid-p '(ptr point)))
  (should-not (nelisp-sys-type-valid-p 42))
  (should-not (nelisp-sys-type-valid-p nil))
  ;; with an env, an unregistered named type is rejected
  (let ((env (nelisp-sys-types-env-make)))
    (nelisp-sys-types-env-add env 'point 'c '((x i32)))
    (should (nelisp-sys-type-valid-p 'point env))
    (should-not (nelisp-sys-type-valid-p 'nope env))
    (should-not (nelisp-sys-type-valid-p '(ptr nope) env))))

(ert-deftest nelisp-sys-types-struct-env ()
  (let ((env (nelisp-sys-types-env-make)))
    (nelisp-sys-types-env-add env 'point 'c '((x i32) (y i32)))
    (should (eq 'c (nelisp-sys-types-struct-repr env 'point)))
    (should (eq 'i32 (nelisp-sys-types-struct-field-type env 'point 'y)))
    (should (null (nelisp-sys-types-struct-field-type env 'point 'z)))
    (should-error (nelisp-sys-types-env-add env 'point 'c '((a i8)))
                  :type 'nelisp-sys-type-error)))

(ert-deftest nelisp-sys-types-copy-classification ()
  (let ((env (nelisp-sys-types-env-make)))
    (nelisp-sys-types-env-add env 'point 'c '((x i32) (y i32)))
    (nelisp-sys-types-env-add env 'res 'c '((buf (owned u8)) (n usize)))
    ;; Copy
    (should (nelisp-sys-type-copy-p 'i32 env))
    (should (nelisp-sys-type-copy-p 'bool env))
    (should (nelisp-sys-type-copy-p '(ptr i32) env))
    (should (nelisp-sys-type-copy-p '(& i32) env))
    (should (nelisp-sys-type-copy-p '(slice u8) env))
    (should (nelisp-sys-type-copy-p '(array i32 4) env))
    (should (nelisp-sys-type-copy-p '(struct point) env))
    ;; Move-only
    (should-not (nelisp-sys-type-copy-p '(owned u8) env))
    (should-not (nelisp-sys-type-copy-p '(&mut i32) env))
    (should-not (nelisp-sys-type-copy-p '(slice-mut u8) env))
    (should-not (nelisp-sys-type-copy-p '(struct res) env)) ; has owned field
    (should (nelisp-sys-type-move-only-p '(owned u8) env))))

(ert-deftest nelisp-sys-types-equality ()
  (should (nelisp-sys-type-equal 'i32 'i32))
  (should-not (nelisp-sys-type-equal 'i32 'u32))
  (should (nelisp-sys-type-equal '(ptr i32) '(ptr i32)))
  (should-not (nelisp-sys-type-equal '(ptr i32) '(ptr u32))))

;;; nelisp-sys-types-test.el ends here
