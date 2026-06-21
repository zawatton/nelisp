;;; nelisp-heap-image-test.el --- ERT tests for the heap-v0 image codec  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Round-trip tests for `nelisp-heap-image' (heap-v0 / heap-v0-bin).
;;
;; These run under host Emacs.  The codec is also exercised on the
;; standalone `target/nelisp' runtime by a manual harness; the standalone
;; run is what motivated two fixes covered here:
;;
;;  - signed i64 read/write must not assume bignum semantics
;;    (`(ash 1 64)' masks to 0 under a native-i64 runtime, which silently
;;    decremented every positive object id and immediate integer);
;;  - `bool-vector' objects are now encoded/decoded (guarded by `fboundp'
;;    so a runtime without bool-vectors stays unaffected).

;;; Code:

(require 'ert)
(require 'nelisp-heap-image)

(defun nelisp-heap-image-test--roundtrip (roots)
  "Encode ROOTS to a heap-v0-bin image and decode it back."
  (nelisp-heap-image-read-string
   (nelisp-heap-image-dump-string roots)))

(ert-deftest nelisp-heap-image-test-i64-roundtrip ()
  "Signed i64 must survive `--i64-le' -> `--binary-read-i64'."
  (dolist (n (list 0 1 42 255 256 65535 65536
                   (ash 1 40) (ash 1 62)
                   -1 -42 -256 (- (ash 1 40)) (- (ash 1 62))))
    (let ((pos (cons 0 nil)))
      (should (= n (nelisp-heap-image--binary-read-i64
                    (nelisp-heap-image--i64-le n) pos))))))

(ert-deftest nelisp-heap-image-test-immediate-roundtrip ()
  "Immediate scalars round-trip through the binary image."
  (let ((dec (nelisp-heap-image-test--roundtrip
              (list (cons "nil" nil) (cons "t" t)
                    (cons "i" 42) (cons "neg" -7)
                    (cons "s" "hello") (cons "sym" 'foo)))))
    (should (eq nil (cdr (assoc "nil" dec))))
    (should (eq t (cdr (assoc "t" dec))))
    (should (= 42 (cdr (assoc "i" dec))))
    (should (= -7 (cdr (assoc "neg" dec))))
    (should (string= "hello" (cdr (assoc "s" dec))))
    (should (eq 'foo (cdr (assoc "sym" dec))))))

(ert-deftest nelisp-heap-image-test-object-id-roundtrip ()
  "Allocated objects (cons/vector) keep their values through object ids."
  (let ((dec (nelisp-heap-image-test--roundtrip
              (list (cons "c" (list 1 2 3))
                    (cons "v" (vector 10 20 30))
                    (cons "nested" (cons (list 1) (vector 2)))))))
    (should (equal (list 1 2 3) (cdr (assoc "c" dec))))
    (should (equal (vector 10 20 30) (cdr (assoc "v" dec))))
    (should (equal (cons (list 1) (vector 2)) (cdr (assoc "nested" dec))))))

(ert-deftest nelisp-heap-image-test-shared-structure ()
  "Shared substructure is reconstructed as a single object."
  (let* ((shared (list 'x))
         (dec (nelisp-heap-image-test--roundtrip
               (list (cons "a" shared) (cons "b" shared))))
         (da (cdr (assoc "a" dec)))
         (db (cdr (assoc "b" dec))))
    (should (eq da db))))

(ert-deftest nelisp-heap-image-test-bool-vector-roundtrip ()
  "Bool-vectors round-trip bit-for-bit (host runtime has bool-vectors)."
  (skip-unless (fboundp 'make-bool-vector))
  (let* ((bv (make-bool-vector 10 nil)))
    (aset bv 0 t) (aset bv 3 t) (aset bv 7 t) (aset bv 9 t)
    (let ((dec (nelisp-heap-image-test--roundtrip
                (list (cons "bv" bv)
                      (cons "full" (make-bool-vector 8 t))
                      (cons "empty" (make-bool-vector 0 nil))
                      (cons "nested" (list bv 42))))))
      (should (equal bv (cdr (assoc "bv" dec))))
      (should (equal (make-bool-vector 8 t) (cdr (assoc "full" dec))))
      (should (equal (make-bool-vector 0 nil) (cdr (assoc "empty" dec))))
      (should (equal bv (car (cdr (assoc "nested" dec))))))))

(provide 'nelisp-heap-image-test)

;;; nelisp-heap-image-test.el ends here
