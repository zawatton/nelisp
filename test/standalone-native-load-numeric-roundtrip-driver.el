;;; standalone-native-load-numeric-roundtrip-driver.el --- Exact numeric root copies -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-load)
(defun numeric-roundtrip-assert (value label)
  (unless value (error "numeric roundtrip: %s" label)))
(let* ((env (nelisp--native-env))
       (begin (nelisp-native-load--symbol-addr "nl_root_pin_begin_v2"))
       (reserve (nelisp-native-load--symbol-addr "nl_root_pin_reserve_v2"))
       (end (nelisp-native-load--symbol-addr "nl_root_pin_end_v2"))
       (ticket (ptr-call begin env 0 0 0 0 0))
       (frame (ptr-call reserve env ticket 0 0 0 0))
       (input (ptr-call reserve env ticket 0 0 0 0))
       (output (ptr-call reserve env ticket 0 0 0 0)))
  (numeric-roundtrip-assert (and (> ticket 0) (> frame 0) (> input 0) (> output 0)) "root allocation")
  (unwind-protect
      (progn
        (nelisp-native-load-box frame nil env frame)
        (dolist (value (list 9223372036854775808 -9223372036854775809
                            (+ (ash 1 200) 12345)))
          (numeric-roundtrip-assert (= (nelisp--native-pin-copy-v2 env ticket 1 value) input) "bignum boxing")
          (garbage-collect)
          (let ((actual (nelisp-native-load-unbox input env frame)))
            (numeric-roundtrip-assert (and (eq actual value) (= actual value)) "bignum identity/value")
            (numeric-roundtrip-assert (= (nelisp--native-pin-copy-v2 env ticket 2 actual) output) "bignum reboxing")
            (numeric-roundtrip-assert (eq (nelisp-native-load-unbox output env frame) value) "bignum second roundtrip")))
        ;; Explicit IEEE words avoid parsing or arithmetic canonicalizing NaNs.
        ;; Include both NaN signs/payloads, infinities, signed zeros, the smallest
        ;; subnormal and the largest finite double. Only the live float payload
        ;; is changed; no pointer or root authentication field is modified.
        (dolist (words '((0 0) (2147483648 0) (2146435072 0) (4293918720 0)
                         (2146959360 1) (4294443008 74565) (2146435072 1)
                         (0 1) (2146435071 4294967295)))
          (numeric-roundtrip-assert (= (nelisp--native-pin-copy-v2 env ticket 1 0.0) input) "float boxing")
          (ptr-write-u32 input 8 (cadr words))
          (ptr-write-u32 input 12 (car words))
          (garbage-collect)
          (let ((actual (nelisp-native-load-unbox input env frame)))
            (numeric-roundtrip-assert (floatp actual) "float result")
            (numeric-roundtrip-assert (= (nelisp--native-pin-copy-v2 env ticket 2 actual) output) "float reboxing")
            (numeric-roundtrip-assert
             (and (= (ptr-read-u32 output 8) (cadr words))
                  (= (ptr-read-u32 output 12) (car words))) "float exact sign/payload")))
        (dolist (request (list (list input (+ env 1) frame)
                               (list input env (+ frame 32))
                               (list (+ input 1) env frame)
                               (list (+ output 32) env frame)))
          (numeric-roundtrip-assert
           (condition-case nil (progn (apply #'nelisp--native-unbox-reference request) nil)
             (wrong-type-argument t)) "foreign/misaligned/unreserved root refused")))
    (numeric-roundtrip-assert (= (ptr-call end env ticket 0 0 0 0) 1) "root release"))
  (numeric-roundtrip-assert
   (condition-case nil (progn (nelisp--native-unbox-reference input env frame) nil)
     (wrong-type-argument t)) "released frame refused"))
(princ "NUMERIC-ROUNDTRIP-PASS bignums=3 ieee=9 refusals=5\n")
(exit)
