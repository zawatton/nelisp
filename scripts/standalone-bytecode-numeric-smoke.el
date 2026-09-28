;;; standalone-bytecode-numeric-smoke.el --- Numeric byte-code smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(defvar bytecode-numeric-smoke--n 0)
(defvar bytecode-numeric-smoke--bad 0)

(defmacro bytecode-numeric-smoke--check (label form)
  `(progn
     (setq bytecode-numeric-smoke--n (1+ bytecode-numeric-smoke--n))
     (unless ,form
       (setq bytecode-numeric-smoke--bad (1+ bytecode-numeric-smoke--bad))
       (princ (format "FAIL %s\n" ,label)))))

(defun bytecode-numeric-smoke--run (opcode value)
  (funcall (make-byte-code 257 (unibyte-string opcode 135) [] 2) value))

(bytecode-numeric-smoke--check "ADD1 fixnum" (= (bytecode-numeric-smoke--run 84 17) 18))
(bytecode-numeric-smoke--check "SUB1 fixnum" (= (bytecode-numeric-smoke--run 83 -17) -18))
(bytecode-numeric-smoke--check "ADD1 float" (= (bytecode-numeric-smoke--run 84 2.5) 3.5))
(bytecode-numeric-smoke--check "SUB1 float" (= (bytecode-numeric-smoke--run 83 2.5) 1.5))
(bytecode-numeric-smoke--check "ADD1 bignum" (= (bytecode-numeric-smoke--run 84 1267650600228229401496703205376)
                                                   1267650600228229401496703205377))
(bytecode-numeric-smoke--check "SUB1 bignum" (= (bytecode-numeric-smoke--run 83 1267650600228229401496703205376)
                                                   1267650600228229401496703205375))
(bytecode-numeric-smoke--check "ADD1 positive fixnum edge promotes"
                                (= (bytecode-numeric-smoke--run 84 most-positive-fixnum)
                                   2305843009213693952))
(bytecode-numeric-smoke--check "SUB1 negative fixnum edge promotes"
                                (= (bytecode-numeric-smoke--run 83 most-negative-fixnum)
                                   -2305843009213693953))

(princ (format "BYTECODE-NUMERIC-SMOKE cases=%d mismatches=%d\n"
               bytecode-numeric-smoke--n bytecode-numeric-smoke--bad))
(when (> bytecode-numeric-smoke--bad 0)
  (error "BYTECODE-NUMERIC-SMOKE failed: %d mismatch(es)"
         bytecode-numeric-smoke--bad))
nil
;;; standalone-bytecode-numeric-smoke.el ends here
