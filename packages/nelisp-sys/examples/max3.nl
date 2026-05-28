;;; max3.nl --- nelisp-sys example: maximum of three integers -*- lexical-binding: t; -*-
;; Returns the maximum of three i32 values using if and cond.
;; Exported as nl_max3(a,b,c) with C ABI.
(sys:defun max3 ((a i32) (b i32) (c i32)) i32
  (:abi c :export "nl_max3" :alloc none :panic abort)
  (cond
    ((and (>= a b) (>= a c)) a)
    ((>= b c) b)
    (else c)))
