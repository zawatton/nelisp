;;; point.nl --- nelisp-sys example: struct pointer + field access -*- lexical-binding: t; -*-
;; Defines a C-repr struct `point' with unsigned fields x and y (u32, non-negative).
;; nl_point_sum reads x+y from a pointer to point via sys:load-field.
;; Exported as nl_point_sum(p) with C ABI.
(sys:defstruct point
  (:repr c)
  (x u32)
  (y u32))

(sys:defun point_sum ((p (ptr point))) u32
  (:abi c :export "nl_point_sum" :alloc none :panic abort)
  (let ((x u32 (sys:load-field p x))
        (y u32 (sys:load-field p y)))
    (+ x y)))
