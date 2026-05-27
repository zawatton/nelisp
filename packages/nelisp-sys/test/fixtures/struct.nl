;;; struct.nl --- nelisp-sys fixture: C-repr struct + pointer field access -*- lexical-binding: t; -*-
(sys:defstruct point
  (:repr c)
  (x i32)
  (y i32))

;; distance squared via a pointer to a point (read-only field loads).
(sys:defun distance2 ((p (ptr point))) i64
  (:abi c :export "nl_distance2" :alloc none :panic abort)
  (let ((x i32 (sys:load-field p x))
        (y i32 (sys:load-field p y)))
    (+ (* (sys:cast i64 x) (sys:cast i64 x))
       (* (sys:cast i64 y) (sys:cast i64 y)))))
