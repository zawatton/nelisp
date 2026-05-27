;;; add.nl --- nelisp-sys fixture: exported C-ABI integer function -*- lexical-binding: t; -*-
;; A minimal C-ABI function: add two i32 and return i32.
(sys:defun add ((a i32) (b i32)) i32
  (:abi c :export "nl_add" :alloc none :panic abort)
  (+ a b))
