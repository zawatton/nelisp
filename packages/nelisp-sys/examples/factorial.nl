;;; factorial.nl --- nelisp-sys example: iterative factorial -*- lexical-binding: t; -*-
;; Computes n! iteratively using let + while + set!.
;; Inits result and i from n so the Phase 47 backend allocates mutable
;; frame slots (constant-init bindings get folded into env and cannot be
;; updated by set!).
;; Exported as nl_fact(n) with C ABI.
(sys:defun fact ((n i64)) i64
  (:abi c :export "nl_fact" :alloc none :panic abort)
  (let ((result i64 n)
        (i i64 n))
    (set! result 1)
    (set! i 1)
    (while (<= i n)
      (set! result (* result i))
      (set! i (+ i 1)))
    result))
