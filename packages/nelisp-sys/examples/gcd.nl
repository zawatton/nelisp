;;; gcd.nl --- nelisp-sys example: Euclidean GCD via while + mod -*- lexical-binding: t; -*-
;; Computes gcd(a,b) using the Euclidean algorithm (a,b must be positive).
;; Uses (< 0 y) as the loop guard to avoid the not-of-= pattern which
;; triggers a AOT runtime helper; (>= y 1) is equivalent for positive inputs.
;; Inits tmp from a (non-constant) so AOT allocates a mutable frame slot.
;; Exported as nl_gcd(a,b) with C ABI.
(sys:defun gcd ((a i32) (b i32)) i32
  (:abi c :export "nl_gcd" :alloc none :panic abort)
  (let ((x i32 a)
        (y i32 b)
        (tmp i32 a))
    (set! tmp 0)
    (while (< 0 y)
      (set! tmp (mod x y))
      (set! x y)
      (set! y tmp))
    x))
