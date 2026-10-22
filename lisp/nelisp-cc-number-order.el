;;; nelisp-cc-number-order.el --- Exact tagged numeric ordering -*- lexical-binding: t -*-

;; SPDX-License-Identifier: GPL-3.0-or-later
;; Implementation of existing numeric primitives, not a new Lisp entry.
;; Native surface clauses 1/5: IEEE payload and integer limb access. Accept
;; WORDs including boxed integers; return -1/0/1, or 2 for unordered NaNs.
(defconst nelisp-cc-number-order--source
  '(seq
    (defun nl_number_integer_limb (word index)
      (if (= (nl_val_tag word) 13)
          (heap-read-u32 (nl_big_limbs word) (* index 4))
        (if (>= index 2) 0
          (let* ((v (nl_word_payload word)) (mag (if (< v 0) (- 0 v) v)))
            (logand (shr mag (* index 32)) 4294967295)))))
    (defun nl_number_integer_float_order (integer floatword)
      ;; Compare the exact significand and exponent, never round the integer
      ;; through f64. This also handles infinities, NaNs and subnormals.
      (let* ((tag (nl_val_tag integer))
             (v (if (= tag 13) (nl_big_sign integer) (nl_word_payload integer)))
             (negative (if (= tag 13) v (if (< v 0) 1 0)))
             (n (if (= tag 13) (nl_big_count integer) 2))
             (bits (nl_float_bits floatword))
             (exp (logand (shr bits 52) 2047))
             (fraction (logand bits 4503599627370495))
             (fnegative (logand (shr bits 63) 1))
             (top 0) (width 0) (order 0))
        (seq
         (while (and (> n 0) (= (nl_number_integer_limb integer (- n 1)) 0)) (setq n (- n 1)))
         (if (= exp 2047)
             (if (/= fraction 0) 2 (if (= fnegative 1) 1 -1))
           (if (= n 0)
               (if (= (f64-lt (i64-to-f64 0) (bits-to-f64 bits)) 1) -1
                 (if (= (f64-gt (i64-to-f64 0) (bits-to-f64 bits)) 1) 1 0))
             (if (/= negative fnegative) (if (= negative 1) -1 1)
               (seq
                (setq top (nl_number_integer_limb integer (- n 1)))
                (while (> top 0) (setq width (+ width 1)) (setq top (shr top 1)))
                (setq width (+ width (* (- n 1) 32)))
                (setq exp (- exp 1023))
                (setq fraction (+ fraction 4503599627370496))
                (if (< width (+ exp 1)) (setq order -1)
                  (if (> width (+ exp 1)) (setq order 1)
                    (if (< exp 52)
                        (let* ((shift (- 52 exp)) (whole (shr fraction shift))
                               (mag (+ (nl_number_integer_limb integer 0)
                                       (shl (if (> n 1) (nl_number_integer_limb integer 1) 0) 32))))
                          (setq order (if (< mag whole) -1 (if (> mag whole) 1
                                        (if (/= (logand fraction (- (shl 1 shift) 1)) 0) -1 0)))))
                      (let* ((shift (- exp 52)) (i (- n 1)))
                        (while (and (= order 0) (>= i 0))
                          (let* ((offset (- (* i 32) shift))
                                 (flimb (if (>= offset 64) 0
                                          (if (>= offset 0) (logand (shr fraction offset) 4294967295)
                                            (if (<= offset -32) 0 (logand (shl fraction (- 0 offset)) 4294967295)))))
                                 (ilimb (nl_number_integer_limb integer i)))
                            (seq (setq order (if (< ilimb flimb) -1 (if (> ilimb flimb) 1 0)))
                                 (setq i (- i 1)))))))))
                (if (= negative 1) (- 0 order) order))))))))
    (defun nl_number_big_small_order (big small)
      (let* ((value (nl_word_payload small)) (negative (if (< value 0) 1 0))
             (bsign (nl_big_sign big)))
        (if (/= negative bsign) (if (= bsign 1) -1 1)
          (let* ((order (nl_bignum_mag_cmp_i64 (nl_big_limbs big) (nl_big_count big)
                                             (if (= negative 1) (- 0 value) value))))
            (if (= bsign 1) (- 0 order) order)))))
    (defun nl_number_order_words (a b)
      (let* ((ta (nl_val_tag a)) (tb (nl_val_tag b)))
        (cond
         ((= ta 3)
          (if (= tb 3)
              (let* ((fa (nl_float_bits a)) (fb (nl_float_bits b)))
                (if (= (f64-lt (bits-to-f64 fa) (bits-to-f64 fb)) 1) -1
                  (if (= (f64-gt (bits-to-f64 fa) (bits-to-f64 fb)) 1) 1
                    (if (= (f64-le (bits-to-f64 fa) (bits-to-f64 fb)) 1) 0 2))))
            (let* ((r (nl_number_integer_float_order b a))) (if (= r 2) 2 (- 0 r)))))
         ((= tb 3) (nl_number_integer_float_order a b))
         ((= ta 13) (if (= tb 13) (nl_bignum_cmp_bignum a b) (nl_number_big_small_order a b)))
         ((= tb 13) (- 0 (nl_number_big_small_order b a)))
         (t (let* ((va (nl_word_payload a)) (vb (nl_word_payload b)))
              (if (< va vb) -1 (if (> va vb) 1 0)))))))
))

(provide 'nelisp-cc-number-order)
;;; nelisp-cc-number-order.el ends here
