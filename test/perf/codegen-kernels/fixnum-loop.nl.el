;; fixnum-loop: sum and compare in a counted loop.
;; NW is a fixnum WORD ((N << 2) | 1); the result is the fixnum WORD of
;; the reduced sum.
(defun ck_kernel (nw)
  (let ((lim (sar nw 2))
        (s 0)
        (i 0))
    (while (< i lim)
      (setq s (+ s i))
      (if (>= s 1000003)
          (setq s (- s 1000003))
        0)
      (setq i (+ i 1)))
    (+ (shl s 2) 1)))

(defun ck_main (n r)
  (let ((res 0) (j 0))
    (while (< j r)
      (setq res (ck_kernel (+ (shl n 2) 1)))
      (setq j (+ j 1)))
    (sar res 2)))
