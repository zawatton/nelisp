;;; -*- lexical-binding: t; -*-
;; fx shape variants (F396) VM coverage: after warming, a 400-marker shape of a body also warmed under (t) must stay far below the interpreted cost
;; the same order as the plain shape (ratio < 5: the interpreter call path walks the 400 markers per call, ratio ~3 on the pre-F391 build, ~10 when declined).  Prints t on GNU and on a build with variants (fx on); nil on a build that declines a second
;; shape to the interpreter (F391 (b)) and with NELISP_FX_DISABLE=1.  Timing row, not a parity row.
(defmacro row (n f) `(princ (format "%-26s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
;; VM coverage: after warming, the 400-marker shape B must cost about the same as shape A (P2-like, two shapes)
(defun fs-names (n) (let (l (i 0)) (dotimes (_ n) (push (intern (format "fs-v%d" (setq i (1+ i)))) l)) l))
(dolist (s (fs-names 400)) (eval `(defvar ,s 0) t))
(row "P3 two shapes ratio" (let* ((form '(lambda () (+ 1 (* 2 3) (car '(4)))))
                                  (a (eval form '(t))) (b (eval form (append (fs-names 400) '(t)))) ta tb)
                             (dotimes (_ 300) (funcall a)) (dotimes (_ 300) (funcall b))
                             (setq ta (float-time)) (dotimes (_ 20000) (funcall a)) (setq ta (- (float-time) ta) tb (float-time))
                             (dotimes (_ 20000) (funcall b)) (setq tb (- (float-time) tb))
                             (< (/ tb ta) 5.0)))
