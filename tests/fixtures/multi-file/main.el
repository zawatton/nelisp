;;; main.el --- multi-file load chain entry point  -*- lexical-binding: t; -*-

(require 'helpers)

(defun main-compute (n)
  (helpers-add
   (helpers-square (helpers-incr n))
   (helpers-square n)))

;; (main-compute 3) = (square 4) + (square 3) = 16 + 9 = 25
(main-compute 3)

;;; main.el ends here
