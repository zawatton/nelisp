;;; nelisp-stdlib-hof.el --- Sweep 9 G2 higher-order functions  -*- lexical-binding: t; -*-

(defun mapcar (fn list)
  (let ((acc nil))
    (while list
      (setq acc (cons (funcall fn (car list)) acc))
      (setq list (cdr list)))
    (let ((out nil))
      (while acc
        (setq out (cons (car acc) out))
        (setq acc (cdr acc)))
      out)))

(defun mapc (fn list)
  (let ((orig list))
    (while list
      (funcall fn (car list))
      (setq list (cdr list)))
    orig))

;; nelisp-stdlib-hof.el ends here
