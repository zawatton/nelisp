;;; nelisp-stdlib-list.el --- Sweep 9 G1 list operations  -*- lexical-binding: t; -*-

(defun nthcdr (n list)
  (if (= n 0) list
    (if (null list) nil
      (nthcdr (1- n) (cdr list)))))

(defun nth (n list)
  (car (nthcdr n list)))

(defun reverse (list)
  (let ((acc nil))
    (while list
      (setq acc (cons (car list) acc))
      (setq list (cdr list)))
    acc))

(defun nreverse (list)
  (reverse list))

;; nelisp-stdlib-list.el ends here
