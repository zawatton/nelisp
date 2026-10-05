;;; nelisp-r2-reference.el --- Lisp reference for existing native marker coercion  -*- lexical-binding: t; -*-
;; Minimal native surface clauses 1 and 5. No builtin surface is introduced.
(defun nelisp-r2-reference-number (value)
  "Return VALUE or a marker's position, retaining GNU's detached-marker error."
  (if (markerp value)
      (or (marker-position value) (error "Marker does not point anywhere"))
    value))
(defun nelisp-r2-reference-arithmetic (operator arguments)
  "Call ordinary numeric OPERATOR after coercing marker ARGUMENTS."
  (apply operator
         (mapcar (lambda (value)
                   (let ((number (nelisp-r2-reference-number value)))
                     (unless (numberp number)
                       (signal 'wrong-type-argument (list 'number-or-marker-p value)))
                     number))
                 arguments)))
(provide 'nelisp-r2-reference)
(defun nelisp-r2-reference-compare (operator arguments)
  "Resolve only each visited comparison pair, preserving GNU's early return."
  (let ((result t))
    (while (and result (cdr arguments))
      (let ((left (nelisp-r2-reference-number (car arguments)))
            (right (nelisp-r2-reference-number (cadr arguments))))
        (setq result (funcall operator left right)))
      (setq arguments (cdr arguments)))
    result))
