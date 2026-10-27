;;; -*- lexical-binding: t; -*-
;; Shared corpus helpers: results are encoded without the printer under test (character codes and flags).
(defvar b2-n 0)
(defvar b2-flags nil "Non-nil: also encode whether a string is multibyte.")
(defun b2-enc (r)
  (cond ((stringp r) (format "S%s[%s]" (if b2-flags (if (multibyte-string-p r) "m" "u") "")
                             (mapconcat (lambda (c) (number-to-string c)) (append r nil) ",")))
        ((integerp r) (format "I%d" r))
        ((null r) "nil") ((eq r t) "t")
        ((symbolp r) (let ((b2-flags nil)) (concat "Y" (b2-enc (symbol-name r)))))
        ((consp r) (concat "(" (mapconcat #'b2-enc r " ") ")"))
        (t "?")))
(defmacro b2-c (form)
  `(progn (setq b2-n (1+ b2-n))
          (princ (format "%d %s\n" b2-n
                         (b2-enc (condition-case e ,form (error (list 'ERR (car e) (if (stringp (cadr e)) (cadr e)))))))) nil))
(defmacro b2-l (label form)
  "Like `b2-c' with a LABEL (a list of strings/nil encoded into the row) for readable diffs."
  `(progn (setq b2-n (1+ b2-n))
          (princ (format "%d %s => %s\n" b2-n (b2-enc ,label)
                         (b2-enc (condition-case e ,form (error (list 'ERR (car e) (if (stringp (cadr e)) (cadr e))))))))
          nil))
