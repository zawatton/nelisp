;;; nested-locals-probe.el --- Lexical environment locals -*- lexical-binding: t; -*-
(defun gnl-row (name thunk)
  (princ (format "%s %S\n" name (funcall thunk))))
(gnl-row 'two
  (lambda ()
    (let ((outer 8))
      (let ((parameter 7))
        (backtrace--locals 1 'backtrace--locals)))))
(gnl-row 'three
  (lambda ()
    (let ((outer 8))
      (let ((middle 9))
        (let ((parameter 7))
          (backtrace--locals 1 'backtrace--locals))))))
(gnl-row 'shadow
  (lambda ()
    (let ((outer 8))
      (let ((outer 9))
        (let ((parameter 7))
          (backtrace--locals 1 'backtrace--locals))))))
(kill-emacs 0)
