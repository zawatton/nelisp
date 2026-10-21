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

;;; -*- lexical-binding: t; -*-
(defvar btcd-special 'root)
(defun btcd-inner ()
  (let ((btcd-special 'younger))
    (backtrace--locals 1 'btcd-inner)))
(let ((outer 8))
  (princ (format "dynamic-only %S\n"
                 (let ((btcd-special nil) (btcd-special t)) (btcd-inner))))
  (princ (format "dynamic-declaration %S\n"
                 (let ((btcd-special nil) (btcd-special t))
                   (defvar btcd-unbound)
                   (btcd-inner)))))
(let ((outer 8))
  (princ (format "empty-direct %S\n"
                 (let () (backtrace--locals 1 'backtrace--locals)))))
(let ((outer 8))
  (princ (format "declaration-direct %S\n"
                 (let ()
                   (defvar btcd-direct-unbound)
                   (backtrace--locals 1 'backtrace--locals)))))

(kill-emacs 0)
