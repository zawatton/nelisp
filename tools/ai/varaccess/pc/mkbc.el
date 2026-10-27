;;; -*- lexical-binding: t; -*-
;; emacs -Q --batch -l mkbc.el  -> bc.el : the defuns of src.el byte-compiled by GNU 31.1
(setq byte-compile-warnings nil)
(let ((src (expand-file-name "src.el" default-directory)) forms)
  (with-temp-buffer (insert-file-contents src) (goto-char (point-min))
    (condition-case nil (while t (push (read (current-buffer)) forms)) (end-of-file nil)))
  (setq forms (nreverse forms))
  (with-temp-file "bc.el"
    (insert ";;; -*- lexical-binding: t; -*-\n")
    (let ((print-escape-newlines t) (print-escape-control-characters t) (print-length nil) (print-level nil) (print-circle nil))
      (dolist (f forms)
        (if (eq (car f) 'defun)
            (progn (eval f t) (byte-compile (cadr f))
                   (insert (format "(fset '%s %S)\n" (cadr f) (symbol-function (cadr f)))))
          (eval f t) (insert (format "%S\n" f)))))))
