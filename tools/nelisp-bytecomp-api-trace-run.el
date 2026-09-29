;;; nelisp-bytecomp-api-trace-run.el --- run traced S6 byte-compile-form corpus -*- lexical-binding: t; -*-
(let* ((root (or (getenv "NELISP_BYTECOMP_ROOT") default-directory))
       (vendor (expand-file-name "vendor/emacs-lisp/emacs-lisp" root))
       (fixture (expand-file-name "test/fixtures/s6-corpus" root)))
  (add-to-list 'load-path vendor)
  ;; Match the VM source phase: load bytecomp, then the S6.10 wrapper and its
  ;; two corpus argument lists, and call the wrapper by symbol.
  (require 'bytecomp)
  (load (expand-file-name "byte-compile-form.wrapper.el" fixture) nil t)
  (with-temp-buffer
    (insert-file-contents (expand-file-name "byte-compile-form.el" fixture))
    (goto-char (point-min))
    (dolist (args (read (current-buffer)))
      (funcall (quote s6-corpus--byte-compile-form) (car args) (cadr args))))
  (nelisp-bytecomp-api-trace-write))
;;; nelisp-bytecomp-api-trace-run.el ends here
