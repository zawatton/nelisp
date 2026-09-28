;;; nelisp-internal-format-docstring-line-provider-test.el --- GNU helper stage -*- lexical-binding: t; -*-

(require 'ert)
(add-to-list 'load-path (expand-file-name "scripts" default-directory))
(require 'nelisp-standalone-build)

(ert-deftest nelisp-internal-format-docstring-line/exact-pinned-form ()
  (dolist (function '(internal--fill-string-single-line
                      internal--format-docstring-line))
    (let ((form (nelisp-vendor-source-form
                 "vendor/staged-emacs-lisp/subr.el" function)))
      (should (equal (string-trim form)
                     (string-trim
                      (nelisp-standalone--vendor-function-source
                       "vendor/staged-emacs-lisp/subr.el" function))))
      (should (string-match-p
               (format "^(defun %s\\_>" function) form)))))

(ert-deftest nelisp-internal-format-docstring-line/staged-before-easy-mmode ()
  (let* ((source (nelisp-standalone--load-path-src))
         (fill-form (nelisp-vendor-source-form
                     "vendor/staged-emacs-lisp/subr.el"
                     'internal--fill-string-single-line))
         (format-form (nelisp-vendor-source-form
                "vendor/staged-emacs-lisp/subr.el"
                'internal--format-docstring-line))
         (fill-position (string-match (regexp-quote fill-form) source))
         (format-position (string-match (regexp-quote format-form) source))
         (consumer (string-match "(require 'easy-mmode)" source)))
    (should fill-position)
    (should format-position)
    (should consumer)
    (should (< fill-position format-position))
    (should (< format-position consumer))
    (dolist (function '(internal--fill-string-single-line
                        internal--format-docstring-line))
      (should (= 1 (with-temp-buffer
                     (insert source)
                     (goto-char (point-min))
                     (how-many
                      (regexp-quote (format "(defun %s" function)))))))))

(provide 'nelisp-internal-format-docstring-line-provider-test)

;;; nelisp-internal-format-docstring-line-provider-test.el ends here
