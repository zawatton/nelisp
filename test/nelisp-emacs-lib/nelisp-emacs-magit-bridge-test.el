;;; nelisp-emacs-magit-bridge-test.el --- Tests for Magit bridge shims -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name "../src/nelisp-emacs-magit-bridge.el"
                        (file-name-directory (or load-file-name buffer-file-name)))
      nil 'no-message t t)

(ert-deftest nelisp-emacs-magit-bridge-test/symbol-function-accepts-function-form ()
  (let ((orig (symbol-function 'symbol-function))
        (saved-wrap nelisp-emacs-magit-bridge--symbol-function-orig))
    (unwind-protect
        (progn
          (setq nelisp-emacs-magit-bridge--symbol-function-orig nil)
          (nelisp-emacs-magit-bridge--ensure-symbol-function-function-form)
          (should (eq (symbol-function #'car)
                      (funcall nelisp-emacs-magit-bridge--symbol-function-orig 'car))))
      (fset 'symbol-function orig)
      (setq nelisp-emacs-magit-bridge--symbol-function-orig saved-wrap))))

(provide 'nelisp-emacs-magit-bridge-test)

;;; nelisp-emacs-magit-bridge-test.el ends here
