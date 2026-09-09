;;; emacs-parity-clmacros-test.el --- CL macro parity tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(setq load-prefer-newer t)
(require 'emacs-parity-clmacros)

(ert-deftest emacs-parity-clmacros-test/cl-set-difference-honors-test-keyword ()
  "The standalone compatibility implementation accepts CC Mode's `:test'."
  (let ((old (and (fboundp 'cl-set-difference)
                  (symbol-function 'cl-set-difference))))
    (unwind-protect
        (progn
          (fmakunbound 'cl-set-difference)
          (emacs-parity-clmacros--install-cl-set-difference)
          (should (equal '("C")
                         (cl-set-difference '("C" "c") '("c")
                                             :test #'string-equal)))
          (should (equal '(1 3)
                         (cl-set-difference '(1 2 3) '(2)
                                             :test #'=)))
          (should (equal '(1 3)
                         (cl-set-difference '(1 2 3) '(2)
                                             :test-not (lambda (a b)
                                                         (not (= a b))))))
          (should (equal '("a")
                         (cl-set-difference '("a" "B") '("b")
                                             :test #'string-equal
                                             :key #'downcase))))
      (if old
          (fset 'cl-set-difference old)
        (fmakunbound 'cl-set-difference)))))

(provide 'emacs-parity-clmacros-test)
;;; emacs-parity-clmacros-test.el ends here
