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

(ert-deftest emacs-parity-clmacros-test/cl-defmacro-key-default-and-supplied-p ()
  "Keyword defaults and supplied-p variables are bound in order."
  (let* ((args (make-symbol "args"))
         (bindings
          (emacs-parity-clmacros--macro-bindings
           '(&key (foo 1 foo-p) ((:bar bar) 2 bar-p)) args)))
    (should (equal '(1 nil 9 t)
                   (eval `(let ((,args '(:bar 9)))
                            (let* ,bindings
                              (list foo foo-p bar bar-p))))))
    (should (equal '(1 nil 2 nil)
                   (eval `(let ((,args nil))
                            (let* ,bindings
                              (list foo foo-p bar bar-p))))))))

(ert-deftest emacs-parity-clmacros-test/cl-defmacro-optional-and-rest-order ()
  "Multiple optional arguments precede the rest binding."
  (let* ((args (make-symbol "args"))
         (bindings
          (emacs-parity-clmacros--macro-bindings
           '(&optional (first 1 first-p) (second 2 second-p) &rest rest)
           args)))
    (should (equal '(a t b t (c))
                   (eval `(let ((,args '(a b c)))
                            (let* ,bindings
                              (list first first-p second second-p rest))))))
    (should (equal '(1 nil 2 nil nil)
                   (eval `(let ((,args nil))
                            (let* ,bindings
                              (list first first-p second second-p rest))))))))

(provide 'emacs-parity-clmacros-test)
;;; emacs-parity-clmacros-test.el ends here
