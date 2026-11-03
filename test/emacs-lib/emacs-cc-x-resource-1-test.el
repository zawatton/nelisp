;;; emacs-cc-x-resource-1-test.el --- x-get-resource batch contract -*- lexical-binding: t; -*-

(require 'ert)
(defconst emacs-cc-x-resource-1-test--source
  (expand-file-name
   "../../packages/nelisp-emacs-foundation/src/emacs-cc-x-resource-1.el"
   (file-name-directory (or load-file-name buffer-file-name))))
(load emacs-cc-x-resource-1-test--source nil t)

(defun emacs-cc-x-resource-1-test--with-shim (thunk)
  "Run THUNK with the batch shim installed, restoring GNU's primitive."
  (let ((original (symbol-function 'x-get-resource)))
    (unwind-protect
        (progn
          (fmakunbound 'x-get-resource)
          (load emacs-cc-x-resource-1-test--source nil t)
          (funcall thunk))
      (fset 'x-get-resource original))))

(ert-deftest emacs-cc-x-resource-1/guard-preserves-gnu-primitive ()
  (let ((original (symbol-function 'x-get-resource)))
    (load emacs-cc-x-resource-1-test--source nil t)
    (should (eq original (symbol-function 'x-get-resource)))))

(ert-deftest emacs-cc-x-resource-1/no-display-signals-gnu-error-before-argument-validation ()
  (should-not window-system)
  (emacs-cc-x-resource-1-test--with-shim
   (lambda ()
     (dolist (args '(("geometry" "Geometry")
                     ("foreground" "Foreground")
                     ("reverseVideo" "ReverseVideo")
                     ("absent" "Absent")
                     ("geometry" "Geometry" "app")
                     ("geometry" "Geometry" "app" "App")
                     (nil "Class")
                     (7 "Class")))
       (let ((err (should-error (apply #'x-get-resource args) :type 'error)))
         (should (equal (cadr err)
                        "Window system is not in use or not initialized")))))))

(ert-deftest emacs-cc-x-resource-1/arity-is-two-through-four ()
  (emacs-cc-x-resource-1-test--with-shim
   (lambda ()
     (should-error (x-get-resource) :type 'wrong-number-of-arguments)
     (should-error (x-get-resource "geometry") :type 'wrong-number-of-arguments)
     (should-error (x-get-resource "geometry" "Geometry" nil nil nil)
                   :type 'wrong-number-of-arguments))))

(provide 'emacs-cc-x-resource-1-test)
;;; emacs-cc-x-resource-1-test.el ends here
