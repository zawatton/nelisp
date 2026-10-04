;;; emacs-cc-x-focus-1-test.el --- x-focus-frame batch contract -*- lexical-binding: t; -*-

(require 'ert)
(defconst emacs-cc-x-focus-1-test--source
  (expand-file-name
   "../../packages/nelisp-emacs-foundation/src/emacs-cc-x-focus-1.el"
   (file-name-directory (or load-file-name buffer-file-name))))
(load emacs-cc-x-focus-1-test--source nil t)

(defun emacs-cc-x-focus-1-test--with-shim (thunk)
  "Run THUNK with the batch shim installed, restoring GNU's primitive."
  (let ((original (symbol-function 'x-focus-frame)))
    (unwind-protect
        (progn
          (fmakunbound 'x-focus-frame)
          (load emacs-cc-x-focus-1-test--source nil t)
          (funcall thunk))
      (fset 'x-focus-frame original))))

(ert-deftest emacs-cc-x-focus-1/guard-preserves-gnu-primitive ()
  (let ((original (symbol-function 'x-focus-frame)))
    (load emacs-cc-x-focus-1-test--source nil t)
    (should (eq original (symbol-function 'x-focus-frame)))))

(ert-deftest emacs-cc-x-focus-1/terminal-frame-errors-without-state-change ()
  (should-not window-system)
  (let ((frame (selected-frame))
        (before (list window-system (length (frame-list))
                      (eq (selected-frame) (selected-frame)))))
    (emacs-cc-x-focus-1-test--with-shim
     (lambda ()
       (dolist (args (list (list frame) '(nil) (list frame nil)
                           (list frame t) (list frame 7)))
         (let ((err (should-error (apply #'x-focus-frame args) :type 'error)))
           (should (equal (cadr err) "Window system frame should be used")))))
     )
    (should (eq frame (selected-frame)))
    (should (equal before (list window-system (length (frame-list)) t)))))

(ert-deftest emacs-cc-x-focus-1/invalid-frame-types-and-arity ()
  (emacs-cc-x-focus-1-test--with-shim
   (lambda ()
     (dolist (frame '(t fake-frame 7 "frame"))
       (should-error (x-focus-frame frame) :type 'wrong-type-argument))
     (should-error (x-focus-frame) :type 'wrong-number-of-arguments)
     (should-error (x-focus-frame nil nil nil)
                   :type 'wrong-number-of-arguments))))

(provide 'emacs-cc-x-focus-1-test)
;;; emacs-cc-x-focus-1-test.el ends here
