;;; nelisp-bytecode-logb-provider-test.el --- Pinned provider controls -*- lexical-binding: t; -*-
(require 'ert)
(let ((source (getenv "NELISP_LOGB_OVERLAY")))
  (with-temp-buffer
    (insert-file-contents source)
    (goto-char (point-min))
    (let (form done)
      (while (not done)
        (setq form (read (current-buffer)))
        (when (memq (car-safe form) '(defconst defun))
          (eval form t))
        (setq done (eq (nth 1 form) 'nelisp-bc-logb-provider-forms))))))

(ert-deftest nelisp-bytecode-logb-provider-source-and-owner-controls ()
  (let* ((provider (getenv "NELISP_LOGB_PROVIDER"))
         (equal-owner (symbol-function 'equal))
         (logb-owner (symbol-function 'logb))
         (forms (nelisp-bc-logb-provider-forms provider))
         (temporary (make-temp-file "nelisp-logb-provider-")))
    (unwind-protect
        (progn
          (should (= (length forms) 2))
          (mapc (lambda (form) (eval form nil)) forms)
          (should (eq equal-owner (symbol-function 'equal)))
          (should (eq logb-owner (symbol-function 'logb)))
          (should (= (logb 16) 4))
          (with-temp-buffer
            (insert-file-contents provider)
            (goto-char (point-min))
            (search-forward "(defun logb (arg)")
            (insert " ")
            (write-region (point-min) (point-max) temporary nil 'silent))
          (should-error (nelisp-bc-logb-provider-forms temporary))
          (should (eq equal-owner (symbol-function 'equal)))
          (should (eq logb-owner (symbol-function 'logb))))
      (delete-file temporary))))

(ert-deftest nelisp-bytecode-logb-provider-missing-function-install ()
  (let ((old (symbol-function 'logb))
        (equal-owner (symbol-function 'equal)))
    (unwind-protect
        (progn
          (fmakunbound 'logb)
          (mapc (lambda (form) (eval form nil))
                (nelisp-bc-logb-provider-forms
                 (getenv "NELISP_LOGB_PROVIDER")))
          (should (= (logb 16) 4))
          (should (= (logb 0.5) -1))
          (should-error (logb 'invalid) :type 'wrong-type-argument)
          (should (eq equal-owner (symbol-function 'equal))))
      (fset 'logb old))))
