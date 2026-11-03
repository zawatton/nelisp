;;; nelisp-display-batch-test.el --- tests for the scoped batch provider -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(defvar nelisp-display-batch-test--gnu-function
  (and (fboundp 'display-color-cells)
       (symbol-function 'display-color-cells)))
(require 'nelisp-display-batch)

(ert-deftest nelisp-display-batch-color-cells-matches-gnu-batch-oracle ()
  (should (= (display-color-cells) 0))
  (cl-letf (((symbol-function 'selected-frame) (lambda () nil)))
    (should (= (nl-display-batch-color-cells) 0))))

(ert-deftest nelisp-display-batch-color-cells-rejects-explicit-display ()
  (should-error (nl-display-batch-color-cells "other-display")))

(ert-deftest nelisp-display-batch-color-cells-rejects-graphics-and-color-tty ()
  (cl-letf (((symbol-function 'selected-frame) (lambda () nil))
            ((symbol-function 'display-graphic-p) (lambda (&optional _) t)))
    (should-error (nl-display-batch-color-cells)))
  (cl-letf (((symbol-function 'selected-frame) (lambda () nil))
            ((symbol-function 'tty-display-color-p) (lambda (&optional _) t)))
    (should-error (nl-display-batch-color-cells))))

(ert-deftest nelisp-display-batch-color-cells-rejects-missing-capability ()
  (let ((saved (symbol-function 'display-graphic-p)))
    (unwind-protect
        (progn
          (fmakunbound 'display-graphic-p)
          (should-error (nl-display-batch-color-cells)))
      (fset 'display-graphic-p saved)))
  (let ((saved (symbol-function 'tty-display-color-p)))
    (unwind-protect
        (progn
          (fmakunbound 'tty-display-color-p)
          (should-error (nl-display-batch-color-cells)))
      (fset 'tty-display-color-p saved))))

(ert-deftest nelisp-display-batch-does-not-replace-gnu-provider ()
  (when nelisp-display-batch-test--gnu-function
    (should (eq (symbol-function 'display-color-cells)
                nelisp-display-batch-test--gnu-function))))

(provide 'nelisp-display-batch-test)
;;; nelisp-display-batch-test.el ends here
