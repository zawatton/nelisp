;;; standalone-print-circle-default.el --- Genuine C startup declaration -*- lexical-binding: t; -*-
(unless (and (boundp 'print-circle) (null print-circle)
             (special-variable-p 'print-circle))
  (error "GNU print-circle initial special-variable state differs"))
(defun nelisp-test-print-circle-value () print-circle)
(unless (eq (let ((print-circle t)) (nelisp-test-print-circle-value)) t)
  (error "Print-circle dynamic binding was not visible"))
(catch 'nelisp-test-print-circle-unwind
  (let ((print-circle t)) (throw 'nelisp-test-print-circle-unwind nil)))
(unless (null print-circle) (error "Print-circle unwind did not restore nil"))
(let ((print-circle t) (documentation (get 'print-circle 'variable-documentation)))
  (unless (and (eq (nelisp-stdlib-print-circle-install) 'preserved)
               (eq print-circle t)
               (equal documentation (get 'print-circle 'variable-documentation)))
    (error "Print-circle installer changed an existing value or documentation")))
(let ((saved (symbol-function 'nelisp-bytecode-compiler-input-dialect)) (refused nil))
  (unwind-protect
      (progn
        (fset 'nelisp-bytecode-compiler-input-dialect (lambda () nil))
        (condition-case nil (nelisp-stdlib-print-circle-install)
          (error (setq refused t)))
        (unless refused (error "Changed print-circle dialect owner admitted")))
    (fset 'nelisp-bytecode-compiler-input-dialect saved)))
(princ "PRINT-CIRCLE-DEFAULT-PASS\n")
