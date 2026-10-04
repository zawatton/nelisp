;;; standalone-print-quoted-default.el --- Genuine C printing policy default -*- lexical-binding: t; -*-
(unless (and (boundp 'print-quoted) (eq print-quoted t)
             (special-variable-p 'print-quoted))
  (error "GNU print-quoted initial special-variable state differs"))
(defun nelisp-test-print-quoted-value () print-quoted)
(unless (null (let ((print-quoted nil)) (nelisp-test-print-quoted-value)))
  (error "Print-quoted dynamic binding was not visible"))
(catch 'nelisp-test-print-quoted-unwind
  (let ((print-quoted nil)) (throw 'nelisp-test-print-quoted-unwind nil)))
(unless (eq print-quoted t) (error "Print-quoted unwind did not restore true"))
(let ((print-quoted nil) (documentation (get 'print-quoted 'variable-documentation)))
  (unless (and (eq (nelisp-stdlib-print-quoted-install) 'preserved)
               (null print-quoted)
               (equal documentation (get 'print-quoted 'variable-documentation)))
    (error "Print-quoted installer changed existing value or documentation")))
(let ((saved (symbol-function 'nelisp-bytecode-compiler-input-dialect)) (refused nil))
  (unwind-protect
      (progn
        (fset 'nelisp-bytecode-compiler-input-dialect (lambda () nil))
        (condition-case nil (nelisp-stdlib-print-quoted-install)
          (error (setq refused t)))
        (unless refused (error "Changed print-quoted dialect owner admitted")))
    (fset 'nelisp-bytecode-compiler-input-dialect saved)))
(princ "PRINT-QUOTED-DEFAULT-PASS\n")
