;;; emacs-cc-census-other-w401.el --- Debugger breakpoint entry  -*- lexical-binding: t; -*-

;; Stack inspection and evaluation require access to evaluator activation
;; frames, which the standalone does not expose to Lisp.  Positioned symbols
;; likewise require the native symbol-with-position type: an ordinary Lisp
;; record is neither accepted by bare-symbol nor recognized by
;; symbol-with-pos-p.  Obarray records retain a symbol table but not GNU's
;; allocation size or bucket chains.  Focus-in handling needs the native
;; frame focus and event queue machinery.  Leave those primitives untouched
;; rather than substitute incomplete implementations.

(unless (fboundp 'debugger-trap)
  (defun debugger-trap (&rest arguments)
    "Provide a debugger breakpoint entry and return nil.
This function takes no arguments and has no Lisp-visible side effects."
    (interactive)
    ;; GNU's entry is a no-op; only its argument-count check is observable.
    (unless (= (length arguments) 0)
      (signal 'wrong-number-of-arguments
              (list 'debugger-trap (length arguments))))))

(provide 'emacs-cc-census-other-w401)
;;; emacs-cc-census-other-w401.el ends here
