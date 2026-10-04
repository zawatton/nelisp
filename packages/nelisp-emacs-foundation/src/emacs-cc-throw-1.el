;;; emacs-cc-throw-1.el --- Callable nonlocal throw -*- lexical-binding: t; -*-

(unless (fboundp 'throw)
  (defun throw (&rest arguments)
    "Exit the nearest matching catch, returning VALUE."
    (unless (= (length arguments) 2)
      (signal 'wrong-number-of-arguments
              (list (symbol-function 'throw) (length arguments))))
    ;; The evaluator handles this form directly, independently of its
    ;; function cell.  This wrapper also permits funcall and apply.
    (throw (car arguments) (cadr arguments))))

(provide 'emacs-cc-throw-1)
;;; emacs-cc-throw-1.el ends here
