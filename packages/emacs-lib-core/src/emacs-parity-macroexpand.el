;;; emacs-parity-macroexpand.el --- correct macroexpand for (macro CLOSURE) -*- lexical-binding: t; -*-
;; GNU supplies macroexpand in eval.c.  The macroexp.el library supplies
;; macroexpand-1 and macroexpand-all earlier in the source bundle.

(when (fboundp 'nelisp--write-stdout-bytes)

(defun macroexpand (form &optional environment)
  "Repeatedly expand FORM until its head is no longer a macro call."
  (let ((cur form)
        (next (macroexpand-1 form environment)))
    (while (not (eq next cur))
      (setq cur next)
      (setq next (macroexpand-1 cur environment)))
    cur))

)

(provide 'emacs-parity-macroexpand)
;;; emacs-parity-macroexpand.el ends here
