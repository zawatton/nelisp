;;; prim-callfloor-parity.el --- Print exact GNU calling results -*- lexical-binding: t; -*-

(load (getenv "PCF_SOURCE") nil t)
(setq print-escape-newlines t print-escape-control-characters t)
(when (and (equal (getenv "PCF_MODE") "vm")
           (fboundp 'nelisp--fx-register))
  (dolist (name '(pcf-optional pcf-rest pcf-both pcf-read-dynamic pcf-closure
                 pcf-arguments pcf-few pcf-many pcf-nil-body))
    (nelisp--fx-register name))
  (princ "PCF-VM ")
  (prin1 (mapcar (lambda (name)
                  (cons name (vectorp (gethash (nthcdr 3 (symbol-function name))
                                               nelisp--fx-table))))
                '(pcf-optional pcf-rest pcf-both pcf-read-dynamic pcf-closure
                  pcf-arguments pcf-nil-body)))
  (terpri))
(dolist (name '(pcf-arguments pcf-control pcf-few pcf-many pcf-builtin-few
               pcf-builtin-many pcf-apply-bad pcf-literals))
  (princ (format "PCF %s " name))
  (prin1 (funcall name))
  (terpri))
(princ "PCF-COMPLETE 8\n")
(kill-emacs 0)

