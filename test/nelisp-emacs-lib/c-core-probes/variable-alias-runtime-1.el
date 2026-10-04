;;; -*- lexical-binding: t; -*-
;; This retains the direct evaluator path independently of funcall coverage.
(defvaralias
 (let ((target (make-symbol "target")) (alias (make-symbol "alias")))
   (set target 12)
   (defvaralias alias target)
   (set target 23)
   (list (eq (indirect-variable alias) target) (symbol-value alias))))
