(threadp
 (threadp (current-thread))
 (mapcar #'threadp '(nil t 1 "x" [1 2 3] (a b)))
 (threadp (make-mutex))
 (condition-case err (threadp) (error err))
 (condition-case err (threadp nil nil) (error err)))

(mutexp
 (mutexp (make-mutex "named"))
 (mapcar #'mutexp '(nil t 1 "x" [1 2 3] (a b)))
 (mutexp (current-thread))
 (mutexp (make-condition-variable (make-mutex)))
 (condition-case err (mutexp) (error err))
 (condition-case err (mutexp nil nil) (error err)))

(condition-variable-p
 (condition-variable-p (make-condition-variable (make-mutex) "named"))
 (mapcar #'condition-variable-p '(nil t 1 "x" [1 2 3] (a b)))
 (condition-variable-p (make-mutex))
 (condition-variable-p (current-thread))
 (condition-case err (condition-variable-p) (error err))
 (condition-case err (condition-variable-p nil nil) (error err)))
