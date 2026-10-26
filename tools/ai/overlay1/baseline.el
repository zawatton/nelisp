;;; Prints VOID when `make-overlay' is not defined (the v17 baseline), HAS otherwise.
(condition-case nil
    (progn (make-overlay 1 1) (princ "HAS\n"))
  (void-function (princ "VOID\n")))
