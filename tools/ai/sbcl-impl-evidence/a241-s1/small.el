(let ((fail 0))
  (dolist (n '("car" "cdr" "cons" "list" "mapcar" "format" "setq" "defun" "let" "if"
               "eq" "equal" "length" "append" "intern" "symbol-name" "princ" "t" "load"))
    (unless (intern-soft n) (princ (format "MISSING %s\n" n)) (setq fail (1+ fail))))
  (when (intern-soft "a241-absent-name-xyz") (princ "UNEXPECTED present\n") (setq fail (1+ fail)))
  (let ((a (intern "a241-new-name")) (b (intern "a241-new-name")))
    (unless (and (eq a b) (equal (symbol-name a) "a241-new-name"))
      (princ "NEW-NAME mismatch\n") (setq fail (1+ fail))))
  ;; (intern-soft "nil") returns the symbol nil, which is false, so nil is checked by identity.
  (unless (eq (intern "nil") nil) (princ "nil identity mismatch\n") (setq fail (1+ fail)))
  (unless (eq 'car (intern "car")) (princ "car eq mismatch\n") (setq fail (1+ fail)))
  (princ (if (= fail 0) "INTERN-PROBE-PASS\n" "INTERN-PROBE-FAIL\n")))
