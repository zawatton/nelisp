;;; fixture.el --- GNU 31.1 dolist parity cases -*- lexical-binding: t; -*-
;; Reference: GNU lisp/subr.el:390-407.  Generate expected by running GNU,
;; without replacement.el; the candidate loads replacement.el first.
(defun dolist-fixture-emit (label value)
  (prin1 (list label value)) (terpri))

(let ((functions nil))
  (dolist (value '(nil t 0 -7)) (push (lambda () value) functions))
  (dolist-fixture-emit 'fresh-bindings (mapcar #'funcall (nreverse functions))))
(let ((value 'outer))
  (dolist-fixture-emit 'result-scope
    (dolist (value '(one two) value) (setq value 'changed)))
  (dolist-fixture-emit 'outer-after value))
(let ((value 'outer))
  (dolist-fixture-emit 'empty-result-scope (dolist (value nil value))))
(let ((seen nil))
  (dolist (--nl-dolist-list '(one two three)) (push --nl-dolist-list seen))
  (dolist-fixture-emit 'cursor-hygiene (nreverse seen)))
(let ((calls 0) (value '(outer)) (seen nil))
  (dolist (value (progn (setq calls (1+ calls)) value))
    (push value seen))
  (dolist-fixture-emit 'list-evaluated-once (list calls (nreverse seen) value)))
(let ((value 'outer) (functions nil))
  (dolist (value '(1 2 3))
    (push (lambda () value) functions)
    (setq value (* value 10)))
  (dolist-fixture-emit 'assignment-binding
    (list value (mapcar #'funcall (nreverse functions)))))
(dolist-fixture-emit 'nonlocal
  (catch 'done (dolist (value '(1 2 3) 'result)
                (when (= value 2) (throw 'done value)))))
(dolist-fixture-emit 'no-result (dolist (value '(1 2)) value))
(dolist (spec '(nil 7 (x) (x nil nil extra) (x . 1)))
  (dolist-fixture-emit (list 'invalid-spec spec)
    (condition-case err (macroexpand-1 (list 'dolist spec))
      (error err))))
(dolist-fixture-emit 'dotted-list
  (condition-case err (dolist (value '(one . 7)) value)
    (error err)))
(dolist-fixture-emit 'result-outside-unbound-var
  (condition-case err (dolist (dolist-fixture-unbound '(1) dolist-fixture-unbound))
    (error err)))
(princ "DOLIST-FOCUSED-PASS\n")
nil

(kill-emacs 0)
