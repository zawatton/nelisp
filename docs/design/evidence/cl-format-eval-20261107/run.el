;;; -*- lexical-binding: t; -*-
;; Evaluation-only: run each ERT body directly, one result line per test.
(let ((pass 0) (fail 0) (tests (ert-select-tests t t)))
  (dolist (tt tests)
    (let ((name (ert-test-name tt))
          (res (condition-case e (progn (funcall (ert-test-body tt)) 'ok) (error e))))
      (if (eq res 'ok) (setq pass (1+ pass))
        (setq fail (1+ fail))
        (let ((s (condition-case _ (let ((print-length 20) (print-level 8)) (prin1-to-string res)) (error "<unprintable>"))))
          (princ (format "FAIL %s %s\n" name (if (> (length s) 420) (substring s 0 420) s)))))))
  (princ (format "TOTAL tests=%d pass=%d fail=%d\n" (length tests) pass fail)))
