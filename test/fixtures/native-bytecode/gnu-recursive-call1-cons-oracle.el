;;; -*- lexical-binding: t; -*-
;;; Host oracle for the recursive CALL1 and CONS bytecode fixture.
(message "checkpoint:oracle-load-start")
(load (getenv "RECURSION_ELC"))
(message "checkpoint:oracle-loaded")
(defvar recursion-depth 0)
(defvar recursion-calls 0)
(fset 'nelisp-recursive-vm-callback
      (lambda (value)
        (let ((recursion-depth (1+ recursion-depth)))
          (setq recursion-calls (1+ recursion-calls))
          (if (< recursion-depth 5)
              (nelisp-recursive-call1-wrapper value)
            value))))
(let* ((payload '(root (mutable)))
       (result (nelisp-recursive-call1-wrapper payload)))
  (unless (and (= recursion-calls 5) (eq result payload)
               (equal result '(root (mutable))))
    (error "GNU recursive oracle failed: %S calls=%d"
           result recursion-calls))
  (princ (format "%S" result))
  (message "GNU-ORACLE-PASS calls=%d" recursion-calls))
