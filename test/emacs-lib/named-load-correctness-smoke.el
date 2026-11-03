;;; named-load-correctness-smoke.el --- named source escapes -*- lexical-binding: t; -*-
(when (fboundp 'emacs-callproc-populate-process-environment)
  (setq process-environment nil) (emacs-callproc-populate-process-environment))
(let ((load-path (cons (getenv "K1_NAMED_CASE_ROOT") load-path)))
  (require 'k1-named-real)
  (princ (format "K1-NAMED|colon-characters|%S|\n" k1-named-colons))
  (princ (format "K1-NAMED|named-strings|%S|\n" k1-named-strings))
  (princ (format "K1-NAMED|literal-escapes|%S|%S|\n" k1-named-literal k1-named-punctuation))
  (princ (format "K1-NAMED|unknown-name|%S|\n"
                 (condition-case err (load "k1-named-unknown" nil t)
                   (error (car err))))))
(princ "K1-NAMED-DONE\n")
