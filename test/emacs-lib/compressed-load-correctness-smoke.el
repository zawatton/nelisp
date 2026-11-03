;;; compressed-load-correctness-smoke.el --- gzip load semantics -*- lexical-binding: t; -*-
(unless (fboundp 'nelisp--repr) (require 'jka-compr))
(when (fboundp 'emacs-callproc-populate-process-environment)
  (setq process-environment nil) (emacs-callproc-populate-process-environment))
(let ((load-path (cons (getenv "K1_COMPRESSED_CASE_ROOT") load-path))
      (load-suffixes '(".el")) (load-file-rep-suffixes '("" ".gz")))
  (princ (format "K1-COMPRESSED|real-source|%S|%S|\n"
                 (require 'k1-compressed-real) k1-compressed-real-value))
  (princ (format "K1-COMPRESSED|corrupt-source|%S|\n"
                 (condition-case err (load "k1-compressed-corrupt" nil t)
                   (error (car err)))))
  (princ (format "K1-COMPRESSED|missing-feature|%S|%S|\n"
                 (condition-case err (require 'k1-compressed-missing)
                   (error (car err)))
                 (featurep 'k1-compressed-missing))))
(princ "K1-COMPRESSED-DONE\n")
