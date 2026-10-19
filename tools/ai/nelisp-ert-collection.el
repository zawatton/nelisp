;;; nelisp-ert-collection.el --- Collect standalone test drivers without exiting -*- lexical-binding: t; -*-
;; Some -test.el files also support direct batch execution. Their footer
;; must not run a partial suite or exit before the gate has loaded all files.
;; The gate runs the complete collected suite after removing this advice.
(require 'ert)

(defvar nelisp-ert-collection--deferred 0)

(defun nelisp-ert-collection--defer (_original &rest _arguments)
  "Defer a standalone driver's batch run until collection is complete."
  (setq nelisp-ert-collection--deferred (1+ nelisp-ert-collection--deferred))
  nil)

(defun nelisp-ert-collection-finish ()
  "Restore ordinary ERT execution before running any test body."
  (advice-remove 'ert-run-tests-batch-and-exit #'nelisp-ert-collection--defer)
  (message "ERT collection: deferred %d standalone driver footer(s)"
           nelisp-ert-collection--deferred))

(advice-add 'ert-run-tests-batch-and-exit :around #'nelisp-ert-collection--defer)
(provide 'nelisp-ert-collection)
