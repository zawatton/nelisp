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

(defun nelisp-ert-collection--loaded-keys (table)
  "Record every file in `load-history' in TABLE, extension-free and truenamed."
  (dolist (entry load-history)
    (when (stringp (car entry))
      (puthash (file-name-sans-extension (file-truename (car entry))) t table))))

(defun nelisp-ert-collection-load-files (files)
  "Load each test file in FILES once, in order.
Test files `load' or `require' other test files, so a plain `-l' list
can load one twice and ERT then aborts with \"redefined (or loaded
twice)\"; a file already in `load-history' is skipped.  A file that
signals while loading becomes one failing test named
test-file-load/BASENAME, so the rest of the suite still runs and one
run reports every failure."
  (let ((seen (make-hash-table :test #'equal)))
    (nelisp-ert-collection--loaded-keys seen)
    (dolist (file files)
      (let* ((path (expand-file-name file))
             (key (file-name-sans-extension (file-truename path))))
        (unless (gethash key seen)
          (condition-case err
              (load path nil t)
            (error
             (message "TEST-LOAD-ERROR %s: %S" file err)
             (let ((reason (format "%s: %S" file err)))
               (eval `(ert-deftest
                          ,(intern (concat "test-file-load/"
                                           (file-name-base file)))
                          ()
                        (ert-fail ,reason))
                     t))))
          (nelisp-ert-collection--loaded-keys seen))))))

(defun nelisp-ert-collection-load-env ()
  "Load the whitespace-separated test files in $NELISP_ERT_FILES."
  (nelisp-ert-collection-load-files
   (split-string (or (getenv "NELISP_ERT_FILES") "") "[ \t\n]+" t)))

(advice-add 'ert-run-tests-batch-and-exit :around #'nelisp-ert-collection--defer)
(provide 'nelisp-ert-collection)
