;;; gnu-tests-run.el --- Run one GNU test file under GNU ERT on NeLisp -*- lexical-binding: t -*-

;; Driven by tools/ai/gnu-tests.sh inside the conformance image (bundle,
;; tools/ai/gnu-tests-support.el and GNU ert.el already loaded).
;;
;; Environment:
;;   GNU_TESTS_FILE  absolute path of the *-tests.el file to load
;;   GNU_TESTS_SKIP  optional file: test names (one per line) not to run,
;;                   either excluded or already reported by an earlier
;;                   process that aborted part way
;;
;; Protocol on stdout (ERT's own report goes to stderr through `message'):
;;   GT|LOADED|N            the file loaded and defined N tests
;;   GT|LOAD-ERROR|SEXP     loading signalled; tests defined so far still run
;;   GT|START|NAME          a test is about to run
;;   GT|R|STATUS|NAME       a test ended; STATUS is passed, failed-expected,
;;                          skipped or unexpected-<type>
;;   GT|DONE                every selected test reported

(defvar gnu-tests--names nil
  "Test names in definition order (reversed), recorded by `ert-set-test'.")

(defun gnu-tests--emit (&rest parts)
  (princ (concat "GT|" (mapconcat (lambda (p) (if (stringp p) p (format "%S" p)))
                                  parts "|")
                 "\n")))

(defun gnu-tests--status (test result)
  (cond ((ert-test-skipped-p result) "skipped")
        ((ert-test-result-expected-p test result)
         (if (ert-test-passed-p result) "passed" "failed-expected"))
        ((ert-test-passed-p result) "unexpected-passed")
        ((ert-test-failed-p result) "unexpected-failed")
        (t (format "unexpected-%s" (type-of result)))))

(defun gnu-tests--read-skip ()
  (let ((f (getenv "GNU_TESTS_SKIP")) names)
    (when (and f (file-readable-p f))
      (with-temp-buffer
        (insert-file-contents f)
        (dolist (line (split-string (buffer-string) "\n" t))
          (push (intern line) names))))
    names))

(defun gnu-tests-run ()
  "Load GNU_TESTS_FILE and run its tests, reporting through the protocol."
  (let ((file (getenv "GNU_TESTS_FILE"))
        (skip (gnu-tests--read-skip))
        (orig-set (symbol-function 'ert-set-test))
        (orig-run (symbol-function 'ert-run-tests)))
    ;; Record names as they are defined instead of enumerating the obarray,
    ;; so the run does not depend on `apropos-internal'/`mapatoms'.
    (fset 'ert-set-test
          (lambda (name definition)
            (unless (memq name gnu-tests--names) (push name gnu-tests--names))
            (funcall orig-set name definition)))
    (condition-case err
        (load file nil t)
      (error (gnu-tests--emit "LOAD-ERROR" (prin1-to-string err))))
    (fset 'ert-set-test orig-set)
    (gnu-tests--emit "LOADED" (number-to-string (length gnu-tests--names)))
    (fset 'ert-run-tests
          (lambda (selector listener &optional interactively)
            (funcall orig-run selector
                     (lambda (event &rest args)
                       (pcase event
                         ('test-started
                          (gnu-tests--emit "START" (ert-test-name (nth 1 args))))
                         ('test-ended
                          (let ((test (nth 1 args)) (result (nth 2 args)))
                            (gnu-tests--emit "R" (gnu-tests--status test result)
                                             (ert-test-name test)))))
                       (apply listener event args))
                     interactively)))
    (let ((selected (seq-remove (lambda (n) (memq n skip))
                                (reverse gnu-tests--names))))
      (when selected
        (ert-run-tests-batch (cons 'member selected))))
    (gnu-tests--emit "DONE")))

(provide 'gnu-tests-run)
;;; gnu-tests-run.el ends here
