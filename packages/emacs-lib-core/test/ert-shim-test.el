;;; ert-shim-test.el --- standalone ERT shim subprocess tests  -*- lexical-binding: t; -*-

;;; Commentary:

;; These tests run the host ERT harness and spawn `bin/nemacs --driver=nelisp'
;; only when NEMACS_NELISP points at an executable standalone reader.

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst ert-shim-test--repo-root
  (expand-file-name
   "../"
   (file-name-directory (or load-file-name buffer-file-name)))
  "Absolute path to the nelisp-emacs repo root.")

(defconst ert-shim-test--bin
  (expand-file-name "bin/nemacs" ert-shim-test--repo-root)
  "Path to the nemacs wrapper.")

(defun ert-shim-test--nelisp ()
  "Return an executable standalone reader path, or nil."
  (let ((path (getenv "NEMACS_NELISP")))
    (and path (file-executable-p path) path)))

(defmacro ert-shim-test--skip-unless-standalone (&rest body)
  "Run BODY only when the standalone subprocess prerequisites exist."
  (declare (indent 0) (debug t))
  `(let ((nelisp (ert-shim-test--nelisp)))
     (cond
      ((not (file-executable-p ert-shim-test--bin))
       (ert-skip "bin/nemacs not executable"))
      ((not nelisp)
       (ert-skip "set NEMACS_NELISP to an executable standalone reader"))
      (t
       ,@body))))

(cl-defstruct (ert-shim-test--result
               (:constructor ert-shim-test--make-result))
  status
  output)

(defun ert-shim-test--run-source (source &rest extra-args)
  "Run standalone shim with test SOURCE and EXTRA-ARGS.
Return an `ert-shim-test--result' containing exit status and output."
  (let ((test-file (make-temp-file "nemacs-ert-shim-test-" nil ".el"))
        (stderr-file (make-temp-file "nemacs-ert-shim-test-stderr-"))
        (status nil)
        (stdout nil)
        (stderr nil))
    (unwind-protect
        (progn
          (with-temp-file test-file
            (insert source))
          (let ((process-environment
                 (append (list (format "NEMACS_NELISP=%s" (ert-shim-test--nelisp))
                               "NEMACS_RUNTIME_IMAGE=")
                         process-environment)))
            (setq stdout
                  (with-temp-buffer
                    (setq status
                          (apply #'call-process
                                 ert-shim-test--bin nil
                                 (list t stderr-file) nil
                                 (append (list "--driver=nelisp"
                                               "--batch" "--no-banner"
                                               "-L" "src"
                                               "-l" "ert"
                                               "-l" test-file)
                                         extra-args)))
                    (buffer-string))))
          (setq stderr
                (with-temp-buffer
                  (when (file-readable-p stderr-file)
                    (insert-file-contents stderr-file))
                  (buffer-string)))
          (ert-shim-test--make-result
           :status status
           :output (concat stdout stderr)))
      (when (file-exists-p test-file)
        (delete-file test-file))
      (when (file-exists-p stderr-file)
        (delete-file stderr-file)))))

(ert-deftest ert-shim-test/selector-does-not-run-unselected-body ()
  "A selector must prevent excluded test bodies from running."
  (ert-shim-test--skip-unless-standalone
    (let ((result
           (ert-shim-test--run-source
            "(ert-deftest probe/selected () (should t))
(ert-deftest probe/unselected () (error \"selector should skip this\"))
"
            "--eval" "(ert-run-tests-batch-and-exit \"probe/selected\")")))
      (should (= 0 (ert-shim-test--result-status result)))
      (should (string-match-p "Running 1 tests"
                              (ert-shim-test--result-output result)))
      (should (string-match-p "passed  probe/selected"
                              (ert-shim-test--result-output result)))
      (should-not (string-match-p "probe/unselected"
                                  (ert-shim-test--result-output result))))))

(ert-deftest ert-shim-test/should-error-type-mismatch-fails-run ()
  "`should-error :type' mismatches should fail the standalone run."
  (ert-shim-test--skip-unless-standalone
    (let ((result
           (ert-shim-test--run-source
            "(ert-deftest probe/type-mismatch ()
  (should-error (error \"boom\") :type 'wrong-type-argument))
"
            "-f" "ert-run-tests-batch-and-exit")))
      (should-not (= 0 (ert-shim-test--result-status result)))
      (should (string-match-p "FAILED  probe/type-mismatch"
                              (ert-shim-test--result-output result)))
      (should (string-match-p "Ran 1 tests, 1 failed"
                              (ert-shim-test--result-output result))))))

(provide 'ert-shim-test)

;;; ert-shim-test.el ends here
