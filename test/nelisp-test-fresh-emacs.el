;;; nelisp-test-fresh-emacs.el --- Run a test in a fresh Emacs when this one cannot -*- lexical-binding: t; -*-

;;; Commentary:

;; Some tests need a host Emacs that has not loaded a library other test
;; files load: the standalone builder refuses a host that already defines
;; the prelude's core records (`src/nelisp-buffer.el' does), and the FX IR
;; admission must activate before the builder is loaded.  Run per file
;; (tools/run-tests-parallel.sh, CI) the host is clean and the test body
;; runs here.  When every test file shares one Emacs
;; (tools/ai/s1-9-ert/ert-single.sh) it is not, and
;; `nelisp-test-fresh-emacs-unless' runs the same test, by name, in a child
;; `emacs --batch -Q' with this Emacs's `load-path' and only its own file
;; loaded -- the per-file environment -- and reports the child's result.
;; The test's checks are unchanged; only where it runs differs.

;;; Code:

(require 'ert)

(defconst nelisp-test-fresh-emacs--child-variable "NELISP_TEST_FRESH_EMACS_CHILD"
  "Environment variable set in the child, so it never re-spawns.")

(defun nelisp-test-fresh-emacs-core-records-defined-p ()
  "Non-nil when this Emacs defines a prelude core record type.
scripts/nelisp-boot-image.el refuses to expand the prelude then."
  (or (fboundp 'nelisp-buffer-p) (fboundp 'nelisp-marker-p)
      (fboundp 'nelisp-marker-ins-p) (fboundp 'nelisp-overlay-p)))

(defun nelisp-test-fresh-emacs--run (test)
  "Run ERT TEST in a fresh child Emacs and turn its result into this one's."
  (let* ((name (ert-test-name test))
         (file (or (ert-test-file-name test)
                   (error "No file recorded for test %S" name)))
         (file (if (string-match-p "\\.elc\\'" file)
                   (concat (file-name-sans-extension file) ".el")
                 file))
         (args (append (list "--batch" "-Q")
                       (apply #'append
                              ;; `-L' keeps the order given.
                              (mapcar (lambda (dir) (list "-L" dir)) load-path))
                       (list "-l" "ert" "-l" file "--eval"
                             (format "(ert-run-tests-batch-and-exit '(member %S))"
                                     name))))
         (process-environment
          (cons (concat nelisp-test-fresh-emacs--child-variable "=1")
                process-environment))
         (status nil) (output nil))
    (with-temp-buffer
      (setq status (apply #'call-process
                          (expand-file-name invocation-name invocation-directory)
                          nil t nil args))
      (setq output (buffer-string)))
    (cond
     ((not (string-match-p "^Ran 1 tests?," output))
      (ert-fail (list :fresh-emacs-did-not-run name :status status
                      :output (substring output (max 0 (- (length output) 3000))))))
     ((string-match "^ +skipped +1/1 .*$" output)
      (ert-skip (format "In a fresh Emacs: %s" (match-string 0 output))))
     ((not (eq status 0))
      (ert-fail (list :fresh-emacs-failed name :status status
                      :output (substring output (max 0 (- (length output) 3000)))))))))

(defmacro nelisp-test-fresh-emacs-unless (clean &rest body)
  "Run BODY when CLEAN holds in this Emacs; else rerun this test in a fresh one.
CLEAN is the host precondition the test needs (for example that no core
record type is defined).  In the child, BODY always runs here, so a CLEAN
that is false even in a fresh Emacs fails the test instead of looping."
  (declare (indent 1) (debug t))
  `(if (or ,clean (getenv nelisp-test-fresh-emacs--child-variable))
       (progn ,@body)
     (nelisp-test-fresh-emacs--run (ert-running-test))))

(provide 'nelisp-test-fresh-emacs)

;;; nelisp-test-fresh-emacs.el ends here
