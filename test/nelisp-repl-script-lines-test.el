;;; nelisp-repl-script-lines-test.el --- script -> REPL line tests -*- lexical-binding: t; -*-
(require 'ert)

;; The generator lives in `tools/ai', which the TESTS glob runners
;; (`make test', `test-fast', `test-parallel', `test-one') do not put on the
;; load path -- and this file matches that glob.  Resolve it from this file's
;; own location instead of adding a `-L' to four runners.
;; `byte-compile-current-file' is the only one of the three that is set while
;; this file is being byte-compiled rather than loaded.
(require 'nelisp-repl-script-lines
         (expand-file-name
          "tools/ai/nelisp-repl-script-lines"
          (locate-dominating-file
           (or load-file-name buffer-file-name
               (bound-and-true-p byte-compile-current-file)
               default-directory)
           "Makefile")))

(defun nelisp-repl-script-lines-test--file (text)
  "Write TEXT to a temporary .el file and return its path."
  (let ((path (make-temp-file "nelisp-repl-script-lines-" nil ".el"))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file path (insert text))
    path))

(ert-deftest nelisp-repl-script-lines/multi-line-form-becomes-one-line ()
  (let ((path (nelisp-repl-script-lines-test--file
               "(princ\n (format \"%S\"\n         (+ 1 2)))\n")))
    (unwind-protect
        (let ((lines (nelisp-repl-script-lines path)))
          (should (= (length lines) 2))
          (should (equal (car lines) nelisp-repl-script-lines-helper))
          (dolist (line lines)
            (should-not (string-match-p "\n" line))))
      (delete-file path))))

(ert-deftest nelisp-repl-script-lines/source-text-is-copied-verbatim ()
  (let* ((form "(defun f (x)\n  ;; inner comment stays\n  (+ x 1))")
         (path (nelisp-repl-script-lines-test--file (concat form "\n"))))
    (unwind-protect
        (let* ((line (nth 1 (nelisp-repl-script-lines path)))
               (call (car (read-from-string line))))
          (should (eq (car call) 'nelisp-repl--eval-source))
          ;; The argument is the form's exact bytes, not a re-printed form.
          (should (equal (nth 1 call) form)))
      (delete-file path))))

(ert-deftest nelisp-repl-script-lines/comments-between-forms-are-not-forms ()
  (let ((path (nelisp-repl-script-lines-test--file
               ";; leading\n(setq a 1)\n;; between\n\n(setq b 2)\n;; trailing\n")))
    (unwind-protect
        (let ((texts (nelisp-repl-script-lines--form-texts path)))
          (should (equal texts '("(setq a 1)" "(setq b 2)"))))
      (delete-file path))))

(ert-deftest nelisp-repl-script-lines/newline-in-a-string-is-escaped ()
  (let ((path (nelisp-repl-script-lines-test--file
               "(princ \"first\nsecond\")\n")))
    (unwind-protect
        (let* ((line (nth 1 (nelisp-repl-script-lines path)))
               (call (car (read-from-string line))))
          (should-not (string-match-p "\n" line))
          ;; The literal newline inside the string survives the round trip.
          (should (equal (nth 1 call) "(princ \"first\nsecond\")")))
      (delete-file path))))

(ert-deftest nelisp-repl-script-lines/incomplete-final-form-is-reported ()
  (let ((path (nelisp-repl-script-lines-test--file "(setq a 1)\n(defun g (x)\n")))
    (unwind-protect
        (let ((err (should-error (nelisp-repl-script-lines--form-texts path))))
          ;; The line number of the form that never closed, not of EOF.
          (should (string-match-p ":2: file ends inside a form"
                                  (error-message-string err))))
      (delete-file path))))

(ert-deftest nelisp-repl-script-lines/batch-appends-to-its-output ()
  (let ((in (nelisp-repl-script-lines-test--file "(setq a 1)\n"))
        (out (make-temp-file "nelisp-repl-script-lines-out-" nil ".txt")))
    (unwind-protect
        (progn
          (with-temp-file out (insert "(already-there)\n"))
          (let ((process-environment
                 (append (list (concat "NELISP_REPL_SCRIPT_IN=" in)
                               (concat "NELISP_REPL_SCRIPT_OUT=" out))
                         process-environment)))
            (nelisp-repl-script-lines-batch))
          (with-temp-buffer
            (insert-file-contents out)
            (let ((lines (split-string (buffer-string) "\n" t)))
              (should (equal (car lines) "(already-there)"))
              (should (equal (nth 1 lines) nelisp-repl-script-lines-helper))
              (should (string-match-p "(setq a 1)" (nth 2 lines))))))
      (delete-file in)
      (delete-file out))))

(ert-deftest nelisp-repl-script-lines/line-cap-matches-the-repl-read-cap ()
  ;; `skip-unless' must be inline in the test body: wrapping it in a helper
  ;; leaves it unrecognised at byte-compile time.
  (skip-unless (require 'nelisp-standalone-build nil t))
  (should (nelisp-repl-script-lines-cap-matches-reader-p)))

(ert-deftest nelisp-repl-script-lines/oversized-form-is-refused ()
  ;; Refused, not truncated: the REPL would cut the line at its own cap and
  ;; then fail to read the remains.  Checked against the real cap rather than
  ;; a rebound one, so the test cannot pass with the boundary moved.
  (let ((err (should-error
              (nelisp-repl-script-lines--line
               (make-string (1+ nelisp-repl-script-lines-line-cap) ?x)
               "oversized.el" 1))))
    (should (string-match-p "over the REPL line cap"
                            (error-message-string err)))))

(provide 'nelisp-repl-script-lines-test)

;;; nelisp-repl-script-lines-test.el ends here
