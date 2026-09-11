;;; nelisp-repl-session-test.el --- tests for explicit REPL session tools -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cl-lib)
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../lisp" here)))
(require 'nelisp-repl-session)

(ert-deftest nelisp-repl-session-failure-retry-is-explicit ()
  "A failed call is retained and can be retried after the same REPL is fixed."
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--next-id 1)
        (attempts 0))
    (cl-labels ((flaky (value)
                  (if (= attempts 0)
                      (progn (setq attempts 1) (error "temporary failure: %s" value))
                    (+ value 1))))
      (let ((condition nil))
        (condition-case err
            (nelisp-repl-session-call #'flaky 41)
          (error (setq condition err)))
        (should (eq (car condition) 'error)))
      (let* ((failure (car (nelisp-repl-session-failures)))
             (id (plist-get failure :id)))
        (should (= id 1))
        (should (equal (plist-get failure :args) '(41)))
        (should (equal (plist-get failure :message) "temporary failure: 41"))
        (should (= (nelisp-repl-session-retry id) 42))
        ;; No retry happens merely by reading the failure list.
        (should (= attempts 1))
        ;; Reading again returns the same complete history and does not mutate it.
        (should (= (length (nelisp-repl-session-failures)) 1))))))

(ert-deftest nelisp-repl-session-bounds-retry-and-message-safely ()
  "Truncated arguments cannot be retried, including multibyte diagnostics."
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--next-id 1))
    (condition-case nil
        (nelisp-repl-session-call
         (lambda (&rest ignored) (error "日本語の失敗"))
         0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)
      (error nil))
    (let ((failure (car (nelisp-repl-session-failures))))
      (should (plist-get failure :truncated))
      (should-not (plist-get failure :retryable))
      (should (equal (plist-get failure :message) "日本語の失敗"))
      (should-error (nelisp-repl-session-retry (plist-get failure :id))
                    :type 'user-error))))

(ert-deftest nelisp-repl-session-export-replays-in-new-session ()
  "Explicitly registered forms and files replay when loaded by a new REPL."
  (let ((source (make-temp-file "nelisp-repl-source-" nil ".el"))
        (export (make-temp-file "nelisp-repl-export-" nil ".el")))
    (unwind-protect
        (progn
          (with-temp-file source
            (insert "(setq nelisp-repl-loaded-value 23)\n"))
          (let ((nelisp-repl-session--records nil))
            (nelisp-repl-session-record-setting 'nelisp-repl-recorded-value '(17 18))
            (nelisp-repl-session-record '(setq nelisp-repl-form-value 19))
            (nelisp-repl-session-record-load source)
            (nelisp-repl-session-export export)
            (should (= (length nelisp-repl-session--records) 3))
            (should (= (length (plist-get (cl-find :load nelisp-repl-session--records
                                                  :key (lambda (record)
                                                         (plist-get record :kind)))
                                          :sha256))
                       64)))
          ;; This is a fresh registration context: loading the exported replay
          ;; executes only the forms explicitly registered above.
          (let ((nelisp-repl-session--records nil))
            (makunbound 'nelisp-repl-recorded-value)
            (makunbound 'nelisp-repl-form-value)
            (makunbound 'nelisp-repl-loaded-value)
            (load export nil nil t)
            (should (equal (symbol-value 'nelisp-repl-recorded-value) '(17 18)))
            (should (= (symbol-value 'nelisp-repl-form-value) 19))
            (should (= (symbol-value 'nelisp-repl-loaded-value) 23))))
      (delete-file source)
      (delete-file export))))

(ert-deftest nelisp-repl-session-rejects-cycles-and-unreadable-values ()
  "Session recording rejects values which cannot be safely read back."
  (let ((nelisp-repl-session--records nil)
        (cycle (list 'secret)))
    (setcdr cycle cycle)
    (should-error (nelisp-repl-session-record cycle))
    (should-error (nelisp-repl-session-record (make-hash-table)))
    ;; Secrets are not collected from bindings by a successful call or export.
    (let ((secret "do-not-export")
          (export (make-temp-file "nelisp-repl-secret-" nil ".el")))
      (unwind-protect
          (progn
            (should (= (nelisp-repl-session-call #'identity 5) 5))
            (nelisp-repl-session-record '(setq nelisp-repl-safe-value 1))
            (nelisp-repl-session-export export)
            (with-temp-buffer
              (insert-file-contents-literally export)
              (should-not (search-forward secret nil t))))
        (delete-file export)))))

(ert-deftest nelisp-repl-session-clear-releases-history ()
  "The explicit clear operation releases retained failure and replay state."
  (nelisp-repl-session-clear)
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--records nil)
        (nelisp-repl-session--next-id 1))
    (condition-case nil
        (nelisp-repl-session-call (lambda () (error "clear me")))
      (error nil))
    (nelisp-repl-session-record '(setq nelisp-repl-clear-value 1))
    (should (= (length (nelisp-repl-session-failures)) 1))
    (nelisp-repl-session-clear)
    (should-not (nelisp-repl-session-failures))
    (should-not nelisp-repl-session--records)
    (should (= nelisp-repl-session--next-id 1))))

(provide 'nelisp-repl-session-test)

(ert-deftest nelisp-repl-session-cyclic-arguments-retain-identity ()
  "History inspection never traverses argument graphs or changes the retry."
  (let ((nelisp-repl-session--failures nil)
        (nelisp-repl-session--next-id 1)
        (cycle (list 'value)))
    (setcdr cycle cycle)
    (should-error (nelisp-repl-session-call (lambda (_arg) (error "failed")) cycle))
    (let ((record (car (nelisp-repl-session-failures))))
      (should (eq (car (plist-get record :args)) cycle))
      (setcar (plist-get record :args) 'modified)
      (should (eq (car (plist-get (car (nelisp-repl-session-failures)) :args)) cycle)))))

(ert-deftest nelisp-repl-session-print-settings-and-readable-text ()
  "Ambient printer limits and literal unreadable-looking text preserve values."
  (let ((print-length 1) (print-level 1))
    (should (equal (read (nelisp-repl-session--form-string '(list "#<text" (1 2 3))))
                   '(list "#<text" (1 2 3))))))

(ert-deftest nelisp-repl-session-overflow-preserves-setup ()
  "A full replay recipe must not silently drop its earlier setup forms."
  (let ((nelisp-repl-session--records nil)
        (nelisp-repl-session-max-records 1))
    (nelisp-repl-session-record '(setq setup 1))
    (should-error (nelisp-repl-session-record '(use-setup)) :type 'user-error)
    (should-error (nelisp-repl-session-record-load "missing.el") :type 'user-error)
    (should (equal (plist-get (car nelisp-repl-session--records) :form)
                   "(setq setup 1)"))))
;;; nelisp-repl-session-test.el ends here
