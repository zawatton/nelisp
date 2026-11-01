;;; nl-typecheck-recover-test.el --- recovery tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nl-typecheck-recover)

(ert-deftest nl-typecheck-recover-rejected-input-blame-and-retry ()
  (nl-typecheck-recover-clear)
  (let ((nl-typecheck-repl--sigs nil) (nl-typecheck-repl--defs nil)
        (nl-typecheck-repl--callers nil) (nl-typecheck-repl--count 0)
        (nelisp-typecheck-strict t)
        (nelisp-repl-input-check-function #'nl-typecheck-repl-check-input))
    (with-output-to-string
      (nl-typecheck-repl-eval-input
       '(defun rc-fn (n) (declare (ftype (function (integer) integer))) (* n 2)))
      (should-error (nl-typecheck-repl-eval-input '(rc-fn "s")) :type 'nl-typecheck-rejected))
    (let ((id (plist-get (car (last (nelisp-repl-session-failures))) :id)))
      (should (equal (nl-typecheck-recover-blame id)
                     '(:callee rc-fn :param 1 :expected integer :actual-type string)))
      ;; Retrying without a fix is refused again.
      (with-output-to-string
        (should-error (nl-typecheck-recover-retry id) :type 'nl-typecheck-rejected)))))

(ert-deftest nl-typecheck-recover-boundary-failure-retry ()
  (nl-typecheck-recover-clear)
  (let ((nelisp-typecheck-enforce t) (nelisp-repl-input-check-function nil)
        (v "bad"))
    (nl-typecheck-entry-load
     (expand-file-name "fixtures/entry-typed.el"
                       (file-name-directory (or load-file-name buffer-file-name))))
    (defvar nl-typecheck-recover-test--arg)
    (setq nl-typecheck-recover-test--arg v)
    (should-error (nl-typecheck-repl-eval-input
                   '(funcall 'entry-add nl-typecheck-recover-test--arg 1))
                  :type 'wrong-type-argument)
    (let* ((id (plist-get (car (last (nelisp-repl-session-failures))) :id))
           (blame (nl-typecheck-recover-blame id)))
      (should (eq (plist-get blame :callee) 'entry-add))
      (should (eq (plist-get blame :param) 'a))
      (should (eq (plist-get blame :expected) 'integer))
      (should (eq (plist-get blame :actual-type) 'string))
      (setq nl-typecheck-recover-test--arg 41)
      (should (= (nl-typecheck-recover-retry id) 42)))))

(provide 'nl-typecheck-recover-test)
;;; nl-typecheck-recover-test.el ends here
