;;; nl-typecheck-recover-test.el --- recovery tests -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Code:

(require 'ert)
(require 'nl-typecheck-recover)

(defconst nl-typecheck-recover-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of this file, captured at load time.
`load-file-name' is nil when the test body runs after loading.")

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
                       nl-typecheck-recover-test--dir))
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

(ert-deftest nl-typecheck-recover-mode-records ()
  (nl-typecheck-recover-clear)
  (let ((nl-typecheck-repl--sigs nil) (nl-typecheck-repl--defs nil)
        (nl-typecheck-repl--callers nil) (nl-typecheck-repl--count 0)
        (nelisp-typecheck-strict t))
    (unwind-protect
        (progn
          (nelisp-typecheck-repl-mode 1)
          (should (eq nelisp-repl-input-eval-function
                      #'nl-typecheck-repl-eval-input))
          (let ((before (length (nelisp-repl-session-failures)))
                (run (if (fboundp 'nelisp--repl-input-run)
                         #'nelisp--repl-input-run
                       nelisp-repl-input-eval-function)))
            (with-output-to-string
              (funcall run '(defun rm-fn (n)
                              (declare (ftype (function (integer) integer)))
                              (* n 2)))
              (should-error (funcall run '(rm-fn "s"))
                            :type 'nl-typecheck-rejected))
            (should (= (length (nelisp-repl-session-failures)) (1+ before)))
            (let ((id (plist-get (car (last (nelisp-repl-session-failures))) :id)))
              (should (nl-typecheck-recover-blame id)))))
      (nelisp-typecheck-repl-mode 0)
      (nl-typecheck-recover-clear))))

(ert-deftest nl-typecheck-recover-mode-off-negative-control ()
  (nl-typecheck-recover-clear)
  (nelisp-typecheck-repl-mode 0)
  (should-not nelisp-repl-input-check-function)
  (should-not nelisp-repl-input-eval-function)
  (let ((before (length (nelisp-repl-session-failures))))
    (should-error (eval '(car 1) t))
    (should (= (length (nelisp-repl-session-failures)) before))))

(provide 'nl-typecheck-recover-test)
;;; nl-typecheck-recover-test.el ends here
