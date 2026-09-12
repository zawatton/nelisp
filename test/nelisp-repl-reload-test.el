;;; nelisp-repl-reload-test.el --- module defun selection tests -*- lexical-binding: t; -*-
(require 'ert)
(require 'nelisp-repl-reload)

(defconst nelisp-repl-reload-test--module
  ";;; m.el --- a module  -*- lexical-binding: t; -*-
(require 'cl-lib)

(defvar nelisp-repl-reload-test--var 1)

;; A comment before the function.
(defun nelisp-repl-reload-test--fn (x)
  (+ x nelisp-repl-reload-test--var))

(defun nelisp-repl-reload-test--other (y)
  (* y 2))

(provide 'm)
")

(defun nelisp-repl-reload-test--file (text)
  (let ((path (make-temp-file "nelisp-repl-reload-test-" nil ".el"))
        (coding-system-for-write 'utf-8-unix))
    (with-temp-file path (insert text))
    path))

(ert-deftest nelisp-repl-reload/selects-only-defuns ()
  (let ((path (nelisp-repl-reload-test--file nelisp-repl-reload-test--module)))
    (unwind-protect
        (let ((plan (nelisp-repl-reload-select path)))
          (should (equal (plist-get plan :selected)
                         '(nelisp-repl-reload-test--fn
                           nelisp-repl-reload-test--other)))
          ;; The load-time forms are named, not run.
          (should (equal (mapcar #'car (plist-get plan :skipped))
                         '(require defvar provide)))
          (should (null (plist-get plan :missing))))
      (delete-file path))))

(ert-deftest nelisp-repl-reload/staged-source-keeps-the-original-bytes ()
  (let ((path (nelisp-repl-reload-test--file nelisp-repl-reload-test--module)))
    (unwind-protect
        (let ((source (plist-get (nelisp-repl-reload-select path) :source)))
          (should (string-match-p
                   "(defun nelisp-repl-reload-test--fn (x)\n  (\\+ x nelisp-repl-reload-test--var))"
                   source))
          ;; Nothing that would re-run on load survives into the staging file.
          (should-not (string-match-p "(require 'cl-lib)" source))
          (should-not (string-match-p "(provide 'm)" source))
          ;; No form that would assign anything at load time survives.
          (should-not (string-match-p "(defvar nelisp-repl-reload-test--var"
                                      source)))
      (delete-file path))))

(ert-deftest nelisp-repl-reload/reports-the-modules-variables ()
  (let ((path (nelisp-repl-reload-test--file
               (concat nelisp-repl-reload-test--module
                       "(defconst nelisp-repl-reload-test--const 2)\n"
                       "(defcustom nelisp-repl-reload-test--opt 3 \"doc\""
                       " :type 'integer :group 'test)\n"))))
    (unwind-protect
        (let ((plan (nelisp-repl-reload-select path)))
          (should (equal (plist-get plan :declared)
                         '(nelisp-repl-reload-test--var
                           nelisp-repl-reload-test--const
                           nelisp-repl-reload-test--opt)))
          (should-not (string-match-p ":type 'integer"
                                      (plist-get plan :source))))
      (delete-file path))))

(ert-deftest nelisp-repl-reload/copies-the-lexical-binding-cookie ()
  (let ((with-cookie (nelisp-repl-reload-test--file
                      nelisp-repl-reload-test--module))
        (without-cookie (nelisp-repl-reload-test--file
                         "(defun nelisp-repl-reload-test--plain (x) x)\n")))
    (unwind-protect
        (progn
          (should (string-prefix-p
                   ";;; m.el --- a module  -*- lexical-binding: t; -*-"
                   (plist-get (nelisp-repl-reload-select with-cookie) :source)))
          (should (string-prefix-p
                   "(defun nelisp-repl-reload-test--plain"
                   (plist-get (nelisp-repl-reload-select without-cookie)
                              :source))))
      (delete-file with-cookie)
      (delete-file without-cookie))))

(ert-deftest nelisp-repl-reload/names-limit-the-selection ()
  (let ((path (nelisp-repl-reload-test--file nelisp-repl-reload-test--module)))
    (unwind-protect
        (let ((plan (nelisp-repl-reload-select
                     path 'nelisp-repl-reload-test--other)))
          (should (equal (plist-get plan :selected)
                         '(nelisp-repl-reload-test--other)))
          (should-not (string-match-p "nelisp-repl-reload-test--fn (x)"
                                      (plist-get plan :source))))
      (delete-file path))))

(ert-deftest nelisp-repl-reload/unknown-name-is-refused-before-staging ()
  (let ((path (nelisp-repl-reload-test--file nelisp-repl-reload-test--module)))
    (unwind-protect
        (let ((result (nelisp-repl-reload-defuns path 'no-such-function)))
          (should (eq (plist-get result :status) 'rejected))
          (should (eq (plist-get result :phase) 'select))
          (should (equal (plist-get result :missing) '(no-such-function)))
          (should (null (plist-get result :staged-source))))
      (delete-file path))))

(ert-deftest nelisp-repl-reload/module-without-defuns-is-refused ()
  (let ((path (nelisp-repl-reload-test--file "(require 'cl-lib)\n(provide 'm)\n")))
    (unwind-protect
        (let ((result (nelisp-repl-reload-defuns path)))
          (should (eq (plist-get result :status) 'rejected))
          (should (equal (car (plist-get result :reason)) :no-defun-selected)))
      (delete-file path))))

(provide 'nelisp-repl-reload-test)

;;; nelisp-repl-reload-test.el ends here
