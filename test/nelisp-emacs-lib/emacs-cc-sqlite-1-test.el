;;; emacs-cc-sqlite-1-test.el --- SQLite C fallback tests -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)

(defconst emacs-cc-sqlite-1-test--root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-cc-sqlite-1-test--load-paths ()
  (list (expand-file-name "packages/nelisp-emacs-foundation/src"
                          emacs-cc-sqlite-1-test--root)
        (expand-file-name "packages/nelisp-emacs-io/src"
                          emacs-cc-sqlite-1-test--root)))

(defun emacs-cc-sqlite-1-test--load-unit ()
  (let ((load-path (append (emacs-cc-sqlite-1-test--load-paths) load-path)))
    (load (expand-file-name
           "packages/nelisp-emacs-foundation/src/emacs-cc-sqlite-1.el"
           emacs-cc-sqlite-1-test--root)
          nil t)))

(ert-deftest emacs-cc-sqlite-1-does-not-replace-host-bindings ()
  (let* ((names '(sqlite-available-p sqlite-open sqlite-close sqlitep
                  sqlite-execute sqlite-execute-batch sqlite-select
                  sqlite-columns sqlite-finalize sqlite-load-extension
                  sqlite-more-p sqlite-next sqlite-version))
         (before (mapcar (lambda (name) (cons name (symbol-function name))) names)))
    (emacs-cc-sqlite-1-test--load-unit)
    (dolist (entry before)
      (should (eq (symbol-function (car entry)) (cdr entry))))))

(ert-deftest emacs-cc-sqlite-1-missing-optional-provider-installs-no-ffi-callers ()
  (let* ((names '(emacs-sqlite-ffi--call nelisp-sqlite-open
                  nelisp-sqlite-close nelisp-sqlitep))
         (before (mapcar (lambda (name)
                           (cons name (and (fboundp name) (symbol-function name))))
                         names))
         (saved-batch (and (fboundp 'sqlite-execute-batch)
                           (symbol-function 'sqlite-execute-batch)))
         (saved-features features))
    (unwind-protect
        (progn
          (dolist (name names) (fmakunbound name))
          (fmakunbound 'sqlite-execute-batch)
          (let ((features (delq 'emacs-sqlite-ffi (copy-sequence features)))
                (load-path nil))
            (emacs-cc-sqlite-1-test--load-unit))
          (dolist (name names) (should-not (fboundp name)))
          (should-error (sqlite-execute-batch nil "select 1")
                        :type 'sqlite-error))
      (setq features saved-features)
      (if saved-batch (fset 'sqlite-execute-batch saved-batch)
        (fmakunbound 'sqlite-execute-batch))
      (dolist (entry before)
        (if (cdr entry) (fset (car entry) (cdr entry))
          (fmakunbound (car entry)))))))

(ert-deftest emacs-cc-sqlite-1-defines-local-provider-only-with-ffi-capability ()
  (let ((saved-open (and (fboundp 'nelisp-sqlite-open)
                         (symbol-function 'nelisp-sqlite-open)))
        (saved-p (and (fboundp 'nelisp-sqlitep)
                      (symbol-function 'nelisp-sqlitep))))
    (unwind-protect
        (progn
          (fmakunbound 'nelisp-sqlite-open)
          (fmakunbound 'nelisp-sqlitep)
          (cl-letf (((symbol-function 'nl-ffi-call) (lambda (&rest _) nil)))
            (emacs-cc-sqlite-1-test--load-unit))
          (should (fboundp 'nelisp-sqlite-open))
          (should (fboundp 'nelisp-sqlitep))
          (should-not (nelisp-sqlitep [not-a-sqlite-object])))
      (if saved-open (fset 'nelisp-sqlite-open saved-open)
        (fmakunbound 'nelisp-sqlite-open))
      (if saved-p (fset 'nelisp-sqlitep saved-p)
        (fmakunbound 'nelisp-sqlitep)))))

(ert-deftest emacs-cc-sqlite-1-batch-fallback-uses-capability-and-preserves-errors ()
  (let ((saved (symbol-function 'sqlite-execute-batch))
        (calls nil))
    (unwind-protect
        (progn
          (let ((load-path (append (emacs-cc-sqlite-1-test--load-paths)
                                   load-path)))
            (require 'emacs-sqlite-ffi))
          (fmakunbound 'sqlite-execute-batch)
          (cl-letf (((symbol-function 'nl-ffi-call) (lambda (&rest _) nil))
                    ((symbol-function 'emacs-sqlite-ffi--handle)
                     (lambda (db) (if (eq db 'database) 17
                                    (signal 'wrong-type-argument
                                            (list 'sqlitep db)))))
                    ((symbol-function 'emacs-sqlite-ffi--cstring)
                     (lambda (text) text))
                    ((symbol-function 'emacs-sqlite-ffi--call)
                     (lambda (name &rest args)
                       (push (cons name args) calls)
                       0)))
            (emacs-cc-sqlite-1-test--load-unit)
            (should (sqlite-execute-batch 'database "create table t(x)"))
            (should (equal (caar calls) "sqlite3_exec"))
            (should (equal (cddr (car calls)) '("create table t(x)" 0 0 0)))
            (should-error (sqlite-execute-batch 'database nil)
                          :type 'wrong-type-argument)
            (should-error (sqlite-execute-batch nil "select 1")
                          :type 'wrong-type-argument)))
      (fset 'sqlite-execute-batch saved))))

(provide 'emacs-cc-sqlite-1-test)

;;; emacs-cc-sqlite-1-test.el ends here
