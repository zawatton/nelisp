;;; nelisp-windows-sqlite-test.el --- SQLite FFI initialization -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'ert)
(require 'cl-lib)
(require 'nelisp-standalone-build)

(defun nelisp-windows-sqlite-test--lower (form)
  "Make the dispatch IR executable with mocked C calls under host Emacs."
  (if (not (consp form)) form
    (cond ((eq (car form) 'extern-call)
           `(funcall sqlite-extern ',(cadr form)
                     ,@(mapcar #'nelisp-windows-sqlite-test--lower (cddr form))))
          ((eq (car form) 'seq)
           (cons 'progn (mapcar #'nelisp-windows-sqlite-test--lower (cdr form))))
          (t (mapcar #'nelisp-windows-sqlite-test--lower form)))))

(defun nelisp-windows-sqlite-test--run (target init-result)
  "Execute the real TARGET dispatch arm, injecting INIT-RESULT."
  (let* ((table (if (eq target 'windows-x86_64)
                    (nelisp-standalone--windows-reader-extern-table)
                  nelisp-standalone--reader-extern-table))
         (form (nelisp-standalone--build-ffi-dispatch table target))
         (calls nil) (result nil)
         (external (lambda (name &rest args)
                     (push (cons name args) calls)
                     (if (eq name 'sqlite3_initialize) init-result 17))))
    (cl-letf (((symbol-function 'wf_arg_ptr) (lambda (args i) (nth i args)))
              ((symbol-function 'wf_argval) (lambda (args i) (nth i args)))
              ((symbol-function 'sexp-name-eq) (lambda (a b) (if (equal a b) 1 0)))
              ((symbol-function 'wf_write_int_w) (lambda (_pad value) value)))
      ;; WORD dispatch returns its result; OUT is deliberately absent so a
      ;; legacy write cannot be hidden by a fixture-only output slot.
      (setq result
            (eval (nelisp-windows-sqlite-test--lower form)
                  `((args . ("sqlite3_open_v2" 100 200 6 0))
                    (sqlite-extern . ,external)))))
    (list result (nreverse calls))))

(ert-deftest nelisp-windows-sqlite-initializes-before-open ()
  (should (equal (nelisp-windows-sqlite-test--run 'windows-x86_64 0)
                 '(17 ((sqlite3_initialize) (sqlite3_open_v2 100 200 6 0)))))
  (should (assoc "sqlite3_initialize" (nelisp-standalone--windows-reader-extern-table))))

(ert-deftest nelisp-windows-sqlite-initialize-error-does-not-open ()
  (should (equal (nelisp-windows-sqlite-test--run 'windows-x86_64 7)
                 '(7 ((sqlite3_initialize))))))

(ert-deftest nelisp-windows-sqlite-linux-retains-direct-open ()
  (should (equal (nelisp-windows-sqlite-test--run 'linux-x86_64 7)
                 '(17 ((sqlite3_open_v2 100 200 6 0))))))

(ert-deftest nelisp-windows-sqlite-initializer-is-real-pe-import ()
  "Catch a malformed DLL identity before spending minutes building units."
  (cl-letf (((symbol-function 'nelisp-standalone-arena-rewrite-target)
             (lambda () 'windows-x86_64)))
    (let ((imports (nelisp-standalone--reader-pe-imports)))
      (should (cl-every (lambda (group) (stringp (car group))) imports))
      (should (member "sqlite3_initialize" (cdr (assoc "winsqlite3.dll" imports)))))))

;;; nelisp-windows-sqlite-test.el ends here
