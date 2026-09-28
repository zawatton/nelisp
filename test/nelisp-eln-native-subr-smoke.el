;;; nelisp-eln-native-subr-smoke.el --- managed native subr smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

(require 'nelisp-eln-system-loader)
(require 'nelisp-eln-native-subr)

(defun nelisp-eln-native-subr-smoke--check (condition message)
  (unless condition (error "NativeSubr smoke failed: %s" message)))

(defun nelisp-eln-native-subr-smoke--error (thunk)
  (condition-case err
      (progn (funcall thunk) nil)
    (error (car err))))

(defvar nelisp-eln-native-subr-smoke--handle nil)
(defvar nelisp-eln-native-subr-smoke--module-id nil)
(defvar nelisp-eln-native-subr-smoke--native nil)

(defun nelisp-eln-native-subr-smoke-run ()
  "Check scalar0 invocation, predicates, arity, identity, and live lease."
  (let* ((path (getenv "NELISP_TEST_ELN_PATH"))
         (name (getenv "NELISP_TEST_ELN_LEAF"))
         (handle (or nelisp-eln-native-subr-smoke--handle
                     (nelisp-eln-system-loader-open path))))
    (nelisp-eln-native-subr-smoke--check path "missing fixture path")
    (nelisp-eln-native-subr-smoke--check name "missing fixture leaf")
    (setq nelisp-eln-native-subr-smoke--handle handle
          nelisp-eln-native-subr-smoke--module-id
          (nelisp-eln-system-loader-module-id handle)
          nelisp-eln-native-subr-smoke--native
          (nelisp-eln-native-subr-create handle name))
    (let ((native nelisp-eln-native-subr-smoke--native))
      (nelisp-eln-native-subr-smoke--check
       (and (subrp native) (functionp native) (eq (type-of native) 'subr))
       "native function predicates/type")
      (nelisp-eln-native-subr-smoke--check
       (= (funcall native) 17) "native scalar0 return")
      (nelisp-eln-native-subr-smoke--check
       (eq (nelisp-eln-native-subr-smoke--error
            (lambda ()
              (nelisp-eln-native-subr-create handle "top_level_run")))
           'nelisp-eln-native-subr-error)
       "non-leaf top_level_run admission")
      (nelisp-eln-native-subr-smoke--check
       (eq (nelisp-eln-native-subr-smoke--error
            (lambda () (funcall native 1)))
           'wrong-number-of-arguments)
       "wrong arity must fail before native entry")
      (fset 'nelisp-eln-native-subr-smoke-function native)
      (nelisp-eln-native-subr-smoke--check
       (eq native (symbol-function 'nelisp-eln-native-subr-smoke-function))
       "fset/symbol-function identity")
      (nelisp-eln-native-subr-smoke--check
       (= (funcall 'nelisp-eln-native-subr-smoke-function) 17)
       "fset native call")
      (fmakunbound 'nelisp-eln-native-subr-smoke-function)
      (garbage-collect)
      (nelisp-eln-native-subr-smoke--check
       (= (nelisp--native-subr-live-count
           nelisp-eln-native-subr-smoke--module-id) 1)
       "captured native function lease after fmakunbound and GC")
      (nelisp-eln-native-subr-smoke--check
       (eq (nelisp-eln-native-subr-smoke--error
            (lambda () (nelisp-eln-system-loader-close handle)))
           'nelisp-eln-system-loader-error)
       "module close must be refused while callable is live"))))

(defun nelisp-eln-native-subr-smoke-release-run ()
  "Release the callable only after the check frame has returned."
  (setq nelisp-eln-native-subr-smoke--native nil)
  (garbage-collect)
  (nelisp-eln-native-subr-smoke--check
   (= (nelisp--native-subr-live-count nelisp-eln-native-subr-smoke--module-id) 0)
   "weak lease must disappear after native function becomes unreachable")
  t)

(defun nelisp-eln-native-subr-smoke-close-run ()
  (nelisp-eln-native-subr-smoke--check
   (nelisp-eln-system-loader-close nelisp-eln-native-subr-smoke--handle)
   "module close after native function collection")
  t)

(nelisp-eln-native-subr-smoke-run)
(nelisp-eln-native-subr-smoke-release-run)
(nelisp-eln-native-subr-smoke-close-run)
(princ "NELISP-ELN-NATIVE-SUBR-SMOKE-PASS\n")
nil

;;; nelisp-eln-native-subr-smoke.el ends here
