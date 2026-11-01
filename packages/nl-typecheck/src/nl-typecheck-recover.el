;;; nl-typecheck-recover.el --- Doc 236 layer 4: recovery -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Host driver for REPL inputs.  Refused inputs and boundary failures are
;; recorded through `nelisp-repl-session-call' together with a blame plist
;; (:callee :param :expected :actual-type).  After the user redefines the
;; caller or callee, `nl-typecheck-recover-retry' re-runs a failure by its
;; explicit ID.

;;; Code:

(require 'nl-typecheck-repl)
(require 'nl-typecheck-entry)
(require 'nelisp-repl-session)

(defvar nl-typecheck-recover--blames nil "Alist: failure ID -> blame plist.")

(defun nl-typecheck-repl--run-input (form)
  "Check FORM through the installed hook (if any), then evaluate it."
  (if (or (null nelisp-repl-input-check-function)
          (funcall nelisp-repl-input-check-function form))
      (eval form t)
    nil))

(defun nl-typecheck-recover--blame-of (err)
  "Return the blame plist for the error ERR."
  (cond
   ((eq (car err) 'nl-typecheck-rejected) (nth 2 err))
   ((eq (car err) 'wrong-type-argument)
    (copy-sequence nl-typecheck-entry-last-blame))))

(defun nl-typecheck-repl-eval-input (form)
  "Evaluate REPL input FORM; record any failure with its blame."
  (condition-case err
      (nelisp-repl-session-call #'nl-typecheck-repl--run-input form)
    (error
     (let ((last (car (last (nelisp-repl-session-failures)))))
       (when last
         (setq nl-typecheck-recover--blames
               (cons (cons (plist-get last :id) (nl-typecheck-recover--blame-of err))
                     nl-typecheck-recover--blames))))
     (signal (car err) (cdr err)))))

(defun nl-typecheck-recover-blame (id)
  "Return the blame plist recorded for failure ID."
  (cdr (assq id nl-typecheck-recover--blames)))

(defun nl-typecheck-recover-retry (id)
  "Retry failure ID explicitly, re-running the input in the current environment."
  (nelisp-repl-session-retry id))

(defun nl-typecheck-recover-clear ()
  "Forget recorded failures and blames."
  (setq nl-typecheck-recover--blames nil)
  (nelisp-repl-session-clear))

(provide 'nl-typecheck-recover)
;;; nl-typecheck-recover.el ends here
