;;; emacs-callproc.el --- NeLisp port of Emacs C core callproc.c env API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Ports `getenv' / `setenv' / `process-environment' from Emacs C
;; core's `callproc.c'.  The bridge first honors the elisp
;; `process-environment' overlay, then falls through to a NeLisp
;; syscall/runtime getenv primitive when one is bound.

;;; Code:

(defvar process-environment nil
  "List of `KEY=VALUE' strings used as the elisp environment overlay.")

(declare-function nl-syscall-getenv "nelisp-runtime" (variable))
(declare-function nelisp-sys-getenv "nelisp-sys" (variable))

(defconst emacs-callproc--sys-getenv-functions
  '(nelisp-sys-getenv nl-syscall-getenv)
  "Candidate NeLisp runtime getenv functions, in preferred order.")

(defvar emacs-callproc--sys-getenv-active nil
  "Non-nil while `emacs-callproc--sys-getenv' is inside a backend call.")

(defun emacs-callproc--lookup-process-environment (variable)
  "Return VARIABLE from `process-environment', or nil when absent."
  (let ((cur process-environment)
        (prefix (concat variable "="))
        (prefix-len 0)
        (found nil)
        (result nil))
    (setq prefix-len (length prefix))
    (while (and cur (not found))
      (let ((entry (car cur)))
        (if (and (stringp entry)
                 (>= (length entry) prefix-len)
                 (equal (substring entry 0 prefix-len) prefix))
            (progn (setq result (substring entry prefix-len))
                   (setq found t))))
      (setq cur (cdr cur)))
    result))

(defun emacs-callproc--sys-getenv (variable)
  "Return VARIABLE from a NeLisp getenv primitive, or nil if unavailable."
  (unless emacs-callproc--sys-getenv-active
    (let ((emacs-callproc--sys-getenv-active t))
      (catch 'done
        (dolist (fn emacs-callproc--sys-getenv-functions)
          (when (fboundp fn)
            (let ((value (condition-case nil
                             (funcall fn variable)
                           (error nil))))
              (when (stringp value)
                (throw 'done value)))))
        nil))))

(defun emacs-callproc-getenv (variable &optional frame)
  "Look VARIABLE up in the elisp overlay, then the NeLisp runtime env."
  (ignore frame)
  (or (emacs-callproc--lookup-process-environment variable)
      (emacs-callproc--sys-getenv variable)))

(unless (fboundp 'getenv)
  (defun getenv (variable &optional frame)
    "Polyfill: look VARIABLE up in the NeLisp-compatible environment."
    (emacs-callproc-getenv variable frame)))

(unless (fboundp 'setenv)
  (defun setenv (variable &optional value substitute-env-vars)
    "Polyfill: prepend `VARIABLE=VALUE' to `process-environment'.
Returns VALUE.  When VALUE is nil this removes the entry (= matches
Emacs C semantics).  SUBSTITUTE-ENV-VARS is accepted for arglist
parity and ignored (= no `$VAR' interpolation in the polyfill)."
    (ignore substitute-env-vars)
    ;; Strip any existing entry for VARIABLE.
    (let ((prefix (concat variable "="))
          (prefix-len 0)
          (acc nil)
          (cur process-environment))
      (setq prefix-len (length prefix))
      (while cur
        (let ((entry (car cur)))
          (unless (and (>= (length entry) prefix-len)
                       (equal (substring entry 0 prefix-len) prefix))
            (setq acc (cons entry acc))))
        (setq cur (cdr cur)))
      ;; Reverse acc back to original order, then prepend new entry.
      (let ((forward nil))
        (while acc
          (setq forward (cons (car acc) forward))
          (setq acc (cdr acc)))
        (setq process-environment
              (if value
                  (cons (concat variable "=" value) forward)
                forward))))
    value))


(provide 'emacs-callproc)

;;; emacs-callproc.el ends here
