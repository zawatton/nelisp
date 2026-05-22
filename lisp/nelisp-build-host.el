;;; nelisp-build-host.el --- Doc 49 Wave 7 — NeLisp as build host  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 49 Wave 7 — pure-elisp shims that fill the gap between the
;; nelisp standalone runtime and the Emacs batch-mode functions used by
;; `scripts/compile-elisp-objects.el'.
;;
;; Goal: `target/release/nelisp --batch -L lisp \
;;            -l scripts/compile-elisp-objects.el \
;;            -f compile-elisp-objects-emit-all'
;; runs without Emacs on the build host.
;;
;; This file is NOT embedded in `Env::new_global' (no Rust change).
;; It is loaded on-demand by `nelisp-cli-main' when --batch mode is
;; detected (see `nelisp-cli.el' batch-mode dispatcher).
;;
;; Coverage
;; --------
;;   process-environment  -- Emacs-style list of "VAR=VAL" strings.
;;   getenv / setenv      -- read/write process-environment entries.
;;   add-to-list          -- idempotent list prepend (used for load-path).
;;   make-directory       -- wrapper around the `nl-make-directory' primitive.
;;   system-name          -- hostname via HOSTNAME env var or "localhost".
;;   noninteractive       -- always t in batch mode.
;;   load-path            -- defvar'd to nil; populated by -L argv processing.
;;
;; Intentional omissions:
;;   cl-lib       -- already provided by the nelisp runtime.
;;   require/load -- already in nelisp-stdlib-misc.el.

;;; Code:

;; ---------------------------------------------------------------------------
;; 1. noninteractive — always t in batch mode.
;; ---------------------------------------------------------------------------

;; `noninteractive' is a well-known Emacs global.  In NeLisp batch mode
;; we initialize it to t.  The `defvar' form only sets the default; we
;; force-set it to t afterwards so existing code using `noninteractive'
;; (= `(when noninteractive ...)') sees the right value even if it was
;; previously set to nil by the runtime's default.
(defvar noninteractive nil  ;; noqa: NeLisp builtin-compat
  "Non-nil during NeLisp batch-mode execution (Emacs compat).")
(setq noninteractive t)

;; ---------------------------------------------------------------------------
;; 2. load-path — list of directories searched by `locate-library'.
;; ---------------------------------------------------------------------------

(defvar load-path nil
  "List of directories `require' / `load' search for files.
Populated by the `nelisp --batch -L DIR' CLI arguments before the
first `-l FILE' is processed.")

;; ---------------------------------------------------------------------------
;; 3. process-environment — Emacs-style list of \"VAR=VAL\" strings.
;; ---------------------------------------------------------------------------

(defvar process-environment nil
  "List of environment variable bindings, each a string \"VAR=VAL\".

In NeLisp batch mode this list is authoritative for `getenv' /
`setenv'.  The batch CLI pre-populates it from `--setenv VAR=VAL'
arguments (or via early `--eval (setenv ...)' calls) before loading
any `-l FILE'.")

;; ---------------------------------------------------------------------------
;; 4. getenv / setenv
;; ---------------------------------------------------------------------------

(defun getenv (variable)
  "Return the value of environment variable VARIABLE, or nil.

Looks up VARIABLE in `process-environment' (a list of \"VAR=VAL\"
strings).  Returns the VAL substring on match, nil if not found.
An entry whose value portion is the empty string returns \"\"."
  (let ((prefix (concat variable "="))
        (plen   (+ (length variable) 1))
        (found  nil)
        (cur    process-environment))
    (while (and cur (not found))
      (let ((entry (car cur)))
        (when (and (stringp entry)
                   (>= (length entry) plen)
                   (string-equal (substring entry 0 plen) prefix))
          (setq found (substring entry plen))))
      (setq cur (cdr cur)))
    found))

(defun setenv (variable &optional value substitute)
  "Set environment variable VARIABLE to VALUE in `process-environment'.

If VALUE is nil, remove VARIABLE from the list.  SUBSTITUTE is
accepted for Emacs-API compatibility but ignored.  Returns VALUE."
  (let ((prefix (concat variable "="))
        (plen   (+ (length variable) 1))
        (new-env nil)
        (found  nil))
    ;; Walk the list, rebuild without the old entry.
    (let ((cur process-environment))
      (while cur
        (let ((entry (car cur)))
          (if (and (stringp entry)
                   (>= (length entry) plen)
                   (string-equal (substring entry 0 plen) prefix))
              (setq found t)           ; drop old entry
            (setq new-env (cons entry new-env))))
        (setq cur (cdr cur))))
    (setq new-env (nreverse new-env))
    ;; Prepend new entry when VALUE is given.
    (when value
      (setq new-env (cons (concat variable "=" value) new-env)))
    (setq process-environment new-env)
    value))

;; ---------------------------------------------------------------------------
;; 5. add-to-list — idempotent list prepend.
;; ---------------------------------------------------------------------------

(defun add-to-list (list-var element &optional append compare-fn)
  "Add ELEMENT to the value of LIST-VAR if not already present.

APPEND non-nil means append rather than prepend.  COMPARE-FN
defaults to `equal'.  Returns the (possibly updated) list value."
  (let* ((lst     (symbol-value list-var))
         (cmp-fn  (or compare-fn 'equal))
         (present (let ((cur lst))
                    (catch 'hit
                      (while cur
                        (when (funcall cmp-fn (car cur) element)
                          (throw 'hit t))
                        (setq cur (cdr cur)))
                      nil))))
    (unless present
      (if append
          (set list-var (append lst (list element)))
        (set list-var (cons element lst))))
    (symbol-value list-var)))

;; ---------------------------------------------------------------------------
;; 6. make-directory — forward to nl-make-directory primitive.
;; ---------------------------------------------------------------------------

(defun make-directory (dir &optional parents)
  "Create directory DIR.

When PARENTS is non-nil, create any missing parent directories
first (like `mkdir -p').  Signals `file-error' on failure.

Delegates to the `nl-make-directory' Rust primitive which accepts
a path string and a boolean.  Returns nil on success, matching the
Emacs contract."
  (let ((rc (nl-make-directory dir (if parents t nil))))
    (when (and (integerp rc) (< rc 0))
      (signal 'file-error (list "Cannot create directory" dir rc)))
    nil))

;; ---------------------------------------------------------------------------
;; 7. system-name — hostname from environment or static fallback.
;; ---------------------------------------------------------------------------

(defun system-name ()
  "Return the host name of the current machine.

In NeLisp batch mode we read the HOSTNAME environment variable;
if unset, fall back to \"localhost\"."
  (or (getenv "HOSTNAME") "localhost"))

;; ---------------------------------------------------------------------------
;; 8. cl-lib shims — `require 'cl-lib' already works in the runtime,
;;    but add-to-list is missing from the pre-load environment so the
;;    `(add-to-list 'load-path ...)' form in compile-elisp-objects.el
;;    fails before it can demand-load us.  Nothing to do here: the -L
;;    processing in nelisp-cli-batch-dispatch installs add-to-list and
;;    load-path BEFORE anything is loaded.
;; ---------------------------------------------------------------------------

(provide 'nelisp-build-host)

;;; nelisp-build-host.el ends here
