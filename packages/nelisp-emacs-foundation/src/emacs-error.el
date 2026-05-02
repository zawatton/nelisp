;;; emacs-error.el --- NeLisp port of Emacs error / signal primitives  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 2 — Layer 2.
;;
;; Ports `user-error', `signal' helpers, and the small set of error /
;; warning primitives that real anvil modules use during load and tool
;; dispatch.  All polyfills are gated on `unless (fboundp ...)' so they
;; remain inert under host Emacs.

;;; Code:

;; `user-error' — like `error' but flagged in Emacs as a "user mistake"
;; so commands can suppress the trace dump.  In standalone mode the
;; distinction is moot — Rust translates either to a JSON-RPC error.
(unless (fboundp 'user-error)
  (defun user-error (format-string &rest args)
    "Polyfill: signal a user-level error.
Equivalent to `error' under NeLisp standalone."
    (signal 'user-error (list (apply #'format format-string args)))))

;; `display-warning' — Emacs warning surface (= shows in *Warnings*).
;; Standalone has no buffer; route to `message' so the warning goes to
;; stderr at minimum.  TYPE / MESSAGE / LEVEL accepted for arglist parity.
(unless (fboundp 'display-warning)
  (defun display-warning (type message &optional level buffer-name)
    "Polyfill: print warning to message-stream (= stderr in batch)."
    (ignore level buffer-name)
    (message "Warning [%s]: %s" type message)))

;; `define-error' — Emacs 24.4+ builtin for declaring custom error
;; symbols.  Sets `error-conditions' / `error-message' on NAME.
;; Polyfill stores the same properties so `(signal 'NAME ...)' and
;; `condition-case' interact correctly.
(unless (fboundp 'define-error)
  (defun define-error (name message &optional parent)
    "Polyfill: define an error symbol NAME with MESSAGE under PARENT."
    (let ((conditions (cons name
                            (or (and parent
                                     (get parent 'error-conditions))
                                '(error)))))
      (put name 'error-conditions conditions)
      (put name 'error-message message))
    name))


(provide 'emacs-error)

;;; emacs-error.el ends here
