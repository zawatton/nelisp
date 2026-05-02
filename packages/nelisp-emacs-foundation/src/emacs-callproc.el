;;; emacs-callproc.el --- NeLisp port of Emacs C core callproc.c env API  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 1.6 — Layer 2.
;;
;; Ports `getenv' / `setenv' / `process-environment' from Emacs C
;; core's `callproc.c'.  Real OS access requires a syscall route;
;; Phase 1.6 ships a STUB that always returns nil for `getenv' and
;; silently no-ops `setenv'.  Phase 2 will route through NeLisp's
;; `nelisp-syscall-types' extension once that crate exposes a
;; user-visible `nelisp-syscall-types-getenv' (or equivalent).
;;
;; Stub semantics are deliberate: most callers (= anvil-memory's
;; `anvil-memory-effective-db-path' for example) treat a missing env
;; var as "use my default".  Returning nil from `getenv' makes the
;; default branch fire, which is exactly what we want until the real
;; route lands.  Callers that NEED the env var (= scripts driven by
;; an explicit ANVIL_FOO=...) will surface as "bug — expected env
;; override but got default", and we know to wire `getenv' through
;; the syscall extension at that point.

;;; Code:

(defvar process-environment nil
  "List of `KEY=VALUE' strings — the polyfill keeps it nil so callers
that walk it explicitly find no entries.  Phase 2 will populate from
the host env at startup via the NeLisp syscall extension.")

(unless (fboundp 'getenv)
  (defun getenv (variable &optional frame)
    "Polyfill: look VARIABLE up in `process-environment'.
Phase 1.6 keeps `process-environment' nil so this always returns nil
and callers fall through to their default branch.  Phase 2 replaces
the implementation with a NeLisp syscall route."
    (ignore frame)
    (let ((cur process-environment)
          (prefix (concat variable "="))
          (prefix-len 0)
          (found nil)
          (result nil))
      (setq prefix-len (length prefix))
      (while (and cur (not found))
        (let ((entry (car cur)))
          (if (and (>= (length entry) prefix-len)
                   (equal (substring entry 0 prefix-len) prefix))
              (progn (setq result (substring entry prefix-len))
                     (setq found t))))
        (setq cur (cdr cur)))
      result)))

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
