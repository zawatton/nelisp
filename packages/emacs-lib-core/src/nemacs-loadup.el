;;; nemacs-loadup.el --- nemacs bootstrap entry point  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track J (2026-05-03) — Layer 2.
;;
;; The bootstrap glue that turns NeLisp + the Layer 2 Emacs C-core
;; ports into a runnable `nemacs' (= NeLisp-cored Emacs).
;; Mirrors Emacs's classic `loadup.el' role: pulls the dependency
;; graph in order, sets up the initial buffer, fires the startup
;; hook, and signals readiness.
;;
;; Use:
;;
;;   nemacs --batch -l nemacs-loadup -f nemacs-init [...]
;;
;; or from elisp:
;;
;;   (require 'nemacs-loadup)
;;   (nemacs-init)
;;
;; The bootstrap is idempotent — `nemacs-init' guards on
;; `nemacs-initialized' and signals if called twice; tests reset
;; via `nemacs-uninit'.
;;
;; GNU startup.el processes the command line and init files.  The
;; terminal frontend binds its own input and redisplay backend.
;;
;; Track L (2026-05-03): dump file save/load is wired here as
;; `nemacs-save-dump' / `nemacs-load-dump' helpers; the underlying
;; engine lives in `emacs-dump.el'.

;;; Code:

(require 'emacs-init)
(require 'emacs-dump)

;;;; --- version + hook surface ----------------------------------------

(defconst nemacs-version "0.1.0-mvp"
  "Current nemacs version.  Format: SEMVER + suffix tag.")

(defvar nemacs-startup-hook nil
  "Hook run by `nemacs-init' after the bootstrap completes.")

(defvar nemacs-initialized nil
  "Non-nil once `nemacs-init' has run.  Reset by `nemacs-uninit'.")

(defvar nemacs--initial-buffer nil
  "The `*scratch*'-equivalent buffer created during bootstrap.")

(unless (boundp 'default-frame-alist)
  (defvar default-frame-alist nil
    "Default frame parameter alist."))

(unless (boundp 'initial-frame-alist)
  (defvar initial-frame-alist nil
    "Initial frame parameter alist."))

(when (and (not (and (featurep 'gv) (fboundp 'gv-deref)))
           (or (fboundp 'nl-write-file)
               (fboundp 'nelisp--write-stdout-bytes)
               (not (boundp 'emacs-version))))
  (defmacro push (item place)
    "Standalone compatibility macro: cons ITEM onto PLACE.
PLACE may be a symbol (`setq') or a generalized place the substrate's
`setf' polyfill handles (real Emacs `push' accepts any gv place; a
symbol-only expansion emits `(setq (cdr ...) ...)', which the
standalone evaluator aborts on flagless)."
    (if (symbolp place)
        (list 'setq place (list 'cons item place))
      (list 'setf place (list 'cons item place)))))

(define-error 'nemacs-error "nemacs bootstrap error")
(define-error 'nemacs-already-initialized
  "nemacs already initialized" 'nemacs-error)

;;;; --- bootstrap -----------------------------------------------------

(defun nemacs-prepare-runtime ()
  "Initialize the editor substrate for startup or image baking."
  (emacs-standalone-init)
  (setq nemacs--initial-buffer (get-buffer-create "*scratch*")))

(defun nemacs-init (&optional batch-p arguments frontend)
  "Initialize NeLisp and run GNU startup with ARGUMENTS.
BATCH-P selects GNU batch startup, and FRONTEND identifies the display.
When embedded in GNU Emacs, its command loop already owns startup."
  (when nemacs-initialized
    (signal 'nemacs-already-initialized nil))
  (nemacs-prepare-runtime)
  (setq nemacs-initialized t)
  (when (fboundp 'nelisp--write-stdout-bytes)
    (require 'nemacs-gnu-startup)
    ;; The bootstrap defers isearch, which GNU guarantees is already
    ;; available when site-start.el and the user's init files execute.
    (or (bound-and-true-p isearch-mode-map) (require 'isearch))
    (switch-to-buffer nemacs--initial-buffer)
    (nemacs-gnu-startup-run batch-p arguments frontend))
  (run-hooks 'nemacs-startup-hook)
  'ready)

(defun nemacs-uninit ()
  "Reset the bootstrap so `nemacs-init' can run again.
Test-only helper.  Returns nil."
  (setq nemacs-initialized nil
        nemacs--initial-buffer nil
        init-file-had-error nil
        early-init-file nil
        user-init-file nil)
  (when (fboundp 'emacs-standalone-uninit)
    (emacs-standalone-uninit))
  nil)

;;;; --- introspection -------------------------------------------------

(defun nemacs-status ()
  "Return a plist describing the bootstrap state.

Keys:
  :version          — `nemacs-version'
  :initialized      — `nemacs-initialized'
  :initial-buffer   — `nemacs--initial-buffer' (= the scratch buffer)
  :major-mode       — current substrate major-mode (= via Track H)
  :feature-count    — number of features `featurep'-true.

Useful for smoke-testing the boot order from elisp."
  (list :version        nemacs-version
        :initialized    nemacs-initialized
        :initial-buffer nemacs--initial-buffer
        :major-mode     (and (fboundp 'emacs-mode-major-mode)
                             (emacs-mode-major-mode))
        :feature-count  (and (boundp 'features) (length features))))

;;;; --- dump helpers (Track L wiring) ---------------------------------

(defun nemacs-save-dump (path)
  "Write a lisp-image dump of the running session to PATH.
Returns the image plist that was written."
  (emacs-dump-save path))

(defun nemacs-load-dump (path &optional restore-buffers)
  "Load a lisp-image dump from PATH and re-establish bindings.
When RESTORE-BUFFERS is non-nil, also recreates the persisted
buffers' contents.  Returns the loaded image plist."
  (emacs-dump-load path restore-buffers))

(defun nemacs-dump-info (path)
  "Return a summary plist of the dump at PATH (without applying it)."
  (emacs-dump-image-info path))

(provide 'nemacs-loadup)

;;; nemacs-loadup.el ends here
