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
;; Out of scope for this MVP:
;;   - command-line option parsing (= --eval / --load / -l etc.)
;;   - terminal init (= the TUI backend wakes up via
;;     `emacs-tui-event-init', driven separately)
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

(define-error 'nemacs-error "nemacs bootstrap error")
(define-error 'nemacs-already-initialized
  "nemacs already initialized" 'nemacs-error)

;;;; --- bootstrap -----------------------------------------------------

(defun nemacs--ensure-scratch-buffer ()
  "Return the bootstrap's initial buffer, creating it if absent."
  (or nemacs--initial-buffer
      (and (fboundp 'nelisp-ec-generate-new-buffer)
           (setq nemacs--initial-buffer
                 (or (and (boundp 'nelisp-ec--buffers)
                          (cdr (assoc "*scratch*" nelisp-ec--buffers)))
                     (nelisp-ec-generate-new-buffer "*scratch*"))))))

(defun nemacs--report-banner (batch-p)
  "Emit the readiness banner.  No-op under BATCH-P."
  (unless batch-p
    (when (fboundp 'message)
      (message "nemacs %s ready (Layer 2 / Doc 51)" nemacs-version)))
  nil)

(defun nemacs-init (&optional batch-p)
  "Run the nemacs bootstrap sequence.

When BATCH-P is non-nil, suppresses interactive output.  Idempotent
guard: calling `nemacs-init' twice signals
`nemacs-already-initialized' rather than re-running.

Steps:
  1. Ensure the initial buffer exists (= scratch-equivalent).
  2. Activate `fundamental-mode' on it (= via the Track H bridge).
  3. Run `nemacs-startup-hook'.
  4. Mark `nemacs-initialized' = t.

Returns the symbol `ready' on success."
  (when nemacs-initialized
    (signal 'nemacs-already-initialized nil))
  ;; Step 0 — initialise the standalone-mode dispatch scaffold so
  ;; `emacs-standalone-active-p' returns a stable value for the rest
  ;; of bootstrap.
  (when (fboundp 'emacs-standalone-init)
    (emacs-standalone-init))
  ;; Step 1.
  (let ((buf (nemacs--ensure-scratch-buffer)))
    (when (and buf (fboundp 'nelisp-ec-set-buffer))
      (nelisp-ec-set-buffer buf)))
  ;; Step 2.  We call the prefixed substrate API directly; if the
  ;; bridge has been loaded, the unprefixed `fundamental-mode' is
  ;; aliased to the same target — both paths converge.
  (when (fboundp 'emacs-mode-fundamental-mode)
    (emacs-mode-fundamental-mode))
  ;; Step 3.
  (when (fboundp 'run-hooks)
    (run-hooks 'nemacs-startup-hook))
  ;; Step 4.
  (setq nemacs-initialized t)
  (nemacs--report-banner batch-p)
  'ready)

(defun nemacs-uninit ()
  "Reset the bootstrap so `nemacs-init' can run again.
Test-only helper.  Returns nil."
  (setq nemacs-initialized nil
        nemacs--initial-buffer nil)
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
