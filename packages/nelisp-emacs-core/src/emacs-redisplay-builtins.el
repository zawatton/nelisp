;;; emacs-redisplay-builtins.el --- Unprefixed redisplay bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track G (2026-05-03) — Layer 2.
;;
;; Closes the Phase 3 redisplay close-gate item "force-mode-line-
;; update / redraw-display / redraw-frame trigger handler" (Doc 01
;; §3.3, mirroring Doc 43 §3.2 close gate).
;;
;; The redisplay engine in `emacs-redisplay.el' uses explicit
;; handles (= per-`emacs-redisplay-init' invocation).  The
;; conventional Emacs trigger functions (`force-mode-line-update'
;; etc.) take no handle, so we maintain an
;; `emacs-redisplay--current-handle' defvar that the standalone
;; bootstrap (= nemacs-loadup) or test fixtures can set.  The
;; bridge functions are no-ops when no current handle is bound,
;; matching Emacs's host C-builtin behaviour during bootstrap
;; before a frame is realised.
;;
;; Bridged today (Track G):
;;   - force-mode-line-update    (= mark current-handle's selected
;;                                  window dirty so its mode-line
;;                                  row gets rebuilt)
;;   - redraw-display            (= mark every cached window dirty
;;                                  on the current-handle)
;;   - redraw-frame              (= same; frame arg ignored in MVP)
;;   - redisplay                 (= run a redisplay pass on the
;;                                  current-handle; nil = no-op)
;;
;; Plus the introspection helper:
;;   - emacs-redisplay-set-current-handle
;;   - emacs-redisplay-current-handle
;;
;; Deferred:
;;   - per-frame distinction (= redraw-frame currently treats all
;;     frames as the current handle)
;;   - selective redraw based on point movement / face change

;;; Code:

(require 'emacs-redisplay)

;;;; --- current-handle slot -------------------------------------------

(defvar emacs-redisplay--current-handle nil
  "Active redisplay handle for unprefixed trigger functions.
Set via `emacs-redisplay-set-current-handle'; nil means no
trigger fires (= early bootstrap or after shutdown).")

(defun emacs-redisplay-set-current-handle (handle)
  "Bind the substrate's current redisplay handle to HANDLE.

HANDLE must satisfy `emacs-redisplay-handlep' or be nil.  Returns
HANDLE.  After this call, the unprefixed trigger functions
(`force-mode-line-update' etc.) will operate on HANDLE."
  (cond
   ((null handle)
    (setq emacs-redisplay--current-handle nil))
   ((emacs-redisplay-handlep handle)
    (setq emacs-redisplay--current-handle handle))
   (t (signal 'emacs-redisplay-bad-handle (list handle))))
  handle)

(defun emacs-redisplay-current-handle ()
  "Return the currently-bound redisplay handle, or nil."
  emacs-redisplay--current-handle)

;;;; --- trigger handlers ----------------------------------------------

(defun emacs-redisplay-force-mode-line-update (&optional all)
  "Phase 3 close-gate trigger: invalidate mode-line caches.

When ALL is non-nil, dirties every window in the current frame.
With ALL nil, dirties the selected window only.  No-op when there
is no `emacs-redisplay-current-handle'.  Returns the cleared
window count."
  (let ((handle (emacs-redisplay-current-handle)))
    (cond
     ((null handle) 0)
     (all
      (emacs-redisplay-mark-frame-dirty handle))
     (t
      (let ((w (and (fboundp 'emacs-window-selected-window)
                    (emacs-window-selected-window))))
        (cond
         ((null w) 0)
         (t (if (emacs-redisplay-mark-window-dirty handle w) 1 0))))))))

(defun emacs-redisplay-redraw-display ()
  "Phase 3 close-gate trigger: full-frame invalidation.

Drops every cached glyph-matrix on the current handle so the next
flush rebuilds from scratch.  No-op when no current handle.
Returns the cleared count."
  (let ((handle (emacs-redisplay-current-handle)))
    (if handle
        (emacs-redisplay-mark-frame-dirty handle)
      0)))

(defun emacs-redisplay-redraw-frame (&optional _frame)
  "Phase 3 close-gate trigger: same as `emacs-redisplay-redraw-display'.
The optional FRAME argument is accepted for API parity but ignored
in the MVP (= one global handle covers all frames)."
  (emacs-redisplay-redraw-display))

(defun emacs-redisplay-trigger-redisplay (&optional _force)
  "Phase 3 close-gate trigger: run a redisplay pass on the current handle.

When no current handle is bound, returns nil (= bootstrap no-op).
Otherwise calls `emacs-redisplay-redisplay-window' on the
selected window and returns the resulting glyph-matrix.  FORCE is
accepted for API parity.  Renamed from the unprefixed shape to
avoid clobbering the existing substrate `emacs-redisplay-redisplay'."
  (let ((handle (emacs-redisplay-current-handle)))
    (cond
     ((null handle) nil)
     (t
      (let ((w (and (fboundp 'emacs-window-selected-window)
                    (emacs-window-selected-window))))
        (when w
          (emacs-redisplay-redisplay-window handle w)))))))

;;;; --- function bridges (gated) --------------------------------------

(unless (fboundp 'force-mode-line-update)
  (defalias 'force-mode-line-update
    #'emacs-redisplay-force-mode-line-update))

(unless (fboundp 'redraw-display)
  (defalias 'redraw-display #'emacs-redisplay-redraw-display))

(unless (fboundp 'redraw-frame)
  (defalias 'redraw-frame #'emacs-redisplay-redraw-frame))

;; Note: `redisplay' under host Emacs is a C primitive that takes
;; an optional FORCE arg; our impl matches the arity contract.  We
;; gate the bridge so the host's C builtin keeps ownership; the
;; prefixed `emacs-redisplay-trigger-redisplay' helper above is
;; reachable explicitly from standalone callers.
(unless (fboundp 'redisplay)
  (defalias 'redisplay #'emacs-redisplay-trigger-redisplay))

(provide 'emacs-redisplay-builtins)

;;; emacs-redisplay-builtins.el ends here
