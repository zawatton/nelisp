;;; emacs-frame-builtins.el --- Unprefixed frame.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.C'' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* frame builtins (= `make-frame',
;; `framep', `selected-frame', `frame-parameter', ...) to the existing
;; `emacs-frame-*' prefixed implementations in `emacs-frame.el',
;; mirroring the Phase 11.B' `emacs-search-builtins.el' pattern.
;;
;; Why this exists: until Phase 11.C'' the unprefixed names lived as
;; nil-stubs inside `emacs-stub.el', so consumers calling `make-frame'
;; got a `(cons 'frame nil)' sentinel even though `emacs-frame.el'
;; provides a real frame model with parameters / size / backend
;; dispatch.  Bridging unifies the two namespaces.
;;
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub.el' no-op shims.
;;
;; Bridgeable today (= covered by `emacs-frame.el'):
;;
;;   - `make-frame' / `framep' / `frame-live-p' / `frame-list'
;;   - `selected-frame' / `window-frame'
;;   - `delete-frame' / `delete-other-frames'
;;   - `frame-width' / `frame-height' / `frame-char-width' /
;;     `frame-char-height' / `frame-pixel-width' / `frame-pixel-height'
;;   - `set-frame-size' / `set-frame-position'
;;   - `frame-parameter' / `frame-parameters'
;;   - `set-frame-parameter' / `modify-frame-parameters'
;;   - `frame-visible-p' / `make-frame-visible' /
;;     `make-frame-invisible' / `raise-frame' / `lower-frame'
;;   - `select-frame' / `frame-focus'
;;   - `frame-windows' / `display-pixel-width' / `display-pixel-height'

;;; Code:

(require 'emacs-frame)

(defun emacs-frame-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

;;;; --- constructors / predicates --------------------------------------

(when (emacs-frame-builtins--install-function-p 'make-frame)
  (defalias 'make-frame #'emacs-frame-make-frame))

(when (emacs-frame-builtins--install-function-p 'framep)
  (defalias 'framep #'emacs-frame-framep))

(when (emacs-frame-builtins--install-function-p 'frame-live-p)
  (defalias 'frame-live-p #'emacs-frame-frame-live-p))

(when (emacs-frame-builtins--install-function-p 'frame-list)
  (defalias 'frame-list #'emacs-frame-frame-list))

(when (emacs-frame-builtins--install-function-p 'selected-frame)
  (defalias 'selected-frame #'emacs-frame-selected-frame))

(when (emacs-frame-builtins--install-function-p 'window-frame)
  (defalias 'window-frame #'emacs-frame-window-frame))

;;;; --- lifecycle -------------------------------------------------------

(when (emacs-frame-builtins--install-function-p 'delete-frame)
  (defalias 'delete-frame #'emacs-frame-delete-frame))

(when (emacs-frame-builtins--install-function-p 'delete-other-frames)
  (defalias 'delete-other-frames #'emacs-frame-delete-other-frames))

;;;; --- size / position -------------------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-width)
  (defalias 'frame-width #'emacs-frame-frame-width))

(when (emacs-frame-builtins--install-function-p 'frame-height)
  (defalias 'frame-height #'emacs-frame-frame-height))

(when (emacs-frame-builtins--install-function-p 'frame-char-width)
  (defalias 'frame-char-width #'emacs-frame-frame-char-width))

(when (emacs-frame-builtins--install-function-p 'frame-char-height)
  (defalias 'frame-char-height #'emacs-frame-frame-char-height))

(when (emacs-frame-builtins--install-function-p 'frame-pixel-width)
  (defalias 'frame-pixel-width #'emacs-frame-frame-pixel-width))

(when (emacs-frame-builtins--install-function-p 'frame-pixel-height)
  (defalias 'frame-pixel-height #'emacs-frame-frame-pixel-height))

(when (emacs-frame-builtins--install-function-p 'set-frame-size)
  (defalias 'set-frame-size #'emacs-frame-set-frame-size))

(when (emacs-frame-builtins--install-function-p 'set-frame-position)
  (defalias 'set-frame-position #'emacs-frame-set-frame-position))

;;;; --- parameter access ------------------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-parameter)
  (defalias 'frame-parameter #'emacs-frame-frame-parameter))

(when (emacs-frame-builtins--install-function-p 'frame-parameters)
  (defalias 'frame-parameters #'emacs-frame-frame-parameters))

(when (emacs-frame-builtins--install-function-p 'set-frame-parameter)
  (defalias 'set-frame-parameter #'emacs-frame-set-frame-parameter))

(when (emacs-frame-builtins--install-function-p 'modify-frame-parameters)
  (defalias 'modify-frame-parameters #'emacs-frame-modify-frame-parameters))

;;;; --- visibility / z-order -------------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-visible-p)
  (defalias 'frame-visible-p #'emacs-frame-frame-visible-p))

(when (emacs-frame-builtins--install-function-p 'make-frame-visible)
  (defalias 'make-frame-visible #'emacs-frame-make-frame-visible))

(when (emacs-frame-builtins--install-function-p 'make-frame-invisible)
  (defalias 'make-frame-invisible #'emacs-frame-make-frame-invisible))

(when (emacs-frame-builtins--install-function-p 'raise-frame)
  (defalias 'raise-frame #'emacs-frame-raise-frame))

(when (emacs-frame-builtins--install-function-p 'lower-frame)
  (defalias 'lower-frame #'emacs-frame-lower-frame))

;;;; --- selection / focus ----------------------------------------------

(when (emacs-frame-builtins--install-function-p 'select-frame)
  (defalias 'select-frame #'emacs-frame-select-frame))

(when (emacs-frame-builtins--install-function-p 'frame-focus)
  (defalias 'frame-focus #'emacs-frame-frame-focus))

;;;; --- frame->windows + display ---------------------------------------

(when (emacs-frame-builtins--install-function-p 'frame-windows)
  (defalias 'frame-windows #'emacs-frame-frame-windows))

(when (emacs-frame-builtins--install-function-p 'display-pixel-width)
  (defalias 'display-pixel-width #'emacs-frame-display-pixel-width))

(when (emacs-frame-builtins--install-function-p 'display-pixel-height)
  (defalias 'display-pixel-height #'emacs-frame-display-pixel-height))

(provide 'emacs-frame-builtins)

;;; emacs-frame-builtins.el ends here
