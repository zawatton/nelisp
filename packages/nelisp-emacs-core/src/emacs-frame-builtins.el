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
;; Each definition is gated on `unless (fboundp ...)' so loading inside
;; a host Emacs is a cheap no-op (= host's C builtins win).
;;
;; Bridgeable today (= covered by `emacs-frame.el'):
;;
;;   - `make-frame' / `framep' / `frame-live-p' / `frame-list'
;;   - `selected-frame'
;;   - `frame-parameter' / `frame-parameters'
;;   - `set-frame-parameter' / `modify-frame-parameters'
;;   - `delete-frame'
;;
;; Deferred (= keep `emacs-stub.el' nil-stubs):
;;
;;   - `display-graphic-p' / `display-color-p' / `display-multi-frame-p':
;;     no display-side capability primitive yet.  `emacs-frame.el' has
;;     `emacs-frame-capability-p' but the bridge would need a mapping
;;     from display class to capability key — out of scope for 11.C''.

;;; Code:

(require 'emacs-frame)

;;;; --- constructors / predicates --------------------------------------

(unless (fboundp 'make-frame)
  (defalias 'make-frame #'emacs-frame-make-frame))

(unless (fboundp 'framep)
  (defalias 'framep #'emacs-frame-framep))

(unless (fboundp 'frame-live-p)
  (defalias 'frame-live-p #'emacs-frame-frame-live-p))

(unless (fboundp 'frame-list)
  (defalias 'frame-list #'emacs-frame-frame-list))

(unless (fboundp 'selected-frame)
  (defalias 'selected-frame #'emacs-frame-selected-frame))

;;;; --- parameter access ------------------------------------------------

(unless (fboundp 'frame-parameter)
  (defalias 'frame-parameter #'emacs-frame-frame-parameter))

(unless (fboundp 'frame-parameters)
  (defalias 'frame-parameters #'emacs-frame-frame-parameters))

(unless (fboundp 'set-frame-parameter)
  (defalias 'set-frame-parameter #'emacs-frame-set-frame-parameter))

(unless (fboundp 'modify-frame-parameters)
  (defalias 'modify-frame-parameters #'emacs-frame-modify-frame-parameters))

;;;; --- lifecycle -------------------------------------------------------

(unless (fboundp 'delete-frame)
  (defalias 'delete-frame #'emacs-frame-delete-frame))

(provide 'emacs-frame-builtins)

;;; emacs-frame-builtins.el ends here
