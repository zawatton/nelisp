;;; emacs-window-builtins.el --- Unprefixed window.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 11.C'' — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* window builtins (=
;; `selected-window', `windowp', `window-list', `window-buffer',
;; `set-window-buffer') to the existing `emacs-window-*' prefixed
;; implementations in `emacs-window.el', mirroring the Phase 11.B'
;; `emacs-search-builtins.el' pattern.
;;
;; Why this exists: until Phase 11.C'' the unprefixed names lived as
;; nil-stubs inside `emacs-stub.el', so callers calling
;; `(selected-window)' got a `(cons 'window nil)' sentinel even though
;; `emacs-window.el' provides a real window-tree model rooted on a
;; `nelisp-emacs-compat' buffer.  Bridging unifies the two.
;;
;; Each definition is gated on `unless (fboundp ...)' so loading inside
;; a host Emacs is a cheap no-op (= host's C builtins win).
;;
;; Bridgeable today (= covered by `emacs-window.el'):
;;
;;   - `selected-window' / `windowp'
;;   - `window-list'
;;   - `window-buffer' / `set-window-buffer'
;;
;; Deferred (= keep `emacs-stub.el' nil-stubs):
;;
;;   - `window-live-p': `emacs-window.el' has no `emacs-window-window-live-p'
;;     yet — the host builtin checks the window's live flag, which the
;;     prefixed model doesn't track explicitly.
;;   - `frame-selected-window': straddles frame/window; the prefixed
;;     side has no per-frame selected-window slot yet.

;;; Code:

(require 'emacs-window)

;;;; --- predicates ------------------------------------------------------

(unless (fboundp 'windowp)
  (defalias 'windowp #'emacs-window-windowp))

;;;; --- accessors -------------------------------------------------------

(unless (fboundp 'selected-window)
  (defalias 'selected-window #'emacs-window-selected-window))

(unless (fboundp 'window-list)
  (defalias 'window-list #'emacs-window-window-list))

(unless (fboundp 'window-buffer)
  (defalias 'window-buffer #'emacs-window-window-buffer))

;;;; --- mutation --------------------------------------------------------

(unless (fboundp 'set-window-buffer)
  (defalias 'set-window-buffer #'emacs-window-set-window-buffer))

(provide 'emacs-window-builtins)

;;; emacs-window-builtins.el ends here
