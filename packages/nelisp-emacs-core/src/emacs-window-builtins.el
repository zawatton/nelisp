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
;;   - `window-list' / `window-list-1' / `next-window' / `previous-window'
;;   - `window-buffer' / `set-window-buffer'
;;   - `select-window'
;;   - `split-window' / `split-window-below' / `split-window-right'
;;     + legacy `split-window-vertically' / `split-window-horizontally'
;;   - `delete-window' / `delete-other-windows' / `delete-windows-on'
;;   - `one-window-p' / `balance-windows'
;;   - `get-buffer-window' / `get-buffer-window-list'
;;   - `other-window' (polyfilled — `emacs-window.el' has no direct equivalent)
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

(unless (fboundp 'window-list-1)
  (defalias 'window-list-1 #'emacs-window-window-list-1))

(unless (fboundp 'next-window)
  (defalias 'next-window #'emacs-window-next-window))

(unless (fboundp 'previous-window)
  (defalias 'previous-window #'emacs-window-previous-window))

(unless (fboundp 'window-buffer)
  (defalias 'window-buffer #'emacs-window-window-buffer))

(unless (fboundp 'one-window-p)
  (defalias 'one-window-p #'emacs-window-one-window-p))

(unless (fboundp 'get-buffer-window)
  (defalias 'get-buffer-window #'emacs-window-get-buffer-window))

(unless (fboundp 'get-buffer-window-list)
  (defalias 'get-buffer-window-list #'emacs-window-get-buffer-window-list))

;;;; --- mutation --------------------------------------------------------

(unless (fboundp 'set-window-buffer)
  (defalias 'set-window-buffer #'emacs-window-set-window-buffer))

(unless (fboundp 'select-window)
  (defalias 'select-window #'emacs-window-select-window))

;;;; --- split / delete (Track V, 2026-05-04) ----------------------------

(unless (fboundp 'split-window)
  (defalias 'split-window #'emacs-window-split-window))

(unless (fboundp 'split-window-below)
  (defalias 'split-window-below #'emacs-window-split-window-vertically))

(unless (fboundp 'split-window-right)
  (defalias 'split-window-right #'emacs-window-split-window-horizontally))

(unless (fboundp 'split-window-vertically)
  (defalias 'split-window-vertically #'emacs-window-split-window-vertically))

(unless (fboundp 'split-window-horizontally)
  (defalias 'split-window-horizontally #'emacs-window-split-window-horizontally))

(unless (fboundp 'delete-window)
  (defalias 'delete-window #'emacs-window-delete-window))

(unless (fboundp 'delete-other-windows)
  (defalias 'delete-other-windows #'emacs-window-delete-other-windows))

(unless (fboundp 'delete-windows-on)
  (defalias 'delete-windows-on #'emacs-window-delete-windows-on))

(unless (fboundp 'balance-windows)
  (defalias 'balance-windows #'emacs-window-balance-windows))

;;;; --- other-window (Track V) -----------------------------------------
;;
;; `emacs-window.el' has no direct `emacs-window-other-window'; we
;; build it from `next-window' + `select-window'.  COUNT is the number
;; of windows to skip (default 1, can be negative for backwards).
;; Wraps around at the ends.  ALL-FRAMES is accepted for API parity.

(defun emacs-window-other-window-impl (&optional count all-frames)
  "Bridge implementation of `other-window'.
COUNT defaults to 1; negative values walk backwards.  ALL-FRAMES is
accepted for API parity and ignored (= single-frame Phase 1)."
  (interactive "p")
  (let* ((n   (or count 1))
         (cur (emacs-window-selected-window))
         (forward-fn (lambda (w) (emacs-window-next-window w nil all-frames)))
         (back-fn    (lambda (w) (emacs-window-previous-window w nil all-frames)))
         (step (if (>= n 0) forward-fn back-fn))
         (steps (abs n))
         (target cur))
    (dotimes (_ steps)
      (setq target (funcall step target)))
    (when target
      (emacs-window-select-window target))
    target))

(unless (fboundp 'other-window)
  (defalias 'other-window #'emacs-window-other-window-impl))

(provide 'emacs-window-builtins)

;;; emacs-window-builtins.el ends here
