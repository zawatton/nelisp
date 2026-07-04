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
;; Loading inside a host Emacs is a cheap no-op (= host's C builtins
;; win).  Standalone NeLisp deliberately overwrites the earlier
;; `emacs-stub.el' no-op shims.
;;
;; Bridgeable today (= covered by `emacs-window.el'):
;;
;;   - `selected-window' / `windowp'
;;   - `window-live-p' / `window-valid-p'
;;   - `frame-selected-window'
;;   - `window-list' / `window-list-1' / `next-window' / `previous-window'
;;   - `window-buffer' / `set-window-buffer'
;;   - `select-window'
;;   - `split-window' / `split-window-below' / `split-window-right'
;;     + legacy `split-window-vertically' / `split-window-horizontally'
;;   - `delete-window' / `delete-other-windows' / `delete-windows-on'
;;   - `one-window-p' / `balance-windows'
;;   - `get-buffer-window' / `get-buffer-window-list'
;;   - `other-window' (polyfilled — `emacs-window.el' has no direct equivalent)
;;   - `window-start' / `window-end' / `window-point' / `set-window-point'
;;     / `set-window-start' / `window-height' / `window-width'
;;     / `window-body-height' (Doc 33 §4 item 9 — line-based, see below)
;;   - `recenter' / `scroll-up' / `scroll-down' / `scroll-up-command'
;;     / `scroll-down-command' / `pos-visible-in-window-p' (Doc 33 §4
;;     item 9 — real buffer-line-based semantics via
;;     `emacs-window-recenter' / `emacs-window-scroll-up' /
;;     `emacs-window-scroll-down' / `emacs-window-pos-visible-in-window-p',
;;     replacing the nil no-op stubs that `emacs-stub-bulk.el' would
;;     otherwise install for these names)
;;
;;; Code:

(require 'emacs-window)

(defun emacs-window-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this bridge.

`(boundp \\='emacs-version)' alone is not a reliable \"are we really
inside host Emacs\" test: some NeLisp standalone-reader builds bind
`emacs-version' to a non-string sentinel (so `boundp' is true but the
value is not a version string) rather than leaving it unbound.  Also
check `stringp' so the standalone reader is still detected correctly
and this bridge overrides the unconditional `emacs-stub.el' nil-stub
installs for names such as `windowp'/`window-list'/`window-buffer'
that would otherwise win by loading first."
  (or (not (boundp 'emacs-version))
      (not (stringp emacs-version))
      (not (fboundp symbol))))

;;;; --- predicates ------------------------------------------------------

(when (emacs-window-builtins--install-function-p 'windowp)
  (defalias 'windowp #'emacs-window-windowp))

(when (emacs-window-builtins--install-function-p 'window-live-p)
  (defalias 'window-live-p #'emacs-window-window-live-p))

(when (emacs-window-builtins--install-function-p 'window-valid-p)
  (defalias 'window-valid-p #'emacs-window-window-valid-p))

;;;; --- accessors -------------------------------------------------------

(when (emacs-window-builtins--install-function-p 'selected-window)
  (defalias 'selected-window #'emacs-window-selected-window))

(when (emacs-window-builtins--install-function-p 'frame-selected-window)
  (defalias 'frame-selected-window #'emacs-window-frame-selected-window))

(when (emacs-window-builtins--install-function-p 'window-list)
  (defalias 'window-list #'emacs-window-window-list))

(when (emacs-window-builtins--install-function-p 'window-list-1)
  (defalias 'window-list-1 #'emacs-window-window-list-1))

(when (emacs-window-builtins--install-function-p 'next-window)
  (defalias 'next-window #'emacs-window-next-window))

(when (emacs-window-builtins--install-function-p 'previous-window)
  (defalias 'previous-window #'emacs-window-previous-window))

(when (emacs-window-builtins--install-function-p 'window-buffer)
  (defalias 'window-buffer #'emacs-window-window-buffer))

(when (emacs-window-builtins--install-function-p 'one-window-p)
  (defalias 'one-window-p #'emacs-window-one-window-p))

(when (emacs-window-builtins--install-function-p 'get-buffer-window)
  (defalias 'get-buffer-window #'emacs-window-get-buffer-window))

(when (emacs-window-builtins--install-function-p 'get-buffer-window-list)
  (defalias 'get-buffer-window-list #'emacs-window-get-buffer-window-list))

(when (emacs-window-builtins--install-function-p 'window-height)
  (defalias 'window-height #'emacs-window-window-height))

(when (emacs-window-builtins--install-function-p 'window-width)
  (defalias 'window-width #'emacs-window-window-width))

(when (emacs-window-builtins--install-function-p 'window-body-height)
  (defun window-body-height (&optional window _pixelwise)
    "Phase 11 polyfill: body height, excluding the mode-line row."
    (max 1 (1- (emacs-window-window-height window)))))

(when (emacs-window-builtins--install-function-p 'window-start)
  (defalias 'window-start #'emacs-window-window-start))

(when (emacs-window-builtins--install-function-p 'window-end)
  (defalias 'window-end #'emacs-window-window-end))

(when (emacs-window-builtins--install-function-p 'window-point)
  (defalias 'window-point #'emacs-window-window-point))

;;;; --- mutation --------------------------------------------------------

(when (emacs-window-builtins--install-function-p 'set-window-buffer)
  (defalias 'set-window-buffer #'emacs-window-set-window-buffer))

(when (emacs-window-builtins--install-function-p 'set-window-point)
  (defalias 'set-window-point #'emacs-window-set-window-point))

(when (emacs-window-builtins--install-function-p 'set-window-start)
  (defalias 'set-window-start #'emacs-window-set-window-start))

(when (emacs-window-builtins--install-function-p 'select-window)
  (defalias 'select-window #'emacs-window-select-window))

;;;; --- split / delete (Track V, 2026-05-04) ----------------------------

(when (emacs-window-builtins--install-function-p 'split-window)
  (defalias 'split-window #'emacs-window-split-window))

(when (emacs-window-builtins--install-function-p 'split-window-below)
  (defun split-window-below (&optional size)
    "Phase 11 polyfill: split selected window into two stacked windows.
Bound to C-x 2 in `nemacs-main-keymap'."
    (interactive "P")
    (emacs-window-split-window-vertically size)))

(when (emacs-window-builtins--install-function-p 'split-window-right)
  (defun split-window-right (&optional size)
    "Phase 11 polyfill: split selected window into two side-by-side windows.
Bound to C-x 3 in `nemacs-main-keymap'."
    (interactive "P")
    (emacs-window-split-window-horizontally size)))

(when (emacs-window-builtins--install-function-p 'split-window-vertically)
  (defalias 'split-window-vertically #'emacs-window-split-window-vertically))

(when (emacs-window-builtins--install-function-p 'split-window-horizontally)
  (defalias 'split-window-horizontally #'emacs-window-split-window-horizontally))

(when (emacs-window-builtins--install-function-p 'delete-window)
  (defun delete-window (&optional window)
    "Phase 11 polyfill: delete WINDOW (default = selected).
Bound to C-x 0 in `nemacs-main-keymap'."
    (interactive)
    (emacs-window-delete-window window)))

(when (emacs-window-builtins--install-function-p 'delete-other-windows)
  (defun delete-other-windows (&optional window)
    "Phase 11 polyfill: delete every window except WINDOW (default = selected).
Bound to C-x 1 in `nemacs-main-keymap'."
    (interactive)
    (emacs-window-delete-other-windows window)))

(when (emacs-window-builtins--install-function-p 'delete-windows-on)
  (defalias 'delete-windows-on #'emacs-window-delete-windows-on))

(when (emacs-window-builtins--install-function-p 'balance-windows)
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

(when (emacs-window-builtins--install-function-p 'other-window)
  (defalias 'other-window #'emacs-window-other-window-impl))

;;;; --- display-buffer / pop-to-buffer (M3 display policy) --------------

(when (emacs-window-builtins--install-function-p 'display-buffer)
  (defalias 'display-buffer #'emacs-window-display-buffer))

(when (emacs-window-builtins--install-function-p 'pop-to-buffer)
  (defalias 'pop-to-buffer #'emacs-window-pop-to-buffer))

(when (emacs-window-builtins--install-function-p 'quit-window)
  (defun quit-window (&optional kill window)
    "Phase 11 polyfill: quit WINDOW, closing a popup or burying its buffer.
Bound to `q' in help/special-buffer keymaps."
    (interactive "P")
    (emacs-window-quit-window kill window)))

;;;; --- scroll / recenter / visibility (Doc 33 §4 item 9) ----------------
;;
;; Real buffer-line-based implementations (see `emacs-window.el').
;; These names are in `emacs-stub-bulk.el's nil-no-op list (or, for
;; `pos-visible-in-window-p', void entirely); this file loads first in
;; the standalone bootstrap, so the `(unless (fboundp ...))' guards
;; there defer to the real definitions installed here.

(when (emacs-window-builtins--install-function-p 'recenter)
  (defun recenter (&optional arg _redisplay)
    "Phase 11 polyfill: real line-based recenter.
See `emacs-window-recenter'."
    (interactive "P")
    (emacs-window-recenter nil arg)))

(when (emacs-window-builtins--install-function-p 'scroll-up)
  (defun scroll-up (&optional n)
    "Phase 11 polyfill: real line-based scroll-up.
See `emacs-window-scroll-up'."
    (interactive "P")
    (emacs-window-scroll-up nil n)))

(when (emacs-window-builtins--install-function-p 'scroll-down)
  (defun scroll-down (&optional n)
    "Phase 11 polyfill: real line-based scroll-down.
See `emacs-window-scroll-down'."
    (interactive "P")
    (emacs-window-scroll-down nil n)))

(when (emacs-window-builtins--install-function-p 'scroll-up-command)
  (defalias 'scroll-up-command #'scroll-up))

(when (emacs-window-builtins--install-function-p 'scroll-down-command)
  (defalias 'scroll-down-command #'scroll-down))

(when (emacs-window-builtins--install-function-p 'pos-visible-in-window-p)
  (defalias 'pos-visible-in-window-p #'emacs-window-pos-visible-in-window-p))

(provide 'emacs-window-builtins)

;;; emacs-window-builtins.el ends here
