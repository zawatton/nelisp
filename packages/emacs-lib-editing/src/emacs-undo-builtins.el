;;; emacs-undo-builtins.el --- Unprefixed undo bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track E.2 (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs C-core / `simple.el' undo surface to the
;; substrate in `emacs-undo.el'.  Function definitions use a
;; host-aware install gate: host Emacs keeps its C/simple.el
;; definitions, while standalone NeLisp overwrites bootstrap stubs
;; with the real undo substrate.  Variables are still gated on
;; `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridged today:
;;
;;   - Functions: undo / undo-boundary / primitive-undo /
;;     buffer-disable-undo / buffer-enable-undo
;;   - Variable: buffer-undo-list (= MVP global, real Emacs has
;;     this buffer-local; under standalone NeLisp the per-buffer
;;     state lives in `emacs-undo--lists' alist regardless)
;;
;; Deferred: undo-only / undo-redo / `(apply ...)' record support /
;; marker records / text-property records.

;;; Code:

(require 'emacs-undo)

(defun emacs-undo-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-undo-builtins--install-function-p 'undo)
  (defalias 'undo #'emacs-undo-undo))

(when (emacs-undo-builtins--install-function-p 'undo-boundary)
  (defalias 'undo-boundary #'emacs-undo-undo-boundary))

(when (emacs-undo-builtins--install-function-p 'primitive-undo)
  (defalias 'primitive-undo #'emacs-undo-primitive-undo))

(when (emacs-undo-builtins--install-function-p 'buffer-disable-undo)
  (defun buffer-disable-undo (&optional buffer)
    "Track E.2 bridge: disable undo recording for BUFFER (= current).
MVP: ignores BUFFER and operates on the substrate's notion of
`current buffer' — sets the per-buffer undo list to t."
    (ignore buffer)
    (emacs-undo-set-buffer-undo-list t)
    nil))

(when (emacs-undo-builtins--install-function-p 'buffer-enable-undo)
  (defun buffer-enable-undo (&optional buffer)
    "Track E.2 bridge: re-enable undo recording for BUFFER (= current).
Mirror of `buffer-disable-undo' — sets the per-buffer undo list
back to nil."
    (ignore buffer)
    (emacs-undo-set-buffer-undo-list nil)
    nil))

(unless (boundp 'buffer-undo-list)
  (defvar buffer-undo-list nil
    "Track E.2 bridge: standalone-mode mirror of the per-buffer
undo list.  Real Emacs makes this buffer-local automatically; our
substrate stores per-buffer state in `emacs-undo--lists' and
exposes the current buffer's slot through the prefixed accessors.
This defvar is provided for `(boundp 'buffer-undo-list)' parity."))

(provide 'emacs-undo-builtins)

;;; emacs-undo-builtins.el ends here
