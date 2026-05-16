;;; emacs-undo-builtins.el --- Unprefixed undo bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track E.2 (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs C-core / `simple.el' undo surface to the
;; substrate in `emacs-undo.el'.  Same gating pattern as Track B / C
;; / D / E / F / J / 11.C'': `unless (fboundp ...)' / `unless
;; (boundp ...)' so loading inside a host Emacs is a cheap no-op
;; and the host's C builtins keep ownership of the unprefixed names.
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

(unless (fboundp 'undo)
  (defalias 'undo #'emacs-undo-undo))

(unless (fboundp 'undo-boundary)
  (defalias 'undo-boundary #'emacs-undo-undo-boundary))

(unless (fboundp 'primitive-undo)
  (defalias 'primitive-undo #'emacs-undo-primitive-undo))

(unless (fboundp 'buffer-disable-undo)
  (defun buffer-disable-undo (&optional buffer)
    "Track E.2 bridge: disable undo recording for BUFFER (= current).
MVP: ignores BUFFER and operates on the substrate's notion of
`current buffer' — sets the per-buffer undo list to t."
    (ignore buffer)
    (emacs-undo-set-buffer-undo-list t)
    nil))

(unless (fboundp 'buffer-enable-undo)
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
