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
;;   - Variables: buffer-undo-list (localized when enabling or recording
;;     undo; the prefixed substrate keeps its own `emacs-undo--lists'), plus
;;     the C-owned undo history limits used by packages such as undo-tree
;;
;; Deferred: undo-only / undo-redo / `(apply ...)' record support /
;; marker records / text-property records.

;;; Code:

(require 'emacs-undo)

(defun emacs-undo-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      ;; Vendor compatibility binds emacs-version in standalone too.  These
      ;; bridges must still install over the substrate-only undo aliases.
      (and (memq symbol '(buffer-enable-undo undo-boundary))
           (fboundp 'nelisp--write-stdout-bytes))
      (not (fboundp symbol))))

(when (emacs-undo-builtins--install-function-p 'undo)
  (defalias 'undo #'emacs-undo-undo))

(when (emacs-undo-builtins--install-function-p 'undo-boundary)
  (defun undo-boundary ()
    "Separate preceding changes from subsequent undo information.
Do nothing when undo is disabled, the history is empty, or its head
already is a boundary.  Return nil."
    (when (and (consp buffer-undo-list) (car buffer-undo-list))
      (unless (local-variable-p 'buffer-undo-list)
        (make-local-variable 'buffer-undo-list))
      (setq buffer-undo-list (cons nil buffer-undo-list)))
    nil))

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
    "Start keeping undo information for BUFFER, defaulting to current.
BUFFER may be a buffer or its name.  Clear disabled undo lists, but
preserve any existing undo history.  Return nil."
    (let ((target
           (cond
            ((null buffer) (current-buffer))
            ((bufferp buffer) buffer)
            ((stringp buffer)
             (or (get-buffer buffer)
                 (error "No buffer named %s" buffer)))
            (t (signal 'wrong-type-argument (list 'stringp buffer))))))
      ;; Killed buffers are accepted, but must not be selected.
      (when (buffer-live-p target)
        (with-current-buffer target
          (unless (local-variable-p 'buffer-undo-list)
            (make-local-variable 'buffer-undo-list))
          (when (eq buffer-undo-list t)
            (setq buffer-undo-list nil))
          ;; The undo substrate uses a separate current-buffer tracker.
          ;; Bind it to the selected buffer instead of changing another
          ;; buffer's history when the native buffer family owns selection.
          (let ((nelisp-ec--current-buffer target))
            (when (eq (emacs-undo-buffer-undo-list) t)
              (emacs-undo-set-buffer-undo-list nil))))))
    nil))

(unless (boundp 'buffer-undo-list)
  (defvar buffer-undo-list nil
    "Undo information for the current buffer, or t if recording is disabled.
Enabling undo and recording insertions create a buffer-local binding.
The prefixed undo substrate has separate per-buffer storage."))

;; GNU Emacs defines these in undo.c.  Keep the definitions here beside the
;; `buffer-undo-list' compatibility surface, and leave every host value intact.
(unless (boundp 'undo-limit)
  (defvar undo-limit 160000
    "Soft size limit for undo information in the current buffer."))

(unless (boundp 'undo-strong-limit)
  (defvar undo-strong-limit 240000
    "Size beyond which undo information is discarded aggressively."))

(unless (boundp 'undo-outer-limit)
  (defvar undo-outer-limit 24000000
    "Maximum size for a single undo command's information."))

(unless (boundp 'undo-ask-before-discard)
  (defvar undo-ask-before-discard nil
    "Non-nil means ask before discarding undo data over the outer limit."))

(provide 'emacs-undo-builtins)

;;; emacs-undo-builtins.el ends here
