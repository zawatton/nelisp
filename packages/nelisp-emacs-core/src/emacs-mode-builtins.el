;;; emacs-mode-builtins.el --- Major-mode bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track H (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed major-mode framework to the
;; substrate in `emacs-mode.el'.  Function definitions use a
;; host-aware install gate: host Emacs keeps its builtin/simple.el
;; mode framework, while standalone NeLisp overwrites bootstrap stubs
;; with the real mode substrate.  Variables are still gated on
;; `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridged today:
;;   - Variables: major-mode / mode-name / auto-mode-alist /
;;     fundamental-mode-hook / text-mode-hook /
;;     emacs-lisp-mode-hook / change-major-mode-after-body-hook /
;;     after-change-major-mode-hook
;;   - Functions: fundamental-mode / text-mode / emacs-lisp-mode /
;;     run-mode-hooks / kill-all-local-variables / set-auto-mode
;;   - Macro: define-derived-mode
;;
;; Deferred to later γ phases:
;;   - real font-lock-mode integration (= currently no-op)
;;   - syntax-table per-mode binding
;;   - mode-line-format integration

;;; Code:

(require 'emacs-mode)

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'major-mode)
  (defvar major-mode 'fundamental-mode
    "Track H bridge: the active major-mode symbol."))

(unless (boundp 'mode-name)
  (defvar mode-name "Fundamental"
    "Track H bridge: human-readable mode name."))

(unless (boundp 'mode-line-format)
  (defvar mode-line-format " %b "
    "Track H bridge: default mode-line format for preloaded buffers."))

(unless (boundp 'auto-mode-alist)
  (defvar auto-mode-alist nil
    "Track H bridge: file-extension → major-mode association list."))

(unless (boundp 'fundamental-mode-hook)
  (defvar fundamental-mode-hook nil
    "Track H bridge: hook run when entering `fundamental-mode'."))

;; NOTE (Doc 33 §8 item 221): do NOT pre-defvar `text-mode-hook' here.
;; Vendor text-mode.el owns it (as a defcustom whose standalone replay
;; becomes a plain `defvar' with the default
;; \\='(text-mode-hook-identify)); pre-binding it to nil made that
;; vendor `defvar' a no-op, leaving the hook empty on the standalone
;; reader.  `emacs-mode-text-mode' already guards its reference with
;; `boundp', so leaving the variable undefined until vendor text-mode.el
;; loads is safe.

(unless (boundp 'emacs-lisp-mode-hook)
  (defvar emacs-lisp-mode-hook nil
    "Track H bridge: hook run when entering `emacs-lisp-mode'."))

(unless (boundp 'change-major-mode-after-body-hook)
  (defvar change-major-mode-after-body-hook nil
    "Track H bridge: ran by every derived mode's body, after the
parent mode is initialised but before the user hooks fire."))

(unless (boundp 'after-change-major-mode-hook)
  (defvar after-change-major-mode-hook nil
    "Track H bridge: ran after every major-mode switch completes."))

;;;; --- function bridges ----------------------------------------------

(defun emacs-mode-builtins--function-cell-live-p (symbol)
  "Return non-nil when SYMBOL has a usable function cell."
  (and (fboundp symbol)
       (condition-case nil
           (symbol-function symbol)
         (error nil))))

(defun emacs-mode-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (get symbol 'emacs-stub-bulk)
      (not (stringp emacs-version))
      (not (emacs-mode-builtins--function-cell-live-p symbol))))

(defun emacs-mode-builtins--make-mode-line-mouse-map (mouse function)
  "Return a sparse mode-line keymap binding MOUSE to FUNCTION."
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'mode-line mouse) function)
    map))

(defun emacs-mode-builtins--substitute-command-keys-reduced
    (string &optional _no-face)
  "Return a reduced `substitute-command-keys' expansion for STRING.

This standalone-oriented fallback handles only three Emacs escape forms:
`\\<MAPVAR>' by removing the sequence, `\\[COMMAND]' by substituting
`M-x COMMAND', and `\\=' by unescaping the following character.  It does
not attempt real keymap lookup or face interpolation."
  (let ((index 0)
        (length (length string))
        (parts nil))
    (while (< index length)
      (cond
       ((and (<= (+ index 4) length)
             (string= (substring string index (+ index 4)) "\\=\\="))
        (push "=" parts)
        (setq index (+ index 4)))
       ((and (<= (+ index 4) length)
             (string= (substring string index (+ index 4)) "\\=\\<"))
        (let ((end (string-match ">" string (+ index 4))))
          (setq index (if end (1+ end) length))))
       ((and (<= (+ index 4) length)
             (string= (substring string index (+ index 4)) "\\=\\["))
        (let ((end (string-match "]" string (+ index 4))))
          (if end
              (progn
                (push "M-x " parts)
                (push (substring string (+ index 4) end) parts)
                (setq index (1+ end)))
            (push "\\[" parts)
            (setq index (+ index 4)))))
       ((and (<= (+ index 2) length)
             (string= (substring string index (+ index 2)) "\\<"))
        (let ((end (string-match ">" string (+ index 2))))
          (setq index (if end (1+ end) length))))
       ((and (<= (+ index 2) length)
             (string= (substring string index (+ index 2)) "\\["))
        (let ((end (string-match "]" string (+ index 2))))
          (if end
              (progn
                (push "M-x " parts)
                (push (substring string (+ index 2) end) parts)
                (setq index (1+ end)))
            (push "\\[" parts)
            (setq index (+ index 2)))))
       ((and (<= (+ index 2) length)
             (string= (substring string index (+ index 2)) "\\="))
        (if (< (+ index 2) length)
            (progn
              (push (char-to-string (aref string (+ index 2))) parts)
              (setq index (+ index 3)))
          (push "\\=" parts)
          (setq index (+ index 2))))
       (t
        (push (char-to-string (aref string index)) parts)
        (setq index (1+ index)))))
    (apply #'concat (nreverse parts))))

(defun emacs-mode-builtins--toggle-flag (symbol arg)
  "Update SYMBOL from ARG using minor-mode toggle conventions."
  (set symbol
       (if (null arg)
           (not (and (boundp symbol) (symbol-value symbol)))
         (not (or (eq arg 0)
                  (eq arg -1)
                  (and (numberp arg) (<= arg 0))))))
  nil)

(defmacro emacs-mode-builtins--define-noop-mode (name doc)
  "Define NAME as a headless no-op minor mode with DOC."
  `(progn
     (unless (boundp ',name)
       (defvar ,name nil ,doc))
     (when (emacs-mode-builtins--install-function-p ',name)
       (defalias ',name #'ignore))))

(when (emacs-mode-builtins--install-function-p 'fundamental-mode)
  (defalias 'fundamental-mode #'emacs-mode-fundamental-mode))

(when (emacs-mode-builtins--install-function-p 'text-mode)
  (defalias 'text-mode #'emacs-mode-text-mode))

(when (emacs-mode-builtins--install-function-p 'emacs-lisp-mode)
  (defalias 'emacs-lisp-mode #'emacs-mode-emacs-lisp-mode))

(when (emacs-mode-builtins--install-function-p 'run-mode-hooks)
  (defalias 'run-mode-hooks #'emacs-mode-run-mode-hooks))

(when (emacs-mode-builtins--install-function-p 'kill-all-local-variables)
  (defalias 'kill-all-local-variables
    #'emacs-mode-kill-all-local-variables))

(when (emacs-mode-builtins--install-function-p 'set-auto-mode)
  (defalias 'set-auto-mode #'emacs-mode-set-auto-mode))

(when (emacs-mode-builtins--install-function-p 'make-mode-line-mouse-map)
  (defalias 'make-mode-line-mouse-map
    #'emacs-mode-builtins--make-mode-line-mouse-map))

(when (emacs-mode-builtins--install-function-p 'substitute-command-keys)
  (defalias 'substitute-command-keys
    #'emacs-mode-builtins--substitute-command-keys-reduced))

(emacs-mode-builtins--define-noop-mode
 global-hl-line-mode
 "Headless no-op compatibility stub for `global-hl-line-mode'.")

(emacs-mode-builtins--define-noop-mode
 global-so-long-mode
 "Headless no-op compatibility stub for `global-so-long-mode'.")

(emacs-mode-builtins--define-noop-mode
 show-paren-mode
 "Headless no-op compatibility stub for `show-paren-mode'.")

;;;; --- macro bridge --------------------------------------------------

(when (emacs-mode-builtins--install-function-p 'define-derived-mode)
  (defmacro define-derived-mode (child parent name &optional doc &rest body)
    "Track H bridge: delegates to `emacs-mode-define-derived-mode'.

Built without backquote syntax on purpose (Doc 33 §8 item 221): the
standalone NeLisp reader's macro system does not correctly invoke a
user-defined macro whose expansion-producing body is a backquote
template.  See `emacs-mode-define-derived-mode' for the fuller note."
    (append (list 'emacs-mode-define-derived-mode child parent name doc)
            body)))

(provide 'emacs-mode-builtins)

;;; emacs-mode-builtins.el ends here
