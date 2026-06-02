;;; emacs-minibuffer-builtins.el --- Unprefixed minibuffer.c builtin bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track C (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs C-core *unprefixed* minibuffer + completion
;; builtins (= `read-from-minibuffer', `read-string', `completing-read',
;; `yes-or-no-p', `y-or-n-p', `read-number', ...) to the existing
;; `emacs-minibuffer-*' prefixed implementation in `emacs-minibuffer.el',
;; mirroring the Phase 11.C'' / J / L1 / D bridge pattern.
;;
;; Why this exists: until Track C the unprefixed names were nil-stubs in
;; `emacs-stub-bulk.el', so standalone NeLisp callers like
;; `(read-string "Filename: ")' silently returned nil even though
;; `emacs-minibuffer.el' provides a full pluggable reader (=
;; `emacs-minibuffer-feed-input' lets tests / future command-loop
;; inject deterministic input).
;;
;; Function definitions use a host-aware install gate: host Emacs keeps
;; its C builtins, while standalone NeLisp overwrites any bootstrap
;; stubs with the real minibuffer substrate.  Variables are still gated
;; on `unless (boundp ...)' so host-owned special variables win.
;;
;; Bridgeable today (= covered by `emacs-minibuffer.el'):
;;
;;   - read-from-minibuffer / read-string / read-no-blanks-input
;;   - read-key / read-buffer / read-file-name / read-directory-name
;;   - read-passwd / read-number
;;   - y-or-n-p / yes-or-no-p
;;   - completing-read
;;   - minibufferp / active-minibuffer-window / minibuffer-window
;;   - minibuffer-prompt / minibuffer-contents
;;   - minibuffer-prompt-end / minibuffer-prompt-width
;;   - exit-minibuffer / abort-recursive-edit / minibuffer-message
;;
;; Plus history defvars: `minibuffer-history' / `command-history' /
;; `file-name-history' / `read-string-history' / `buffer-name-history' /
;; `regexp-history' / `extended-command-history'.

;;; Code:

(require 'emacs-minibuffer)

;;;; --- core readers ----------------------------------------------------

(defun emacs-minibuffer-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-minibuffer-builtins--install-function-p 'read-from-minibuffer)
  (defalias 'read-from-minibuffer #'emacs-minibuffer-read-from-minibuffer))

(when (emacs-minibuffer-builtins--install-function-p 'read-string)
  (defalias 'read-string #'emacs-minibuffer-read-string))

(when (emacs-minibuffer-builtins--install-function-p 'read-no-blanks-input)
  (defalias 'read-no-blanks-input #'emacs-minibuffer-read-no-blanks-input))

(when (emacs-minibuffer-builtins--install-function-p 'read-key)
  (defalias 'read-key #'emacs-minibuffer-read-key))

;;;; --- typed readers ---------------------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'read-buffer)
  (defalias 'read-buffer #'emacs-minibuffer-read-buffer))

(when (emacs-minibuffer-builtins--install-function-p 'read-file-name)
  (defalias 'read-file-name #'emacs-minibuffer-read-file-name))

(when (emacs-minibuffer-builtins--install-function-p 'read-directory-name)
  (defalias 'read-directory-name #'emacs-minibuffer-read-directory-name))

(when (emacs-minibuffer-builtins--install-function-p 'read-passwd)
  (defalias 'read-passwd #'emacs-minibuffer-read-passwd))

(when (emacs-minibuffer-builtins--install-function-p 'read-number)
  (defalias 'read-number #'emacs-minibuffer-read-number))

;;;; --- confirmation ---------------------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'y-or-n-p)
  (defalias 'y-or-n-p #'emacs-minibuffer-y-or-n-p))

(when (emacs-minibuffer-builtins--install-function-p 'yes-or-no-p)
  (defalias 'yes-or-no-p #'emacs-minibuffer-yes-or-no-p))

;;;; --- completion -----------------------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'completing-read)
  (defalias 'completing-read #'emacs-minibuffer-completing-read))

;;;; --- minibuffer state / control --------------------------------------

(when (emacs-minibuffer-builtins--install-function-p 'minibufferp)
  (defalias 'minibufferp #'emacs-minibuffer-minibufferp))

(when (emacs-minibuffer-builtins--install-function-p 'active-minibuffer-window)
  (defalias 'active-minibuffer-window #'emacs-minibuffer-active-minibuffer-window))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-window)
  (defalias 'minibuffer-window #'emacs-minibuffer-minibuffer-window))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-prompt)
  (defalias 'minibuffer-prompt #'emacs-minibuffer-minibuffer-prompt))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-contents)
  (defalias 'minibuffer-contents #'emacs-minibuffer-minibuffer-contents))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-prompt-end)
  (defalias 'minibuffer-prompt-end #'emacs-minibuffer-minibuffer-prompt-end))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-prompt-width)
  (defalias 'minibuffer-prompt-width #'emacs-minibuffer-minibuffer-prompt-width))

(when (emacs-minibuffer-builtins--install-function-p 'exit-minibuffer)
  (defalias 'exit-minibuffer #'emacs-minibuffer-exit-minibuffer))

(when (emacs-minibuffer-builtins--install-function-p 'abort-recursive-edit)
  (defalias 'abort-recursive-edit #'emacs-minibuffer-abort-recursive-edit))

(when (emacs-minibuffer-builtins--install-function-p 'minibuffer-message)
  (defalias 'minibuffer-message #'emacs-minibuffer-minibuffer-message))

;;;; --- history defvars ------------------------------------------------

;; Most callers expect these defvars to exist as the HIST symbol they
;; pass to read-from-minibuffer.  Pre-defining them prevents void-variable
;; under standalone NeLisp.

(unless (boundp 'minibuffer-history)
  (defvar minibuffer-history nil
    "Track C bridge: alias for `emacs-minibuffer-history'."))

(unless (boundp 'command-history)
  (defvar command-history nil
    "Track C bridge: list of commands previously executed."))

(unless (boundp 'file-name-history)
  (defvar file-name-history nil
    "Track C bridge: history list for file-name reads."))

(unless (boundp 'read-string-history)
  (defvar read-string-history nil
    "Track C bridge: default history list for `read-string'."))

(unless (boundp 'buffer-name-history)
  (defvar buffer-name-history nil
    "Track C bridge: history list for `read-buffer'."))

(unless (boundp 'regexp-history)
  (defvar regexp-history nil
    "Track C bridge: history list for regexp prompts."))

(unless (boundp 'extended-command-history)
  (defvar extended-command-history nil
    "Track C bridge: history list for `M-x' / `execute-extended-command'."))

(provide 'emacs-minibuffer-builtins)

;;; emacs-minibuffer-builtins.el ends here
