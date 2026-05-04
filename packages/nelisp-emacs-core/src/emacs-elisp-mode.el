;;; emacs-elisp-mode.el --- Elisp-mode font-lock keywords  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track T (2026-05-04) — minimal `emacs-lisp-mode' font-lock
;; keyword set, wired through the existing
;;   emacs-font-lock + emacs-syntax-table
;; pipeline (Tracks D / G / J / R / S).
;;
;; Coverage targets (= what the test suite verifies end-to-end):
;;
;;   defun foo …   - `defun' = keyword-face, `foo' = function-name-face
;;   defvar v …    - `defvar' = keyword-face, `v'   = variable-name-face
;;   let / let* / if / when / unless / while / lambda … = keyword-face
;;   t / nil       - constant-face
;;   :keyword      - constant-face
;;   "string"      - string-face   (= via the syntactic post-pass)
;;   ;; comment    - comment-face  (= via the syntactic post-pass)
;;
;; This is intentionally NOT the full upstream elisp-mode set —
;; the full set has tens of regexps and references to internal
;; helper-fns that don't exist in our substrate yet.  The MVP set
;; below covers the demonstrative cases and gives the test a
;; concrete contract.

;;; Code:

(require 'emacs-font-lock)
(require 'emacs-syntax-table)

;;;; --- keyword set -----------------------------------------------------------

(defvar emacs-elisp-mode-font-lock-keywords
  (let* ((definers
          '("defun" "defmacro" "defvar" "defconst" "defcustom"
            "defalias" "defgroup" "defface" "defsubst"
            "define-derived-mode" "define-error" "define-minor-mode"
            "define-key"))
         (special-forms
          '("let" "let\\*" "if" "cond" "when" "unless" "while"
            "lambda" "progn" "prog1" "prog2" "setq" "setq-default"
            "condition-case" "unwind-protect" "catch" "throw"
            "and" "or" "not" "quote" "function"
            "save-excursion" "save-current-buffer" "save-restriction"
            "with-current-buffer" "with-temp-buffer"
            "dolist" "dotimes" "while-let" "if-let" "when-let")))
    (list
     ;; Definers — `defun', etc.
     (cons (concat "\\<\\(" (mapconcat #'identity definers "\\|") "\\)\\>")
           '(0 font-lock-keyword-face))
     ;; Special forms.
     (cons (concat "\\<\\(" (mapconcat #'identity special-forms "\\|") "\\)\\>")
           '(0 font-lock-keyword-face))
     ;; Function name after a definer (= captures NAME in `(defun NAME …)').
     ;; nelisp-rx does not yet support shy groups (= `\(?:...\)'), so the
     ;; alternation here is a regular capturing group at index 1 and
     ;; NAME ends up at index 2.
     (list (concat "(\\<\\(defun\\|defmacro\\|defalias\\|defsubst"
                   "\\|define-derived-mode\\|define-minor-mode\\)\\>[ \t\n]+"
                   "\\([A-Za-z_][A-Za-z0-9_-]*\\)")
           '(2 font-lock-function-name-face))
     ;; Variable name after defvar / defconst / defcustom.
     (list (concat "(\\<\\(defvar\\|defconst\\|defcustom\\)\\>[ \t\n]+"
                   "\\([A-Za-z_][A-Za-z0-9_-]*\\)")
           '(2 font-lock-variable-name-face))
     ;; Booleans + constants.
     (list "\\<\\(t\\|nil\\)\\>" '(0 font-lock-constant-face))
     ;; Keyword arguments / property keys.
     (list ":[A-Za-z][A-Za-z0-9_-]*" '(0 font-lock-constant-face))))
  "Doc 51 Track T (MVP) font-lock-keywords for `emacs-lisp-mode'.
Each entry is a (REGEXP HIGHLIGHT) cons in the canonical
emacs-font-lock form.  The string / comment faces are NOT here
— they are applied by the syntactic post-pass in
`emacs-font-lock-default-fontify-region'.")

;;;; --- setup hook ------------------------------------------------------------

(defun emacs-elisp-mode-setup-font-lock (&optional buf)
  "Install elisp-mode keywords on BUF (default = current) and
turn font-lock-mode on.  Idempotent — re-running just rewrites
the keyword set.  No-op (= returns nil) when no current buffer
is available, so the function is safe to install on the
emacs-lisp-mode-hook even in test fixtures that activate the
mode without a buffer (= the existing emacs-mode-builtins-test
pattern)."
  (let ((b (or buf
               (and (boundp 'nelisp-ec--current-buffer)
                    nelisp-ec--current-buffer)
               (condition-case _
                   (and (fboundp 'emacs-buffer--current)
                        (emacs-buffer--current))
                 (error nil)))))
    (when b
      (emacs-font-lock-add-keywords nil
                                    emacs-elisp-mode-font-lock-keywords
                                    'set)
      (emacs-font-lock-mode 1))))

;; Auto-attach to the existing major-mode hook so ANY buffer that
;; enters emacs-lisp-mode picks up the keyword set without each
;; caller having to remember to invoke setup explicitly.  Both
;; the prefixed `emacs-mode-emacs-lisp-mode-hook' and the
;; unprefixed `emacs-lisp-mode-hook' are populated for parity.
(when (boundp 'emacs-mode-emacs-lisp-mode-hook)
  (add-hook 'emacs-mode-emacs-lisp-mode-hook
            #'emacs-elisp-mode-setup-font-lock))
(when (boundp 'emacs-lisp-mode-hook)
  (add-hook 'emacs-lisp-mode-hook
            #'emacs-elisp-mode-setup-font-lock))

(provide 'emacs-elisp-mode)

;;; emacs-elisp-mode.el ends here
