;;; nemacs-next-m5-fixture.el --- M5 pure-Elisp package smoke fixture -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; A deliberately small external-package-shaped fixture for Doc 31 M5.
;; It exercises mode activation, keymaps, hooks, faces, and buffer edits
;; through reusable nelisp-emacs APIs.

;;; Code:

(require 'emacs-mode)
(require 'emacs-keymap)
(require 'emacs-faces)
(require 'emacs-buffer)

(defvar nemacs-next-m5-fixture-mode-hook nil
  "Hook run by `nemacs-next-m5-fixture-mode'.")

(defvar nemacs-next-m5-fixture-hook-count 0
  "Number of times the M5 fixture mode hook ran in the current smoke.")

(defvar nemacs-next-m5-fixture-mode-map
  (let ((map (emacs-keymap-make-sparse-keymap)))
    (emacs-keymap-define-key
     map "n" 'nemacs-next-m5-fixture-insert-stamp)
    map)
  "Keymap used by `nemacs-next-m5-fixture-mode'.")

(emacs-faces-defface
 nemacs-next-m5-fixture-face
 '((t :foreground "green" :weight bold))
 "Face used by the M5 package fixture.")

(defconst nemacs-next-m5-fixture-package-compat-debt
  '((:api "byte-compile-file"
     :status "unsupported"
     :owner "package-compat"
     :reason "M5 loads source fixtures only; byte/native compilation remains outside protocol V0.")
    (:api "autoload-cookie-generation"
     :status "unsupported"
     :owner "package-compat"
     :reason "M5 requires explicit fixture loading; package autoload generation is future package-compat work."))
  "Known package-compat debt exposed by the M5 smoke.")

(defun nemacs-next-m5-fixture--hook-marker ()
  "Record that the fixture mode hook ran."
  (setq nemacs-next-m5-fixture-hook-count
        (+ nemacs-next-m5-fixture-hook-count 1)))

(defun nemacs-next-m5-fixture-mode ()
  "Activate the M5 fixture major mode."
  (interactive)
  (emacs-mode-set-major-mode 'nemacs-next-m5-fixture-mode "M5-Fixture")
  (emacs-keymap-use-local-map nemacs-next-m5-fixture-mode-map)
  (emacs-mode-run-mode-hooks 'nemacs-next-m5-fixture-mode-hook)
  nil)

(defun nemacs-next-m5-fixture-insert-stamp ()
  "Insert fixture text and apply the fixture face through buffer APIs."
  (interactive)
  (let ((start (nelisp-ec-point)))
    (nelisp-ec-insert "M5 fixture edit")
    (emacs-buffer-put-text-property
     start (nelisp-ec-point) 'face 'nemacs-next-m5-fixture-face)
    (nelisp-ec-buffer-string)))

(provide 'nemacs-next-m5-fixture)

;;; nemacs-next-m5-fixture.el ends here
