;;; emacs-select.el --- Shared GUI selection policy -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Backend callbacks own transport. This module owns select.el policy and the
;; explicit compatibility shim; loading it never replaces host Emacs functions.
(defvar emacs-select-backend nil "Plist of :set, :get, :owner and :exists callbacks.")
(defvar select-enable-clipboard t)
(defvar select-enable-primary nil)
(defvar interprogram-cut-function nil)
(defvar interprogram-paste-function nil)
(defvar x-select-request-type nil)
(defvar emacs-select--last nil)
(defvar gui-last-cut-in-clipboard nil)
(defvar gui-last-cut-in-primary nil)

(defun emacs-select--type (type)
  (cond ((null type) 'PRIMARY) ((eq type t) 'SECONDARY) (t type)))
(defun emacs-select--call (operation &rest args)
  (let ((fn (plist-get emacs-select-backend operation)))
    (when fn (apply fn args))))
(defun emacs-select-set (type data)
  "Set TYPE to string DATA, or disown our selection when DATA is nil."
  (unless (or (null data) (stringp data))
    (signal 'wrong-type-argument (list 'stringp data)))
  (emacs-select--call :set (emacs-select--type type) data)
  data)
(defun emacs-select-get (&optional type data-type _time _terminal)
  "Get TYPE as DATA-TYPE (default STRING); return nil on unavailable targets."
  (emacs-select--call :get (emacs-select--type type) (or data-type 'STRING)))
(defun emacs-select-owner-p (&optional type)
  (and (emacs-select--call :owner (emacs-select--type type)) t))
(defun emacs-select-exists-p (&optional type)
  (and (emacs-select--call :exists (emacs-select--type type)) t))
(defun emacs-select--remember (type text)
  (let ((value (list text (emacs-select-get type 'TIMESTAMP)))
        (cell (assq type emacs-select--last)))
    (if cell (setcdr cell value) (push (cons type value) emacs-select--last))))
(defun emacs-select-text (text)
  "Publish TEXT to enabled selections, as GNU gui-select-text does."
  (when select-enable-primary
    (emacs-select-set 'PRIMARY text) (emacs-select--remember 'PRIMARY text))
  (when select-enable-clipboard
    (emacs-select-set 'CLIPBOARD text) (emacs-select--remember 'CLIPBOARD text))
  (setq gui-last-cut-in-clipboard select-enable-clipboard
        gui-last-cut-in-primary select-enable-primary))
(defun emacs-select--text (type)
  (let ((targets (or x-select-request-type '(UTF8_STRING STRING))) (text nil))
    (unless (consp targets) (setq targets (list targets)))
    (condition-case nil
        (while (and targets (not text))
          (setq text (emacs-select-get type (car targets)) targets (cdr targets)))
      (error nil))
    text))
(defun emacs-select--new-text (type cut)
  (unless (and (eq type 'CLIPBOARD) cut (emacs-select-owner-p type))
    (let* ((text (emacs-select--text type))
           (timestamp (and text (emacs-select-get type 'TIMESTAMP)))
           (old (cdr (assq type emacs-select--last))))
      (when (and (stringp text) (> (length text) 0))
        (emacs-select--remember type text)
        (unless (and cut (equal old (list text timestamp))) text)))))
(defun emacs-select-value ()
  "Return changed external text, preferring CLIPBOARD over PRIMARY.
Remember both independently; a timestamp change permits identical new text."
  (let ((clip (and select-enable-clipboard
                   (emacs-select--new-text 'CLIPBOARD gui-last-cut-in-clipboard)))
        (primary (and select-enable-primary
                      (emacs-select--new-text 'PRIMARY gui-last-cut-in-primary))))
    (or clip primary)))
(defun emacs-select-primary ()
  (or (emacs-select--text 'PRIMARY) (error "No selection is available")))
(defun emacs-select-install (backend)
  "Install BACKEND and explicit select.el shims in standalone NeLisp only."
  (setq emacs-select-backend backend)
  (when (fboundp 'nelisp--write-stdout-bytes)
    (dolist (entry '((gui-set-selection . emacs-select-set)
                     (gui-get-selection . emacs-select-get)
                     (gui-selection-owner-p . emacs-select-owner-p)
                     (gui-selection-exists-p . emacs-select-exists-p)
                     (gui-select-text . emacs-select-text)
                     (gui-selection-value . emacs-select-value)
                     (gui-get-primary-selection . emacs-select-primary)
                     (gui-backend-set-selection . emacs-select-set)
                     (gui-backend-get-selection . emacs-select-get)
                     (gui-backend-selection-owner-p . emacs-select-owner-p)
                     (gui-backend-selection-exists-p . emacs-select-exists-p)))
      (defalias (car entry) (cdr entry)))
    (setq interprogram-cut-function #'gui-select-text
          interprogram-paste-function #'gui-selection-value)
    (provide 'select)))
(provide 'emacs-select)
