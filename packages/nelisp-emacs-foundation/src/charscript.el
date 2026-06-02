;;; charscript.el --- lightweight character script table  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Small pure-Elisp substitute for Emacs' generated charscript table.
;; The full vendor file covers the complete Unicode range; the daily-driver
;; path only needs a stable `char-script-table' and script list without paying
;; to materialize every generated range during startup.

;;; Code:

(require 'case-table)

(defconst charscript--scripts
  '(latin phonetic greek coptic cyrillic armenian hebrew arabic syriac
          devanagari bengali tamil thai kana hangul han cjk-misc symbol
          emoji)
  "Script names exposed by the lightweight charscript facade.")

(defun charscript--standalone-p ()
  "Return non-nil under standalone NeLisp."
  (not (boundp 'emacs-version)))

(defun charscript--make-table ()
  "Return a lightweight script table for common daily-driver characters."
  (let ((table (make-char-table 'char-script-table nil)))
    (set-char-table-range table '(#x00 . #x7f) 'latin)
    (set-char-table-range table '(#xa0 . #xff) 'latin)
    (set-char-table-extra-slot table 0 charscript--scripts)
    table))

(defvar charscript--table (charscript--make-table)
  "Standalone lightweight character script table.")

(defun charscript--ensure-table ()
  "Ensure `char-script-table' is bound to a usable table."
  (when (or (charscript--standalone-p)
            (not (boundp 'char-script-table)))
    (setq char-script-table charscript--table))
  (when (and (boundp 'char-script-table)
             (char-table-p char-script-table)
             (not (char-table-extra-slot char-script-table 0)))
    (set-char-table-extra-slot char-script-table 0 charscript--scripts))
  char-script-table)

(defun charscript--char-script (char)
  "Return script symbol for CHAR using the lightweight table."
  (if (and (integerp char)
           (>= char 0)
           (< char 256))
      (aref (charscript--ensure-table) char)
    nil))

(charscript--ensure-table)

(provide 'charscript)

;;; charscript.el ends here
