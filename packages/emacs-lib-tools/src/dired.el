;;; dired.el --- Lightweight dired shim for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; GNU Emacs's lisp/dired.el is large and pulls in far more editor
;; substrate than the daily-driver gate needs.  Re-export the existing
;; `emacs-dired-min' implementation as the `dired' feature so `(require
;; 'dired)' selects the lightweight directory browser while the full
;; vendored file remains a later compatibility target.

;;; Code:

(require 'emacs-dired-min)

(provide 'dired)

;;; dired.el ends here
