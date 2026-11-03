;;; nelisp-emacs-libs.el --- Reusable NeLisp Emacs-core library facade -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs-libs.

;;; Commentary:

;; New product-name facade for the reusable library stack.  The historical
;; `(require 'nelisp-emacs)' entry remains the compatibility facade; this
;; feature aliases it during the migration to `nelisp-emacs-libs'.

;;; Code:

(require 'nelisp-emacs)

(provide 'nelisp-emacs-libs)

;;; nelisp-emacs-libs.el ends here
