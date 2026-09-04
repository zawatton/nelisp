;;; org.el --- Lightweight Org compatibility entry for nelisp-emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; This compatibility entry makes `(require 'org)' resolve to the
;; reusable lightweight Org subset shipped in `emacs-org-*'.  It is not a
;; full upstream Org replacement; parser-heavy Org Element, Babel, export,
;; and wider integration layers remain separate compatibility targets.

;;; Code:

(require 'emacs-org-outline)
(require 'emacs-org-todo)
(require 'emacs-org-table)

(provide 'org)

;;; org.el ends here
