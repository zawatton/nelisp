;;; e2e-repl-standalone-setup.el --- load nl-typecheck inside a standalone REPL -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The REPL feeds `(setq nl-e2e-root "REPO")' first; this file then puts the
;; checker packages on `load-path' and requires the REPL layer.

;;; Code:

(dolist (d '("packages/nl-typecheck/src" "packages/nl-static/src"
             "packages/nl-safe/src" "packages/nl-prelude/src"))
  (add-to-list 'load-path (expand-file-name d nl-e2e-root)))
(require 'nl-typecheck-repl)
(require 'nl-typecheck-entry)
;;; e2e-repl-standalone-setup.el ends here
