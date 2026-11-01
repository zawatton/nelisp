;;; nl-typecheck-standalone-smoke.el --- run nl-typecheck on target/nelisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Arguments after `--': SRC-DIR FILE.  Loads the checker sources from
;; SRC-DIR (plus the sibling nl-prelude/nl-safe/nl-static packages) and
;; prints the diagnostics for FILE.

;;; Code:

(let* ((args command-line-args-left)
       (src (car args))
       (file (car (cdr args)))
       (pkgs (expand-file-name ".." src)))
  (dolist (d (list (expand-file-name "../nl-prelude/src" pkgs)
                   (expand-file-name "../nl-safe/src" pkgs)
                   (expand-file-name "../nl-static/src" pkgs)
                   src))
    (add-to-list 'load-path d))
  (require 'nl-typecheck)
  (dolist (d (nl-typecheck-file file))
    (princ (concat (nl-typecheck-format-diagnostic d) "\n"))))
;;; nl-typecheck-standalone-smoke.el ends here
