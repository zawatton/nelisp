;;; e2e-entry.el --- entry-check sessions for e2e-entry.sh -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Arguments after `--': ROOT ENFORCE(on|off) BAD(0|1).  ROOT is the repo.

;;; Code:

(let* ((args (if (equal (car command-line-args-left) "--")
                  (cdr command-line-args-left) command-line-args-left))
       (root (nth 0 args)) (on (equal (nth 1 args) "on")) (bad (equal (nth 2 args) "1")))
  (dolist (d '("packages/nl-typecheck/src" "packages/nl-static/src"
               "packages/nl-safe/src" "packages/nl-prelude/src"))
    (add-to-list 'load-path (expand-file-name d root)))
  (require 'nl-typecheck-entry)
  (setq nelisp-typecheck-enforce on)
  (nl-typecheck-entry-load
   (expand-file-name "packages/nl-typecheck/test/fixtures/entry-typed.el" root))
  (if bad
      (princ (condition-case err (format "bad call returned %S\n" (funcall 'entry-label 1.5))
               (wrong-type-argument (format "SIGNALLED %S\n" err))
               (error "OTHER-ERROR\n")))
    (princ (format "%S\n" (list (funcall 'entry-add 1 2) (funcall 'entry-greet "a")
                                (funcall 'entry-greet "a" "!") (funcall 'entry-label 7)
                                (funcall 'entry-label "s") (funcall 'entry-untyped 1))))))
;;; e2e-entry.el ends here
