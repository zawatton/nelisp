;;; emacs-parity-cc-test.el --- Tests for cc parity rewrite -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;;; Commentary:

;; Focused host-side tests for the safe partial `c--macroexpand-all' rewrite.

;;; Code:

(require 'ert)
(require 'emacs-parity-cc)

(ert-deftest emacs-parity-cc-rewrites-c-lang-const-calls ()
  (should
   (equal
    (emacs-parity-cc--rewrite-c-lang-const
     '(list (c-lang-const c-symbol-start)
            (concat (c-lang-const c-symbol-chars c++)
                    (c-lang-const c-simple-ws))))
    '(list (c-get-lang-constant 'c-symbol-start)
           (concat (c-get-lang-constant 'c-symbol-chars c++)
                   (c-get-lang-constant 'c-simple-ws))))))

(ert-deftest emacs-parity-cc-leaves-quoted-and-backquoted-data-alone ()
  (should
   (equal
    (emacs-parity-cc--rewrite-c-lang-const
     '(list '(c-lang-const c-symbol-start)
            (function (lambda () (c-lang-const c-symbol-start)))
            (backquote ((comma x) (c-lang-const c-symbol-start)))))
    '(list '(c-lang-const c-symbol-start)
           (function (lambda () (c-lang-const c-symbol-start)))
           (backquote ((comma x) (c-lang-const c-symbol-start)))))))

(ert-deftest emacs-parity-cc-rewrites-backquote-eval-positions-only ()
  (should
   (equal
    (emacs-parity-cc--rewrite-c-lang-const
     '(backquote
       ((comma (c-lang-const c-symbol-start))
        (comma-at (list (c-lang-const c-simple-ws)))
        (c-lang-const c-symbol-chars))))
    '(backquote
      ((comma (c-get-lang-constant 'c-symbol-start))
       (comma-at (list (c-get-lang-constant 'c-simple-ws)))
       (c-lang-const c-symbol-chars))))))

(ert-deftest emacs-parity-cc-rewrites-vectors-and-dotted-tails ()
  (should
   (equal
    (emacs-parity-cc--rewrite-c-lang-const
     '(list [(c-lang-const c-symbol-start)]
            ((c-lang-const c-symbol-chars) . tail)))
    '(list [(c-get-lang-constant 'c-symbol-start)]
           ((c-get-lang-constant 'c-symbol-chars) . tail)))))

(provide 'emacs-parity-cc-test)

;;; emacs-parity-cc-test.el ends here
