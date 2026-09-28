;;; nelisp-backquote-parity-test.el --- GNU backquote provider parity -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Load the pinned GNU Emacs 31.1 provider directly, compare its expansions
;; with the host provider, and assert that the loaded definition came from the
;; vendored source file. Standalone execution is covered by the reader smoke.

;;; Code:

(require 'ert)

(defconst nelisp-backquote-parity-test--root
  (locate-dominating-file
   (or load-file-name buffer-file-name default-directory)
   "vendor"))

(defconst nelisp-backquote-parity-test--vendor
  (expand-file-name "vendor/emacs-lisp/emacs-lisp/backquote.el"
                    nelisp-backquote-parity-test--root))

(defconst nelisp-backquote-parity-test--cases
  '("`atom"
    "`,nelisp-backquote-parity-test--n"
    "`(a b c)"
    "`(a ,nelisp-backquote-parity-test--n b)"
    "`(a ,@nelisp-backquote-parity-test--lst b)"
    "`(a . ,nelisp-backquote-parity-test--n)"
    "`(quote ,nelisp-backquote-parity-test--n)"
    "`[,nelisp-backquote-parity-test--n]"
    "`[a ,nelisp-backquote-parity-test--n b]"
    "`(a (b ,nelisp-backquote-parity-test--n) c)"
    "`(,@nelisp-backquote-parity-test--lst)"
    "`(a . b)"
    "``(a ,,nelisp-backquote-parity-test--n)"
    "`(a `(b ,(+ 1 2)))"
    "`(setq x ,nelisp-backquote-parity-test--n)"
    "`(,nelisp-backquote-parity-test--s . ,nelisp-backquote-parity-test--n)"))

(defvar nelisp-backquote-parity-test--n 42)
(defvar nelisp-backquote-parity-test--lst '(1 2 3))
(defvar nelisp-backquote-parity-test--s "x")

(ert-deftest nelisp-backquote-parity-test-uses-vendored-provider ()
  "The vendored GNU provider expands the same values as the host provider."
  (let ((host-function (symbol-function 'backquote)))
    (unwind-protect
        (progn
          (load nelisp-backquote-parity-test--vendor nil t)
          (should
           (equal (file-truename nelisp-backquote-parity-test--vendor)
                  (file-truename
                   (symbol-file 'backquote 'defun))))
          (dolist (source nelisp-backquote-parity-test--cases)
            (let* ((form (car (read-from-string source)))
                   (structure (cadr form))
                   (host-expansion (funcall (cdr host-function) structure))
                   (vendor-expansion (macroexpand form)))
              (should (equal (eval host-expansion t)
                             (eval vendor-expansion t)))))
          ;; GNU Emacs 31.1's reader/evaluator/printer leave the inner comma
          ;; payload inert at this nesting depth.  Pin that oracle result so
          ;; the standalone smoke does not expect a structural list spelling.
          (let* ((form (car (read-from-string "`(a `(b ,(+ 1 2)))")))
                 (host-value
                  (eval (funcall (cdr host-function) (cadr form)) t))
                 (vendor-value (eval (macroexpand form) t)))
            (should (equal host-value vendor-value))
            (should (equal (prin1-to-string host-value)
                           "(a `(b ,(+ 1 2)))"))))
      (fset 'backquote host-function))))

(provide 'nelisp-backquote-parity-test)
;;; nelisp-backquote-parity-test.el ends here
