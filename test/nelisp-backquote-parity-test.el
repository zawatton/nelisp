;;; nelisp-backquote-parity-test.el --- backquote matches Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; NeLisp must read and expand quasiquote the way GNU Emacs does.
;;
;; a4f8c7570 (2026-08-15) aligned the reader and printer with Emacs: `,'
;; became the symbol `\,' rather than an in-house `comma'.  The expander in
;; nelisp-cl-macros.el kept matching the old spelling and was not part of
;; that commit, so from then on every `,' walked past it unexpanded -- the
;; macro handed back the marker itself and callers failed with
;; "(wrong-type-argument symbolp ,name)".  Nothing caught it because there
;; was no test for backquote at all.
;;
;; These compare VALUES against Emacs's own backquote rather than
;; expansions: the two build different forms legitimately (`list' versus
;; `cons' chains), but what they evaluate to may not differ.

;;; Code:

(require 'ert)

;; `nelisp-backquote-parity-test--load-expander' evaluates these out of
;; nelisp-cl-macros.el while a test runs, rather than requiring the file,
;; so the byte-compiler has no way to see them.
(declare-function nelisp--bq-comma-p "nelisp-cl-macros" (sym))
(declare-function nelisp--bq-comma-at-p "nelisp-cl-macros" (sym))
(declare-function nelisp--bq-backquote-p "nelisp-cl-macros" (sym))
(declare-function nelisp--bq-expand "nelisp-cl-macros" (form &optional level))

(defvar nelisp-backquote-parity-test--n 42)
(defvar nelisp-backquote-parity-test--lst '(1 2 3))
(defvar nelisp-backquote-parity-test--s "x")

(defconst nelisp-backquote-parity-test--source
  (expand-file-name
   "lisp/nelisp-cl-macros.el"
   (locate-dominating-file
    (or load-file-name buffer-file-name default-directory)
    "lisp"))
  "The expander source this test file belongs to.

Resolved while this file loads, because `load-file-name' is nil by the
time ERT runs a test body: resolving it there falls back to
`default-directory' and the test then reads whichever checkout the
caller happened to be standing in, not this one.  That silently turned
a control run against the pre-fix expander green.")

(defun nelisp-backquote-parity-test--load-expander ()
  "Evaluate the expander out of nelisp-cl-macros.el.

Loading the whole file would replace Emacs's own `backquote', and then
the comparison would be against NeLisp on both sides."
  (let ((file nelisp-backquote-parity-test--source))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (let (forms)
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (when (and (consp form)
                           (eq (car form) 'defun)
                           (string-prefix-p "nelisp--bq" (format "%s" (cadr form))))
                  (push form forms))))
          (error nil))
        (dolist (form (nreverse forms)) (eval form t))))))

(defconst nelisp-backquote-parity-test--cases
  '("`atom"
    "`,nelisp-backquote-parity-test--n"
    "`(a b c)"
    "`(a ,nelisp-backquote-parity-test--n b)"
    "`(a ,@nelisp-backquote-parity-test--lst b)"
    "`(a . ,nelisp-backquote-parity-test--n)"
    ;; The shape that first surfaced this: `ert-deftest' expands to
    ;; (put ',name ...), i.e. a comma directly inside a quote.
    "`(quote ,nelisp-backquote-parity-test--n)"
    "`[,nelisp-backquote-parity-test--n]"
    "`[a ,nelisp-backquote-parity-test--n b]"
    "`(a (b ,nelisp-backquote-parity-test--n) c)"
    "`(,@nelisp-backquote-parity-test--lst)"
    "`(a . b)"
    "``(a ,,nelisp-backquote-parity-test--n)"
    "`(setq x ,nelisp-backquote-parity-test--n)"
    "`(,nelisp-backquote-parity-test--s . ,nelisp-backquote-parity-test--n)")
  "Source texts evaluated under both Emacs and the NeLisp expander.")

(ert-deftest nelisp-backquote-parity-test-matches-emacs ()
  "Every case evaluates to what Emacs's own backquote produces."
  (nelisp-backquote-parity-test--load-expander)
  (dolist (src nelisp-backquote-parity-test--cases)
    (let* ((form (car (read-from-string src)))
           (expected (eval form t))
           (actual (eval (nelisp--bq-expand (cadr form)) t)))
      (should (equal expected actual)))))

(ert-deftest nelisp-backquote-parity-test-reads-the-emacs-markers ()
  "The expander is driven by the symbols the reader actually produces.

Asserted separately from the value comparison above: if the expander
went back to matching an in-house spelling, every case would still
evaluate correctly under host Emacs -- whose reader hands it the Emacs
symbols anyway -- while NeLisp's own reader output walked past
unexpanded, which is exactly the regression that shipped."
  (nelisp-backquote-parity-test--load-expander)
  (should (nelisp--bq-comma-p '\,))
  (should (nelisp--bq-comma-at-p '\,@))
  (should (nelisp--bq-backquote-p '\`))
  ;; What `read' gives for source text, without going through Emacs's macro.
  (let ((inner (cadr (cadr (car (read-from-string "`(quote ,n)"))))))
    (should (consp inner))
    (should (nelisp--bq-comma-p (car inner)))))

(provide 'nelisp-backquote-parity-test)
;;; nelisp-backquote-parity-test.el ends here
