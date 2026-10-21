;;; elc-load-fixture.el --- Pinned GNU file-format corpus -*- lexical-binding: t; -*-

;; Compile with GNU Emacs 31.1, without suppressing dynamic docstrings.
;; This exercises #@, #$, #[...], defalias and top-level byte-code forms.
(defvar elc-load-fixture-var 17 "A dynamically stored variable docstring.")
(defconst elc-load-fixture-const '(2 3 5) "A constant docstring.")
(defvar elc-load-fixture-origin load-file-name)
(setq elc-load-fixture-top (mapcar #'1+ '(3 4 5)))
(defun elc-load-fixture-add (a b)
  "Add A and B.  The loader must skip this dynamic docstring correctly."
  (+ a b elc-load-fixture-var))
(defalias 'elc-load-fixture-alias #'elc-load-fixture-add)
(defun elc-load-fixture-closure (a)
  "Return a closure retaining A across a later call."
  (lambda (b) (+ a b)))
(provide 'elc-load-fixture)
