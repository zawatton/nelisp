;;; standalone-macro-alias-check.el --- symbolic macro aliases in eval -*- lexical-binding: t; -*-

;; GNU eval.c eval_sub follows the operator's function cell through
;; indirect_function before it tests for a macro, and macroexpand follows
;; the same chain.  The golden output (.expected) is GNU 31.1's:
;;   emacs -Q --batch -l test/standalone-macro-alias-check.el
;; Cases: an alias of `cl-block' (as libraries define `block'), an alias
;; defined before its target became a macro, aliases nested in function
;; bodies (also inside `cl-flet'), a two-link chain, a target redefined
;; after the alias was used, eval, and macroexpand/macroexpand-1.

(require 'cl-lib)

(defmacro mac-target (x) (list 'list ''expanded x))
(defalias 'mac-alias 'mac-target)
(defalias 'mac-alias2 'mac-alias)
;; Defined before its target is a macro, as in a file loaded out of order.
(defalias 'mac-early 'mac-late)
(defmacro mac-late (x) (list 'cons ''late x))
(defalias 'mac-block 'cl-block)

(defun mac-nested (y) (list (mac-alias y) (mac-alias2 (1+ y)) (mac-early y)))
;; `cl-flet' rather than `cl-flet*': the standalone prelude's cl-lib has
;; no `cl-flet*' at all, which is a library gap, not an alias one.
(defun mac-flet (z) (cl-flet ((g (v) (mac-alias v))) (g z)))
(defun mac-block-search (n)
  (mac-block outer
    (dolist (i '(1 2 3)) (when (= i n) (cl-return-from outer (* 10 i))))
    'none))
(defun mac-try (thunk) (condition-case err (funcall thunk) (error (list 'err (car err)))))
;; NeLisp only, no output: compile the bodies for the VM as well, so the
;; nested aliases also go through the function compiler's expansion.
(when (fboundp 'nelisp--fx-register)
  (dolist (f '(mac-nested mac-flet mac-block-search)) (nelisp--fx-register f)))

(defvar mac-results nil)
(push (mac-try (lambda () (mac-alias 1))) mac-results)
(push (mac-try (lambda () (mac-alias2 2))) mac-results)
(push (mac-try (lambda () (mac-early 3))) mac-results)
(push (mac-try (lambda () (mac-nested 4))) mac-results)
(push (mac-try (lambda () (mac-flet 5))) mac-results)
(push (mac-try (lambda () (mac-block-search 2))) mac-results)
(push (mac-try (lambda () (mac-block-search 9))) mac-results)
(push (mac-try (lambda () (mac-block x (cl-return-from x 'from-block)))) mac-results)
(push (mac-try (lambda () (eval '(mac-alias 6) t))) mac-results)
(push (macroexpand '(mac-alias 7)) mac-results)
(push (macroexpand-1 '(mac-alias2 8)) mac-results)
(push (macroexpand '(mac-early 9)) mac-results)
(push (macroexpand-all '(progn (mac-alias 10) (mac-early 11))) mac-results)
;; The alias follows a later redefinition of its target, as GNU's does.
(defmacro mac-target (x) (list 'list ''redefined x))
;; (A function defined before the redefinition keeps GNU's load-time eager
;; expansion, a loader property this check leaves out.)
(push (mac-try (lambda () (mac-alias 12))) mac-results)
;; The runtime announces the capability so a loader can drop its rewrite.
(push (or (not (fboundp 'nelisp--fx-register))
          (featurep 'nelisp-native-macro-alias))
      mac-results)
(princ (format "MACRO-ALIAS %S\n" (nreverse mac-results)))
(kill-emacs 0)
