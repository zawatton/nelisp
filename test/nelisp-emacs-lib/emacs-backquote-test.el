;;; emacs-backquote-test.el --- Tests for emacs-backquote  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the `backquote' polyfill (Doc 51 Phase 2).
;;
;; Tests construct `(backquote ...)` literals explicitly and feed them
;; to `emacs-backquote--expand' so they exercise the polyfill expander
;; regardless of which reader (= host Emacs vs NeLisp) produced them.
;;
;; Variable bindings used inside the backquoted forms are declared as
;; `defvar' (= dynamic / special) so `(eval EXPANSION nil)' (= dynamic
;; eval) sees them.  Using lexical `let' would not work because the
;; polyfill's expansion references the variable by symbol, and lexical
;; `eval' requires the lexical environment to be passed explicitly.

;;; Code:

(require 'ert)
(require 'emacs-backquote)

;; Test fixtures — `defvar'd as special variables so `eval' (dynamic)
;; can resolve them.
(defvar emacs-backquote-test--x nil)
(defvar emacs-backquote-test--xs nil)


;;;; --- atomic + literal list -----------------------------------------------

(ert-deftest emacs-backquote-test/atom-passthrough ()
  (should (equal (eval (emacs-backquote--expand 42) nil) 42))
  (should (equal (eval (emacs-backquote--expand "hello") nil) "hello"))
  (should (eq    (eval (emacs-backquote--expand 'sym) nil) 'sym)))

(ert-deftest emacs-backquote-test/literal-list ()
  (should (equal (eval (emacs-backquote--expand '(a b c)) nil) '(a b c))))

(ert-deftest emacs-backquote-test/nested-literal-list ()
  (should (equal (eval (emacs-backquote--expand '(a (b c) d)) nil)
                 '(a (b c) d))))

(ert-deftest emacs-backquote-test/empty-list ()
  (should (null (eval (emacs-backquote--expand nil) nil))))


;;;; --- comma (unquote) ----------------------------------------------------

(ert-deftest emacs-backquote-test/comma-unquotes-symbol-value ()
  (let ((emacs-backquote-test--x 5))
    (should (equal (eval (emacs-backquote--expand
                          '(a (comma emacs-backquote-test--x) c))
                         nil)
                   '(a 5 c)))))

(ert-deftest emacs-backquote-test/comma-unquotes-expression ()
  (should (equal (eval (emacs-backquote--expand
                        '(a (comma (+ 1 2)) c))
                       nil)
                 '(a 3 c))))

(ert-deftest emacs-backquote-test/comma-at-head ()
  (let ((emacs-backquote-test--x 'hello))
    (should (equal (eval (emacs-backquote--expand
                          '((comma emacs-backquote-test--x) world))
                         nil)
                   '(hello world)))))


;;;; --- comma-at (splice) --------------------------------------------------

(ert-deftest emacs-backquote-test/comma-at-tail ()
  (let ((emacs-backquote-test--xs '(1 2 3)))
    (should (equal (eval (emacs-backquote--expand
                          '(a (comma-at emacs-backquote-test--xs)))
                         nil)
                   '(a 1 2 3)))))

(ert-deftest emacs-backquote-test/comma-at-middle ()
  (let ((emacs-backquote-test--xs '(1 2)))
    (should (equal (eval (emacs-backquote--expand
                          '(a (comma-at emacs-backquote-test--xs) c))
                         nil)
                   '(a 1 2 c)))))

(ert-deftest emacs-backquote-test/comma-at-empty-splice ()
  (let ((emacs-backquote-test--xs nil))
    (should (equal (eval (emacs-backquote--expand
                          '(a (comma-at emacs-backquote-test--xs) c))
                         nil)
                   '(a c)))))


;;;; --- mixed comma + comma-at ---------------------------------------------

(ert-deftest emacs-backquote-test/mixed ()
  (let ((emacs-backquote-test--x 'foo)
        (emacs-backquote-test--xs '(a b)))
    (should (equal (eval (emacs-backquote--expand
                          '((comma emacs-backquote-test--x)
                            (comma-at emacs-backquote-test--xs)
                            end))
                         nil)
                   '(foo a b end)))))


;;;; --- top-level comma ----------------------------------------------------

(ert-deftest emacs-backquote-test/top-level-comma ()
  (let ((emacs-backquote-test--x '(1 2 3)))
    (should (equal (eval (emacs-backquote--expand
                          '(comma emacs-backquote-test--x))
                         nil)
                   '(1 2 3)))))

(ert-deftest emacs-backquote-test/top-level-comma-at-errors ()
  (should-error (emacs-backquote--expand '(comma-at xs))))


(provide 'emacs-backquote-test)

;;; emacs-backquote-test.el ends here
