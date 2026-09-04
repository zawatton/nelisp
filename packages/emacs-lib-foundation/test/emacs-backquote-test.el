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
(defvar emacs-backquote-test--v nil)
(defvar emacs-backquote-test--w nil)

(defconst emacs-backquote-test--repo-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root for locating read-only stdlib mirrors in tests.")

(defun emacs-backquote-test--ensure-runtime-backquote ()
  "Load stdlib backquote support, then replay the local override."
  (unless (fboundp 'nelisp--bq-expand)
    (load (expand-file-name "vendor/nelisp/lisp/nelisp-cl-macros.el"
                            emacs-backquote-test--repo-root)
          nil t))
  (load (expand-file-name "src/emacs-backquote.el"
                          emacs-backquote-test--repo-root)
        nil t))

(defun emacs-backquote-test--runtime-expand (form)
  "Evaluate FORM through the runtime `nelisp--bq-expand' path."
  (emacs-backquote-test--ensure-runtime-backquote)
  (eval (nelisp--bq-expand form) nil))

(defun emacs-backquote-test--contains-form-p (tree wanted)
  "Return non-nil when TREE contains a subtree equal to WANTED."
  (or (equal tree wanted)
      (and (consp tree)
           (or (emacs-backquote-test--contains-form-p (car tree) wanted)
               (emacs-backquote-test--contains-form-p (cdr tree) wanted)))))

(defun emacs-backquote-test--contains-unquoted-symbol-p (tree symbol)
  "Return non-nil when TREE evaluates SYMBOL outside a quoted subtree."
  (cond
   ((eq tree symbol) t)
   ((atom tree) nil)
   ((eq (car tree) 'quote) nil)
   (t
    (or (emacs-backquote-test--contains-unquoted-symbol-p
         (car tree) symbol)
        (emacs-backquote-test--contains-unquoted-symbol-p
         (cdr tree) symbol)))))


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

(ert-deftest emacs-backquote-test/vector-comma ()
  (let ((emacs-backquote-test--x 42))
    (should (equal (emacs-backquote-test--runtime-expand
                    [a (comma emacs-backquote-test--x) b])
                   [a 42 b]))))

(ert-deftest emacs-backquote-test/vector-comma-at ()
  (let ((emacs-backquote-test--xs '(1 2 3)))
    (should (equal (emacs-backquote-test--runtime-expand
                    [a (comma-at emacs-backquote-test--xs) b])
                   [a 1 2 3 b]))))

(ert-deftest emacs-backquote-test/vector-treemacs-shape ()
  (let ((emacs-backquote-test--v "left")
        (emacs-backquote-test--w "right"))
    (should (equal (emacs-backquote-test--runtime-expand
                    (list
                     [(comma (format "x-%s" emacs-backquote-test--v)) sym1]
                     [(comma (format "y-%s" emacs-backquote-test--w)) sym2]))
                   '(["x-left" sym1] ["y-right" sym2])))))


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

(ert-deftest emacs-backquote-test/non-vector-regression-nested-and-dotted ()
  (let ((emacs-backquote-test--x 9))
    (should (equal (emacs-backquote-test--runtime-expand
                    '(p (comma emacs-backquote-test--x)
                        (nested (comma emacs-backquote-test--x))
                        . tailsym))
                   '(p 9 (nested 9) . tailsym)))))

(ert-deftest emacs-backquote-test/runtime-punctuation-nested-generator-shape ()
  "Outer expansion preserves generator-style inner punctuation backquote."
  (emacs-backquote-test--ensure-runtime-backquote)
  (let* ((outer
          (read
           "`(outer (cl-macrolet ((iter-yield (value) `(cps-internal-yield ,value))) (iter-yield 7)))"))
         (inner (read "`(cps-internal-yield ,value)"))
         (expansion (nelisp--bq-expand (cadr outer))))
    (should
     (emacs-backquote-test--contains-form-p
      expansion (list 'quote inner)))
    (should-not
     (emacs-backquote-test--contains-unquoted-symbol-p expansion 'value))))

(ert-deftest emacs-backquote-test/runtime-convenience-nested-marker-regression ()
  "NeLisp convenience backquote/comma markers remain preserved when nested."
  (emacs-backquote-test--ensure-runtime-backquote)
  (let ((inner '(backquote (cps-internal-yield (comma value)))))
    (should (equal (nelisp--bq-expand inner)
                   (list 'quote inner)))))

(ert-deftest emacs-backquote-test/package-mirror-matches-source ()
  "The extracted foundation package keeps the backquote source in sync."
  (let ((source (expand-file-name "src/emacs-backquote.el"
                                  emacs-backquote-test--repo-root))
        (mirror
         (expand-file-name
          "packages/nelisp-emacs-foundation/lisp/emacs-backquote.el"
          emacs-backquote-test--repo-root)))
    (should (equal (with-temp-buffer
                     (insert-file-contents source)
                     (buffer-string))
                   (with-temp-buffer
                     (insert-file-contents mirror)
                     (buffer-string))))))


(provide 'emacs-backquote-test)

;;; emacs-backquote-test.el ends here
