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

(require 'nemacs-test-paths
         (expand-file-name "../../../test/emacs-lib/nemacs-test-paths"
                           (file-name-directory (or load-file-name buffer-file-name))))

(require 'ert)
(require 'emacs-backquote)

;; Test fixtures — `defvar'd as special variables so `eval' (dynamic)
;; can resolve them.
(defvar emacs-backquote-test--x nil)
(defvar emacs-backquote-test--xs nil)
(defvar emacs-backquote-test--v nil)
(defvar emacs-backquote-test--w nil)

(defconst emacs-backquote-test--repo-root
  nemacs-test-paths-repo-root
  "Repository root for locating read-only stdlib mirrors in tests.")

(defun emacs-backquote-test--bq-provider-defuns (forms)
  "Return the `nelisp--bq-*' defuns found anywhere in FORMS."
  (let (found)
    (dolist (form forms)
      (when (consp form)
        (if (and (eq (car form) 'defun)
                 (symbolp (cadr form))
                 (string-prefix-p "nelisp--bq-" (symbol-name (cadr form))))
            (push form found)
          (when (memq (car form) '(progn when unless if))
            (setq found (append (reverse (emacs-backquote-test--bq-provider-defuns
                                          (cdr form)))
                                found))))))
    (nreverse found)))

(defun emacs-backquote-test--ensure-runtime-backquote ()
  "Install the runtime `nelisp--bq-expand', then replay the local override.
The runtime expander lives in `emacs-parity-macros2.el', which defines it
only under its standalone guard.  Evaluate just its `nelisp--bq-*'
defuns, so host Emacs keeps its own backquote, CL and gv machinery."
  (unless (fboundp 'nelisp--bq-expand)
    (let ((forms nil))
      (with-temp-buffer
        (insert-file-contents
         (nemacs-test-paths-source "emacs-parity-macros2.el"))
        (condition-case nil
            (while t (push (read (current-buffer)) forms))
          (end-of-file nil)))
      (let ((defuns (emacs-backquote-test--bq-provider-defuns
                     (nreverse forms))))
        (unless (memq 'nelisp--bq-expand (mapcar #'cadr defuns))
          (error "emacs-parity-macros2.el no longer defines nelisp--bq-expand"))
        (dolist (defun-form defuns)
          (eval defun-form t)))))
  (load (nemacs-test-paths-source "emacs-backquote.el")
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

(ert-deftest emacs-backquote-test/runtime-list-nil-level-means-outermost ()
  "The compatibility wrapper treats a nil depth as the outer level."
  (emacs-backquote-test--ensure-runtime-backquote)
  (let ((emacs-backquote-test--x 9))
    (should (equal
             (eval (emacs-backquote--runtime-expand-list
                    '(p (comma emacs-backquote-test--x)) nil)
                   nil)
             '(p 9)))))

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

;; Removed `package-mirror-matches-source': it compared the retired
;; packages/emacs-lib-core mirror with src/, which no longer exists.


;;;; --- S2 coverage batch: GNU backquote.el helper API --------------------
;;
;; These are faithful ports of real GNU Emacs 31.1's own
;; `backquote-list*'/`backquote-process'/`backquote-listify'/
;; `backquote-delay-process' (operating on the `\,'/`\,@'/`` \` `` triad,
;; independent of this file's own `(comma X)' polyfill above).  They are
;; `unless (fboundp ...)'-gated, so under host Emacs these tests exercise
;; real Emacs's own functions -- the exact values below are pinned against
;; that ground truth.

(ert-deftest emacs-backquote-test/s2-batch-loads ()
  (dolist (sym '(backquote-list*-function backquote-list*-macro
                 backquote-list* backquote-process backquote-delay-process
                 backquote-listify))
    (should (fboundp sym))))

(ert-deftest emacs-backquote-test/list*-values ()
  (should (equal (backquote-list*-function 'a 'b 'c) '(a b . c)))
  (should (equal (backquote-list*-function 'a) 'a))
  (should (equal (backquote-list* 'a 'b 'c) '(a b . c))))

(ert-deftest emacs-backquote-test/process-values ()
  (should (equal (backquote-process 5) '(0 . 5)))
  (should (equal (backquote-process ''foo) '(0 quote 'foo)))
  (should (equal (backquote-process [1 2 3]) '(0 . [1 2 3])))
  (let ((form (list 'a (list backquote-unquote-symbol 'b) 'c)))
    (should (equal (backquote-process form) '(1 backquote-list* 'a b '(c)))))
  (let ((form (list 'a (list backquote-splice-symbol 'b))))
    (should (equal (backquote-process form) '(1 cons 'a b)))))

(ert-deftest emacs-backquote-test/process-errors ()
  "More than one arg to `,' signals a plain `error', matching real
GNU Emacs 31.1 (`Multiple args to , are not supported')."
  (let ((form (list backquote-unquote-symbol 'x 'y)))
    (should (eq (car (should-error (backquote-process form))) 'error))))

(provide 'emacs-backquote-test)

;;; emacs-backquote-test.el ends here
