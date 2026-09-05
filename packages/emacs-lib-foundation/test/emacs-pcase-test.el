;;; emacs-pcase-test.el --- Tests for emacs-pcase  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the minimal `pcase' port split out of `emacs-stub.el'.
;; Under batch host Emacs the vendor `pcase' family wins by default, so
;; a few tests temporarily unbind the relevant symbols and reload the
;; module to exercise the local helpers and stub macroexpansions.

;;; Code:

(require 'ert)
(require 'emacs-pcase)

(defconst emacs-pcase-test--module-file
  (expand-file-name "../src/emacs-pcase.el"
                    (file-name-directory (or load-file-name buffer-file-name))))

(defun emacs-pcase-test--with-reloaded-module (symbols thunk)
  "Reload `emacs-pcase' with SYMBOLS temporarily unbound, then call THUNK."
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym symbols)
            (push (cons sym (and (fboundp sym) (symbol-function sym))) saved)
            (fmakunbound sym))
          (load emacs-pcase-test--module-file t t)
          (funcall thunk))
      (dolist (cell saved)
        (if (cdr cell)
            (fset (car cell) (cdr cell))
          (fmakunbound (car cell)))))))

;;;; Load / feature contract

(ert-deftest emacs-pcase-test/require-loads-cleanly ()
  (should (featurep 'emacs-pcase))
  (should (featurep 'pcase))
  (should (fboundp 'pcase))
  (should (fboundp 'pcase-defmacro))
  (should (fboundp 'pcase-let))
  (should (fboundp 'pcase-let*))
  (should (fboundp 'pcase-dolist)))

;;;; Helper coverage

(ert-deftest emacs-pcase-test/test-helper-covers-basic-patterns ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '_ 'v) '(t)))
     (should (equal (emacs-pcase--test 'sym 'v) '(t (sym v))))
     (should (equal (emacs-pcase--test 7 'v) '((equal v 7))))
     (should (equal (emacs-pcase--test "x" 'v) '((equal v "x")))))))

(ert-deftest emacs-pcase-test/test-helper-covers-quote-pred-and-bare-cons ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '(quote q) 'v) '((equal v 'q))))
     (should (equal (emacs-pcase--test '(pred symbolp) 'v) '((funcall #'symbolp v))))
     (should (equal (emacs-pcase--test '(foo . bar) 'v) '(t))))))

(ert-deftest emacs-pcase-test/test-helper-covers-and-and-or ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--and '(sym (quote :a)) 'v)
                    '((let* ((sym v)) (and t (equal v ':a))) (sym v))))
     (should (equal (emacs-pcase--or '((quote :a) (quote :b)) 'v)
                    '((or (equal v ':a) (equal v ':b))))))))

(ert-deftest emacs-pcase-test/test-helper-covers-cons-and-not-pred ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--test '(cons a b) 'v)
                    '((and (consp v) t t) (a (car v)) (b (cdr v)))))
     (should (equal (emacs-pcase--test '(pred (not consp)) 'v)
                    '((not (funcall #'consp v))))))))

(ert-deftest emacs-pcase-test/pred-call-form-injects-value ()
  "Support `(pred (fn extra...))' by appending the matched value.
Org's `org-mks' uses `(pred (string-match re))', which must become
`(string-match re VALUE)' rather than `#'(string-match re)'."
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--pred-form '(string-match re) 'v)
                    '(string-match re v)))
     (should (equal (emacs-pcase--pred-form '(memq _ keys) 'v)
                    '(memq v keys))))))

(ert-deftest emacs-pcase-test/install-p-uses-nonstring-emacs-version ()
  "Standalone/nemacs binds `emacs-version' to a non-string sentinel."
  (let ((emacs-version 'nelisp--unbound-marker))
    (unwind-protect
        (progn
          (fset 'emacs-pcase-test--installed-sentinel #'ignore)
          (should (emacs-pcase--install-function-p
                   'emacs-pcase-test--installed-sentinel)))
      (fmakunbound 'emacs-pcase-test--installed-sentinel))))

(ert-deftest emacs-pcase-test/test-helper-covers-backquote-comma-and-comma-at ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--backquote '(comma x) 'v) '(t (x v))))
     (should (equal (emacs-pcase--backquote '(comma-at rest) 'v) '(t (rest v)))))))

(ert-deftest emacs-pcase-test/test-helper-covers-backquote-nested-cons-and-tail ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (should (equal (emacs-pcase--backquote 'foo 'v) '((equal v 'foo))))
     (should (equal (emacs-pcase--backquote '(a (comma x) (comma-at rest) nil) 'v)
                    '((and (consp v)
                           (equal (car v) 'a)
                           (and (consp (cdr v))
                                t
                                (and (consp (cdr (cdr v)))
                                     t
                                     (and (consp (cdr (cdr (cdr v))))
                                          (null (car (cdr (cdr (cdr v)))))
                                          (null (cdr (cdr (cdr (cdr v)))))))))
                      (x (car (cdr v)))
                      (rest (car (cdr (cdr v))))))))))

(ert-deftest emacs-pcase-test/legacy-reader-dotted-backquote-pattern-works ()
  "Old pcase reader syntax for `` `(,a . ,b) '' must destructure as a cons.
The host reader represents this pattern as `((\\, a) \\, b)`, not a literal
dotted pair datum.  Standalone loads see that exact object shape in normalized
vendor forms, so the local pcase polyfill must accept it."
  (emacs-pcase-test--with-reloaded-module
   '(pcase pcase-let)
   (lambda ()
     (let* ((pattern (cadr (car (read-from-string "`(,a . ,b)"))))
            (built (emacs-pcase--backquote pattern 'v)))
       (should (equal built '((and (consp v) t t)
                              (a (car v))
                              (b (cdr v)))))
       (should (equal (eval (car (read-from-string
                                  "(pcase-let ((`(,a . ,b) '(1 . 2)))
                                     (list a b))"))
                            t)
                      '(1 2)))))))

(ert-deftest emacs-pcase-test/pattern-aware-let-and-dolist-evaluate ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase pcase-let pcase-let* pcase-dolist)
   (lambda ()
     (should (equal (eval '(pcase-let ((`(,key . ,def) '("p" . previous-line)))
                             (cons key def))
                          t)
                    '("p" . previous-line)))
     (should (equal (eval '(pcase-let* ((`(,first . ,rest) '(1 2 3))
                                        (`(,second . ,tail) rest))
                             (list first second tail))
                          t)
                    '(1 2 (3))))
     (should (equal (eval '(let (seen)
                             (pcase-dolist (`(,key . ,def)
                                            '(("p" . previous-line)
                                              ("n" . next-line)))
                               (push (cons key def) seen))
                             (nreverse seen))
                          t)
                    '(("p" . previous-line) ("n" . next-line)))))))

(ert-deftest emacs-pcase-test/bulk-stub-macros-are-overwritten ()
  (let ((saved nil))
    (unwind-protect
        (progn
          (dolist (sym '(pcase-let pcase-let* pcase-dolist))
            (push (list sym
                        (and (fboundp sym) (symbol-function sym))
                        (get sym 'emacs-stub-bulk))
                  saved)
            (fset sym (cons 'macro (lambda (&rest _) nil)))
            (put sym 'emacs-stub-bulk t))
          (load emacs-pcase-test--module-file t t)
          (should (equal (eval '(pcase-let ((`(,key . ,def) '("p" . previous-line)))
                                  (cons key def))
                               t)
                         '("p" . previous-line)))
          (should (equal (eval '(let (seen)
                                  (pcase-dolist (`(,key . ,def)
                                                 '(("p" . previous-line)
                                                   ("n" . next-line)))
                                    (push (cons key def) seen))
                                  (nreverse seen))
                               t)
                         '(("p" . previous-line) ("n" . next-line)))))
      (dolist (cell saved)
        (if (cadr cell)
            (fset (car cell) (cadr cell))
          (fmakunbound (car cell)))
        (put (car cell) 'emacs-stub-bulk (caddr cell))))))

(ert-deftest emacs-pcase-test/pcase-expands-and-evaluates ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase)
   (lambda ()
     (let ((expanded (macroexpand '(pcase x ((quote :a) 1) ('b 2) (_ 3)))))
       (should (eq 'let (car expanded)))
       (should (memq 'cond (flatten-tree expanded)))
       (should (equal (let ((x :a))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      1))
       (should (equal (let ((x 'b))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      2))
     (should (equal (let ((x 99))
                        (pcase x ((quote :a) 1) ('b 2) (_ 3)))
                      3))))))

(ert-deftest emacs-pcase-test/pcase-defmacro-expands-top-level-or-branches ()
  (emacs-pcase-test--with-reloaded-module
   '(pcase pcase-defmacro)
   (lambda ()
     (eval '(pcase-defmacro nelisp-test-leaf (vpat)
              `(or `(t . ,,vpat) (and (pred (not consp)) ,vpat)))
           t)
     (should (eq 'nelisp-test-leaf--pcase-macroexpander
                 (get 'nelisp-test-leaf 'pcase-macroexpander)))
     (let* ((form (list 'pcase (list 'quote '(t . 42))
                        '((nelisp-test-leaf v) v)
                        '(_ :no)))
            (expanded (macroexpand form)))
       (should (memq 'let (flatten-tree expanded)))
       (should (equal (eval expanded t) 42)))
     (let* ((form (list 'pcase (list 'quote 'leaf)
                        '((nelisp-test-leaf v) v)
                        '(_ :no)))
            (expanded (macroexpand form)))
       (should (equal (eval expanded t) 'leaf)))
     (let* ((form (list 'pcase (list 'quote '(branch . nil))
                        '((nelisp-test-leaf v) v)
                        '(_ :no)))
            (expanded (macroexpand form)))
       (should (equal (eval expanded t) :no))))))

(ert-deftest emacs-pcase-test/doc16-round26-pcase-lambda ()
  "Doc 16 round 26: pcase-lambda destructures pattern parameters on call.
The batch host has the real pcase-lambda, pinning the contract."
  (let ((f (pcase-lambda (`(,a ,b) c) (list a b c))))
    (should (equal '(1 2 3) (funcall f '(1 2) 3))))
  ;; plain-symbol parameters pass through unchanged
  (let ((g (pcase-lambda (x y) (+ x y))))
    (should (= 5 (funcall g 2 3))))
  ;; mix of plain and pattern parameters
  (let ((h (pcase-lambda (a `(,b . ,c)) (list a b c))))
    (should (equal '(1 2 3) (funcall h 1 '(2 . 3))))))

;;;; T61 — nested `or' pattern binding propagation (nelisp-core fix)

;; NeLisp core's `nelisp-pcase--or' (vendor/nelisp lisp/nelisp-pcase.el,
;; not part of this repo) drops ALL bindings when an `or' pattern is
;; nested inside another pattern -- only a TOP-LEVEL `(or ...)' clause
;; pattern keeps its bindings, by being split into one `pcase' clause
;; per arm before `nelisp-pcase--test' ever sees the `or' head
;; (`nelisp-pcase--distribute-or', called once by the `pcase' macro on
;; the outermost clause pattern only).  This broke `(require 'evil)' on
;; the standalone: `evil-keybindings.el' -> `evil-add-hjkl-bindings' ->
;; `evil-define-key' -> `evil-with-delay' -> `macroexp-let2*' (vendor
;; emacs-lisp/macroexp.el, loaded verbatim) whose own clause pattern
;; nests an `or' inside a backquote comma position:
;;   `(,(or `(,var ,exp) (and (pred symbolp) var (let exp var))) . ,tl)
;; -- `var'/`exp' came out as literal, unbound template symbols:
;; `(void-variable var)'.  `src/emacs-pcase.el' now redefines
;; `nelisp-pcase--or' by NAME (guarded on `(fboundp 'nelisp-pcase--or)',
;; a no-op under host Emacs, which never binds that name) to propagate
;; each arm's own bindings into the result, gated on that arm's own
;; test -- the same shim idiom this file already uses for `pcase' /
;; `pcase-let' / etc.
;;
;; Host Emacs never defines `nelisp-pcase--*'.  Simulate the relevant
;; slice of the NeLisp-core engine here: a faithful subset of its
;; dispatcher (`--test'/`--and'/`--backquote', covering exactly the
;; pattern heads this one clause pattern uses) plus its ORIGINAL,
;; un-fixed `--or' as the "before" baseline.  Confirm the baseline
;; reproduces the exact `void-variable' failure, then let this repo's
;; `src/emacs-pcase.el' install its guarded override on top (exactly
;; the sequence that happens for real on the standalone: NeLisp core's
;; engine loads first, this repo's shim loads second), and confirm the
;; SAME dispatcher now answers exactly what real Emacs's own native
;; `pcase-exhaustive' answers for the identical pattern and input.
;;
;; Scope note: this fixes the arm `evil-with-delay' actually exercises
;; -- an explicit `(SYM EXPR)' bindings element, matching this
;; pattern's FIRST `or' arm (a nested backquote).  The pattern's SECOND
;; arm, `(and (pred symbolp) var (let exp var))' (a bare-symbol
;; bindings element, where `exp' looks up the sibling `var' by name),
;; surfaces a separate, sequential-binding gap even after this fix --
;; `nelisp-pcase--and' and `pcase' itself wrap their bindings in a
;; plain (parallel) `let', so `exp''s `(let exp var)' value-form can't
;; see `var' yet.  Not reached by `(require 'evil)' or any other vendor
;; package this repo loads today (checked); left as a named follow-up
;; rather than widening this ticket's fix into the shared `pcase'
;; body-wrap other constructs depend on.

(defun emacs-pcase-test--t61-nelisp-test (pattern value-form)
  "Faithful subset of NeLisp core's `nelisp-pcase--test', T61 pattern only."
  (cond
   ((symbolp pattern) (cons t (list (list pattern value-form))))
   ((consp pattern)
    (let ((head (car pattern)) (rest (cdr pattern)))
      (cond
       ((eq head 'pred)
        (cons (list 'funcall (list 'function (car rest)) value-form) nil))
       ((eq head 'let)
        (emacs-pcase-test--t61-nelisp-test (car rest) (car (cdr rest))))
       ((eq head 'and) (emacs-pcase-test--t61-nelisp-and rest value-form))
       ((eq head 'or) (nelisp-pcase--or rest value-form))
       ((or (eq head 'backquote) (eq head '\`))
        (emacs-pcase-test--t61-nelisp-backquote (car rest) value-form))
       (t (error "t61 test stub: unhandled head %S" head)))))
   (t (error "t61 test stub: unhandled pattern %S" pattern))))

(defun emacs-pcase-test--t61-nelisp-and (patterns value-form)
  "Faithful subset of NeLisp core's `nelisp-pcase--and'."
  (let ((tests nil) (bindings nil))
    (dolist (p patterns)
      (let ((built (emacs-pcase-test--t61-nelisp-test p value-form)))
        (push (car built) tests)
        (setq bindings (append bindings (cdr built)))))
    (let ((joined (cons 'and (nreverse tests))))
      (cons (if bindings (list 'let bindings joined) joined) bindings))))

(defun emacs-pcase-test--t61-nelisp-backquote (pat value-form)
  "Faithful subset of NeLisp core's `nelisp-pcase--backquote'."
  (cond
   ((and (consp pat) (memq (car pat) '(comma \,)))
    (let ((sym (car (cdr pat))))
      (if (symbolp sym)
          (cons t (list (list sym value-form)))
        (emacs-pcase-test--t61-nelisp-test sym value-form))))
   ((consp pat)
    (let* ((hb (emacs-pcase-test--t61-nelisp-backquote (car pat) (list 'car value-form)))
           (tb (emacs-pcase-test--t61-nelisp-backquote (cdr pat) (list 'cdr value-form))))
      (cons (list 'and (list 'consp value-form) (car hb) (car tb))
            (append (cdr hb) (cdr tb)))))
   ((null pat) (cons (list 'null value-form) nil))
   (t (cons (list 'equal value-form (list 'quote pat)) nil))))

(defconst emacs-pcase-test--t61-pattern
  '`(,(or `(,var ,exp) (and (pred symbolp) var (let exp var))) . ,tl)
  "Verbatim copy of `macroexp-let2*''s own clause pattern
\(vendor/emacs-lisp emacs-lisp/macroexp.el, function `macroexp-let2*').")

(defun emacs-pcase-test--t61-eval (input)
  "Build+eval the (TEST . BINDINGS) protocol for the T61 pattern against INPUT.
Assumes `nelisp-pcase--test'/`nelisp-pcase--and'/`nelisp-pcase--backquote'/
`nelisp-pcase--or' are already bound (baseline or fixed)."
  (let* ((built (emacs-pcase-test--t61-nelisp-test
                 emacs-pcase-test--t61-pattern 'v))
         (test (car built))
         (bindings (cdr built))
         (form (list 'let (list (list 'v (list 'quote input)))
                      (list 'if test
                            (if bindings
                                (cons 'let (cons bindings '((list var exp tl))))
                              '(list var exp tl))
                            ''NO-MATCH))))
    (eval form t)))

(defun emacs-pcase-test--t61-original-or (patterns value-form)
  "Faithful copy of NeLisp core's ORIGINAL (pre-T61) `nelisp-pcase--or':
builds one combined test, drops ALL bindings (its own documented
trade-off for a NESTED `or' -- see vendor/nelisp lisp/nelisp-pcase.el)."
  (cons (cons 'or (mapcar (lambda (p)
                             (car (emacs-pcase-test--t61-nelisp-test p value-form)))
                           patterns))
        nil))

(defmacro emacs-pcase-test--t61-with-simulated-core (or-fn &rest body)
  "Bind NeLisp core's `nelisp-pcase--test'/`--and'/`--backquote'/`--or'
\(the last to OR-FN) for the dynamic extent of BODY, then restore
whatever they were before (normally unbound, on host)."
  (declare (indent 1))
  `(let ((emacs-pcase-test--t61-saved
          (mapcar (lambda (sym) (cons sym (and (fboundp sym) (symbol-function sym))))
                  '(nelisp-pcase--test nelisp-pcase--and
                    nelisp-pcase--backquote nelisp-pcase--or))))
     (unwind-protect
         (progn
           (fset 'nelisp-pcase--test #'emacs-pcase-test--t61-nelisp-test)
           (fset 'nelisp-pcase--and #'emacs-pcase-test--t61-nelisp-and)
           (fset 'nelisp-pcase--backquote #'emacs-pcase-test--t61-nelisp-backquote)
           (fset 'nelisp-pcase--or ,or-fn)
           ,@body)
       (dolist (cell emacs-pcase-test--t61-saved)
         (if (cdr cell) (fset (car cell) (cdr cell)) (fmakunbound (car cell)))))))

(ert-deftest emacs-pcase-test/t61-nested-or-baseline-drops-var ()
  "Pin the T61 bug: NeLisp core's ORIGINAL `--or' answers void-variable
for `macroexp-let2*''s own nested-or clause pattern."
  (emacs-pcase-test--t61-with-simulated-core #'emacs-pcase-test--t61-original-or
    (should (eq 'void-variable
                (car (condition-case e
                         (emacs-pcase-test--t61-eval '((foo (bar baz)) qux))
                       (error e)))))))

(ert-deftest emacs-pcase-test/t61-nested-or-fix-matches-host ()
  "This repo's fix: nested `or' binding propagation matches real Emacs
for the ARM `evil-with-delay' actually exercises -- an explicit
`(SYM EXPR)' bindings element.  Reloads `src/emacs-pcase.el' with
`nelisp-pcase--test'/`--or' simulating NeLisp core already loaded, so
the file's guarded T61 override installs over the baseline, then
checks it against real Emacs's own native `pcase-exhaustive' for the
identical pattern and input."
  (let ((host-answer
         (pcase-exhaustive '((foo (bar baz)) qux)
           (`(,(or `(,var ,exp) (and (pred symbolp) var (let exp var)))
              . ,tl)
            (list var exp tl)))))
    (should (equal host-answer '(foo (bar baz) (qux))))
    (emacs-pcase-test--t61-with-simulated-core #'emacs-pcase-test--t61-original-or
      (load emacs-pcase-test--module-file t t)
      (should (equal (emacs-pcase-test--t61-eval '((foo (bar baz)) qux))
                     host-answer)))))

(provide 'emacs-pcase-test)

;;; emacs-pcase-test.el ends here
