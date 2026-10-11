;;; nelisp-cl-loop-test.el --- cl-loop subset vs the host's cl-loop  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The standalone runtime ships its own `cl-loop' (an Elisp subset, in
;; `nelisp-cl-macros.el' and byte-identically in the prelude).  An
;; unrecognised clause shape expands to nil, so a loop that the subset
;; cannot model does not fail -- it silently does not run.  That is how
;; 48 of the tree's 62 `cl-loop' forms came to be dead on the standalone
;; while every host test stayed green: the host has the real macro.
;;
;; These tests close that gap the only way that means anything: every
;; case is evaluated twice, once through the subset's own expander and
;; once through the host's `cl-loop', and the two answers must be equal.
;; A shape the subset declines still expands to nil, and the comparison
;; holds it to that -- the host has to answer nil too.

;;; Code:

(require 'ert)
(require 'cl-lib)
;; Exercise the subset helpers without replacing GNU's macros in the host
;; suite. Other tests seal their GNU owners before this file is loaded.
(with-temp-buffer
  (insert-file-contents (locate-library "nelisp-cl-macros.el"))
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (and (eq (car-safe form) 'defun)
                     (string-prefix-p "nelisp-cl-macros--loop-"
                                      (symbol-name (cadr form))))
            (eval form t))))
    (end-of-file nil)))

(defun nelisp-cl-loop-test--walk (form fn)
  "Call FN on FORM and every cons inside it."
  (when (consp form)
    (funcall fn form)
    (let ((rest form))
      (while (consp rest)
        (nelisp-cl-loop-test--walk (car rest) fn)
        (setq rest (cdr rest))))))

(defmacro nelisp-cl-loop-test--subset (&rest clauses)
  "Evaluate CLAUSES through the subset expander under test."
  (nelisp-cl-macros--loop-build clauses))

(defvar nelisp-cl-loop-test--a '(10 20 30))
(defvar nelisp-cl-loop-test--b '(x y z))
(defvar nelisp-cl-loop-test--v [1 2 3])
(defvar nelisp-cl-loop-test--plist '(:a 1 :b 2))
(defvar nelisp-cl-loop-test--i 0)

(defconst nelisp-cl-loop-test--cases
  '(;; one list iterator, every accumulator and both guards
    (for a in nelisp-cl-loop-test--a collect a)
    (for a in nelisp-cl-loop-test--a sum a)
    (for a in nelisp-cl-loop-test--a count (> a 15))
    (for a in nelisp-cl-loop-test--a append (list a a))
    (for a in nelisp-cl-loop-test--a when (> a 15) collect a)
    (for a in nelisp-cl-loop-test--a unless (> a 15) collect a)
    (for a in nelisp-cl-loop-test--a unless (> a 15) append (list a))
    ;; parallel iterators: the shape the subset used to decline outright
    (for a in nelisp-cl-loop-test--a for b in nelisp-cl-loop-test--b
         collect (list a b))
    (for a in nelisp-cl-loop-test--a for i from 0 collect (list a i))
    (for a in nelisp-cl-loop-test--a for b in nelisp-cl-loop-test--b
         when (eq b 'y) collect a)
    (for a in nelisp-cl-loop-test--a for b in nelisp-cl-loop-test--b
         unless (eq b 'y) collect a)
    (for a in nelisp-cl-loop-test--a for b in nelisp-cl-loop-test--b
         for i from 0 collect (list a b i))
    (for i from 0 below 3 for a in nelisp-cl-loop-test--a collect (cons i a))
    ;; the shorter iterator ends the loop
    (for a in nelisp-cl-loop-test--a for b in '(x) collect (list a b))
    ;; numeric, including the directions and the step
    (for i from 0 below 3 collect i)
    (for i from 0 to 3 collect i)
    (for i from 2 downto 0 collect i)
    (for i from 3 above 0 collect i)
    (for i from 0 below 6 by 2 collect i)
    ;; and with `from' left out, which CL allows and this tree writes
    (for i below 3 collect i)
    (for i to 3 collect i)
    (for i below 6 by 2 collect i)
    ;; tails and vectors
    (for tail on nelisp-cl-loop-test--a collect (car tail))
    (for e across nelisp-cl-loop-test--v collect e)
    (for e across nelisp-cl-loop-test--v for a in nelisp-cl-loop-test--a
         collect (list e a))
    ;; stepped values
    (for a in nelisp-cl-loop-test--a for prev = 0 then a collect prev)
    (for a in nelisp-cl-loop-test--a for s = (* a 2) collect s)
    ;; short-circuiting
    (for a in nelisp-cl-loop-test--a always (> a 5))
    (for a in nelisp-cl-loop-test--a always (> a 15))
    (for a in nelisp-cl-loop-test--a when (> a 25) return a)
    ;; termination clauses, bindings, repeat
    (for a in nelisp-cl-loop-test--a while (< a 30) collect a)
    (for a in nelisp-cl-loop-test--a until (> a 25) collect a)
    (with k = 5 for a in nelisp-cl-loop-test--a collect (+ a k))
    (repeat 3 collect 1)
    ;; A loop with no iterator at all is still a loop.  The branch for it
    ;; was lost when the iterators were generalised and nothing caught it,
    ;; because no form in this tree is written this way.
    (while (< nelisp-cl-loop-test--i 3)
           do (setq nelisp-cl-loop-test--i (1+ nelisp-cl-loop-test--i)))
    (until (> nelisp-cl-loop-test--i 2)
           do (setq nelisp-cl-loop-test--i (1+ nelisp-cl-loop-test--i)))
    ;; counting down from an explicit start, whatever the limit keyword
    (for i downfrom 4 to 2 collect i)
    (for i downfrom 4 above 1 collect i)
    (for i downfrom 8 to 2 by 3 collect i)
    ;; a list walked with something other than `cdr'
    (for k in nelisp-cl-loop-test--plist by #'cddr collect k)
    (for k in nelisp-cl-loop-test--plist by #'cddr always (keywordp k))
    ;; `and' continues the guarded clause rather than ending it
    (for i from 0 to 5 when (= i 2) do (ignore i) and return i)
    (for i from 0 to 3 when (= i 1) do (ignore i) and do (ignore i)))
  "Clause lists evaluated through both the subset and the host macro.")

(ert-deftest nelisp-cl-loop/subset-agrees-with-host ()
  "Every supported shape must answer exactly what the host's `cl-loop' does."
  (dolist (clauses nelisp-cl-loop-test--cases)
    (let* ((nelisp-cl-loop-test--i 0)
           (subset (eval (cons 'nelisp-cl-loop-test--subset clauses) t))
           (subset-i nelisp-cl-loop-test--i)
           (nelisp-cl-loop-test--i 0)
           (host (eval (cons 'cl-loop clauses) t))
           (host-i nelisp-cl-loop-test--i))
      ;; Both the value and the side effect have to agree: a loop that does
      ;; not run answers nil for `do', which equals what the host answers.
      (should (equal subset host))
      (should (equal subset-i host-i)))))

(ert-deftest nelisp-cl-loop/parallel-for-is-modelled ()
  "A second `for' used to make the whole shape unrecognised, and an
unrecognised shape expands to nil -- so this loop ran zero times and
answered nil while the host answered three pairs.  `emit-extern-call' in
the AOT compiler is written this way, which is how a native compile came
back with no functions in it."
  (should (nelisp-cl-macros--loop-build
           '(for a in '(1 2) for b in '(3 4) collect (list a b))))
  (should (equal (eval '(nelisp-cl-loop-test--subset
                         for a in '(1 2) for b in '(3 4) collect (list a b))
                       t)
                 '((1 3) (2 4)))))

(ert-deftest nelisp-cl-loop/unless-guards-the-next-clause ()
  "`unless' was not a clause keyword at all, so any loop using one was
declined and silently did nothing."
  (should (equal (eval '(nelisp-cl-loop-test--subset
                         for a in '(1 2 3) unless (= a 2) collect a)
                       t)
                 '(1 3))))

(ert-deftest nelisp-cl-loop/downto-counts-down ()
  "`downto' was not parsed, so the loop was declined and answered nil."
  (should (equal (eval '(nelisp-cl-loop-test--subset
                         for i from 2 downto 0 collect i)
                       t)
                 '(2 1 0))))

(ert-deftest nelisp-cl-loop/tail-iterator-by-destructures-plist-pairs ()
  "The native import descriptor loop must preserve complete plist pairs."
  (let ((form '(for (key value) on '(:name foo :arity 2 :kind func :return u64)
                   by #'cddr unless (memq key '(:name :kind))
                   append (list key value))))
    (should (equal (eval (cons 'nelisp-cl-loop-test--subset form) t)
                   '(:arity 2 :return u64)))
    (should (equal (eval (cons 'cl-loop form) t)
                   '(:arity 2 :return u64)))))

(ert-deftest nelisp-cl-loop/unsupported-shape-signals-when-it-runs ()
  "An unbuildable shape must not quietly become nil.
That is the whole defect this subset had: a loop nobody could build did
not fail, it did not run, and nothing said so.  The signal is deferred to
run time so vendor Elisp that merely CONTAINS an exotic loop still loads."
  (let* ((clauses '(for k being the hash-keys of (make-hash-table) collect k))
         (expansion (nelisp-cl-macros--loop-build clauses)))
    (should (nelisp-cl-macros--loop-unbuildable-p clauses))
    ;; It is a form, so a file containing one still reads and loads ...
    (should (consp expansion))
    ;; ... and it fails, loudly, only when it runs.
    (should-error (eval expansion t) :type 'error)
    ;; A shape the subset does model is not caught by the same predicate.
    (should-not (nelisp-cl-macros--loop-unbuildable-p
                 '(for a in '(1 2) collect a)))))

(ert-deftest nelisp-cl-loop/every-loop-in-the-tree-is-buildable ()
  "No `cl-loop' in this repository may fall to the unsupported arm.
The arm signals now, so a form that reaches it is a runtime failure
rather than a quiet nil, and the tree must not contain one."
  (let ((root (or (getenv "NELISP_ROOT") default-directory))
        (unbuildable nil))
    (dolist (f (append (file-expand-wildcards (expand-file-name "lisp/*.el" root))
                       (file-expand-wildcards (expand-file-name "src/*.el" root))
                       (file-expand-wildcards (expand-file-name "scripts/*.el" root))
                       (file-expand-wildcards (expand-file-name "packages/*/src/*.el" root))))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (condition-case nil
            (while t
              (let ((form (read (current-buffer))))
                (nelisp-cl-loop-test--walk
                 form
                 (lambda (sub)
                   (when (and (eq (car-safe sub) 'cl-loop)
                              (condition-case _
                                  (nelisp-cl-macros--loop-unbuildable-p (cdr sub))
                                (error t)))
                     (push (cons (file-name-nondirectory f) sub) unbuildable))))))
          (error nil))))
    (should-not unbuildable)))

(provide 'nelisp-cl-loop-test)
;;; nelisp-cl-loop-test.el ends here
