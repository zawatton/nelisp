;;; nelisp-tramp-test.el --- ERT tests for the trampoline evaluator  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Phase 2.5a parity tests.  Each test runs the same form through both
;; `nelisp-eval' (recursive) and `nelisp-tramp-eval' (explicit-stack)
;; and asserts the values match — i.e. the trampoline is a drop-in
;; replacement at the surface level.  A separate cycle-1 install probe
;; verifies that the install loop completes through the trampoline
;; without exhausting the host C stack (which the recursive evaluator
;; cannot do — see docs/design/06-trampoline-evaluator.org).

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'nelisp-tramp)

(defmacro nelisp-tramp-test--parity (form)
  "Assert recursive and trampoline evaluators produce `equal' results."
  `(let ((rec (progn (nelisp--reset) (nelisp-eval ,form)))
         (tra (progn (nelisp--reset) (nelisp-tramp-eval ,form))))
     (should (equal rec tra))))

;;; Atom / symbol / quote ---------------------------------------------

(ert-deftest nelisp-tramp-self-evaluating ()
  (nelisp-tramp-test--parity ''(1 2 3))
  (nelisp-tramp-test--parity '42)
  (nelisp-tramp-test--parity '"hello")
  (nelisp-tramp-test--parity 'nil)
  (nelisp-tramp-test--parity 't)
  (nelisp-tramp-test--parity ':foo))

(ert-deftest nelisp-tramp-quote-cons ()
  (nelisp-tramp-test--parity '(quote (a b c))))

;;; if / cond / when / unless -----------------------------------------

(ert-deftest nelisp-tramp-if ()
  (nelisp-tramp-test--parity '(if t 1 2))
  (nelisp-tramp-test--parity '(if nil 1 2))
  (nelisp-tramp-test--parity '(if 5 (+ 1 2) (* 3 4))))

(ert-deftest nelisp-tramp-cond ()
  (nelisp-tramp-test--parity '(cond ((= 1 2) :one)
                                    ((= 2 2) :two)
                                    (t :other)))
  (nelisp-tramp-test--parity '(cond (5))) ; clause without body
  (nelisp-tramp-test--parity '(cond (nil 1) (nil 2))))

(ert-deftest nelisp-tramp-when-unless ()
  (nelisp-tramp-test--parity '(when t 1 2 3))
  (nelisp-tramp-test--parity '(when nil 1))
  (nelisp-tramp-test--parity '(unless nil 1 2))
  (nelisp-tramp-test--parity '(unless t 1)))

;;; and / or ----------------------------------------------------------

(ert-deftest nelisp-tramp-and-or ()
  (nelisp-tramp-test--parity '(and))
  (nelisp-tramp-test--parity '(and 1 2 3))
  (nelisp-tramp-test--parity '(and 1 nil 3))
  (nelisp-tramp-test--parity '(or))
  (nelisp-tramp-test--parity '(or nil nil 5))
  (nelisp-tramp-test--parity '(or nil nil nil)))

;;; progn / prog1 / prog2 ---------------------------------------------

(ert-deftest nelisp-tramp-progn-prog ()
  (nelisp-tramp-test--parity '(progn 1 2 3))
  (nelisp-tramp-test--parity '(prog1 1 2 3))
  (nelisp-tramp-test--parity '(prog2 1 2 3 4)))

;;; let / let* --------------------------------------------------------

(ert-deftest nelisp-tramp-let-parallel ()
  (nelisp-tramp-test--parity '(let ((x 1) (y 2)) (+ x y))))

(ert-deftest nelisp-tramp-let*-sequential ()
  (nelisp-tramp-test--parity '(let* ((x 1) (y (+ x 1)) (z (+ y 1)))
                                (list x y z))))

(ert-deftest nelisp-tramp-let-bare-symbol-init ()
  ;; bare symbol binding initializes to nil
  (nelisp-tramp-test--parity '(let (a b) (list a b))))

;;; lambda / function call --------------------------------------------

(ert-deftest nelisp-tramp-lambda ()
  (nelisp-tramp-test--parity '((lambda (a b) (+ a b)) 3 4)))

(ert-deftest nelisp-tramp-nested-lambda ()
  (nelisp-tramp-test--parity '((lambda (n) ((lambda (x) (* x x)) n)) 5)))

(ert-deftest nelisp-tramp-mapcar ()
  (nelisp-tramp-test--parity
   '(mapcar (lambda (x) (* x x)) (list 1 2 3 4))))

;;; while / setq ------------------------------------------------------

(ert-deftest nelisp-tramp-while-counter ()
  (nelisp-tramp-test--parity
   '(let ((n 0) (i 0))
      (while (< i 5)
        (setq n (+ n i))
        (setq i (+ i 1)))
      n)))

;;; defun + recursion -------------------------------------------------

(ert-deftest nelisp-tramp-fib ()
  (nelisp-tramp-test--parity
   '(progn
      (defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
      (fib 10))))

(ert-deftest nelisp-tramp-fact ()
  (nelisp-tramp-test--parity
   '(progn
      (defun fact (n) (if (< n 2) 1 (* n (fact (- n 1)))))
      (fact 6))))

;;; catch / throw / unwind-protect ------------------------------------

(ert-deftest nelisp-tramp-catch-throw ()
  (nelisp-tramp-test--parity
   '(catch 'tag (progn (throw 'tag 99) 0))))

(ert-deftest nelisp-tramp-unwind-protect ()
  (nelisp-tramp-test--parity
   '(let ((c 0))
      (unwind-protect (progn 1 2 3) (setq c 1))
      c)))

;;; condition-case ----------------------------------------------------

(ert-deftest nelisp-tramp-condition-case-caught ()
  (nelisp-tramp-test--parity
   '(condition-case e (error "boom") (error :got))))

(ert-deftest nelisp-tramp-condition-case-unmatched ()
  ;; both should re-raise; our parity wrapper would error; rerun manually.
  (nelisp--reset)
  (should-error (nelisp-eval '(condition-case e
                                  (signal 'arith-error nil)
                                (foo-error :nope)))
                :type 'arith-error)
  (nelisp--reset)
  (should-error (nelisp-tramp-eval '(condition-case e
                                        (signal 'arith-error nil)
                                      (foo-error :nope)))
                :type 'arith-error))

;;; defvar / defconst / globals ---------------------------------------

(ert-deftest nelisp-tramp-defvar-and-setq-global ()
  (nelisp-tramp-test--parity
   '(progn (defvar tx 0) (setq tx 7) tx)))

;;; macro call --------------------------------------------------------

(ert-deftest nelisp-tramp-macro-dispatch ()
  ;; rely on core macros installed by (require 'nelisp)
  (nelisp-tramp-test--parity
   '(let ((acc 0))
      (dolist (n (list 1 2 3 4))
        (setq acc (+ acc n)))
      acc)))

;;; closure capture ---------------------------------------------------

(ert-deftest nelisp-tramp-closure-capture ()
  (nelisp-tramp-test--parity
   '(let ((x 10))
      ((lambda (y) (+ x y)) 5))))

;;; ----- Cycle-1 install probe ---------------------------------------

(defconst nelisp-tramp-test--source-dir
  (expand-file-name
   "../src"
   (file-name-directory (or load-file-name buffer-file-name))))

(defconst nelisp-tramp-test--source-files
  '("nelisp-read.el" "nelisp-eval.el" "nelisp-macro.el"
    "nelisp-load.el" "nelisp.el"))

(defun nelisp-tramp-test--install-via-tramp (path)
  "Read every form in PATH and evaluate it via `nelisp-tramp-eval'.
Return the count of forms processed.  Errors propagate."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8))
      (insert-file-contents path))
    (let ((forms (nelisp-read-all (buffer-string)))
          (count 0))
      (dolist (form forms)
        (setq count (1+ count))
        (nelisp-tramp-eval form))
      count)))

(ert-deftest nelisp-tramp-cycle1-install-completes ()
  "All NeLisp source files install through the trampoline without
exhausting the host C stack.  The recursive evaluator hangs at
`(nelisp--install-core-macros)' in src/nelisp.el; the trampoline
must complete.  Default `max-lisp-eval-depth' (1500-ish) is used —
this is the load-bearing assertion of Phase 2.5a."
  (nelisp--reset)
  (let ((total 0))
    (dolist (rel nelisp-tramp-test--source-files)
      (let* ((path (expand-file-name rel nelisp-tramp-test--source-dir))
             (n (nelisp-tramp-test--install-via-tramp path)))
        (should (> n 0))
        (setq total (+ total n))))
    (should (> total 100))))

;;; ----- Cycle-2 fixpoint (Phase 2.5c) ---------------------------------

(defconst nelisp-tramp-test--cycle2-files
  '("nelisp-read.el" "nelisp-eval.el" "nelisp-macro.el"
    "nelisp-load.el" "nelisp-tramp.el" "nelisp.el")
  "Full source set for cycle-2 tests (includes nelisp-tramp.el itself).")

(defun nelisp-tramp-test--full-self-install ()
  "Reset + bootstrap + install every source file through the trampoline.
Used by cycle-2 tests as the shared setup."
  (nelisp--reset)
  (nelisp-bootstrap-shared-tables)
  (dolist (rel nelisp-tramp-test--cycle2-files)
    (nelisp-tramp-test--install-via-tramp
     (expand-file-name rel nelisp-tramp-test--source-dir))))

(ert-deftest nelisp-tramp-cycle2-installs-itself ()
  "After bootstrap, nelisp-tramp.el itself installs through the
trampoline — the installed `nelisp-tramp-eval' is present in
`nelisp--functions' as a NeLisp closure."
  (nelisp-tramp-test--full-self-install)
  (let ((fn (gethash 'nelisp-tramp-eval nelisp--functions
                     nelisp--unbound)))
    (should-not (eq fn nelisp--unbound))
    (should (nelisp--closure-p fn))))

(ert-deftest nelisp-tramp-cycle2-fixpoint-fib ()
  "cycle-1 result == cycle-2 result for `(fib N)'.  Cycle-1 runs the
host trampoline on the form; cycle-2 runs the *installed* (NeLisp
closure) `nelisp-tramp-eval' on the form.  Bit-identical output is
Phase 2 success criterion (docs/05-roadmap.org §3.2)."
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-eval
   '(defun fib (n) (if (< n 2) n
                     (+ (fib (- n 1)) (fib (- n 2))))))
  (dolist (n '(0 1 5 8 10))
    (let ((cyc1 (nelisp-tramp-eval (list 'fib n)))
          (cyc2 (nelisp-tramp-eval
                 (list 'nelisp-tramp-eval
                       (list 'quote (list 'fib n))))))
      (should (equal cyc1 cyc2)))))

(ert-deftest nelisp-tramp-cycle2-fixpoint-fact ()
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-eval
   '(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))))
  (dolist (n '(1 3 5 7))
    (should (equal (nelisp-tramp-eval (list 'fact n))
                   (nelisp-tramp-eval
                    (list 'nelisp-tramp-eval
                          (list 'quote (list 'fact n))))))))

(ert-deftest nelisp-tramp-cycle2-fixpoint-let-closure ()
  "Closures captured in cycle-1 behave identically when exercised
through cycle-2 — the shared-tables bootstrap means a function defined
in the host side is visible from inside the installed tramp-eval."
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-eval '(defun add1 (n) (+ n 1)))
  (let ((c1 (nelisp-tramp-eval '(add1 41)))
        (c2 (nelisp-tramp-eval '(nelisp-tramp-eval (quote (add1 41))))))
    (should (equal c1 42))
    (should (equal c1 c2))))

;;; ----- Phase 3b.6a — Cycle fixpoint with VM auto-compile ----------
;;
;; The full-self-install path with `nelisp-bc-auto-compile' = t blows
;; the host C stack — the trampoline install of every NeLisp source
;; file already recurses deeply, and auto-compile per defun amplifies
;; it.  These tests therefore install the source tree with auto-
;; compile *off*, then bind it on around just the user-level defuns
;; under test.  That setup proves cycle-1 = cycle-2 still holds when
;; the user's function body runs on the VM, even though the cycle-2
;; `nelisp-tramp-eval' that dispatches the call is itself an
;; interpreter closure.  The deep-VM-install variant is tracked as
;; future work (3b.6c — see docs/design/08).

(defun nelisp-tramp-test--install-then-compile (defun-form)
  "Install DEFUN-FORM via the trampoline with auto-compile *on*.
Pre-installed source defuns stay as interpreter closures."
  (require 'nelisp-bytecode)
  (let ((nelisp-bc-auto-compile t))
    (nelisp-tramp-eval defun-form)))

(ert-deftest nelisp-tramp-cycle2-fixpoint-fib-vm ()
  "`fib' compiled to bcl returns the same result via cycle-1
(host trampoline) and cycle-2 (NeLisp-installed trampoline)."
  (require 'nelisp-bytecode)
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-test--install-then-compile
   '(defun fib (n) (if (< n 2) n
                     (+ (fib (- n 1)) (fib (- n 2))))))
  (should (nelisp-bcl-p (gethash 'fib nelisp--functions)))
  (dolist (n '(0 1 5 8 10))
    (let ((cyc1 (nelisp-tramp-eval (list 'fib n)))
          (cyc2 (nelisp-tramp-eval
                 (list 'nelisp-tramp-eval
                       (list 'quote (list 'fib n))))))
      (should (equal cyc1 cyc2)))))

(ert-deftest nelisp-tramp-cycle2-fixpoint-fact-vm ()
  (require 'nelisp-bytecode)
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-test--install-then-compile
   '(defun fact (n) (if (< n 2) 1 (* n (fact (- n 1))))))
  (should (nelisp-bcl-p (gethash 'fact nelisp--functions)))
  (dolist (n '(1 3 5 7))
    (should (equal (nelisp-tramp-eval (list 'fact n))
                   (nelisp-tramp-eval
                    (list 'nelisp-tramp-eval
                          (list 'quote (list 'fact n))))))))

(ert-deftest nelisp-tramp-cycle2-vm-mixed-bcl-and-closure ()
  "After installing some defuns as bcls (compilable) and others as
interpreter closures (uncompilable), a trampoline call that
crosses both kinds returns the right answer."
  (require 'nelisp-bytecode)
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-test--install-then-compile
   '(defun double (n) (+ n n)))
  ;; Uncompilable body: `and' is in the unimplemented set.
  (let ((nelisp-bc-auto-compile t))
    (nelisp-tramp-eval '(defun nzp (n) (and (numberp n) (not (= n 0))))))
  (nelisp-tramp-test--install-then-compile
   '(defun maybe-double (n) (if (nzp n) (double n) 0)))
  (should (nelisp-bcl-p     (gethash 'double nelisp--functions)))
  (should (nelisp--closure-p (gethash 'nzp nelisp--functions)))
  (should (nelisp-bcl-p     (gethash 'maybe-double nelisp--functions)))
  (should (= (nelisp-tramp-eval '(maybe-double 5)) 10))
  (should (= (nelisp-tramp-eval '(maybe-double 0)) 0)))

(ert-deftest nelisp-tramp-cycle2-vm-equivalence-tail ()
  "Tail-recursive bcl works through both cycle-1 and cycle-2."
  (require 'nelisp-bytecode)
  (nelisp-tramp-test--full-self-install)
  (nelisp-tramp-test--install-then-compile
   '(defun sum-to (n acc)
      (if (< n 1) acc (sum-to (- n 1) (+ acc n)))))
  (should (nelisp-bcl-p (gethash 'sum-to nelisp--functions)))
  (let ((cyc1 (nelisp-tramp-eval '(sum-to 50 0)))
        (cyc2 (nelisp-tramp-eval
               '(nelisp-tramp-eval (quote (sum-to 50 0))))))
    (should (= cyc1 (apply #'+ (number-sequence 1 50))))
    (should (equal cyc1 cyc2))))

;;; ----- Phase 3b.6b — Meta-circular compile ----------------------
;;
;; The body of `nelisp-bc-compile' is itself NeLisp Lisp.  Feeding it
;; through `nelisp-bc-try-compile-lambda' produces a bcl of the
;; compiler.  Calling that bcl on a source form must produce a bcl
;; equivalent to what host `nelisp-bc-compile' produces — equivalent
;; in the sense that running both bcls returns the same value.
;;
;; This is the "meta-circular smoke test" of design doc §3b.6 step 3.
;; A full meta-circular bootstrap (re-installing every helper as a
;; bcl) is deferred — see 3b.6c notes.

(defconst nelisp-tramp-test--meta-compile-source
  '(lambda (form)
     (let ((ctx (nelisp-bc--ctx-new)))
       (nelisp-bc--compile-form ctx form)
       (nelisp-bc--emit ctx (quote RETURN))
       (let* ((code (nelisp-bc--resolve (nelisp-bc--ctx-insns ctx)))
              (consts (apply (function vector) (nelisp-bc--ctx-consts ctx)))
              (max-sp (max 1 (nelisp-bc--ctx-max-sp ctx))))
         (nelisp-bc-make nil nil consts code max-sp 0))))
  "A reduced clone of `nelisp-bc-compile' (no optional ENV) suitable
for compilation via `nelisp-bc-try-compile-lambda'.")

(ert-deftest nelisp-tramp-meta-compile-returns-bcl ()
  "The compiler-as-source compiles through the auto-compile hook."
  (require 'nelisp-bytecode)
  (let* ((nelisp-bc-auto-compile t)
         (src nelisp-tramp-test--meta-compile-source)
         (meta (nelisp-bc-try-compile-lambda nil (cadr src) (cddr src))))
    (should (nelisp-bcl-p meta))))

(ert-deftest nelisp-tramp-meta-compile-equivalence ()
  "For each sample form, the bcl produced by host `nelisp-bc-compile'
and the bcl produced by the *meta* compile run to the same value."
  (require 'nelisp-bytecode)
  (let* ((nelisp-bc-auto-compile t)
         (src nelisp-tramp-test--meta-compile-source)
         (meta (nelisp-bc-try-compile-lambda nil (cadr src) (cddr src))))
    (should (nelisp-bcl-p meta))
    (dolist (form '(42
                    "hello"
                    (quote symbol-literal)
                    (+ 1 2 3)
                    (if (< 1 2) 10 20)
                    (let ((x 5)) (+ x 100))
                    (let* ((a 1) (b (+ a 2))) (+ a b))
                    (cons 1 (cons 2 nil))
                    (car (list 9 8 7))))
      (let ((host-bcl (nelisp-bc-compile form))
            (meta-bcl (nelisp--apply meta (list form))))
        (should (nelisp-bcl-p host-bcl))
        (should (nelisp-bcl-p meta-bcl))
        ;; Both bcls when run produce the same value.
        (should (equal (nelisp-bc-run host-bcl)
                       (nelisp-bc-run meta-bcl)))))))

(provide 'nelisp-tramp-test)

;;; nelisp-tramp-test.el ends here
