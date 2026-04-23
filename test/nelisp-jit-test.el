;;; nelisp-jit-test.el --- Phase 3b.8a JIT ERTs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Smoke tests for the Phase 3b.8 JIT translator.  These run the JIT
;; directly via `nelisp-jit-try-compile-lambda' (no advice needed) so
;; the full test suite isn't affected by the advice wrapper, which
;; trips `max-lisp-eval-depth' inside `nelisp--install-core-macros'
;; — see §3b.8a design note.

;;; Code:

(require 'ert)
(require 'nelisp)
(require 'nelisp-bytecode)
(require 'nelisp-jit)

;;; Translation -------------------------------------------------------

(ert-deftest nelisp-jit-translate-literal ()
  (should (eq   (nelisp-jit--translate 42 nil)  42))
  (should (eq   (nelisp-jit--translate t  nil)  t))
  (should (equal (nelisp-jit--translate "s" nil) "s")))

(ert-deftest nelisp-jit-translate-lexical-symbol ()
  ;; A symbol in env is returned as itself (lexical).
  (should (eq (nelisp-jit--translate 'x '(x)) 'x)))

(ert-deftest nelisp-jit-translate-global-symbol ()
  ;; A symbol not in env expands to a NeLisp-globals gethash form.
  (let ((out (nelisp-jit--translate 'foo nil)))
    (should (eq (car out) 'let))
    ;; Inner body calls gethash on 'foo.
    (should (equal (caddr out)
                   '(if (eq v nelisp--unbound)
                        (signal 'nelisp-unbound-variable (list 'foo))
                      v)))))

(ert-deftest nelisp-jit-translate-primitive-call ()
  ;; Primitives are emitted as direct host calls.
  (should (equal (nelisp-jit--translate '(+ 1 2) nil)
                 '(+ 1 2)))
  (should (equal (nelisp-jit--translate '(< n 2) '(n))
                 '(< n 2))))

(ert-deftest nelisp-jit-translate-user-call ()
  ;; Non-primitive symbol calls route through `nelisp--apply'.
  (should (equal (nelisp-jit--translate '(fib n) '(n))
                 '(nelisp--apply 'fib (list n)))))

(ert-deftest nelisp-jit-translate-if ()
  (should (equal (nelisp-jit--translate '(if (< n 2) n (+ n 1)) '(n))
                 '(if (< n 2) n (+ n 1)))))

(ert-deftest nelisp-jit-translate-let ()
  (let ((out (nelisp-jit--translate
              '(let ((a 1) (b (+ a 2))) (+ a b))
              nil)))
    ;; RHS of `b' uses global `a' because `let' is parallel binding —
    ;; the `a' in `(+ a 2)' sees the outer scope, not the new `a'.
    (should (eq (car out) 'let))
    (should (equal (cadr out)
                   '((a 1)
                     (b (+ (let ((v (gethash 'a nelisp--globals nelisp--unbound)))
                             (if (eq v nelisp--unbound)
                                 (signal 'nelisp-unbound-variable (list 'a))
                               v))
                           2)))))
    ;; Body translation extends env with (a b).
    (should (equal (caddr out) '(+ a b)))))

(ert-deftest nelisp-jit-translate-let*-sequential ()
  (let ((out (nelisp-jit--translate
              '(let* ((a 1) (b (+ a 2))) b)
              nil)))
    ;; let* binds a before b's RHS, so b's RHS sees lexical a.
    (should (eq (car out) 'let*))
    (should (equal (cadr out) '((a 1) (b (+ a 2)))))
    (should (equal (caddr out) 'b))))

(ert-deftest nelisp-jit-translate-setq-lexical-and-global ()
  ;; In-env setq is lexical setq; out-of-env setq writes globals.
  (should (equal (nelisp-jit--translate '(setq x 1) '(x))
                 '(setq x 1)))
  (should (equal (nelisp-jit--translate '(setq y 2) nil)
                 '(puthash 'y 2 nelisp--globals))))

;;; End-to-end compile + run ------------------------------------------

(ert-deftest nelisp-jit-compile-runs-fib ()
  "Compile the fib MVP test case and invoke via `nelisp-jit-run'.
Skips the nelisp--apply dispatch for recursive calls — the top-level
call goes through the compiled host lambda and recursive calls
bounce through `nelisp--apply' on the symbol `fib'."
  (let ((nelisp-bc-auto-compile nil)
        (nelisp-jit-enabled t))
    (nelisp--reset)
    (nelisp-jit-install)
    (unwind-protect
        (progn
          (nelisp-eval '(defun fib (n)
                          (if (< n 2) n
                            (+ (fib (- n 1)) (fib (- n 2))))))
          (let ((installed (gethash 'fib nelisp--functions)))
            (should (nelisp-jit-bcl-p installed)))
          (should (= 55 (nelisp-eval '(fib 10)))))
      (nelisp-jit-uninstall))))

(ert-deftest nelisp-jit-captured-env-read ()
  "Phase 3b.8b: captured-env closure with read-only env is JIT-compiled.
Simulates `make-adder' — the returned closure adds captured `x' to
its arg `y'."
  (let ((c (nelisp-jit-try-compile-lambda '((x . 3)) '(y) '((+ x y)))))
    (should (nelisp-jit-bcl-p c))
    ;; Element 4 is the host fn (see `nelisp-jit--wrap-as-bcl' shape).
    (should (= 8 (funcall (nth 4 c) 5)))))

(ert-deftest nelisp-jit-captured-env-multiple-vars ()
  "Captured env with multiple vars reads via `aref' indexes."
  (let ((c (nelisp-jit-try-compile-lambda
            '((a . 1) (b . 2) (c . 3)) '(n) '((+ a b c n)))))
    (should (nelisp-jit-bcl-p c))
    (should (= 10 (funcall (nth 4 c) 4)))))

(ert-deftest nelisp-jit-captured-env-shadowing ()
  "A `let' inside the body shadows captured env symbols — the inner
binding wins as expected."
  (let ((c (nelisp-jit-try-compile-lambda
            '((x . 100)) '(y) '((let ((x 1)) (+ x y))))))
    (should (nelisp-jit-bcl-p c))
    (should (= 6 (funcall (nth 4 c) 5)))))

(ert-deftest nelisp-jit-captured-env-setq-mutation-falls-back ()
  "`setq' on a captured env variable makes the JIT return nil so bcl /
interpreter can handle the counter pattern correctly."
  (should (null (nelisp-jit-try-compile-lambda
                 '((counter . 0)) '() '((setq counter (+ counter 1))))))
  ;; Nested under `progn' / `if' — scan is structural.
  (should (null (nelisp-jit-try-compile-lambda
                 '((x . 0)) '(n)
                 '((if (> n 0) (setq x n) x))))))

(ert-deftest nelisp-jit-captured-env-setq-local-param-ok ()
  "`setq' on a parameter (not a captured env sym) is fine."
  (let ((c (nelisp-jit-try-compile-lambda
            '((base . 10)) '(n) '((setq n (+ n 1)) (+ base n)))))
    (should (nelisp-jit-bcl-p c))
    (should (= 16 (funcall (nth 4 c) 5)))))

(ert-deftest nelisp-jit-fallback-on-unsupported-form ()
  "Forms the translator cannot handle make the whole compile return nil.
We use a raw non-symbol head — lambda-head application path is not in
the MVP."
  (should (null (nelisp-jit-try-compile-lambda nil '(x) '((#'foo x))))))

(provide 'nelisp-jit-test)

;;; nelisp-jit-test.el ends here
