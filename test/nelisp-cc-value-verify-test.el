;;; nelisp-cc-value-verify-test.el --- T84 value-verified gate (3-axis bench correctness) -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; T84 Phase 7.5 callee resolution + arg passing wire — value-verified
;; gate ERT (Doc 28 v2 §5.2 + Doc 32 v2 §3 4-stage cold-init).
;;
;; Pre-T84 (T63 SHIPPED) `make bench-actual' returned wrong native
;; values for fib / fact-iter / alloc-heavy because the SSA
;; `:closure' / `:call' / `:store-var' / `:load-var' lowering used
;; placeholders that compiled but did not execute correct semantics
;; (CALL into self-pointer trampoline returned the trampoline's own
;; address; `:load-var' returned 0; spilled params were never saved
;; from arg-regs; the `:call' arg marshalling clobbered earlier
;; arg-regs).  These tests exercise each fix end-to-end via the
;; in-process FFI module, comparing the native i64 to the bytecode
;; reference.  All tests skip cleanly when the FFI module is missing
;; (= `make runtime-module' has not been run) so they remain CI-safe.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'nelisp-cc)
(require 'nelisp-cc-runtime)

(defun nelisp-cc-value-verify-test--module-available-p ()
  "Return non-nil when the in-process FFI module is loadable."
  (and (fboundp 'module-load)
       (boundp 'module-file-suffix)
       module-file-suffix
       (ignore-errors
         (file-readable-p
          (nelisp-cc-runtime--locate-runtime-module)))))

(defun nelisp-cc-value-verify-test--exec (form)
  "Compile FORM through the new T84 pipeline and exec via in-process FFI.
Returns the native i64 value as an integer, or signals when the
pipeline crashes."
  (let* ((nelisp-cc-runtime-exec-mode 'in-process)
         (result (nelisp-cc-runtime-compile-and-allocate form 'x86_64))
         (final  (plist-get result :final-bytes))
         (exec   (nelisp-cc-runtime--exec-in-process final)))
    (cond
     ((and (consp exec) (eq (car exec) :result))
      (nth 2 exec))
     (t
      (error "T84 native exec failed: %S" exec)))))

;;; Phase 1: primitive trampolines correct -----------------------------

(ert-deftest nelisp-cc-value-verify-prim-plus ()
  "`(+ 2 3)' native exec returns 5 — T84 fix for the XOR-then-CMP
flag-trash bug in the primitive trampoline cheat-sheet."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 5 (nelisp-cc-value-verify-test--exec '(lambda () (+ 2 3))))))

(ert-deftest nelisp-cc-value-verify-prim-minus ()
  "`(- 10 4)' native exec returns 6."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 6 (nelisp-cc-value-verify-test--exec '(lambda () (- 10 4))))))

(ert-deftest nelisp-cc-value-verify-prim-mul ()
  "`(* 6 7)' native exec returns 42."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 42 (nelisp-cc-value-verify-test--exec '(lambda () (* 6 7))))))

(ert-deftest nelisp-cc-value-verify-prim-lt-true ()
  "`(< 1 2)' native exec returns 1 (truthy, NeLisp i64 boolean) —
T84 fix for the XOR-then-CMP flag clobber in `<' trampoline."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 1 (nelisp-cc-value-verify-test--exec '(lambda () (< 1 2))))))

(ert-deftest nelisp-cc-value-verify-prim-lt-false ()
  "`(< 2 1)' native exec returns 0 (falsy)."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 0 (nelisp-cc-value-verify-test--exec '(lambda () (< 2 1))))))

(ert-deftest nelisp-cc-value-verify-prim-1+ ()
  "`(1+ 41)' native exec returns 42."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 42 (nelisp-cc-value-verify-test--exec '(lambda () (1+ 41))))))

;;; Phase 2: branch correctness (T84 reg-marshal fix) -----------------

(ert-deftest nelisp-cc-value-verify-if-then ()
  "`(if (< 1 2) 100 200)' native exec returns 100 — T84 fix for the
arg marshalling conflict that pre-T84 made `<' return 0 (falsy) and
the if always took the else branch."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 100 (nelisp-cc-value-verify-test--exec
                  '(lambda () (if (< 1 2) 100 200))))))

(ert-deftest nelisp-cc-value-verify-if-else ()
  "`(if (< 5 5) 100 200)' native exec returns 200."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 200 (nelisp-cc-value-verify-test--exec
                  '(lambda () (if (< 5 5) 100 200))))))

;;; Phase 3: closure invocation + recursion ---------------------------

(ert-deftest nelisp-cc-value-verify-letrec-id ()
  "`(letrec ((id (lambda (n) n))) (funcall id 42))' native exec
returns 42 — T84 fix for the inner-function compilation + LEA
closure-pointer wire."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 42 (nelisp-cc-value-verify-test--exec
                 '(lambda ()
                    (letrec ((id (lambda (n) n))) (funcall id 42)))))))

(ert-deftest nelisp-cc-value-verify-letrec-inc ()
  "`(letrec ((inc (lambda (n) (+ n 1)))) (funcall inc 41))' native
exec returns 42."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 42 (nelisp-cc-value-verify-test--exec
                 '(lambda ()
                    (letrec ((inc (lambda (n) (+ n 1))))
                      (funcall inc 41)))))))

(ert-deftest nelisp-cc-value-verify-fib-5 ()
  "`fib(5)' native exec returns 5 — T84 minimal recursion gate."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 5 (nelisp-cc-value-verify-test--exec
                '(lambda ()
                   (letrec ((fib (lambda (n)
                                   (if (< n 2) n
                                     (+ (funcall fib (- n 1))
                                        (funcall fib (- n 2)))))))
                     (funcall fib 5)))))))

(ert-deftest nelisp-cc-value-verify-fib-10 ()
  "`fib(10)' native exec returns 55."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 55 (nelisp-cc-value-verify-test--exec
                 '(lambda ()
                    (letrec ((fib (lambda (n)
                                    (if (< n 2) n
                                      (+ (funcall fib (- n 1))
                                         (funcall fib (- n 2)))))))
                      (funcall fib 10)))))))

;;; Phase 4: bench-actual correctness — fib(30) gate ------------------

(ert-deftest nelisp-cc-value-verify-fib-30-eq-832040 ()
  "`fib(30)' native exec equals 832040 (= bytecode VM reference) —
the canonical T84 value-verified gate.  Pre-T84 returned ~1.4e14 (=
~2x trampoline self-pointer) instead of 832040, exposing the
closure / call-indirect bug chain T84 closes."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 832040
             (nelisp-cc-value-verify-test--exec
              '(lambda ()
                 (letrec ((fib (lambda (n)
                                 (if (< n 2) n
                                   (+ (funcall fib (- n 1))
                                      (funcall fib (- n 2)))))))
                   (funcall fib 30)))))))

;;; Phase 5: bench-actual correctness — fact-iter gate ----------------

(ert-deftest nelisp-cc-value-verify-fact-20-eq-bignum ()
  "`fact-iter(20, 1)' native exec equals 2432902008176640000 (= 20!)
which fits in signed i64.  Sanity-check for the multi-arg
`call-indirect' wire (`(funcall fact (- n 1) (* acc n))') that T84
fixes; pre-T84 the recursive funcall returned the closure self-
pointer."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 2432902008176640000
             (nelisp-cc-value-verify-test--exec
              '(lambda ()
                 (letrec ((fact-iter (lambda (n acc)
                                       (if (< n 1) acc
                                         (funcall fact-iter
                                                  (- n 1)
                                                  (* acc n))))))
                   (funcall fact-iter 20 1)))))))

;;; Phase 6: alloc-heavy correctness — counter cell wire --------------

(ert-deftest nelisp-cc-value-verify-alloc-heavy-10 ()
  "`(let ((acc nil) (i 0)) (while (< i 10) (setq acc (cons i acc))
(setq i (1+ i))) (length acc))' native exec equals 10.  Stresses
the T84 setq-rewrite pass + `cons-counter' cell increment +
`length' cell read."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 10
             (nelisp-cc-value-verify-test--exec
              '(lambda ()
                 (let ((acc nil) (i 0))
                   (while (< i 10)
                     (setq acc (cons i acc))
                     (setq i (1+ i)))
                   (length acc)))))))

(ert-deftest nelisp-cc-value-verify-alloc-heavy-1m ()
  "`alloc-heavy' bench-actual full size: native exec equals 1000000
after 1M cons + length walk.  Doc 28 v2 §5.2 alloc-heavy gate."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 1000000
             (nelisp-cc-value-verify-test--exec
              '(lambda ()
                 (let ((acc nil) (i 0))
                   (while (< i 1000000)
                     (setq acc (cons i acc))
                     (setq i (1+ i)))
                   (length acc)))))))

;;; Phase 7: rec-inline survival-arm value-correctness ----------------
;;
;; The Phase 7.1 survival-arm fix (= splice's Pass 3 must skip
;; phi-arm vids that have already been resolved by Pass 2) closed the
;; `fib(n) at depth>=1 returns garbage' leak that the F3 ship surfaced
;; (depth=1 fib(30)=687462 vs 832040 expected, monotonic convergence
;; depth 1→4).  These tests pin the fix end-to-end via the in-process
;; FFI: pipeline ON + rec-inline depth-limit 1..4 must all return the
;; same value as the bytecode VM.  See
;; `src/nelisp-cc-recursive-inline.el' Pass 3 `translated-ids' guard.

(require 'nelisp-cc-pipeline)

(defun nelisp-cc-value-verify-test--exec-with-pipeline (form depth)
  "Compile FORM through the pipeline at DEPTH and return native i64."
  (let* ((nelisp-cc-runtime-exec-mode 'in-process)
         (nelisp-cc-enable-7.7-passes t)
         (nelisp-cc-pipeline-rec-inline-depth-limit depth)
         (result (nelisp-cc-runtime-compile-and-allocate form 'x86_64))
         (final  (plist-get result :final-bytes))
         (exec   (nelisp-cc-runtime--exec-in-process final)))
    (cond
     ((and (consp exec) (eq (car exec) :result))
      (nth 2 exec))
     (t
      (error "rec-inline pipeline native exec failed: %S" exec)))))

(defmacro nelisp-cc-value-verify-test--fib-form (n)
  "Build a fib(N) lambda form."
  `'(lambda ()
      (letrec ((fib (lambda (n)
                      (if (< n 2) n
                        (+ (funcall fib (- n 1))
                           (funcall fib (- n 2)))))))
        (funcall fib ,n))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-5-depth-1 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=1
fib(5) returns 5 (= bytecode reference).  Pre-fix returned a
random int64 because Pass 3 chained the param phi-arm through
value-map into a deeper clone."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 5
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 5) 1))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-10-depth-1 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=1
fib(10) returns 55."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 55
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 10) 1))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-20-depth-1 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=1
fib(20) returns 6765."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 6765
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 20) 1))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-30-depth-1 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=1
fib(30) returns 832040 (= bytecode reference).  Pre-fix returned
687462 (~17% short, monotonic convergence depth 1→4 = F3 report)."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 832040
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 30) 1))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-30-depth-2 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=2
fib(30) returns 832040.  Catches a regression that breaks at the
larger unroll where the splice cascades through more cloned blocks."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 832040
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 30) 2))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-30-depth-3 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=3
fib(30) returns 832040."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 832040
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 30) 3))))

(ert-deftest nelisp-cc-value-verify-rec-inline-fib-30-depth-4 ()
  "Phase 7.1 survival-arm fix: pipeline ON + rec-inline depth=4
fib(30) returns 832040.  Same value across depths 1..4 — the rec-
inline pass must not introduce any value drift as a function of
unroll depth."
  (skip-unless (nelisp-cc-value-verify-test--module-available-p))
  (should (= 832040
             (nelisp-cc-value-verify-test--exec-with-pipeline
              (nelisp-cc-value-verify-test--fib-form 30) 4))))

(provide 'nelisp-cc-value-verify-test)

;;; nelisp-cc-value-verify-test.el ends here
