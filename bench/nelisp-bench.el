;;; nelisp-bench.el --- Phase 3b.7 perf bench harness  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; The bench cases install a single recursive defun, time how long
;; one call takes through (a) the host NeLisp interpreter and (b)
;; the same defun auto-compiled to bcl, and report the speedup.
;; `make bench' batches them up.
;;
;; The Phase 3b.7 merge gate requires the bcl version to be at least
;; 10x faster than the interpreter on the recursive-call cases.  See
;; docs/design/08-bytecode-vm.org §3b.7.
;;
;; Bench cases:
;;   fib          — naive doubly-recursive Fibonacci.  Stresses
;;                  CALL + STACK-REF + LESS + arithmetic dispatch.
;;   fact         — single recursion, small arithmetic loop.
;;   sum-to       — tail-shape recursion with two accumulator args.
;;   let-chain    — lots of `let' bindings + reads, no recursion.

;;; Code:

(require 'nelisp)
(require 'nelisp-bytecode)
(require 'nelisp-jit)

(defconst nelisp-bench--cases
  '((fib
     (lambda (n)
       (if (< n 2) n
         (+ (fib (- n 1)) (fib (- n 2)))))
     20)
    (fact
     (lambda (n)
       (if (< n 2) 1 (* n (fact (- n 1)))))
     12)
    (sum-to
     (lambda (n acc)
       (if (< n 1) acc (sum-to (- n 1) (+ acc n))))
     150 0)
    (let-chain
     (lambda (n)
       (let ((a 1))
         (let ((b (+ a 1)))
           (let ((c (+ b 1)))
             (let ((d (+ c 1)))
               (let ((e (+ d 1)))
                 (+ a b c d e n)))))))
     0))
  "Each entry: (NAME LAMBDA-FORM ARG...).
LAMBDA-FORM is fed to `defun', then the function is invoked with
ARG... once per timing.  All cases are pure (no I/O, no specials).")

(defun nelisp-bench--time-call (form)
  "Return cons (RESULT . SECONDS-ELAPSED) for FORM."
  (garbage-collect)
  (let ((t0 (current-time)))
    (let ((r (nelisp-eval form)))
      (cons r (float-time (time-since t0))))))

(defun nelisp-bench--install (case &optional mode)
  "Reset NeLisp state and install CASE's defun.
MODE selects the compilation target:
  nil   — interpreter (both auto-compile flags off)
  'bcl  — bytecode VM (only auto-compile on)
  'jit  — JIT host lambda (both jit-enabled and auto-compile on;
           JIT sits before bcl in the make-closure chain)."
  (let ((nelisp-bc-auto-compile (if mode t nil))
        (nelisp-jit-enabled (eq mode 'jit)))
    (nelisp--reset)
    (nelisp-eval (cons 'defun (cons (nth 0 case) (cdr (nth 1 case)))))))

(defun nelisp-bench--call-form (case)
  "Build the call expression for CASE."
  (cons (nth 0 case) (nthcdr 2 case)))

(defun nelisp-bench-run-case (case &optional iters)
  "Time CASE under interpreter / VM / JIT, return a plist of results.
ITERS controls how many times each side is invoked (default 1).
Requires `nelisp-jit-install' to have run beforehand so the JIT
advice on `nelisp-bc-try-compile-lambda' is active."
  (let* ((iters (or iters 1))
         (form  (nelisp-bench--call-form case))
         (interp-result nil) (vm-result nil) (jit-result nil)
         (interp-time 0.0)   (vm-time 0.0)   (jit-time 0.0))
    ;; Interpreter
    (nelisp-bench--install case nil)
    (dotimes (_ iters)
      (let ((r-t (nelisp-bench--time-call form)))
        (setq interp-result (car r-t))
        (cl-incf interp-time (cdr r-t))))
    ;; VM
    (nelisp-bench--install case 'bcl)
    (let ((bcl-installed (nelisp-bcl-p
                          (gethash (nth 0 case) nelisp--functions))))
      (dotimes (_ iters)
        (let ((r-t (nelisp-bench--time-call form)))
          (setq vm-result (car r-t))
          (cl-incf vm-time (cdr r-t))))
      ;; JIT
      (nelisp-bench--install case 'jit)
      (let ((jit-installed (nelisp-jit-bcl-p
                            (gethash (nth 0 case) nelisp--functions))))
        (dotimes (_ iters)
          (let ((r-t (nelisp-bench--time-call form)))
            (setq jit-result (car r-t))
            (cl-incf jit-time (cdr r-t))))
        (list :name (nth 0 case)
              :iters iters
              :form form
              :bcl bcl-installed
              :jit jit-installed
              :interp-time interp-time
              :vm-time vm-time
              :jit-time jit-time
              :vm-speedup  (if (zerop vm-time)  0.0 (/ interp-time vm-time))
              :jit-speedup (if (zerop jit-time) 0.0 (/ interp-time jit-time))
              :equal (and (equal interp-result vm-result)
                          (equal interp-result jit-result)))))))

(defun nelisp-bench-run-all (&optional iters)
  "Run every case, print a one-line summary per case, return the
list of result plists."
  (let ((results '()))
    (dolist (case nelisp-bench--cases)
      (let ((r (nelisp-bench-run-case case iters)))
        (push r results)
        (message "%-12s iters=%d interp=%.4fs vm=%.4fs (%.2fx) jit=%.4fs (%.2fx) equal=%s"
                 (plist-get r :name)
                 (plist-get r :iters)
                 (plist-get r :interp-time)
                 (plist-get r :vm-time)
                 (plist-get r :vm-speedup)
                 (plist-get r :jit-time)
                 (plist-get r :jit-speedup)
                 (if (plist-get r :equal) "yes" "NO!"))))
    (nreverse results)))

;;;###autoload
(defun nelisp-bench-batch ()
  "`make bench' entry point.  Runs every case once with sensible
iteration counts and prints a header / footer.  Installs the JIT
advice around the run so `make test' (which also -L's bench/)
isn't affected by the advice wrapper overhead — the self-host
probe is calibrated to the default `max-lisp-eval-depth' budget
and an extra frame per `nelisp-bc-try-compile-lambda' call trips
it (§3b.8a design note)."
  (nelisp-jit-install)
  (unwind-protect
      (nelisp-bench--batch-body)
    (nelisp-jit-uninstall)))

(defun nelisp-bench--batch-body ()
  (message "==== nelisp-bench Phase 3b.8 ====")
  (let* ((results (nelisp-bench-run-all 3))
         (vm-speedups  (mapcar (lambda (r) (plist-get r :vm-speedup))  results))
         (jit-speedups (mapcar (lambda (r) (plist-get r :jit-speedup)) results))
         (vm-worst  (apply #'min vm-speedups))
         (vm-best   (apply #'max vm-speedups))
         (jit-worst (apply #'min jit-speedups))
         (jit-best  (apply #'max jit-speedups)))
    (message "==== vm:  best %.2fx, worst %.2fx ====" vm-best  vm-worst)
    (message "==== jit: best %.2fx, worst %.2fx ====" jit-best jit-worst)
    (when (< jit-worst 10.0)
      (message "WARNING: 10x merge gate not met (JIT worst case %.2fx)"
               jit-worst))))

(provide 'nelisp-bench)

;;; nelisp-bench.el ends here
