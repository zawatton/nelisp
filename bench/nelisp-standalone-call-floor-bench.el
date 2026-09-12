;;; nelisp-standalone-call-floor-bench.el --- what one step costs in the standalone -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Every other bench here measures the HOST NeLisp interpreter, or the bcl
;; against it.  Consumers do not run either: they run `target/nelisp', whose
;; evaluator has never had a bench of its own.  That mattered on 2026-09-12,
;; when the question "why does the consumer loader take 12 s on a 43 KB file"
;; turned out to be about this and not about the loader -- the loader's own
;; phases were 8% of it and the other 92% was evaluating the file's forms.
;;
;; This measures the floor those 92% are made of, in the binary, with no lib
;; and no consumer in the picture:
;;
;;   loop      one `while' iteration that only increments a counter
;;   builtin   the same loop plus one builtin call
;;   call1     the same loop plus one call to a one-argument `defun'
;;   call2     ... a two-argument `defun'
;;   let1      ... one `let' binding and one read of it
;;
;; Every row but `loop' is reported as the DIFFERENCE from `loop', so each
;; says what that one construct adds.  `loop' itself is reported whole,
;; because it is the floor: nothing written in Elisp here costs less.
;;
;; First measurement (Linux x86_64, target/nelisp at 49915397c), taken while
;; a real-init audit had one core busy -- so read the RATIOS, and re-measure
;; on a quiet machine before pinning any of it as a baseline:
;;
;;   loop 10.80 us/iter (absolute)
;;   builtin  +6.92     call1 +17.71     call2 +51.79     let1 +29.73
;;
;; Two of those rows are the reason this file exists.  `call2' is roughly
;; three times `call1', not `call1' plus a little: adding ONE argument nearly
;; triples the cost, which points at the per-argument work in the evaluator's
;; argument walk (a root-slot reservation and a 32-byte cons allocation for
;; every argument) rather than at call dispatch.  And `let1' -- one binding,
;; one read -- costs more than a whole one-argument function call, which
;; matters because `let' is in nearly every real function.  Neither is
;; visible from the host benches, because the host interpreter is not what
;; runs here.
;;
;; Run it against the binary, not the host:
;;   make standalone-call-floor-bench            (or)
;;   ./target/nelisp --load bench/nelisp-standalone-call-floor-bench.el
;;
;; Reading it honestly: these are wall-clock differences of loops, not a
;; profile.  A row says what the construct costs where it sits, including
;; whatever the evaluator does around it; it does not say which part of the
;; evaluator to change.  `lisp/nelisp-repl-profile.el' is for that, and its
;; own shim costs more than most of these rows, so the two are not
;; interchangeable.

;;; Code:

(defvar nelisp-standalone-call-floor-bench-iterations 20000
  "Iterations per case.  Large enough that `float-time' granularity is noise.")

(defun nelisp-standalone-call-floor-bench--leaf1 (x) (+ x 1))
(defun nelisp-standalone-call-floor-bench--leaf2 (x y) (+ x y))

(defun nelisp-standalone-call-floor-bench--loop (n)
  (let ((i 0) (start (float-time)))
    (while (< i n) (setq i (1+ i)))
    (- (float-time) start)))

(defun nelisp-standalone-call-floor-bench--builtin (n)
  (let ((i 0) (acc 0) (start (float-time)))
    (while (< i n) (setq acc (+ i 1)) (setq i (1+ i)))
    (- (float-time) start)))

(defun nelisp-standalone-call-floor-bench--call1 (n)
  (let ((i 0) (acc 0) (start (float-time)))
    (while (< i n)
      (setq acc (nelisp-standalone-call-floor-bench--leaf1 i))
      (setq i (1+ i)))
    (- (float-time) start)))

(defun nelisp-standalone-call-floor-bench--call2 (n)
  (let ((i 0) (acc 0) (start (float-time)))
    (while (< i n)
      (setq acc (nelisp-standalone-call-floor-bench--leaf2 i 1))
      (setq i (1+ i)))
    (- (float-time) start)))

(defun nelisp-standalone-call-floor-bench--let1 (n)
  (let ((i 0) (acc 0) (start (float-time)))
    (while (< i n)
      (setq acc (let ((k i)) k))
      (setq i (1+ i)))
    (- (float-time) start)))

(defun nelisp-standalone-call-floor-bench-run ()
  "Print one row per case.  Returns the rows as an alist."
  (let* ((n nelisp-standalone-call-floor-bench-iterations)
         ;; Run the floor twice and keep the smaller: the first pass through
         ;; anything here also pays for whatever the runtime does once (a
         ;; first GC, a cold path), and attributing that to `loop' would
         ;; make every difference below it look cheaper than it is.
         (loop-a (nelisp-standalone-call-floor-bench--loop n))
         (loop-b (nelisp-standalone-call-floor-bench--loop n))
         (loop (if (< loop-a loop-b) loop-a loop-b))
         (rows nil))
    (princ (format "CALL-FLOOR iterations=%d\n" n))
    (princ (format "CALL-FLOOR %-8s %8.2f us/iter (absolute)\n"
                   "loop" (/ (* 1000000.0 loop) n)))
    (setq rows (list (cons 'loop (/ (* 1000000.0 loop) n))))
    (dolist (case (list (cons "builtin" #'nelisp-standalone-call-floor-bench--builtin)
                        (cons "call1" #'nelisp-standalone-call-floor-bench--call1)
                        (cons "call2" #'nelisp-standalone-call-floor-bench--call2)
                        (cons "let1" #'nelisp-standalone-call-floor-bench--let1)))
      (let* ((seconds (funcall (cdr case) n))
             (delta (/ (* 1000000.0 (- seconds loop)) n)))
        (princ (format "CALL-FLOOR %-8s %+8.2f us/iter (over loop)\n"
                       (car case) delta))
        (setq rows (cons (cons (intern (car case)) delta) rows))))
    (princ "CALL-FLOOR done\n")
    (nreverse rows)))

(nelisp-standalone-call-floor-bench-run)

(provide 'nelisp-standalone-call-floor-bench)

;;; nelisp-standalone-call-floor-bench.el ends here
