;;; nelisp-aot-tco-bench.el --- Doc 171 TCO vs nl-loop native bench -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 171 G4 requires a benchmark proving that a self-tail-recursive
;; `defun' rewritten by the transparent TCO pass performs at least as
;; well as the handwritten `nl-loop' version of the same algorithm.
;;
;; This harness compiles a temporary `.neln' module containing both
;; variants, executes them through the existing host native-exec proof
;; path, and compares elapsed wall-clock time on the supported proof
;; lane (Linux x86_64 with cc + objcopy available).
;;
;; Performance rule:
;;   ratio = loop_time / tco_time
;;   pass  = ratio >= 0.95
;;
;; So the TCO-rewritten `defun' may be at most ~5% slower than the
;; handwritten `nl-loop' baseline, and equal/faster results pass.

;;; Code:

(require 'cl-lib)
(require 'nelisp-artifact)

(defgroup nelisp-aot-tco-bench nil
  "Doc 171 TCO benchmark."
  :group 'nelisp)

(defcustom nelisp-aot-tco-bench-input 1000000
  "Input N for the sum benchmark.
The workload is large enough that native driver process overhead does
not dominate the function-body timing."
  :type 'integer
  :group 'nelisp-aot-tco-bench)

(defcustom nelisp-aot-tco-bench-repeats 20
  "Number of timed runs per function, per round.

Was 5, and 5 could not resolve the 0.95 floor.  One run takes about 5ms,
so five of them close the timing window at 25ms -- before the CPU has
finished ramping -- and the ramp is charged to whichever function ran
first.  Measured 2026-08-19 on the shipped compiler, varying nothing but
this number:

  repeats  ratios                       mean   per-repeat tco/loop
        5  0.936 0.914 0.930 0.921      0.925  5.26 / 4.90 ms
       20  0.954 0.947 0.949 0.950      0.950  5.09 / 4.84 ms
       40  0.967 0.956 0.962 0.945      0.958  5.05 / 4.81 ms

The per-repeat times settle, and with them the ratio.  20 is where the
window (about 100ms) is long enough for that and short enough to stay
cheap in CI.

Raising it does not make the gate easy to pass, which was checked rather
than assumed: with this setting and the median below, the pre-route-2
compiler measures 0.955, 0.958, 0.947 -- straddling the floor, failing
once in three -- while the compiler that ships measures 0.995, 1.004,
1.007.  The gate still tells the two apart, and still says no to code
that is genuinely five percent slower."
  :type 'integer
  :group 'nelisp-aot-tco-bench)

(defcustom nelisp-aot-tco-bench-rounds 5
  "Number of independent (tco, loop) measurement rounds.
The reported ratio is their median.

A single round is one sample from a distribution several points wide --
the same code measured 0.852 to 0.952 across one session on 2026-08-19 --
so a gate reading one sample decides partly by luck.  The median of five
rounds is robust to the occasional round that lands during a burst of
machine load, which is the failure mode observed while this was being
worked on (one probe round reported 1.632x).

Cost is small: a round is about 0.2s of measured time, and the artifact
compile that dominates the target happens once for all rounds."
  :type 'integer
  :group 'nelisp-aot-tco-bench)

(defcustom nelisp-aot-tco-bench-threshold 0.95
  "Minimum acceptable performance ratio for TCO vs `nl-loop'.
The reported ratio is LOOP-TIME / TCO-TIME, so values >= 0.95 pass."
  :type 'number
  :group 'nelisp-aot-tco-bench)

(defvar nelisp-aot-compiler-tco-enabled)
(defvar nelisp-aot-compiler--label-counter)
(declare-function nelisp-aot-compiler--preprocess-source "nelisp-aot-compiler"
                  (sexp))

(defconst nelisp-aot-tco-bench--feature 'doc171-bench
  "Feature provided by the temporary Doc 171 benchmark source file.")

(defun nelisp-aot-tco-bench-supported-p ()
  "Return non-nil when the host can run the Doc 171 native proof lane."
  (and (eq system-type 'gnu/linux)
       (string-match-p "x86_64" (or system-configuration ""))
       (executable-find "cc")
       (executable-find "objcopy")))

(defun nelisp-aot-tco-bench--repo-root ()
  "Return the repository root."
  (expand-file-name
   ".."
   (file-name-directory (or load-file-name buffer-file-name default-directory))))

(defun nelisp-aot-tco-bench--loop-baseline-defun ()
  "Return the nl-loop baseline defun with the macro already expanded.
The AOT native pass compiles plain forms, so an unexpanded `nl-loop'
call is an unknown function to it and the baseline silently drops out
of the native section -- CI reported exactly that as \"native symbol
doc171-bench-loop-sum not in artifact defun metadata\".  Expanding it
here, where nl-prelude is loaded, keeps the baseline the hand-written
`nl-loop' shape while making it natively compilable."
  (require 'nl-prelude)
  ;; Expand the BODY only: `macroexpand-all' on the whole defun would
  ;; rewrite it to `defalias', and the native pass selects forms whose
  ;; head is literally `defun'.
  `(defun doc171-bench-loop-sum (n acc)
     ,(macroexpand-all
       '(nl-loop ((n n) (acc acc))
          (if (= n 0)
              acc
            (nl-recur (- n 1) (+ acc n)))))))

(defun nelisp-aot-tco-bench--source ()
  "Return the temporary benchmark module source."
  (mapconcat
   #'identity
   (list "(defun doc171-bench-tco-sum (n acc)"
         "  (if (= n 0)"
         "      acc"
         "    (doc171-bench-tco-sum (- n 1) (+ acc n))))"
         (prin1-to-string (nelisp-aot-tco-bench--loop-baseline-defun))
         "(provide 'doc171-bench)")
   "\n"))

(defun nelisp-aot-tco-bench--expected-sum (n)
  "Return 1 + ... + N."
  (/ (* n (1+ n)) 2))

(defun nelisp-aot-tco-bench--run-once (artifact-path symbol n)
  "Run SYMBOL from ARTIFACT-PATH with N and ACC=0."
  (nelisp-artifact-native-exec-general artifact-path symbol (list n 0)))

(defun nelisp-aot-tco-bench--measure (artifact-path symbol n repeats)
  "Return elapsed seconds for REPEATS native runs of SYMBOL."
  (let ((start (current-time)))
    (dotimes (_ repeats)
      (nelisp-aot-tco-bench--run-once artifact-path symbol n))
    (float-time (time-since start))))

(defun nelisp-aot-tco-bench--median (values)
  "Return the median of VALUES, a non-empty list of numbers."
  (let* ((sorted (sort (copy-sequence values) #'<))
         (n (length sorted)))
    (if (cl-oddp n)
        (nth (/ n 2) sorted)
      (/ (+ (nth (1- (/ n 2)) sorted) (nth (/ n 2) sorted)) 2.0))))

(cl-defun nelisp-aot-tco-bench-run (&key (input nelisp-aot-tco-bench-input)
                                         (repeats nelisp-aot-tco-bench-repeats)
                                         (rounds nelisp-aot-tco-bench-rounds))
  "Run the Doc 171 benchmark and return a result plist."
  (unless (nelisp-aot-tco-bench-supported-p)
    (error "Doc 171 bench requires Linux x86_64 with cc + objcopy"))
  (let* ((temp-dir (make-temp-file "nelisp-aot-tco-bench-" t))
         (source-path (expand-file-name "doc171-bench.el" temp-dir))
         (artifact-path (concat source-path ".neln"))
         (repo-root (nelisp-aot-tco-bench--repo-root))
         (expected (nelisp-aot-tco-bench--expected-sum input))
         (process-environment (cons "NELISP_TCO=1" process-environment))
         (load-paths (list (expand-file-name "packages/nl-prelude/src" repo-root))))
    (unwind-protect
        (progn
          (write-region (nelisp-aot-tco-bench--source) nil source-path nil 'silent)
          (nelisp-artifact-compile-file
           source-path artifact-path nil nil load-paths nil nil 'neln)
          ;; Warm both cached drivers before timing so the measurement is about
          ;; the native function body, not first-link startup.
          (unless (= (nelisp-aot-tco-bench--run-once artifact-path "doc171-bench-tco-sum" 16)
                     (nelisp-aot-tco-bench--expected-sum 16))
            (error "Doc 171 bench TCO warmup returned the wrong value"))
          (unless (= (nelisp-aot-tco-bench--run-once artifact-path "doc171-bench-loop-sum" 16)
                     (nelisp-aot-tco-bench--expected-sum 16))
            (error "Doc 171 bench nl-loop warmup returned the wrong value"))
          (let* ((tco-value (nelisp-aot-tco-bench--run-once
                             artifact-path "doc171-bench-tco-sum" input))
                 (loop-value (nelisp-aot-tco-bench--run-once
                              artifact-path "doc171-bench-loop-sum" input))
                 (ratios nil)
                 (tco-total 0.0)
                 (loop-total 0.0))
            ;; Rounds alternate tco and loop rather than timing all of one
            ;; then all of the other, so a drift in machine state over the
            ;; measurement lands on both sides instead of on whichever went
            ;; second.
            (dotimes (_ rounds)
              (let ((tco-seconds (nelisp-aot-tco-bench--measure
                                  artifact-path "doc171-bench-tco-sum"
                                  input repeats))
                    (loop-seconds (nelisp-aot-tco-bench--measure
                                   artifact-path "doc171-bench-loop-sum"
                                   input repeats)))
                (setq tco-total (+ tco-total tco-seconds))
                (setq loop-total (+ loop-total loop-seconds))
                (push (if (zerop tco-seconds) 0.0 (/ loop-seconds tco-seconds))
                      ratios)))
            (setq ratios (nreverse ratios))
            (let ((ratio (nelisp-aot-tco-bench--median ratios)))
              (list :input input
                    :repeats repeats
                    :rounds rounds
                    :expected expected
                    :tco-value tco-value
                    :loop-value loop-value
                    :tco-seconds (/ tco-total rounds)
                    :loop-seconds (/ loop-total rounds)
                    :ratios ratios
                    :ratio ratio
                    :threshold nelisp-aot-tco-bench-threshold
                    :pass (>= ratio nelisp-aot-tco-bench-threshold)))))
      (when (file-directory-p temp-dir)
        (delete-directory temp-dir t)))))


;;; Host fallback lane -------------------------------------------------
;;
;; The native lane above measures the real artifact, but it needs the
;; Linux native-exec toolchain (cc + objcopy).  Everywhere else we can
;; still compare the two SOURCE SHAPES the compiler emits -- the TCO
;; rewrite output versus the hand-written `nl-loop' expansion -- by
;; evaluating them on host Emacs through the same `seq' -> `progn'
;; shim `test/nelisp-aot-tco-test.el' uses.  These are host bytecode
;; numbers, NOT native AOT numbers: they show whether the rewrite
;; produces a loop of the same shape and cost class as `nl-loop', not
;; what the emitted machine code does.

(defun nelisp-aot-tco-bench--host-fn (body)
  "Compile BODY (AOT source dialect, params N and ACC) into a function.
Both lanes go through `eval' + `byte-compile' so the comparison is
between the two shapes, not between interpreted and compiled code."
  (byte-compile
   (eval `(cl-macrolet ((seq (&rest fs) (cons 'progn fs)))
            (lambda (n acc) ,body))
         t)))

(defun nelisp-aot-tco-bench--host-time (fn n repeats)
  "Return the best wall time of REPEATS calls of FN with N and 0."
  (let ((best nil))
    (dotimes (_ repeats)
      (garbage-collect)
      (let ((start (current-time)))
        (funcall fn n 0)
        (let ((elapsed (float-time (time-since start))))
          (when (or (null best) (< elapsed best))
            (setq best elapsed)))))
    best))

(defun nelisp-aot-tco-bench-host-run (&optional input repeats)
  "Compare the TCO rewrite against `nl-loop' on host Emacs.
Returns a result plist shaped like `nelisp-aot-tco-bench-run'."
  (require 'nelisp-aot-compiler)
  (require 'nl-prelude)
  (let* ((input (or input 2000000))
         (repeats (or repeats 3))
         (expected (/ (* input (1+ input)) 2))
         (tco-body
          (nth 3 (let ((nelisp-aot-compiler-tco-enabled t)
                       (nelisp-aot-compiler--label-counter 0))
                   (nelisp-aot-compiler--preprocess-source
                    '(defun doc171-host-sum (n acc)
                       (if (= n 0) acc
                         (doc171-host-sum (- n 1) (+ acc n))))))))
         (tco-fn (nelisp-aot-tco-bench--host-fn tco-body))
         ;; Built through `eval' so `nl-loop' expands at run time: the
         ;; bench file must byte-compile without nl-prelude on the
         ;; load-path.
         (loop-fn (byte-compile
                   (eval '(lambda (n acc)
                            (nl-loop ((n n) (acc acc))
                              (if (= n 0)
                                  acc
                                (nl-recur (- n 1) (+ acc n)))))
                         t)))
         (_ (unless (and (= (funcall tco-fn 100 0) 5050)
                         (= (funcall loop-fn 100 0) 5050))
              (error "doc171 host bench: variants disagree on a known input")))
         (tco (nelisp-aot-tco-bench--host-time tco-fn input repeats))
         (loop (nelisp-aot-tco-bench--host-time loop-fn input repeats))
         (ratio (/ loop tco)))
    (unless (= (funcall tco-fn input 0) expected)
      (error "doc171 host bench: TCO variant returned a wrong sum"))
    (list :lane 'host :input input :repeats repeats
          :tco-seconds tco :loop-seconds loop :ratio ratio
          :threshold nelisp-aot-tco-bench-threshold
          :pass (>= ratio nelisp-aot-tco-bench-threshold))))

(defun nelisp-aot-tco-bench-batch ()
  "Batch entry point for the Doc 171 benchmark.
Runs the native lane where it is available, and otherwise falls back
to the host source-shape lane so the target always reports numbers."
  (unless (nelisp-aot-tco-bench-supported-p)
    ;; INFORMATIONAL ONLY.  The Doc 171 G4 floor is defined on native
    ;; AOT code; this lane measures the two source shapes under
    ;; Emacs's byte-code VM, a different cost model, and the numbers
    ;; move with the execution engine and with machine load (observed
    ;; on one host: ~1.29x for the TCO form as interpreted closures,
    ;; 0.83x-1.06x across repeated byte-compiled runs).  So the lane
    ;; reports numbers and never fails the target.
    (let ((result (nelisp-aot-tco-bench-host-run)))
      (message "doc171-bench lane=host INFORMATIONAL (native lane needs Linux x86_64 + cc + objcopy; the G4 floor applies to the native lane only) input=%d repeats=%d tco=%.4fs loop=%.4fs ratio=%.3fx"
               (plist-get result :input)
               (plist-get result :repeats)
               (plist-get result :tco-seconds)
               (plist-get result :loop-seconds)
               (plist-get result :ratio))
      (when noninteractive
        (kill-emacs 0))))
  (let* ((result (nelisp-aot-tco-bench-run))
         (pass (plist-get result :pass)))
    ;; Every round is printed, not just the median: a gate that reports one
    ;; number hides how wide the distribution under it was, and that width
    ;; is what made this measurement undecidable before 2026-08-19.
    (message "doc171-bench input=%d repeats=%d rounds=%d tco=%.4fs loop=%.4fs rounds=[%s] ratio=%.3fx threshold=%.2f %s"
             (plist-get result :input)
             (plist-get result :repeats)
             (plist-get result :rounds)
             (plist-get result :tco-seconds)
             (plist-get result :loop-seconds)
             (mapconcat (lambda (r) (format "%.3f" r))
                        (plist-get result :ratios) " ")
             (plist-get result :ratio)
             (plist-get result :threshold)
             (if pass "PASS" "FAIL"))
    (when noninteractive
      (kill-emacs (if pass 0 1)))))

(provide 'nelisp-aot-tco-bench)

;;; nelisp-aot-tco-bench.el ends here
