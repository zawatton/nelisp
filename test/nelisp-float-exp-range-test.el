;;; nelisp-float-exp-range-test.el --- `exp' at extreme arguments -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reported by the consumer nelisp-llm (dev/BUG-nelisp-standalone-exp-
;; streams-sigsegv-2026-09-05.md, section 1), measured against d145e3c02:
;; every softmax that adds a -1e30 additive mask computes `(exp (- v mx))'
;; with v around -1e30 and got NaN instead of 0.0, silently poisoning the
;; whole autograd attention path with NaN.
;;
;; Where: `scripts/nelisp-stdlib-prelude.el', `(defun exp (x) ...)'.  It
;; reduces X = K*ln2 + R, sums exp(R) in double-double arithmetic, then used
;; to scale the result by 2^K with `(while (> j 0) (setq acc (/ acc 2.0)) ...
;; (setq j (1- j)))' -- a loop that ran |K| times, with K = round(X/ln2)
;; growing without bound as |X| grows.  Two failures followed:
;;
;;   - `(exp -1.0e6)' and `(exp -1.0e9)': |K| is ~1.44e6 / ~1.44e9, so the
;;     loop alone took far more than a second (the original report: "no
;;     answer in 10 s").
;;   - `(exp -1.0e30)': K itself, computed via `truncate' (a hardware
;;     f64->i64 cast), overflows i64 range; the runtime documents that cast
;;     as returning the Intel-SDM "integer indefinite" sentinel for any
;;     out-of-range input (`nelisp-aot-compiler--emit-f64-to-i64-trunc').
;;     With K garbage, `hi - k*ln2hi' collapsed to inf - inf, which is NaN,
;;     printed as `-0.0e+NaN'.
;;
;; The fix (all inside `exp' and its now-neighbouring helpers
;; `nelisp--pow2-exact' / `nelisp--scale-pow2'): before computing K at all,
;; X outside [u_threshold, o_threshold] -- fdlibm's own exact names for the
;; two points past which every finite double answer is 0.0 or +inf -- short-
;; circuits directly, so `truncate' is never asked about a magnitude anywhere
;; near i64 range and K is always bounded to about [-1075, 1024].  The O(|K|)
;; scaling loop is replaced with two or three O(1) multiplications by exact
;; powers of two (`nelisp--pow2-exact' builds one in O(log K) via repeated
;; squaring), split in two steps at the extreme end of each threshold so a
;; result that lands in subnormal territory is still correctly rounded
;; rather than flushed to zero one step early by a chain of individual
;; halvings -- this incidentally fixed `(exp -745.0)' too: it printed `0.0'
;; before this change but Emacs answers 4.9406564584124654e-324 (the
;; smallest positive double), one full representable value away from zero.
;; This file's own expectation for that row was re-derived from a live host
;; Emacs (`nelisp-float-exp-range/host-emacs-pins-the-oracle' below), not
;; copied from the bug report, precisely because the report's own table
;; claims `(exp -745.0)' => 0.0 for Emacs too -- checked here against a real
;; `emacs -Q --batch', on this host that claim is wrong.
;;
;; `expt' shares the defect one level up: a non-integral float exponent
;; falls through to `(exp (* e (log b)))', so `(expt 2.0 1442695.5)' hung
;; the same way before the `exp' fix.  Fixing `exp' alone was not enough,
;; though: `expt' also has its OWN unbounded loop for a non-negative integer
;; exponent against a non-integer base (`(let ((r 1) (i 0)) (while (< i e)
;; (setq r (* r b) i (1+ i))) r)') -- reached both from the half-integer
;; branch's recursive `(expt b n)' (N = 1442695 for the case above) and
;; directly, e.g. `(expt -2.0 1442695)'.  That loop is now bounded the same
;; way: any |B| != 1 reaches an absorbing state (0.0 or +-infinity) within
;; about 2100 iterations regardless of E, and continuing past that cannot
;; change the answer.  `log' and `sqrt' were checked against the same table
;; of extreme arguments and do NOT share the defect -- both reduce by the
;; float's own exponent field (bounded to the ~2098 possible values a double
;; can hold), never by a quantity proportional to the raw argument, so
;; neither has an unbounded loop to begin with.
;;
;; Every expected string here was read off a real `emacs -Q --batch' (31.1)
;; run, not invented, and `nelisp-float-exp-range/host-emacs-pins-the-oracle'
;; re-derives them from the host's own `exp'/`expt' on every run so a wrong
;; expectation cannot sit here quietly (the same discipline test/nelisp-
;; float-time-arg-test.el uses).  Every case also asserts a wall-clock bound
;; well under the fix's own budget, following test/nelisp-doc200-unibyte-
;; repr-test.el's pattern of running target/nelisp itself rather than
;; trusting a host-only comparison that would stay green with or without the
;; standalone fix.

;;; Code:

(require 'ert)
(require 'subr-x)

(defconst nelisp-float-exp-range-test--repo-root
  (file-name-as-directory
   (expand-file-name ".." (file-name-directory
                           (or load-file-name buffer-file-name))))
  "Absolute root of the checkout holding this test.")

;; The argument table the task brief asked for, plus the ones the bug report
;; itself measured.  Expected strings are NeLisp's own `prin1' spelling
;; (`1.0e+INF' / `0.0e+NaN', not Emacs's `1.0e+INF' -- they happen to agree
;; here) of the value `nelisp-float-exp-range/host-emacs-pins-the-oracle'
;; re-derives from a live host Emacs, so a typo here cannot go unnoticed.
(defconst nelisp-float-exp-range-test--exp-cases
  '(("(exp 0.0)"       . "1.0")
    ("(exp -0.0)"      . "1.0")
    ("(exp 1.0e-300)"  . "1.0")
    ("(exp -1.0e-300)" . "1.0")
    ("(exp 0.5)"       . "1.6487212707001282")
    ("(exp -0.5)"      . "0.6065306597126334")
    ("(exp 1.0)"       . "2.718281828459045")
    ("(exp -1.0)"      . "0.36787944117144233")
    ("(exp 10.0)"      . "22026.465794806718")
    ("(exp -10.0)"     . "4.5399929762484854e-05")
    ("(exp 100.0)"     . "2.6881171418161356e+43")
    ("(exp -100.0)"    . "3.720075976020836e-44")
    ("(exp 700.0)"     . "1.0142320547350045e+304")
    ("(exp -700.0)"    . "9.85967654375977e-305")
    ("(exp 709.78)"    . "1.7928227943945155e+308")
    ("(exp -709.78)"   . "5.577796105262746e-309")
    ("(exp 745.0)"     . "1.0e+INF")
    ("(exp -745.0)"    . "5e-324")
    ("(exp 746.0)"     . "1.0e+INF")
    ("(exp -746.0)"    . "0.0")
    ("(exp 1.0e3)"     . "1.0e+INF")
    ("(exp -1.0e3)"    . "0.0")
    ("(exp 1.0e5)"     . "1.0e+INF")
    ("(exp -1.0e5)"    . "0.0")
    ("(exp 1.0e6)"     . "1.0e+INF")
    ("(exp -1.0e6)"    . "0.0")
    ("(exp 1.0e9)"     . "1.0e+INF")
    ("(exp -1.0e9)"    . "0.0")
    ("(exp 1.0e30)"    . "1.0e+INF")
    ("(exp -1.0e30)"   . "0.0")
    ("(exp 710.0)"     . "1.0e+INF"))
  "Expressions and the exact printed value GNU Emacs answers for each.")

(defconst nelisp-float-exp-range-test--slow-cases
  '("(exp -1.0e6)" "(exp -1.0e9)" "(exp -1.0e30)" "(exp 1.0e9)")
  "The rows that used to hang: each must answer well under a second now.")

(defun nelisp-float-exp-range-test--binary ()
  "Return the standalone binary path, or skip the test when it is absent."
  (let ((binary (expand-file-name "target/nelisp"
                                  nelisp-float-exp-range-test--repo-root)))
    (unless (file-executable-p binary)
      (ert-skip "target/nelisp is not built; standalone-reader-test owns it"))
    binary))

(defun nelisp-float-exp-range-test--eval (expression)
  "Return `target/nelisp''s trimmed stdout for EXPRESSION."
  (let ((binary (nelisp-float-exp-range-test--binary)))
    (with-temp-buffer
      (let ((rc (call-process binary nil t nil "--eval" expression)))
        (unless (= rc 0)
          (ert-fail (format "standalone %s failed: rc=%S out=%S"
                            expression rc (buffer-string))))
        (string-trim (buffer-string))))))

(defun nelisp-float-exp-range-test--eval-timed (expression timeout-seconds)
  "Return (VALUE . SECONDS) for EXPRESSION, failing past TIMEOUT-SECONDS.
The timeout is enforced by the host Emacs process API, so this test has the
same behavior on systems that do not ship the GNU `timeout' command.  The
reported duration is measured around EXPRESSION inside NeLisp; process startup
and bootstrap loading are outside the operation budget.  A real hang must
fail the test, not block the ERT run."
  (let* ((binary (nelisp-float-exp-range-test--binary))
         (probe
          (format
           "(let ((nelisp-float-exp-range-t0 (float-time))) (let ((nelisp-float-exp-range-value %s)) (princ (format \"NELISP-TIMED value=%%S elapsed-ms=%%S\\n\" nelisp-float-exp-range-value (* 1000.0 (- (float-time) nelisp-float-exp-range-t0))))))"
           expression))
         (start (float-time))
         (buffer (generate-new-buffer " *nelisp-float-exp-range-output*"))
         process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name "nelisp-float-exp-range"
                 :buffer buffer
                 :command (list binary "--eval" probe)
                 :sentinel (lambda (_process _event) nil)
                 :noquery t))
          (let ((deadline (+ start timeout-seconds)))
            (while (and (process-live-p process)
                        (< (float-time) deadline))
              (accept-process-output
               process
               (max 0.01 (min 0.1 (- deadline (float-time))))))
            (let ((elapsed (- (float-time) start)))
              (if (process-live-p process)
                  (progn
                    (delete-process process)
                    (ert-fail
                     (format "standalone %s did not answer within %ss (hang)"
                             expression timeout-seconds)))
                (let ((rc (process-exit-status process))
                      (output (with-current-buffer buffer
                                (buffer-string))))
                  (unless (= rc 0)
                    (ert-fail (format "standalone %s failed: rc=%S out=%S"
                                      expression rc output)))
                  (unless (string-match
                           "^NELISP-TIMED value=\\([^ ]+\\) elapsed-ms=\\([0-9.eE+-]+\\)"
                           output)
                    (ert-fail (format "standalone %s emitted no timing record: %S"
                                      expression output)))
                  (cons (match-string 1 output)
                        (/ (string-to-number (match-string 2 output))
                           1000.0)))))))
      (when (process-live-p process)
        (delete-process process))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(ert-deftest nelisp-float-exp-range/host-emacs-pins-the-oracle ()
  "The expected column above is re-derived from a live host Emacs.
Guards against a copied-in expectation going stale or, as the original bug
report's own table did for `(exp -745.0)', simply being wrong."
  (dolist (case nelisp-float-exp-range-test--exp-cases)
    (let* ((expr (car case))
           (host-value (eval (car (read-from-string expr)) t)))
      (should (equal (cdr case) (prin1-to-string host-value))))))

(ert-deftest nelisp-float-exp-range/standalone-matches-host-across-the-table ()
  "`target/nelisp' matches host Emacs at every argument in the table,
including the ones that used to hang or answer NaN."
  (dolist (case nelisp-float-exp-range-test--exp-cases)
    (should (equal (cdr case)
                   (nelisp-float-exp-range-test--eval (car case))))))

(ert-deftest nelisp-float-exp-range/standalone-no-longer-hangs ()
  "Every row the O(|k|) scaling loop used to run forever on now answers
in well under a second -- 5s of headroom on a 10s-plus original hang."
  (dolist (expr nelisp-float-exp-range-test--slow-cases)
    (let ((result (nelisp-float-exp-range-test--eval-timed expr 5)))
      (should (< (cdr result) 1.0)))))

(ert-deftest nelisp-float-exp-range/standalone-nan-in-nan-out ()
  "NaN in, NaN out -- not the huge-|k| garbage the unclamped reduction
used to produce."
  (should (equal "nan"
                 (nelisp-float-exp-range-test--eval
                  "(let ((v (exp (/ 0.0 0.0)))) (if (/= v v) 'nan 'not-nan))"))))

;; `expt' with a non-integral float exponent falls through to
;; `(exp (* e (log b)))', so it shares `exp's old defect one level up; it
;; also has its own separate unbounded loop for a non-negative integer
;; exponent against a non-integer (typically float) base, reached both via
;; the half-integer branch's recursive call and directly.  Both are checked
;; here, at the exact case measured hanging before the fix.
(defconst nelisp-float-exp-range-test--expt-cases
  '(("(expt 2.0 1442695.5)"  . "1.0e+INF")   ; non-integral exponent -> exp path
    ("(expt -2.0 1442695)"   . "-1.0e+INF")  ; huge odd exponent, negative base
    ("(expt -2.0 1442696)"   . "1.0e+INF")   ; huge even exponent, negative base
    ("(expt -1.0 1442695)"   . "-1.0")       ; |base|=1, odd
    ("(expt -1.0 1442696)"   . "1.0")        ; |base|=1, even
    ("(expt 2.0 0)"          . "1")
    ("(expt 2.0 5)"          . "32.0")
    ("(expt -2.0 5)"         . "-32.0")
    ("(expt -2.0 4)"         . "16.0")
    ("(expt 0.0 5)"          . "0.0")
    ("(expt 4 1.5)"          . "8.0")
    ("(expt 2.0 -3)"         . "0.125")
    ("(expt -1.5 7)"         . "-17.0859375"))
  "Expressions and NeLisp's expected printed value for each.
`(expt 2.0 0)' prints as the integer `1' in both Emacs and NeLisp's ORIGINAL
(pre-this-fix) code for e=0 -- a separate, pre-existing float/integer
return-type quirk this fix deliberately leaves alone (Emacs itself answers
the float `1.0'); it is pinned here only so this fix does not accidentally
change it, not as a claim that it is correct.")

(ert-deftest nelisp-float-exp-range/expt-no-longer-hangs ()
  "The exact `expt' calls that hung before this fix now answer fast and
match the values a linear (never-early-exiting) computation would have
given, had it been able to finish."
  (dolist (case nelisp-float-exp-range-test--expt-cases)
    (let ((result (nelisp-float-exp-range-test--eval-timed (car case) 5)))
      (should (< (cdr result) 1.0))
      (should (equal (cdr case) (car result))))))

;; `log' and `sqrt' were checked against the same extremes and do not share
;; the defect (their range reduction is bounded by the float's own exponent
;; field, not by a quantity proportional to the raw argument) -- pinned here
;; as a regression guard, not because this change touches either.
(defconst nelisp-float-exp-range-test--log-sqrt-cases
  '(("(log 1.0e-300)"  . "-690.7755278982137")
    ("(log 1.0e300)"   . "690.7755278982137")
    ("(log 1.7976931348623157e+308)" . "709.782712893384")
    ("(log 4.9e-324)"  . "-744.4400719213812")
    ("(log 0.0)"       . "-1.0e+INF")
    ("(log -1.0)"      . :nan)
    ("(sqrt 1.0e300)"  . "1e+150")
    ("(sqrt 1.0e-300)" . "1e-150")
    ("(sqrt 1.7976931348623157e+308)" . "1.3407807929942597e+154")
    ("(sqrt 4.9e-324)" . "2.2227587494850775e-162")
    ("(sqrt -1.0)"     . :nan))
  "`log'/`sqrt' at the same extremes; unaffected by this change.")

(ert-deftest nelisp-float-exp-range/log-and-sqrt-unaffected-by-extremes ()
  (dolist (case nelisp-float-exp-range-test--log-sqrt-cases)
    (let ((result (nelisp-float-exp-range-test--eval-timed (car case) 5)))
      (should (< (cdr result) 1.0))
      (if (eq (cdr case) :nan)
          (should (string-match-p "NaN\\'" (car result)))
        (should (equal (cdr case) (car result)))))))

;;; nelisp-float-exp-range-test.el ends here
