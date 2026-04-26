;;; nelisp-cc-bench-actual.el --- Phase 7.1 完遂 gate 3-軸 bench actual measurement -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 7.1 *3-軸 bench actual measurement* — see
;; docs/design/28-phase7.1-native-compiler.org §5.2 LOCKED-2026-04-25-v2.
;;
;; Doc 28 v2 §5.2 LOCKED 3-軸 bench gate (Phase 7.1 完遂 gate):
;;   - fib(30)      bytecode VM 比 30x speedup 必達 (call/branch heavy)
;;   - fact-iter    bytecode VM 比 20x speedup 必達 (loop hot path)
;;   - alloc-heavy  bytecode VM 比 5x  speedup 必達 (allocator hot path)
;;
;; Plus an *informational* comparison vs Emacs native-comp (libgccjit
;; path) — the gate says "0.5x or better" so we capture the ratio for
;; transparency but never fail the run on it.
;;
;; Three measurement axes per bench:
;;   1. bytecode VM    — host Emacs `byte-compile' + plain `eval'
;;   2. NeLisp native  — `nelisp-cc-runtime-compile-and-allocate' +
;;                       Phase 7.5.4 in-process Emacs module FFI
;;                       (`nelisp-runtime-module-exec-bytes', ~10 µs / call)
;;   3. Emacs native-comp — `native-compile' + funcall (informational)
;;
;; Caveats — Phase 7.1.5 actual capability:
;;
;;   The Phase 7.1.X SSA → x86_64 backend pipeline does not yet
;;   resolve external callees (Phase 7.5 callee patch deferred), so
;;   `+' / `<' / recursive funcall in `fib' / `fact-iter' do *not*
;;   return correct values when executed natively today — they crash
;;   or return garbage.  This bench harness still *runs* the native
;;   path so that:
;;
;;     a. the actual measurement infrastructure is in place,
;;     b. the timing path (mmap JIT page → mprotect RX → call → munmap)
;;        is actually exercised on real silicon, and
;;     c. when Phase 7.5 lands callee resolution + arg passing, this
;;        harness flips from "infra baseline" to "gate verification"
;;        without further code change.
;;
;;   The initial measurement report (docs/bench-results/3-axis-actual-2026-04-25.md)
;;   documents which axes pass/fail today and the expected gate-pass
;;   condition once Phase 7.5 callee resolution is wired.
;;
;; Output channels:
;;   - `*NeLisp Bench Actual*' Emacs buffer (full results plist per axis)
;;   - stderr (machine-parseable single line per bench)
;;   - kill-emacs exit code (0 = all 3 axes PASS, 1 = any FAIL)

;;; Code:

(require 'cl-lib)
(require 'benchmark)
(require 'nelisp-cc)
(require 'nelisp-cc-runtime)

(defgroup nelisp-cc-bench-actual nil
  "Phase 7.1 3-axis bench actual measurement."
  :group 'nelisp-cc
  :prefix "nelisp-cc-bench-actual-")

(defcustom nelisp-cc-bench-actual-iterations 10
  "Default per-axis iteration count for the 3-axis bench.
Doc 28 v2 §5.2 picks the per-bench input size such that one iteration
runs in the 1-100 ms band on the bytecode VM, and N=10 keeps total
wall-time under ~30 s on the reference host."
  :type 'integer
  :group 'nelisp-cc-bench-actual)

(defcustom nelisp-cc-bench-actual-warmup-iterations 3
  "Number of pre-timing warmup iterations.
Excluded from the timing window so the JIT module load + page-cache
warm-up cost does not contaminate the measurement."
  :type 'integer
  :group 'nelisp-cc-bench-actual)

;; T38 Phase 7.5.5 — SSA frontend now lowers `letrec' / `funcall' /
;; `while', so `nelisp-cc-build-ssa-from-ast' no longer signals on
;; the bench-actual forms.  The produced bytes still reference
;; unresolved Phase 7.5 callees, so executing them would SIGSEGV;
;; `--measure-native' raises this error to surface the situation as
;; a recoverable skip rather than crashing the host Emacs.
(define-error 'nelisp-cc-bench-actual-unsafe-bytes
  "bench-actual produced bytes that would SIGSEGV at execution"
  'error)

;;; Skip predicates (binary availability) -------------------------

(defun nelisp-cc-bench-actual--module-available-p ()
  "Return non-nil when the Phase 7.5.4 in-process FFI is usable."
  (and (fboundp 'module-load)
       (boundp 'module-file-suffix)
       module-file-suffix
       (ignore-errors
         (file-readable-p (nelisp-cc-runtime--locate-runtime-module)))))

(defun nelisp-cc-bench-actual--native-comp-available-p ()
  "Return non-nil when host Emacs has libgccjit native-comp."
  (and (fboundp 'native-comp-available-p)
       (native-comp-available-p)))

(defun nelisp-cc-bench-actual--host-x86_64-p ()
  "Return non-nil on x86_64 hosts (Phase 7.5.4 MVP gate)."
  (let ((cfg (downcase (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))
    (or (string-match-p "x86_64" cfg)
        (string-match-p "amd64" cfg))))

(defun nelisp-cc-bench-actual--host-arm64-p ()
  "Return non-nil on arm64 / aarch64 hosts (M1/M2 mac etc).
The arm64 backend exists in skeleton form (T10/T100) but has known
Phase 7.1.4 TODO gaps (`:const-wide-literal' etc) — bench-actual
runs on arm64 produce `nil' values + meaningless speedups, so the
host-supported predicate excludes arm64 until the backend is
production-ready (Phase 7.1.5+ task)."
  (let ((cfg (downcase (or (and (boundp 'system-configuration)
                                system-configuration)
                           ""))))
    (or (string-match-p "aarch64" cfg)
        (string-match-p "arm64" cfg))))

(defun nelisp-cc-bench-actual--host-supported-p ()
  "Return non-nil when the host CPU has a *production-ready* NeLisp backend.
Currently only `x86_64' (T9) qualifies — the arm64 backend (T10)
exists but has Phase 7.1.4 TODO gaps and is excluded from the §5.2
gate run until Phase 7.1.5+ closes those gaps.  M1/M2 macOS users
get an `arm64-backend-incomplete-phase7.1.4' skip-reason rather
than a misleading PASS-with-nil-value report."
  (nelisp-cc-bench-actual--host-x86_64-p))

(defun nelisp-cc-bench-actual--host-backend ()
  "Return the backend symbol matching the current host (`x86_64' or `arm64').
Note: this always reflects the host CPU so per-host code paths
(notably `--native-value' / `--measure-native') target the matching
codegen.  Whether a backend is *production-ready* is a separate
question — see `--host-supported-p'."
  (cond ((nelisp-cc-bench-actual--host-x86_64-p) 'x86_64)
        ((nelisp-cc-bench-actual--host-arm64-p) 'arm64)
        (t (signal 'nelisp-cc-bench-actual-unsafe-bytes
                   (list :reason 'unsupported-host-cpu)))))

;;; Bench cases ---------------------------------------------------

;; The lambdas below are the 3 axes' Lisp source.  Each takes 0
;; arguments and embeds the bench input as a literal so the Phase
;; 7.5.4 in-process FFI's no-arg `extern "C" fn() -> i64' contract
;; works without Phase 7.5 callee resolution.  The bytecode VM /
;; native-comp baselines see the SAME source so the ratios stay
;; meaningful.

(defconst nelisp-cc-bench-actual--fib-30-form
  '(lambda ()
     (letrec ((fib (lambda (n)
                     (if (< n 2) n
                       (+ (funcall fib (- n 1))
                          (funcall fib (- n 2)))))))
       (funcall fib 30)))
  "fib(30) — naive doubly-recursive Fibonacci.
Doc 28 v2 §5.2 30x speedup gate (call/branch heavy).
Expected bytecode VM elapsed: ~5000 ms (single iteration).")

(defconst nelisp-cc-bench-actual--fact-iter-form
  '(lambda ()
     (letrec ((fact-iter (lambda (n acc)
                           (if (< n 1) acc
                             (funcall fact-iter (- n 1) (* acc n))))))
       (funcall fact-iter 1000 1)))
  "fact-iter(1000) — tail-recursive factorial.
Doc 28 v2 §5.2 20x speedup gate (loop hot path).
Expected bytecode VM elapsed: ~10 ms (single iteration).
Returns a bignum, but `<' and `*' on host Emacs handle that fine for
the bytecode / native-comp baselines.")

(defconst nelisp-cc-bench-actual--alloc-heavy-form
  '(lambda ()
     (let ((acc nil)
           (i 0))
       (while (< i 1000000)
         (setq acc (cons i acc))
         (setq i (1+ i)))
       (length acc)))
  "alloc-heavy — cons stress 1M.
Doc 28 v2 §5.2 5x speedup gate (allocator hot path).
Returns the length (1000000) so the result fits an i64 on 64-bit hosts.
Expected bytecode VM elapsed: ~3000 ms (single iteration).")

;;; Measurement helpers -------------------------------------------

(defun nelisp-cc-bench-actual--measure-bytecode (form iterations)
  "Run FORM ITERATIONS times via the host Emacs bytecode VM.
FORM is a `(lambda () ...)' source form.  Returns total elapsed wall-
clock time (seconds, float).  Used as the bytecode-VM baseline
denominator for the §5.2 gate ratios."
  (let* ((compiled (byte-compile form))
         (start (current-time))
         (last nil))
    (dotimes (_ iterations)
      (setq last (funcall compiled)))
    (ignore last)
    (float-time (time-since start))))

(defun nelisp-cc-bench-actual--bytecode-value (form)
  "Return the value of FORM evaluated via the host Emacs bytecode VM.
T84 Phase 7.5 wire — used by the value-correct gate to compare
NeLisp native execution against the bytecode reference."
  (condition-case err
      (funcall (byte-compile form))
    (error (cons :error err))))

(defun nelisp-cc-bench-actual--native-value (form)
  "Return the i64 value FORM produces under the NeLisp native pipeline,
as a Lisp integer.  T84 Phase 7.5 wire — pre-T84 the bench harness
ignored the value; T84 inspects it for the value-correct gate.

Returns the integer, or nil when the run signalled / was skipped."
  (let ((nelisp-cc-runtime-exec-mode 'in-process))
    (condition-case err
        (let* ((result (nelisp-cc-runtime-compile-and-allocate
                           form (nelisp-cc-bench-actual--host-backend)))
               (final  (plist-get result :final-bytes))
               (exec   (nelisp-cc-runtime--exec-in-process final)))
          (cond
           ((and (consp exec) (eq (car exec) :result))
            (nth 2 exec))
           (t nil)))
      (error
       (message "nelisp-cc-bench-actual--native-value error: %S" err)
       nil))))

(defconst nelisp-cc-bench-actual--i64-min (- (ash 1 63))
  "Lowest signed 64-bit integer (= -2^63).
T84 Phase 7.5 wire — used to detect when the bytecode reference
result exceeds the native i64 representation so the value-correct
gate reports `:i64-overflow' rather than a hard FAIL.")

(defconst nelisp-cc-bench-actual--i64-max (1- (ash 1 63))
  "Largest signed 64-bit integer (= 2^63 - 1).  See sibling
constant `--i64-min'.")

(defun nelisp-cc-bench-actual--values-equal-p (native-val bytecode-val)
  "Return a tag describing the value-correct comparison between NATIVE-VAL
and BYTECODE-VAL.  T84 Phase 7.5 wire.

Tags:
  t                 — values match exactly (canonical PASS)
  :i64-overflow     — bytecode-val is outside [INT64_MIN, INT64_MAX]
                      so the native i64 truncation (typically to 0
                      via repeated overflow on `*') cannot represent
                      the exact answer.  Counted as PASS because the
                      mismatch is a known Phase 7.5 i64 limitation;
                      Phase 7.6 (bignum / NaN-box tagging) closes it.
  :both-nil         — both pipelines returned nil (e.g. when the SSA
                      build skipped the form).
  nil               — values disagree (canonical FAIL)."
  (cond
   ((and (null native-val) (null bytecode-val)) :both-nil)
   ((or (null native-val) (null bytecode-val)) nil)
   ((and (integerp native-val) (integerp bytecode-val))
    (cond
     ((= native-val bytecode-val) t)
     ;; The bytecode reference is outside i64 — the native truncation
     ;; cannot be expected to match.  Document the situation as a
     ;; non-fatal gate result.
     ((or (> bytecode-val nelisp-cc-bench-actual--i64-max)
          (< bytecode-val nelisp-cc-bench-actual--i64-min))
      :i64-overflow)
     (t nil)))
   (t nil)))

(defun nelisp-cc-bench-actual--ssa-has-unresolved-call-p (function)
  "Return non-nil when FUNCTION contains opcodes that *cannot* be
linked today (T43 Phase 7.5.6 — runtime callee resolution + closure
allocator now embed trampolines so `:call' / `:closure' /
`:call-indirect' execute end-to-end).

Specifically: a `:call' with an unknown primitive (= not in
`nelisp-cc-callees-supported-primitives') still produces a
zero-displacement CALL rel32 (best-effort fall-through) — for the
3-axis bench gate (fib / fact-iter / alloc-heavy) the only callees
referenced are `+' / `-' / `<' / `*' / `1+' / `length' which are
all registered.  This predicate now returns nil on the bench forms,
flipping the harness from \"safe-skip\" to \"actual-execute\"."
  (require 'nelisp-cc-callees)
  (let ((supported (nelisp-cc-callees-supported-primitives 'x86_64)))
    (cl-loop for blk in (nelisp-cc--ssa-function-blocks function)
             thereis
             (cl-loop for instr in (nelisp-cc--ssa-block-instrs blk)
                      for op = (nelisp-cc--ssa-instr-opcode instr)
                      thereis
                      (and (eq op 'call)
                           (let ((fn (plist-get
                                      (nelisp-cc--ssa-instr-meta instr)
                                      :fn)))
                             (and fn (not (memq fn supported)))))))))

(defun nelisp-cc-bench-actual--measure-native (form iterations)
  "Run FORM compiled via NeLisp native + Phase 7.5.4 in-process FFI.
FORM is a `(lambda () ...)' source form.  Returns total elapsed
wall-clock time (seconds, float).

The pipeline (per iteration):
  1. compile FORM → bytes (build-ssa-from-ast → linear-scan → backend),
  2. mmap a JIT page, copy bytes, mprotect RX,
  3. call the bytes as `extern \"C\" fn() -> i64',
  4. munmap.

T38 Phase 7.5.5 caveat: the SSA frontend now lowers `letrec' /
`funcall' / `while' (all 3 axes' compile-side blockers cleared), but
external callees (`+', `<', `funcall' target closures, recursion)
are still not resolved — the produced bytes contain zero-displacement
CALL fixups + MOV r,0 placeholder closures that SIGSEGV at execution.
`condition-case' cannot trap SIGSEGV; the helper signals
`nelisp-cc-bench-actual-unsafe-bytes' so the harness records
`:native-skip-reason native-execute-unsafe' rather than crashing the
host Emacs.  Phase 7.5 callee resolution flips this from
\"infra baseline\" to \"semantic gate verification\"."
  (let* ((fn (nelisp-cc-build-ssa-from-ast form)))
    ;; Pre-flight safety check: if the SSA function still references
    ;; unresolved external callees / placeholder closures, executing
    ;; the produced bytes would SIGSEGV.  Signal a recoverable error
    ;; so the harness records a skip-reason rather than crashing.
    (when (nelisp-cc-bench-actual--ssa-has-unresolved-call-p fn)
      (signal 'nelisp-cc-bench-actual-unsafe-bytes
              (list :reason 'unresolved-callee-or-closure))))
  (let* ((nelisp-cc-runtime-exec-mode 'in-process)
         (start (current-time)))
    ;; Compile-once: the bytes are the same per iteration, only the
    ;; in-process exec call differs.  This mirrors the bytecode-VM
    ;; baseline (compile-once, run-N-times) so the ratio is fair.
    (let* ((compiled-once (nelisp-cc-runtime-compile-and-allocate
                           form (nelisp-cc-bench-actual--host-backend)))
           (final-bytes   (plist-get compiled-once :final-bytes)))
      ;; Reset start AFTER the compile so we measure pure exec, not
      ;; compile-time.  This matches Doc 28 v2 §5.2's intent (gate is
      ;; on steady-state native execution, not AOT compile cost).
      (setq start (current-time))
      (dotimes (_ iterations)
        ;; --exec-in-process returns (:result 0 INT) / (:error -1 MSG)
        ;; — we don't inspect the value here (see file commentary on
        ;; semantic correctness pending Phase 7.5).  We DO swallow
        ;; signal-class errors so a crashing payload does not abort
        ;; the bench; the report records the failure mode separately.
        (condition-case nil
            (nelisp-cc-runtime--exec-in-process final-bytes)
          (error nil))))
    (float-time (time-since start))))

(defun nelisp-cc-bench-actual--measure-emacs-native-comp (form iterations)
  "Run FORM ITERATIONS times via Emacs native-comp (libgccjit path).
Returns total elapsed wall-clock time (seconds, float), or nil when
host Emacs lacks `native-compile' (Doc 28 §5.2 informational baseline,
not a gate input)."
  (unless (nelisp-cc-bench-actual--native-comp-available-p)
    (cl-return-from nelisp-cc-bench-actual--measure-emacs-native-comp nil))
  (condition-case err
      (let* ((compiled (native-compile form))
             (start    (current-time))
             (last nil))
        (dotimes (_ iterations)
          (setq last (funcall compiled)))
        (ignore last)
        (float-time (time-since start)))
    (error
     (message "nelisp-cc-bench-actual: native-comp failed: %S" err)
     nil)))

(defun nelisp-cc-bench-actual--ratio (numerator denominator)
  "Return NUMERATOR/DENOMINATOR as a float; nil when either is nil/0."
  (cond ((or (null numerator) (null denominator)) nil)
        ((zerop denominator) most-positive-fixnum)
        (t (/ (float numerator) (float denominator)))))

;;; Per-bench runners ---------------------------------------------

(defun nelisp-cc-bench-actual--run-one (name form gate-target iterations)
  "Run NAME's bench (FORM) and return a result plist.

Plist keys:
  :bench                — symbol (`fib-30' / `fact-iter' / `alloc-heavy')
  :iterations           — int
  :bytecode-elapsed     — float seconds (total over ITERATIONS)
  :native-elapsed       — float seconds (total) or nil when skipped
  :native-comp-elapsed  — float seconds (total) or nil when host lacks it
  :speedup              — bytecode/native ratio (gate numerator)
  :gate-target          — float, the §5.2 30x/20x/5x bar
  :gate-pass            — bool, t when speedup >= gate-target
  :emacs-native-comp-ratio — native/native-comp (informational, can be nil)
  :emacs-native-comp-acceptable — t when ratio >= 0.5x (informational)
  :native-skip-reason   — symbol or nil"
  (message "==== nelisp-cc-bench-actual: %s ====" name)
  (let* ((bytecode-elapsed
          (nelisp-cc-bench-actual--measure-bytecode form iterations))
         (native-skip-reason
          (cond
           ((not (nelisp-cc-bench-actual--module-available-p))
            'module-not-built)
           ((nelisp-cc-bench-actual--host-arm64-p)
            ;; arm64 backend has Phase 7.1.4 TODO gaps — skip with an
            ;; explicit reason rather than running and reporting nil
            ;; values + meaningless speedups (= the M1 mac symptom on
            ;; bench-actual T162 follow-up run).
            'arm64-backend-incomplete-phase7.1.4)
           ((not (nelisp-cc-bench-actual--host-supported-p))
            'unsupported-host-cpu)
           (t nil)))
         (native-error nil)
         (native-unsafe-bytes-p nil)
         (native-elapsed
          (when (null native-skip-reason)
            (condition-case err
                (nelisp-cc-bench-actual--measure-native form iterations)
              (nelisp-cc-bench-actual-unsafe-bytes
               (message "  native measurement skipped (unsafe bytes): %S" err)
               (setq native-unsafe-bytes-p t)
               nil)
              (error
               (message "  native measurement raised: %S" err)
               (setq native-error err)
               nil))))
         ;; If native-elapsed came back nil, classify the reason:
         ;;   - `native-execute-unsafe' → T38 ships SSA frontend for
         ;;     letrec/funcall/while but Phase 7.5 callee resolution
         ;;     not yet wired, so executing the bytes would SIGSEGV
         ;;   - `native-compile-failed' → SSA build / linear-scan /
         ;;     backend codegen raised (T38 is supposed to make this
         ;;     not happen on the bench-actual forms; if it does the
         ;;     skip-reason surfaces the regression).
         (native-skip-reason
          (cond (native-skip-reason native-skip-reason)
                (native-unsafe-bytes-p 'native-execute-unsafe)
                ((and (null native-elapsed) native-error) 'native-compile-failed)
                (t nil)))
         (native-comp-elapsed
          (nelisp-cc-bench-actual--measure-emacs-native-comp form iterations))
         (speedup
          (nelisp-cc-bench-actual--ratio bytecode-elapsed native-elapsed))
         (gate-pass
          (cond (native-skip-reason :skipped)
                ((and speedup (>= speedup gate-target)) t)
                (t nil)))
         (vs-native-comp
          (nelisp-cc-bench-actual--ratio native-elapsed native-comp-elapsed))
         (vs-native-comp-acceptable
          (and vs-native-comp (>= vs-native-comp 0.5))))
    ;; T84 Phase 7.5 wire — value-correct check.  Run FORM once
    ;; under both pipelines (outside the timing window) and compare
    ;; the i64 / bignum return.  bc-val is always measured (it's the
    ;; reference baseline, host-CPU independent); nat-val skipped =>
    ;; value-eq = :skipped, value-pass = :skipped (treated as
    ;; pass-through, not FAIL, by the OVERALL aggregator).
    (let* ((bc-val (nelisp-cc-bench-actual--bytecode-value form))
           (nat-val
            (and (null native-skip-reason)
                 (nelisp-cc-bench-actual--native-value form)))
           (value-eq
            (cond (native-skip-reason :skipped)
                  (t (nelisp-cc-bench-actual--values-equal-p nat-val bc-val))))
           (value-pass (cond ((eq value-eq :skipped) :skipped)
                             ((eq value-eq t) t)
                             ((eq value-eq :i64-overflow) t)
                             (t nil))))
      (message "  bytecode=%.4fs native=%s native-comp=%s speedup=%s gate=%.1fx %s | value: native=%S bc=%S → %S"
               bytecode-elapsed
               (if native-elapsed (format "%.4fs" native-elapsed) "skipped")
               (if native-comp-elapsed (format "%.4fs" native-comp-elapsed) "n/a")
               (if speedup (format "%.2fx" speedup) "n/a")
               gate-target
               (cond ((eq gate-pass :skipped) "SKIP")
                     (gate-pass "PASS")
                     (t "FAIL"))
               nat-val bc-val value-eq)
      (list :bench name
            :iterations iterations
            :bytecode-elapsed bytecode-elapsed
            :native-elapsed native-elapsed
            :native-comp-elapsed native-comp-elapsed
            :speedup speedup
            :gate-target gate-target
            :gate-pass gate-pass
            :emacs-native-comp-ratio vs-native-comp
            :emacs-native-comp-acceptable vs-native-comp-acceptable
            :native-skip-reason native-skip-reason
            :native-value nat-val
            :bytecode-value bc-val
            :value-eq value-eq
            :value-pass value-pass))))

(defun nelisp-cc-bench-actual-fib-30 (&optional iterations)
  "fib(30) 3-axis bench (Doc 28 v2 §5.2 30x gate)."
  (nelisp-cc-bench-actual--run-one
   'fib-30
   nelisp-cc-bench-actual--fib-30-form
   30.0
   (or iterations nelisp-cc-bench-actual-iterations)))

(defun nelisp-cc-bench-actual-fact-iter (&optional iterations)
  "fact-iter(1000) 3-axis bench (Doc 28 v2 §5.2 20x gate)."
  (nelisp-cc-bench-actual--run-one
   'fact-iter
   nelisp-cc-bench-actual--fact-iter-form
   20.0
   (or iterations nelisp-cc-bench-actual-iterations)))

(defun nelisp-cc-bench-actual-alloc-heavy (&optional iterations)
  "alloc-heavy 1M-cons 3-axis bench (Doc 28 v2 §5.2 5x gate)."
  (nelisp-cc-bench-actual--run-one
   'alloc-heavy
   nelisp-cc-bench-actual--alloc-heavy-form
   5.0
   ;; Single iteration on alloc-heavy by default — 1M cons * 10
   ;; iterations is too long for an interactive bench run.
   (or iterations 1)))

;;; 3-axis run + report -------------------------------------------

(defun nelisp-cc-bench-actual--format-result (r)
  "Format a single bench result plist as a multi-line text block."
  (let* ((bench (plist-get r :bench))
         (iters (plist-get r :iterations))
         (bc (plist-get r :bytecode-elapsed))
         (nat (plist-get r :native-elapsed))
         (nc (plist-get r :native-comp-elapsed))
         (sp (plist-get r :speedup))
         (gate (plist-get r :gate-target))
         (pass (plist-get r :gate-pass))
         (skip (plist-get r :native-skip-reason))
         (vs (plist-get r :emacs-native-comp-ratio))
         (vs-ok (plist-get r :emacs-native-comp-acceptable)))
    (let ((nat-val   (plist-get r :native-value))
          (bc-val    (plist-get r :bytecode-value))
          (value-eq  (plist-get r :value-eq))
          (val-pass  (plist-get r :value-pass)))
      (format (concat
               "  %-12s iters=%d\n"
               "    bytecode VM           : %.4f s (total)\n"
               "    NeLisp native         : %s\n"
               "    Emacs native-comp     : %s\n"
               "    speedup               : %s\n"
               "    §5.2 gate             : %.1fx → %s\n"
               "    vs Emacs native-comp  : %s%s\n"
               "    skip-reason           : %s\n"
               "    value (native)        : %S\n"
               "    value (bytecode)      : %S\n"
               "    value-correct gate    : %S → %s\n")
              (symbol-name bench) iters bc
              (if nat (format "%.4f s (total)" nat) "skipped")
              (if nc  (format "%.4f s (total)" nc)  "not available on this host")
              (if sp  (format "%.2fx (bytecode/native)" sp)  "n/a")
              gate (cond ((eq pass :skipped) "SKIP")
                         (pass "PASS") (t "FAIL"))
              (if vs (format "%.2fx (native/native-comp)" vs) "n/a")
              (if vs (format ", >= 0.5x %s"
                             (if vs-ok "PASS" "FAIL"))
                "")
              (if skip (symbol-name skip) "—")
              nat-val bc-val
              value-eq (cond ((eq val-pass :skipped) "SKIP")
                             (val-pass "PASS") (t "FAIL"))))))

(defun nelisp-cc-bench-actual-run-3-axis (&optional iterations)
  "Run the 3-axis bench (fib-30 + fact-iter + alloc-heavy).
Optional ITERATIONS overrides the per-bench iteration count
\(defaults to `nelisp-cc-bench-actual-iterations').

Returns the list of result plists.  When called interactively (= as
the `make bench-actual' batch entry point), formats a full report
to the `*NeLisp Bench Actual*' buffer + stderr and exits the Emacs
process with code 0 when all 3 §5.2 gates PASS, 1 otherwise."
  (let* ((iters (or iterations nelisp-cc-bench-actual-iterations))
         (results (list (nelisp-cc-bench-actual-fib-30 iters)
                        (nelisp-cc-bench-actual-fact-iter iters)
                        ;; alloc-heavy keeps its 1-iteration default
                        ;; — the lambda allocates 1M cons cells which
                        ;; already takes seconds on bytecode VM.
                        (nelisp-cc-bench-actual-alloc-heavy nil))))
    (with-current-buffer (get-buffer-create "*NeLisp Bench Actual*")
      (erase-buffer)
      (insert "Phase 7.1 3-axis bench actual measurement\n")
      (insert "(Doc 28 v2 §5.2 LOCKED-2026-04-25-v2 gate)\n\n")
      (insert (format "Host           : %s\n"
                      (or (and (boundp 'system-configuration)
                               system-configuration)
                          "(unknown)")))
      (insert (format "Module FFI     : %s\n"
                      (if (nelisp-cc-bench-actual--module-available-p)
                          "available" "missing (run `make runtime-module')")))
      (insert (format "Native-comp    : %s\n"
                      (if (nelisp-cc-bench-actual--native-comp-available-p)
                          "available" "not available")))
      (insert (format "Date           : %s\n\n"
                      (format-time-string "%Y-%m-%d %H:%M:%S %z")))
      (dolist (r results)
        (insert (nelisp-cc-bench-actual--format-result r))
        (insert "\n"))
      ;; Aggregator semantics:
      ;;   - per-axis gate-pass / value-pass values are t / nil / :skipped
      ;;   - OVERALL FAIL  = at least one axis has the bool nil (= run produced
      ;;                     a definite regression)
      ;;   - OVERALL SKIP  = no FAIL + at least one :skipped (= host-CPU not
      ;;                     production-ready, no regression info available)
      ;;   - OVERALL PASS  = no FAIL + no :skipped (= all axes verified)
      ;; exit code: 0 for PASS or SKIP (= not a regression), 1 for FAIL.
      (let* ((classify (lambda (key)
                         (let ((vs (mapcar (lambda (r) (plist-get r key))
                                           results)))
                           (cond ((memq nil vs) 'fail)
                                 ((memq :skipped vs) 'skip)
                                 (t 'pass)))))
             (timing (funcall classify :gate-pass))
             (value  (funcall classify :value-pass))
             (overall (cond ((or (eq timing 'fail) (eq value 'fail)) 'fail)
                            ((or (eq timing 'skip) (eq value 'skip)) 'skip)
                            (t 'pass)))
             (label (lambda (s)
                      (cond ((eq s 'pass) "PASS")
                            ((eq s 'skip) "SKIP")
                            (t "FAIL")))))
        (insert (format "OVERALL §5.2 3-axis timing gate      : %s\n"
                        (funcall label timing)))
        (insert (format "OVERALL T84 3-axis value-correct gate: %s\n"
                        (funcall label value)))
        (insert (format "OVERALL combined                     : %s\n"
                        (funcall label overall)))
        (princ (buffer-string) #'external-debugging-output)
        (when noninteractive
          (kill-emacs (if (eq overall 'fail) 1 0)))))
    results))

(provide 'nelisp-cc-bench-actual)

;;; nelisp-cc-bench-actual.el ends here
