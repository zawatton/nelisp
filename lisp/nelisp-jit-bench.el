;;; nelisp-jit-bench.el --- JIT lowering bench harness  -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Doc 77b Stage b.5 — elisp port of the former
;; `build-tool/src/jit/bench.rs' (deleted in this Stage).  Validates
;; the dispatcher-bypass speedup hypothesis from Doc 62 §1.2: hot
;; primitives invoked through the JIT-lowered fn-ptr path should run
;; noticeably faster than the dispatcher's name match-arm.  Doc 62
;; §4.2 sets the bar at 5x〜20x.
;;
;; Run with:
;;   NELISP_BENCH=1 make test
;; or interactively:
;;   M-x ert RET bench- RET
;; (after `(setenv "NELISP_BENCH" "1")').
;;
;; Each bench is `skip-unless'-gated so the regular ERT suite stays
;; silent — bench timing is noisy and not worth the per-PR cost.
;;
;; Methodology mirrors bench.rs verbatim:
;; - inner-loop only (= bypass eval overhead, isolate JIT lookup vs
;;   dispatcher arm).
;; - 10k warm-up iters prime the CPU branch predictor.
;; - JIT-only primitives (= `bi_*' arm deleted in Doc 62 Stage C)
;;   print "dispatch = N/A" instead of a misleading lookup-miss
;;   number; cf. `nelisp-jit-bench--jit-only-names'.
;;
;; The bench calls Stage b.2 `nl-jit-call-*' primitives directly —
;; same fn-ptr path the elisp `lowered_X' wrappers use, so we measure
;; the trampoline + helper, not the elisp wrapper overhead.
;;
;; STATE: drafted at Doc 77b Stage b.1 ship time but the
;; `nl-jit-call-*' primitives it depends on are introduced in Stage
;; b.2 (= bridge.rs).  Until then this file loads cleanly (defuns
;; don't execute at load time) but the benches will signal
;; `void-function' if NELISP_BENCH is set early.  Safe to ship as
;; Stage b.5 prep — no impact on regular `make test'.

;;; Code:

(require 'ert)

;; Iteration counts mirror bench.rs (Doc 62 §4.2).
(defconst nelisp-jit-bench--iters-add2        1000000)
(defconst nelisp-jit-bench--iters-syscall      100000)
(defconst nelisp-jit-bench--iters-cons-cycle   100000)
(defconst nelisp-jit-bench--iters-inline-tag  1000000)
(defconst nelisp-jit-bench--warmup              10000)

;; All JIT entries except the two `ENV_TOGGLEABLE_ENTRIES' from
;; build-tool/src/jit/mod.rs are JIT-only — their `bi_*' arms were
;; deleted in Doc 62 Stage C-Phase1/1b, so dispatch returns
;; `UnboundFunction'.  The bench prints "dispatch = N/A" for these
;; instead of a meaningless name-lookup-miss timing.
(defconst nelisp-jit-bench--jit-only-names
  '("nelisp--add2" "eq" "car" "cdr" "cons" "setcar" "setcdr"
    "aref" "aset" "length" "elt")
  "JIT entries with no `bi_*' dispatch fallback.
The complement of ENV_TOGGLEABLE_ENTRIES (= nelisp--syscall,
nelisp--syscall-supported-p) which DO have dispatch fallback.")

(defsubst nelisp-jit-bench--jit-only-p (name)
  (member name nelisp-jit-bench--jit-only-names))

(defun nelisp-jit-bench--time (iters thunk)
  "Run THUNK ITERS times after a fixed warmup; return elapsed seconds (float)."
  (dotimes (_ nelisp-jit-bench--warmup) (funcall thunk))
  (let ((t0 (current-time)))
    (dotimes (_ iters) (funcall thunk))
    (float-time (time-subtract (current-time) t0))))

(defun nelisp-jit-bench--time-or-skip (iters thunk jit-only-p)
  "Time THUNK over ITERS iters; return nil when JIT-ONLY-P is non-nil.
The dispatch path for JIT-only primitives signals (= UnboundFunction
on the Rust side), so timing it would just measure name-lookup miss."
  (and (not jit-only-p) (nelisp-jit-bench--time iters thunk)))

(defun nelisp-jit-bench--report-line (label iters jit-secs disp-secs)
  "Emit one bench result line.  DISP-SECS may be nil for JIT-only."
  (let* ((jit-ns  (/ (* jit-secs 1e9) iters)))
    (if disp-secs
        (let* ((disp-ns (/ (* disp-secs 1e9) iters))
               (speedup (/ disp-secs jit-secs)))
          (princ (format
                  "[bench] %-28s %10d iters | jit %9.2fns/op | dispatch %9.2fns/op | speedup %.2fx\n"
                  label iters jit-ns disp-ns speedup))
          speedup)
      (princ (format
              "[bench] %-28s %10d iters | jit %9.2fns/op | dispatch       N/A    (JIT-only primitive)\n"
              label iters jit-ns))
      nil)))

(defmacro nelisp-jit-bench--report (label iters name jit-thunk disp-thunk)
  "Time JIT-THUNK + (unless JIT-only) DISP-THUNK and report under LABEL."
  (declare (indent 2))
  `(let* ((jit-only (nelisp-jit-bench--jit-only-p ,name))
          (jit-s    (nelisp-jit-bench--time ,iters ,jit-thunk))
          (disp-s   (nelisp-jit-bench--time-or-skip ,iters ,disp-thunk jit-only)))
     (nelisp-jit-bench--report-line ,label ,iters jit-s disp-s)))

;; ---------------------------------------------------------------- benches

(ert-deftest nelisp-jit-bench-add2 ()
  "JIT vs dispatch for `nelisp--add2' (Int+Int).  JIT-only."
  (skip-unless (getenv "NELISP_BENCH"))
  (let ((speedup (nelisp-jit-bench--report
                     "add2 (Int+Int)" nelisp-jit-bench--iters-add2 "nelisp--add2"
                  (lambda () (nl-jit-call-i64-i64 "nelisp--add2" 7 8))
                  (lambda () (funcall (intern "nelisp--add2") 7 8)))))
    (when speedup
      (should (>= speedup 0.9)))))

(ert-deftest nelisp-jit-bench-syscall-getpid ()
  "JIT vs dispatch for `nelisp--syscall' (= SYS_getpid 39).
Has `bi_syscall' fallback (one of the two ENV_TOGGLEABLE_ENTRIES) so
the comparison is meaningful.  Doc 77 §0 baseline 0.92-0.97x — libc
syscall (~130ns) dwarfs JIT trampoline cost (~3-5ns).  Gate at 0.80x."
  (skip-unless (getenv "NELISP_BENCH"))
  (let ((speedup (nelisp-jit-bench--report
                     "syscall(getpid)" nelisp-jit-bench--iters-syscall "nelisp--syscall"
                  (lambda () (nl-jit-call-syscall "nelisp--syscall" 39))
                  (lambda () (funcall (intern "nelisp--syscall") 39)))))
    (should (and speedup (>= speedup 0.80)))))

(ert-deftest nelisp-jit-bench-car-cdr-cons-cycle ()
  "Build a 64-deep cons chain repeatedly, then walk a 1000-cons chain
via car/cdr.  All three are JIT-only — dispatch = N/A."
  (skip-unless (getenv "NELISP_BENCH"))
  (let* ((chain-depth 64)
         (cons-iters (* nelisp-jit-bench--iters-cons-cycle chain-depth))
         (cons-secs
          (let ((t0 (current-time)))
            (dotimes (_ nelisp-jit-bench--warmup)
              (nl-jit-call-ptr-ptr "cons" 1 nil))
            (setq t0 (current-time))
            (dotimes (_ nelisp-jit-bench--iters-cons-cycle)
              (let ((acc nil))
                (dotimes (_ chain-depth)
                  (setq acc (nl-jit-call-ptr-ptr "cons" 1 acc)))))
            (float-time (time-subtract (current-time) t0)))))
    (nelisp-jit-bench--report-line "cons (build 64-deep chain)" cons-iters cons-secs nil)
    (let ((chain nil))
      (dotimes (i 1000) (setq chain (cons i chain)))
      (nelisp-jit-bench--report
          "car (Cons head)" nelisp-jit-bench--iters-cons-cycle "car"
        (lambda () (nl-jit-call-ptr-ptr "car" chain nil))
        (lambda () (car chain)))
      (nelisp-jit-bench--report
          "cdr (Cons tail)" nelisp-jit-bench--iters-cons-cycle "cdr"
        (lambda () (nl-jit-call-ptr-ptr "cdr" chain nil))
        (lambda () (cdr chain))))))

(ert-deftest nelisp-jit-bench-inline-tag-paths ()
  "(car nil) / (cdr nil) / (length nil) / (eq nil nil) — all hit the
inline fast path that skips the helper.  All JIT-only."
  (skip-unless (getenv "NELISP_BENCH"))
  (let ((iters nelisp-jit-bench--iters-inline-tag))
    (nelisp-jit-bench--report "car nil (inline)" iters "car"
      (lambda () (nl-jit-call-ptr-ptr "car" nil nil))
      (lambda () (car nil)))
    (nelisp-jit-bench--report "cdr nil (inline)" iters "cdr"
      (lambda () (nl-jit-call-ptr-ptr "cdr" nil nil))
      (lambda () (cdr nil)))
    (nelisp-jit-bench--report "length nil (inline)" iters "length"
      (lambda () (nl-jit-call-ptr-ptr "length" nil nil))
      (lambda () (length nil)))
    (nelisp-jit-bench--report "eq Int+Int (inline)" iters "eq"
      (lambda () (nl-jit-call-ptr-ptr "eq" 42 42))
      (lambda () (eq 42 42)))))

(provide 'nelisp-jit-bench)
;;; nelisp-jit-bench.el ends here
