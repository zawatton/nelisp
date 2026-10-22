;;; standalone-vm-gc-poll-memory-check.el --- the bytecode VM collects garbage by itself -*- lexical-binding: t -*-

;; Usage: nelisp --load test/standalone-vm-gc-poll-memory-check.el
;;
;; GNU bytecode.c polls the garbage collector at Bcall entry (maybe_gc, :797)
;; and at every 256th backward branch (:861-866).  A VM that never polls
;; collects only where the interpreter does (a `while' back edge, a form
;; boundary), so a bytecode loop that never reaches the interpreter allocates
;; without bound.  Two workloads, each ending with a check of the peak
;; resident set (VmHWM, which only grows) against CEILING-MB:
;;  1. 2.5 million Bcalls of the frozen GNU 31.1 funcall loop of
;;     test/perf/codegen-runtime/bytecode.eld: about 0.67 KB of garbage per
;;     call, 1.7 GB in all and the 8 GiB cap (exit 88) at about 12 million;
;;     only the Bcall poll bounds it.
;;  2. 30 million iterations of a call-free loop that allocates a cons per
;;     iteration (fixtures/vm-gc-poll-loop.eld): about 65 B per iteration,
;;     2 GB in all; only the backward-branch poll bounds it.
;; The idle process is about 290 MB and one collection window about 150 MB.
;; (The shape of the checks follows gc-port's tools/ai/gc-port-vm-poll.py.)

(defconst vmm-calls 2500000)
(defconst vmm-ceiling-mb 1024)
(defconst vmm-iterations 30000000)

(defun vmm-peak-mb ()
  "Peak resident set size of this process in MB, from VmHWM."
  (with-temp-buffer
    (insert-file-contents "/proc/self/status")
    (goto-char (point-min))
    (if (re-search-forward "^VmHWM:[ \t]*\\([0-9]+\\) kB" nil t)
        (/ (string-to-number (match-string 1)) 1024)
      -1)))

(defun vmm-eld (name)
  (with-temp-buffer
    (insert-file-contents (expand-file-name name (file-name-directory load-file-name)))
    (goto-char (point-min))
    (read (current-buffer))))

(let* ((funcall-fn (cdr (assq 'funcall (vmm-eld "perf/codegen-runtime/bytecode.eld"))))
       (loop-fn (cdr (assq 'loop (vmm-eld "fixtures/vm-gc-poll-loop.eld"))))
       (problems nil))
  (fset 'cr-add (cdr (assq 'add (vmm-eld "perf/codegen-runtime/bytecode.eld"))))
  (let ((answer (funcall funcall-fn vmm-calls)) (peak (vmm-peak-mb)))
    (unless (and (= answer (+ vmm-calls 2)) (> peak 0) (< peak vmm-ceiling-mb))
      (push (format "funcall answer=%S peak=%d MB" answer peak) problems)))
  (let ((answer (funcall loop-fn vmm-iterations)) (peak (vmm-peak-mb)))
    (unless (and (= answer (1- vmm-iterations)) (> peak 0) (< peak vmm-ceiling-mb))
      (push (format "loop answer=%S peak=%d MB" answer peak) problems)))
  (if problems
      (princ (format "VM-GC-POLL-MEMORY FAIL %s (ceiling %d MB)\n"
                     (mapconcat #'identity (nreverse problems) "; ") vmm-ceiling-mb))
    (princ "VM-GC-POLL-MEMORY PASS\n"))
  (kill-emacs (if problems 1 0)))
