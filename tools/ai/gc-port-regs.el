;;; gc-port-regs.el --- the collector's stack scan includes the callee-saved registers  -*- lexical-binding: t; -*-

;; GNU's mark_c_stack flushes the callee-saved registers onto the stack before it scans
;; (flush_stack_call_func, alloc.c:5252), so a Lisp_Object that lives only in a register is
;; found.  gc-regs.o's nl_gc_scan_regs does the same before the conservative scan.
;;   (nelisp--debug-switch 73)  loads five sentinels into rbx and r12-r15, calls the real
;;                              nl_gc_scan_regs with a TOP that ends the scan at the spilled
;;                              block and copies that window (nothing is scanned or marked);
;;                              answers how many sentinels the window shows (5), -1 where
;;                              the target has no self-test;
;;   (nelisp--debug-switch 74)  the same with no spill: a window of the same size at the
;;                              caller's frame (0: the negative control);
;;   (nelisp--debug-switch 75)  how many times a collection reached nl_gc_scan_regs.
;; Prints "GCREGS checks=N failures=F PASS|FAIL" last.
(defvar gcregs-checks 0)
(defvar gcregs-failures 0)
(defun gcregs-check (ok label)
  (setq gcregs-checks (1+ gcregs-checks))
  (unless ok
    (setq gcregs-failures (1+ gcregs-failures))
    (princ (format "GCREGS FAIL %s\n" label))))
(let ((spill (nelisp--debug-switch 73)))
  (if (equal spill -1)
      (progn (setq gcregs-checks (1+ gcregs-checks) gcregs-failures (1+ gcregs-failures))
             (princ "GCREGS FAIL no register self-test on this target\n"))
    (gcregs-check (equal spill 5) (format "the spilled window shows %S of 5 sentinel registers" spill))
    (gcregs-check (equal (nelisp--debug-switch 73) 5) "and again")
    (let ((ctl (nelisp--debug-switch 74)))
      (gcregs-check (equal ctl 0) (format "the window without the spill shows %S sentinels, not 0" ctl)))
    ;; the production collection goes through the stub; the probe is not a collection
    (let* ((c0 (nelisp--debug-switch 75))
           (p (progn (nelisp--debug-switch 73) (nelisp--debug-switch 74) (nelisp--debug-switch 75))))
      (gcregs-check (or (equal p c0) (>= p c0)) (format "call count went backwards: %S -> %S" c0 p))
      (garbage-collect)
      (let ((c1 (nelisp--debug-switch 75)))
        (gcregs-check (> c1 p) (format "garbage-collect reached nl_gc_scan_regs: %S -> %S" p c1))))))
(princ (format "GCREGS checks=%d failures=%d %s\n" gcregs-checks gcregs-failures
               (if (= gcregs-failures 0) "PASS" "FAIL")))
nil
