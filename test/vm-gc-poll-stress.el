;;; vm-gc-poll-stress.el --- every bytecode VM GC poll is safe -*- lexical-binding: t -*-

;; Usage: nelisp --load test/vm-gc-poll-stress.el
;;        VM_POLL_BUDGET=all nelisp --load test/vm-gc-poll-stress.el
;; (Not named standalone-*-check.el on purpose: test/perf/standalone-checks.sh
;; gives a file 300 s.  The ledger runs it through tools/ai/batch-f-check.sh
;; vm-stress; the unlimited run is the pipeline's vm-stress-full unit.)
;;
;; The bytecode VM polls the collector like GNU bytecode.c: at Bcall entry
;; (maybe_gc, :797) and at every 256th backward branch (:861-866).  A poll is
;; safe when every live value is reachable from the VM's rooted frame, a
;; recorded evaluator frame or a native stack slot.  This check forces a full
;; mid-form collection at the polls of the GNU 31.1 bytecode corpus
;; (test/bytecode-word-corpus.eld, replayed as standalone-bytecode-word-check.el
;; does, every answer compared with GNU's) and of an allocating loop:
;;  - `(nelisp--debug-switch 32 N)' forces the next N polls; it is set before
;;    each case, so a case that polls thousands of times (a recursive fib makes
;;    about 2000 calls) still gets its first N forced.  VM_POLL_BUDGET=all
;;    forces every poll of every case (about five minutes a linkage).
;;  - `(nelisp--debug-switch 28)' switches the conservative native-stack scan
;;    off, so only the precise roots keep a value alive across a poll.
;; The check also requires that collections really ran (mid-form fired count,
;; `nelisp--gc-diag' element 7), so a build without the hook, where the switch
;; does nothing, cannot pass.  (The shape of the check follows gc-port's
;; tools/ai/gc-port-vm-poll.py `force'.)

(defconst vsc-min-collections 200)
(defconst vsc-budget
  (let ((setting (getenv "VM_POLL_BUDGET")))
    (cond ((equal setting "all") nil)
          ((and setting (string-match-p "\\`[0-9]+\\'" setting)) (string-to-number setting))
          (t 100))))

(defun vsc-force ()
  (if vsc-budget (nelisp--debug-switch 32 vsc-budget) (nelisp--debug-switch 32)))

(let* ((dir (file-name-directory load-file-name))
       (data (with-temp-buffer
               (insert-file-contents (expand-file-name "bytecode-word-corpus.eld" dir))
               (car (read-from-string (buffer-string)))))
       (loop (cdr (assq 'loop (with-temp-buffer
                                (insert-file-contents (expand-file-name "fixtures/vm-gc-poll-loop.eld" dir))
                                (goto-char (point-min))
                                (read (current-buffer))))))
       (fails 0) (n 0) (fired 0))
  (nelisp--debug-switch 28)
  (dolist (p (nth 1 data))
    (eval (list 'defvar (car p)) t)
    (set (car p) (cdr p)))
  (dolist (p (nth 2 data))
    (fset (car p) (cdr p)))
  (dolist (c (nth 3 data))
    (setq n (1+ n))
    (vsc-force)
    (let ((got (format "%S" (condition-case e (apply (car c) (copy-tree (cadr c) t))
                              (error (list 'error (car e)))))))
      (unless (equal got (nth 2 c))
        (setq fails (1+ fails))
        (princ (format "VM-GC-POLL-STRESS FAIL %S %S got %s want %s\n"
                       (car c) (cadr c) got (nth 2 c))))))
  ;; A call-free loop that allocates: only the backward-branch poll can run.
  (vsc-force)
  (let ((answer (funcall loop 5000)))
    (unless (eql answer 4999)
      (setq fails (1+ fails))
      (princ (format "VM-GC-POLL-STRESS FAIL loop got %S want 4999\n" answer))))
  ;; Read the count first: (nelisp--debug-switch 5) re-arms the normal mode and zeroes it.
  (setq fired (nth 7 (nelisp--gc-diag 0)))
  (nelisp--debug-switch 5)
  (nelisp--debug-switch 29)
  (let ((ok (and (= fails 0) (= n (length (nth 3 data))) (integerp fired) (>= fired vsc-min-collections))))
    (princ (if ok
               "VM-GC-POLL-STRESS PASS\n"
             (format "VM-GC-POLL-STRESS FAIL cases=%d/%d collections=%S (want >= %d)\n"
                     (- n fails) n fired vsc-min-collections)))
    (kill-emacs (if ok 0 1))))
