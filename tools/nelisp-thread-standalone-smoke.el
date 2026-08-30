;;; nelisp-thread-standalone-smoke.el --- Doc 199 Tier-2 clone smoke -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Interpreter-driven acceptance for Doc 199 Tier 2.  This file is loaded by
;; target/nelisp itself; it does not invoke the self-host compiler.  On Linux
;; x86_64 it spawns three real clone(2) workers from fixed native registry ID
;; 1.  Each worker sums a counted u64 buffer to 14, publishes to a distinct
;; raw result slot, and SeqCst-increments one shared done counter.  The parent
;; reads result slots only after `nelisp-thread-join' observes all three done
;; increments, then requires 14 + 14 + 14 = 42.
;;
;; The check is mutation-meaningful: suppressing a worker, its result store,
;; or its atomic completion increment makes the run hang or produce a value
;; other than 42.  Worker code touches only mmap addresses passed in A0-A2;
;; it never enters the Lisp heap or allocator.

;;; Code:

(defmacro nl-thread-smoke--should-unsupported (form)
  "Require FORM to signal `nelisp-unsupported-primitive'."
  `(let ((nl-thread-smoke--outcome
          (condition-case nl-thread-smoke--error
              (progn ,form 'nl-thread-smoke--no-error)
            (error (car nl-thread-smoke--error)))))
     (unless (eq nl-thread-smoke--outcome 'nelisp-unsupported-primitive)
       (error "expected nelisp-unsupported-primitive from %S, got %S"
              ',form nl-thread-smoke--outcome))))

(let ((checked 0)
      (names '(nelisp-thread-shared-alloc
               nelisp-thread-atomic-add
               nelisp-thread-atomic-read
               nelisp-thread-spawn
               nelisp-thread-join)))
  (dolist (name names)
    (unless (fboundp name)
      (error "thread primitive is not fboundp: %S" name))
    (setq checked (+ checked 1)))
  (if (and (eq system-type 'gnu/linux)
           (string= system-configuration "x86_64-pc-linux-gnu"))
      (let* ((shared (nelisp-thread-shared-alloc 128))
             ;; Three counted buffers: [count, value0, value1].
             (result0 (+ shared 80))
             (result1 (+ shared 88))
             (result2 (+ shared 96))
             (done (+ shared 104)))
        (when (< shared 0)
          (error "nelisp-thread-shared-alloc failed: %S" shared))
        (ptr-write-u64 shared 0 2)
        (ptr-write-u64 shared 8 6)
        (ptr-write-u64 shared 16 8)
        (ptr-write-u64 shared 24 2)
        (ptr-write-u64 shared 32 4)
        (ptr-write-u64 shared 40 10)
        (ptr-write-u64 shared 48 2)
        (ptr-write-u64 shared 56 7)
        (ptr-write-u64 shared 64 7)
        (unless (= (nelisp-thread-atomic-add done 0) 0)
          (error "fresh done counter was not zero"))
        (unless (= (nelisp-thread-atomic-read done) 0)
          (error "atomic read did not observe fresh done counter"))
        (let ((tid0 (nelisp-thread-spawn 1 0 shared result0 done))
              (tid1 (nelisp-thread-spawn 1 0 (+ shared 24) result1 done))
              (tid2 (nelisp-thread-spawn 1 0 (+ shared 48) result2 done)))
          (unless (and (> tid0 0) (> tid1 0) (> tid2 0))
            (error "thread spawn failed: %S %S %S" tid0 tid1 tid2)))
        (unless (= (nelisp-thread-join done 3) 3)
          (error "thread join returned before all workers completed"))
        ;; Publication rule: result slots are first read after join.
        (let* ((partial0 (nelisp-thread-atomic-read result0))
               (partial1 (nelisp-thread-atomic-read result1))
               (partial2 (nelisp-thread-atomic-read result2))
               (sum (+ partial0 (+ partial1 partial2))))
          (unless (= sum 42)
            (error "parallel sum was %S, expected 42 (%S %S %S)"
                   sum partial0 partial1 partial2)))
        (setq checked (+ checked 1)))
    ;; The names must still exist on unsupported targets, but every call must
    ;; fail loudly before a target-inappropriate syscall can run.
    (progn
      (nl-thread-smoke--should-unsupported
       (nelisp-thread-shared-alloc 128))
      (setq checked (+ checked 1))
      (nl-thread-smoke--should-unsupported
       (nelisp-thread-atomic-add 0 1))
      (setq checked (+ checked 1))
      (nl-thread-smoke--should-unsupported
       (nelisp-thread-atomic-read 0))
      (setq checked (+ checked 1))
      (nl-thread-smoke--should-unsupported
       (nelisp-thread-spawn 1 0 0 0 0))
      (setq checked (+ checked 1))
      (nl-thread-smoke--should-unsupported
       (nelisp-thread-join 0 1))
      (setq checked (+ checked 1))))
  (princ (format "GATE-COUNT checked=%d findings=0\n" checked))
  (princ "nelisp-thread-standalone-smoke: PASS\n"))

;;; nelisp-thread-standalone-smoke.el ends here
