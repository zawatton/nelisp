;;; nelisp-thread-allocating-standalone-smoke.el --- Doc 199 Tier-3a spike -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Feasibility proof for bounded ordinary allocating Lisp on clone(2) workers.
;; Worker registry ID 2 enters `nelisp_eval_call' with an mmap-resident private
;; EvalCtx and a fresh lexical frame stack.  The whole parallel section keeps
;; nl_gc_loop_ctx.in_progress set, disables sweep free-list reuse so allocation
;; stays on the CAS bump path, and requires 8 MiB to remain in the current
;; arena chunk.  Section exit fails if the current chunk changed.
;;
;; Checkpoint 1 evaluates one pre-built form which pushes a `let' frame,
;; allocates a list, attempts `garbage-collect' while the list is live, and
;; publishes its length.  Checkpoint 2 does the same on three workers.  The
;; workers first publish a barrier arrival and join on all three arrivals, so
;; every explicit collection attempt happens while all three private frame
;; stacks hold live freshly-allocated lists.  A worker cannot include itself
;; in its own park request, so these worker-originated collections take the
;; bounded missed-collect fallback; the results remain 5 + 7 + 11 = 23.

;;; Code:

(defmacro nl-thread-alloc-smoke--should-unsupported (form)
  "Require FORM to signal `nelisp-unsupported-primitive'."
  `(let ((nl-thread-alloc-smoke--outcome
          (condition-case nl-thread-alloc-smoke--error
              (progn ,form 'nl-thread-alloc-smoke--no-error)
            (error (car nl-thread-alloc-smoke--error)))))
     (unless (eq nl-thread-alloc-smoke--outcome
                 'nelisp-unsupported-primitive)
       (error "expected nelisp-unsupported-primitive from %S, got %S"
              ',form nl-thread-alloc-smoke--outcome))))

(let ((checked 0)
      (names '(nelisp-thread-shared-alloc
               nelisp-thread-atomic-add
               nelisp-thread-atomic-read
               nelisp-thread-spawn
               nelisp-thread-join
               nelisp-thread-gc-inhibit)))
  (dolist (name names)
    (unless (fboundp name)
      (error "allocating thread primitive is not fboundp: %S" name))
    (setq checked (+ checked 1)))
  (if (and (eq system-type 'gnu/linux)
           (string= system-configuration "x86_64-pc-linux-gnu"))
      (let* ((shared (nelisp-thread-shared-alloc 128))
             (single-result shared)
             (single-done (+ shared 8))
             (result0 (+ shared 16))
             (result1 (+ shared 24))
             (result2 (+ shared 32))
             (barrier (+ shared 40))
             (done (+ shared 48))
             (single-form
              '(let ((xs (list 1 2 3 4 5)))
                 (garbage-collect)
                 (length xs))))
        (when (< shared 0)
          (error "nelisp-thread-shared-alloc failed: %S" shared))

        ;; Checkpoint 1: one ordinary allocating evaluator worker.
        (let ((section-active nil))
          (unwind-protect
              (progn
                (unless (= (nelisp-thread-gc-inhibit 1) 1)
                  (error "single-worker parallel section did not begin"))
                (setq section-active t)
                (let ((tid (nelisp-thread-spawn
                            2 0 single-form single-result single-done)))
                  (unless (> tid 0)
                    (error "allocating worker spawn failed: %S" tid)))
                (unless (= (nelisp-thread-join single-done 1) 1)
                  (error "single allocating worker did not publish"))
                (let ((answer (nelisp-thread-atomic-read single-result)))
                  (unless (= (nelisp-thread-gc-inhibit 0) 1)
                    (error "single-worker section grew the arena"))
                  (setq section-active nil)
                  (unless (= answer 5)
                    (error "single allocating worker returned %S, expected 5"
                           answer))))
            (when section-active
              (nelisp-thread-gc-inhibit 0))))
        (setq checked (+ checked 1))

        ;; Checkpoint 2: the forms are fully built before any clone starts.
        ;; Each worker allocates its XS list, reaches BARRIER, then attempts a
        ;; collection while every worker's private lexical frame owns live
        ;; arena objects.  Join is the only publication edge for RESULT0..2.
        (let* ((form0
                (list 'let '((xs (list 1 2 3 4 5)))
                      (list 'nelisp-thread-atomic-add barrier 1)
                      (list 'nelisp-thread-join barrier 3)
                      '(garbage-collect)
                      '(length xs)))
               (form1
                (list 'let '((xs (list 1 2 3 4 5 6 7)))
                      (list 'nelisp-thread-atomic-add barrier 1)
                      (list 'nelisp-thread-join barrier 3)
                      '(garbage-collect)
                      '(length xs)))
               (form2
                (list 'let '((xs (list 1 2 3 4 5 6 7 8 9 10 11)))
                      (list 'nelisp-thread-atomic-add barrier 1)
                      (list 'nelisp-thread-join barrier 3)
                      '(garbage-collect)
                      '(length xs)))
               (section-active nil))
          (unwind-protect
              (progn
                (unless (= (nelisp-thread-gc-inhibit 1) 1)
                  (error "three-worker parallel section did not begin"))
                (setq section-active t)
                (let ((tid0 (nelisp-thread-spawn 2 0 form0 result0 done))
                      (tid1 (nelisp-thread-spawn 2 0 form1 result1 done))
                      (tid2 (nelisp-thread-spawn 2 0 form2 result2 done)))
                  (unless (and (> tid0 0) (> tid1 0) (> tid2 0))
                    (error "allocating worker spawns failed: %S %S %S"
                           tid0 tid1 tid2)))
                (unless (= (nelisp-thread-join done 3) 3)
                  (error "three allocating workers did not all publish"))
                (let* ((partial0 (nelisp-thread-atomic-read result0))
                       (partial1 (nelisp-thread-atomic-read result1))
                       (partial2 (nelisp-thread-atomic-read result2))
                       (sum (+ partial0 (+ partial1 partial2))))
                  (unless (= (nelisp-thread-gc-inhibit 0) 1)
                    (error "three-worker section grew the arena"))
                  (setq section-active nil)
                  (unless (= sum 23)
                    (error "allocating worker reduction was %S, expected 23 (%S %S %S)"
                           sum partial0 partial1 partial2))))
            (when section-active
              (nelisp-thread-gc-inhibit 0))))
        (setq checked (+ checked 1)))
    ;; Uniform names, loud unsupported behavior, no target-inappropriate
    ;; clone/mmap call on every other standalone target.
    (progn
      (nl-thread-alloc-smoke--should-unsupported
       (nelisp-thread-gc-inhibit 1))
      (setq checked (+ checked 1))
      (nl-thread-alloc-smoke--should-unsupported
       (nelisp-thread-spawn 2 0 nil 0 0))
      (setq checked (+ checked 1))))
  (princ (format "GATE-COUNT checked=%d findings=0\n" checked))
  (princ "nelisp-thread-allocating-standalone-smoke: PASS\n"))

;;; nelisp-thread-allocating-standalone-smoke.el ends here
