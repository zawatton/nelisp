;;; nelisp-thread-percpu-roots-smoke.el --- Doc 199 Tier-3b root enumeration -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Acceptance proof for Doc 199 Tier 3b's park barrier: the marker-side worker
;; registry enumerates every allocating worker's private root reserve while an
;; actual mark/sweep runs.  This file is loaded by target/nelisp itself, using the same
;; fixed worker ID 2 and load-by-path shape as
;; tools/nelisp-thread-allocating-standalone-smoke.el.
;;
;; Three forms are fully built before clone(2).  Each worker allocates a live
;; list in a private lexical frame, publishes its barrier arrival, and parks
;; until the parent releases it.  While all three are parked, the parent
;; requires diagnostic tuple element 16 to report exactly three registered
;; workers and drives an explicit `garbage-collect'.  The collector requests a
;; park, waits for all three join-spin safepoints, runs mark/sweep with their
;; published roots, then resumes them.  Diagnostics must report last-parked=3,
;; missed-collects unchanged, and successful-parked-collects incremented by
;; one.  After release, the workers must return 5, 7, and 11, and section exit
;; must clear the registry back to zero.
;;
;; The fixed public diagnostic exposes the registry count, but not the raw
;; entry addresses.  Consequently the intended per-entry fallback assertion
;; cannot be issued directly from standalone Lisp:
;;
;;   for i = 0..2: atomic-read(nl_thread_registry + 16 + i*16 + 8) > 0
;;
;; Reaching the barrier does exercise the private reserve/release publication
;; path while every live `let' frame exists, but a future public diagnostic for
;; entry root_top values is required to observe those three words separately.

;;; Code:

(defmacro nl-thread-roots-smoke--should-unsupported (form)
  "Require FORM to signal `nelisp-unsupported-primitive'."
  `(let ((nl-thread-roots-smoke--outcome
          (condition-case nl-thread-roots-smoke--error
              (progn ,form 'nl-thread-roots-smoke--no-error)
            (error (car nl-thread-roots-smoke--error)))))
     (unless (eq nl-thread-roots-smoke--outcome
                 'nelisp-unsupported-primitive)
       (error "expected nelisp-unsupported-primitive from %S, got %S"
              ',form nl-thread-roots-smoke--outcome))))

(defun nl-thread-roots-smoke--registry-count ()
  "Return the Doc 199 Tier 3b registered-worker diagnostic."
  (nth 16 (nelisp--debug-switch 0)))

(defun nl-thread-roots-smoke--current-parked ()
  "Return the number of workers still inside the current park request."
  (nth 17 (nelisp--debug-switch 0)))

(defun nl-thread-roots-smoke--last-parked ()
  "Return the satisfied worker count from the last park request."
  (nth 18 (nelisp--debug-switch 0)))

(defun nl-thread-roots-smoke--missed-collects ()
  "Return the bounded-timeout/no-collect diagnostic count."
  (nth 19 (nelisp--debug-switch 0)))

(defun nl-thread-roots-smoke--parked-collects ()
  "Return the number of mark/sweeps completed through the park path."
  (nth 20 (nelisp--debug-switch 0)))

(let ((checked 0)
      (names '(nelisp-thread-shared-alloc
               nelisp-thread-atomic-add
               nelisp-thread-atomic-read
               nelisp-thread-spawn
               nelisp-thread-join
               nelisp-thread-gc-inhibit)))
  (dolist (name names)
    (unless (fboundp name)
      (error "per-CPU root smoke primitive is not fboundp: %S" name))
    (setq checked (+ checked 1)))
  (unless (fboundp 'nelisp--debug-switch)
    (error "per-CPU root diagnostic is not fboundp: nelisp--debug-switch"))
  (setq checked (+ checked 1))
  (if (and (eq system-type 'gnu/linux)
           (string= system-configuration "x86_64-pc-linux-gnu"))
      (let* ((shared (nelisp-thread-shared-alloc 128))
             (result0 shared)
             (result1 (+ shared 8))
             (result2 (+ shared 16))
             (arrived (+ shared 24))
             (release (+ shared 32))
             (done (+ shared 40))
             ;; Build every form before any worker starts.  The second
             ;; counter is a release latch, not the arrival counter, so the
             ;; parent owns the only transition that lets the workers leave
             ;; their live-list barrier.
             (form0
              (list 'let '((xs (list 1 2 3 4 5)))
                    (list 'nelisp-thread-atomic-add arrived 1)
                    (list 'nelisp-thread-join release 1)
                    '(length xs)))
             (form1
              (list 'let '((xs (list 1 2 3 4 5 6 7)))
                    (list 'nelisp-thread-atomic-add arrived 1)
                    (list 'nelisp-thread-join release 1)
                    '(length xs)))
             (form2
              (list 'let '((xs (list 1 2 3 4 5 6 7 8 9 10 11)))
                    (list 'nelisp-thread-atomic-add arrived 1)
                    (list 'nelisp-thread-join release 1)
                    '(length xs))))
        (when (< shared 0)
          (error "nelisp-thread-shared-alloc failed: %S" shared))
        (unless (and (= (nelisp-thread-atomic-read arrived) 0)
                     (= (nelisp-thread-atomic-read release) 0)
                     (= (nelisp-thread-atomic-read done) 0))
          (error "fresh per-CPU root smoke counters were not zero"))
        (setq checked (+ checked 1))

        (let ((section-active nil)
              (workers-released nil))
          (unwind-protect
              (progn
                (unless (= (nelisp-thread-gc-inhibit 1) 1)
                  (error "per-CPU root parallel section did not begin"))
                (setq section-active t)
                (setq checked (+ checked 1))

                (let ((tid0 (nelisp-thread-spawn 2 0 form0 result0 done))
                      (tid1 (nelisp-thread-spawn 2 0 form1 result1 done))
                      (tid2 (nelisp-thread-spawn 2 0 form2 result2 done)))
                  (unless (and (> tid0 0) (> tid1 0) (> tid2 0))
                    (error "per-CPU root worker spawns failed: %S %S %S"
                           tid0 tid1 tid2)))
                (unless (= (nelisp-thread-join arrived 3) 3)
                  (error "per-CPU root workers did not all reach the barrier"))
                (setq checked (+ checked 1))

                ;; All three workers wait in the join spin with live private
                ;; `let' frames.  The main-thread collection must park all
                ;; three, complete one real mark/sweep, and resume them.  A
                ;; timeout is a safe skipped collect, but it is a gate failure
                ;; here because this workload is deliberately parkable.
                (let ((before (nl-thread-roots-smoke--registry-count))
                      (missed-before
                       (nl-thread-roots-smoke--missed-collects))
                      (collects-before
                       (nl-thread-roots-smoke--parked-collects)))
                  (unless (and (integerp before) (= before 3))
                    (error "parked worker registry count was %S, expected 3"
                           before))
                  (garbage-collect)
                  (let ((after (nl-thread-roots-smoke--registry-count))
                        (current (nl-thread-roots-smoke--current-parked))
                        (parked (nl-thread-roots-smoke--last-parked))
                        (missed (nl-thread-roots-smoke--missed-collects))
                        (collects (nl-thread-roots-smoke--parked-collects)))
                    (unless (and (integerp after) (= after 3))
                      (error
                       "worker registry count after collect was %S, expected 3"
                       after))
                    (unless (and (integerp parked) (= parked 3))
                      (error "park barrier satisfied %S workers, expected 3"
                             parked))
                    (unless (and (integerp missed)
                                 (= missed missed-before))
                      (error
                       "parkable collect changed missed count %S -> %S"
                       missed-before missed))
                    (unless (and (integerp collects)
                                 (= collects (+ collects-before 1)))
                      (error
                       "parked collect count was %S -> %S, expected +1"
                       collects-before collects))
                    (princ
                     (format
                      (concat "PARK-DIAG parked=%d current=%d "
                              "missed=%d collections=%d\n")
                      parked current missed collects))))
                (setq checked (+ checked 1))

                (unless (= (nelisp-thread-atomic-add release 1) 0)
                  (error "per-CPU root release latch was not initially zero"))
                (setq workers-released t)
                (unless (= (nelisp-thread-join done 3) 3)
                  (error "per-CPU root workers did not all publish"))
                (let* ((partial0 (nelisp-thread-atomic-read result0))
                       (partial1 (nelisp-thread-atomic-read result1))
                       (partial2 (nelisp-thread-atomic-read result2))
                       (sum (+ partial0 (+ partial1 partial2))))
                  (unless (and (= partial0 5)
                               (= partial1 7)
                               (= partial2 11)
                               (= sum 23))
                    (error (concat "per-CPU root reduction was %S, expected 23 "
                                   "(%S %S %S, expected 5 7 11)")
                           sum partial0 partial1 partial2)))
                (setq checked (+ checked 1))

                (unless (= (nelisp-thread-gc-inhibit 0) 1)
                  (error "per-CPU root section grew the arena"))
                (setq section-active nil)
                (let ((after-clear
                       (nl-thread-roots-smoke--registry-count)))
                  (unless (and (integerp after-clear) (= after-clear 0))
                    (error "worker registry did not clear at section exit: %S"
                           after-clear)))
                (setq checked (+ checked 1)))
            (unless workers-released
              (nelisp-thread-atomic-add release 1))
            (when section-active
              (nelisp-thread-gc-inhibit 0)))))
    ;; Uniform surface on every other standalone target: all six thread
    ;; primitives remain fboundp (checked above), and every call fails loudly
    ;; before a target-inappropriate clone/mmap operation can run.
    (progn
      (nl-thread-roots-smoke--should-unsupported
       (nelisp-thread-shared-alloc 128))
      (setq checked (+ checked 1))
      (nl-thread-roots-smoke--should-unsupported
       (nelisp-thread-atomic-add 0 1))
      (setq checked (+ checked 1))
      (nl-thread-roots-smoke--should-unsupported
       (nelisp-thread-atomic-read 0))
      (setq checked (+ checked 1))
      (nl-thread-roots-smoke--should-unsupported
       (nelisp-thread-spawn 2 0 nil 0 0))
      (setq checked (+ checked 1))
      (nl-thread-roots-smoke--should-unsupported
       (nelisp-thread-join 0 1))
      (setq checked (+ checked 1))
      (nl-thread-roots-smoke--should-unsupported
       (nelisp-thread-gc-inhibit 1))
      (setq checked (+ checked 1))))
  (princ (format "GATE-COUNT checked=%d findings=0\n" checked))
  (princ "nelisp-thread-percpu-roots-smoke: PASS\n"))

;;; nelisp-thread-percpu-roots-smoke.el ends here
