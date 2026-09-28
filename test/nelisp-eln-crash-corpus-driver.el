;;; nelisp-eln-crash-corpus-driver.el --- S7.7 slice 4 corpus-wide crash gate driver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; One process, one job, selected via NELISP_ELN_CRASH_CORPUS_JOB. Companion
;; to tools/nelisp-eln-crash-corpus-gate.sh, which spawns one of these
;; processes per (artifact, check) pair -- never more than one artifact's
;; real .eln content per process -- so that no artifact's admission state
;; (or a crash) can leak into another artifact's result. Unlike
;; test/nelisp-eln-crash-general-driver.el (S7.7 slice 1), which exercises
;; `nelisp-eln-registration--containment-boundary' through mocked Lisp-level
;; non-local exits on a single self-emitted fixture, this driver attempts a
;; genuine `load' of whatever real (or deliberately corrupted) .eln artifact
;; the caller names, and never assumes admission or rejection in advance:
;; both are acceptable outcomes for an uncorrupted artifact, only rejection
;; is acceptable for a corrupted one, and neither ever excuses a crash --
;; a hardware trap (illegal instruction, segfault) unwinds this process
;; before any Lisp form here can run, so that half of the contract is
;; verified by the caller inspecting this process's exit code, not by
;; anything printed here.
;;
;;   load (default) -- attempt (load ARTIFACT) once through the standard
;;                     after-load wrapper (already installed by the caller
;;                     via --load before this driver, generated the same
;;                     way as test/nelisp-eln-same-artifact-smoke.sh's
;;                     normal_load_wrapper), then assert the general
;;                     crash-containment reporter
;;                     (`nelisp-eln-registration-crash-boundary-report')
;;                     shows no inconsistency and that the owner/pending
;;                     counts match what admission or rejection implies.
;;                     Required: NELISP_ELN_CRASH_CORPUS_ARTIFACT,
;;                     NELISP_ELN_CRASH_CORPUS_LABEL. Optional:
;;                     NELISP_ELN_CRASH_CORPUS_EXPECT -- `any' (default,
;;                     for a genuine uncorrupted artifact: admitted or
;;                     rejected are both acceptable) or `reject' (for a
;;                     deliberately corrupted copy: admission itself is a
;;                     FAIL, not only an inconsistency or a crash would be).
;;
;;   negative-control -- no real load. Directly injects the same kind of
;;                     bookkeeping inconsistency as
;;                     test/nelisp-eln-crash-general-driver.el's
;;                     `inconsistent-registry' scenario (a failed-cleanup
;;                     pending entry whose owner was never rooted), then
;;                     either asserts the reporter catches it
;;                     (NELISP_ELN_CRASH_CORPUS_SKIP_REPORTER unset or "0",
;;                     the sound gate) or deliberately skips that assertion
;;                     (NELISP_ELN_CRASH_CORPUS_SKIP_REPORTER=1, the unsound
;;                     gate variant under test) to demonstrate what a gate
;;                     that never calls the reporter would miss. This job
;;                     never touches a real artifact and never crashes by
;;                     construction; the caller runs it twice (skip=0 then
;;                     skip=1) and checks the exit codes come out different.
;;
;; Every job path ends in an explicit `kill-emacs' with a 0 (pass) or 1
;; (fail, but not a crash) argument -- never an uncaught `error' -- so the
;; caller's exit-code test distinguishes exactly three outcomes: 0 (pass),
;; 1 (this driver detected and reported a problem), or anything else
;; (a signal or an abort killed the process outright; the caller treats
;; this as a crash, named by the artifact/label it was checking).

(require 'nelisp-eln-registration)
(require 'nelisp-eln-registration-objects)

(defun nelisp-eln-crash-corpus--report-line
    (label status owners pending inconsistencies detail)
  (format "CORPUS %s status:%s owners:%d pending:%d inconsistencies:%d detail:%s\n"
          label status owners pending inconsistencies (or detail "none")))

(defun nelisp-eln-crash-corpus--run-load ()
  "Attempt one `load' of NELISP_ELN_CRASH_CORPUS_ARTIFACT and assert consistency."
  (let* ((path (getenv "NELISP_ELN_CRASH_CORPUS_ARTIFACT"))
         (label (or (getenv "NELISP_ELN_CRASH_CORPUS_LABEL") "artifact"))
         (expect (or (getenv "NELISP_ELN_CRASH_CORPUS_EXPECT") "any"))
         (load-error nil))
    (unless (and (stringp path) (file-readable-p path))
      (princ (format "CORPUS %s status:missing-artifact\n" label))
      (princ (format "CORPUS_RESULT %s=FAIL\n" label))
      (kill-emacs 1))
    (condition-case err
        (load path)
      ;; `t', not `error': a rejection may also arrive as a `quit' signal
      ;; from deep inside a native call; either way it is an ordinary
      ;; non-local exit this process survives, which is exactly what is
      ;; under test here, distinct from a hardware trap this process does
      ;; not survive.
      (t (setq load-error err)))
    (let* ((report (nelisp-eln-registration-crash-boundary-report))
           (owners (plist-get report :owners))
           (pending (plist-get report :pending-cleanups))
           (problems (plist-get report :inconsistencies))
           (admitted (not load-error))
           (status (if admitted "admitted" "rejected"))
           (ok t))
      (princ (nelisp-eln-crash-corpus--report-line
              label status owners pending (length problems)
              (if load-error (format "%S" (car load-error)) nil)))
      (when problems
        (setq ok nil))
      (if admitted
          (unless (= owners 1)
            (setq ok nil))
        (unless (and (= owners 0) (= pending 0))
          (setq ok nil)))
      (when (and (equal expect "reject") admitted)
        ;; A corrupted copy that still gets admitted is not a crash, but it
        ;; is exactly the "is rejected" half of the contract failing, and
        ;; the per-artifact table must say so rather than silently pass.
        (setq ok nil))
      (princ (format "CORPUS_RESULT %s=%s\n" label (if ok "PASS" "FAIL")))
      (kill-emacs (if ok 0 1)))))

(defun nelisp-eln-crash-corpus--inject-inconsistency ()
  "Mirror test/nelisp-eln-crash-general-driver.el's `inconsistent-registry'.
Roots a well-formed FAKE-OWNER normally, and references a second,
never-rooted LOST-OWNER only from a failed-cleanup pending entry -- the
exact inconsistency `nelisp-eln-registration-crash-boundary-report' is
documented to flag as `lost-failed-cleanup-owner'."
  (let ((fake-owner (make-vector nelisp-eln-registration--owner-size nil))
        (lost-owner (make-vector nelisp-eln-registration--owner-size nil)))
    (aset fake-owner 0 nelisp-eln-registration--owner-marker)
    (aset fake-owner 3 'corpus-fake-name)
    (aset lost-owner 0 nelisp-eln-registration--owner-marker)
    (aset lost-owner 3 'corpus-lost-name)
    (setq nelisp-eln-registration--owners (list fake-owner))
    (setq nelisp-eln-registration--pending-cleanups
          (list (list :phase 'failed-cleanup :owner lost-owner)))))

(defun nelisp-eln-crash-corpus--run-negative-control ()
  "Show that skipping the reporter assertion misses an injected inconsistency."
  (let ((skip (equal (getenv "NELISP_ELN_CRASH_CORPUS_SKIP_REPORTER") "1")))
    (nelisp-eln-crash-corpus--inject-inconsistency)
    (let* ((report (nelisp-eln-registration-crash-boundary-report))
           (n (length (plist-get report :inconsistencies))))
      (princ (format "CORPUS_NEGATIVE_CONTROL skip-reporter:%s inconsistencies:%d\n"
                      (if skip "1" "0") n))
      (if skip
          ;; The unsound gate variant: it injected the same inconsistency
          ;; but never looks at `report', so it reports success regardless.
          (progn
            (princ "CORPUS_RESULT negative-control-skip=PASS-BUT-UNSOUND\n")
            (kill-emacs 0))
        ;; The sound gate: it must find exactly the injected problem.
        (if (> n 0)
            (progn
              (princ "CORPUS_RESULT negative-control-assert=DETECTED\n")
              (kill-emacs 1))
          (progn
            (princ "CORPUS_RESULT negative-control-assert=MISSED-BUG\n")
            (kill-emacs 0)))))))

(let ((job (or (getenv "NELISP_ELN_CRASH_CORPUS_JOB") "load")))
  (cond
   ((equal job "load") (nelisp-eln-crash-corpus--run-load))
   ((equal job "negative-control") (nelisp-eln-crash-corpus--run-negative-control))
   (t
    (princ (format "CORPUS unknown-job:%S\n" job))
    (kill-emacs 2))))

;;; nelisp-eln-crash-corpus-driver.el ends here
