;;; nelisp-eln-crash-corpus-batch-driver.el --- S7.7.4 batched safe-check driver -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;; Companion to test/nelisp-eln-crash-corpus-driver.el and
;; tools/nelisp-eln-crash-corpus-gate.sh. That driver is one process, one
;; (artifact, check) pair, by design: any check whose corrupted bytes
;; could still reach `nl-ffi--dlopen' (corrupt-text, corrupt-abi -- see the
;; gate script's own header commentary and the S7.7.4 investigation notes)
;; must stay isolated so a native crash can never take out more than the
;; one check that caused it, and never leaks admission state into another
;; artifact's result.
;;
;; This driver instead handles MANY checks in one process, but only the
;; two kinds proven safe by construction:
;;
;;   normal          -- a genuine, uncorrupted artifact. `load' either
;;                       admits it or `nelisp-eln-registration--validate-preopen'
;;                       rejects it before `nl-ffi--dlopen' ever runs; a
;;                       genuine, previously-surveyed (S6) artifact is
;;                       never expected to crash this process on a real
;;                       load -- that is what distinguishes it from a
;;                       deliberately corrupted copy in the first place.
;;
;;   corrupt-truncate -- cut to 60% of the original size. This is caught
;;                       by `nelisp-eln-system-loader--file-symbols''s own
;;                       ELF section/symbol-table parsing, which runs
;;                       BEFORE `nelisp-eln-registration--validate-preopen'
;;                       and before `nl-ffi--dlopen' -- section/symbol
;;                       tables sit at the end of a GNU-toolchain .eln, so
;;                       truncating to 60% always destroys them first.
;;                       Confirmed empirically for this corpus: dlopen was
;;                       traced as never called for corrupt-truncate on
;;                       every artifact probed (smallest/largest/self-
;;                       emitted/GNU arithmetic fixture), unlike
;;                       corrupt-text and corrupt-abi, each of which was
;;                       traced actually reaching `nl-ffi--dlopen' on at
;;                       least one real corpus artifact. See the S7.7.4
;;                       handoff note for the trace probe and its output.
;;
;; corrupt-text and corrupt-abi remain one-process-per-check, dispatched
;; by test/nelisp-eln-crash-corpus-driver.el exactly as before.
;;
;; Unlike the single-check driver, `nelisp-eln-registration--owners' and
;; `nelisp-eln-registration-objects--live-units' are NOT expected to come
;; back to zero between checks in this process: a successfully admitted
;; artifact's owner is, by the Doc 206 P5 contract documented on
;; `nelisp-eln-registration--containment-boundary', kept rooted for this
;; process's whole lifetime and never torn down after the fact -- there is
;; no unload/unregister API, and recovery from a bad admission requires a
;; new process, not a rollback within this one. Each of this corpus's
;; artifacts registers a distinct target function name (a different
;; source function per artifact), so one admitted owner never blocks a
;; later, different artifact's own admission in the same process; if two
;; ever did collide, `nelisp-eln-registration--load-1' would simply reject
;; the second under `registration-name-already-bound', which is already
;; an acceptable outcome for a `normal'/`any' check (see below) -- no
;; special-casing needed here for that.
;;
;; What DOES need checking freshly per line, instead of once at process
;; exit, is that each check's OWN admission/rejection left exactly the
;; residue expected of IT alone: `nelisp-eln-registration-crash-boundary-report'
;; is read once before and once after each `load' attempt, and only the
;; DELTA between those two snapshots is asserted (a fresh owner lease, not
;; the cumulative total) -- see `nelisp-eln-crash-corpus-batch--run-one'.
;; `:inconsistencies' is checked absolutely (never non-nil) after every
;; single check, not just as a delta, since a malformed/duplicate/orphaned
;; owner from any earlier check in this process would keep showing up in
;; every later report too; that is deliberate belt-and-braces, catching a
;; residue left by an earlier check as soon as the very next check looks.
;;
;; Every result line this driver decides is written straight to its own
;; "$label.$check_kind.result" file (RUN_DIR, from the environment) the
;; instant it is decided -- not buffered until this whole process exits --
;; so that if this process is killed outright partway through its chunk
;; (a bug here, resource exhaustion, anything), every check already
;; decided keeps its real, already-written result, and the gate script's
;; existing step 7c ("missing result file (a queued check never wrote
;; it)") already reports every UNdecided check in the same chunk as FAIL,
;; never silently drops it. No new plumbing was needed there: this
;; driver's incremental per-line writes are the entire mechanism.
;;
;; NELISP_ELN_CRASH_CORPUS_BATCH_CRASH_AFTER (test-only, unset by default):
;; when set to a 0-based line index, this process calls `(kill-emacs 139)'
;; -- a non-0/1/124 exit the gate script's own `$worker'-style case
;; statement already classifies as CRASH -- immediately before processing
;; that line, to validate the "a crash inside a batched worker must be
;; reported as FAIL" requirement without needing an actual native fault.
;; Lines before the injection point have already been decided and written
;; for real by that point.

(require 'nelisp-eln-registration)
(require 'nelisp-eln-registration-objects)

(defun nelisp-eln-crash-corpus-batch--split-tsv-line (line)
  "Split LINE on tabs into a list of fields."
  (split-string line "\t"))

(defun nelisp-eln-crash-corpus-batch--write-result (run-dir label check-kind status rc)
  "Write one \"label<TAB>check_kind<TAB>status<TAB>rc\" result file, flushed now.
Same path and format `emit_row'/`$worker' use in
tools/nelisp-eln-crash-corpus-gate.sh, so step 7c's replay of $order
cannot tell this result apart from one an isolated process would have
produced."
  (let ((path (expand-file-name (format "%s.%s.result" label check-kind) run-dir)))
    (with-temp-buffer
      (insert (format "%s\t%s\t%s\t%s\n" label check-kind status rc))
      (write-region (point-min) (point-max) path nil 'silent))))

(defun nelisp-eln-crash-corpus-batch--run-one (label check-kind artifact expect)
  "Attempt one `load' of ARTIFACT, asserting only the residue THIS check
left, by snapshotting `nelisp-eln-registration-crash-boundary-report'
before and after -- see the file-level commentary above for why a delta,
not an absolute count, is the right invariant in a process that may
already hold other checks' rooted owners. Returns (STATUS . RC), the
same vocabulary emit_row/$worker use."
  (let* ((before (nelisp-eln-registration-crash-boundary-report))
         (owners-before (plist-get before :owners))
         (pending-before (plist-get before :pending-cleanups))
         (load-error nil))
    (condition-case err
        (load artifact)
      (t (setq load-error err)))
    (let* ((after (nelisp-eln-registration-crash-boundary-report))
           (owners-after (plist-get after :owners))
           (pending-after (plist-get after :pending-cleanups))
           (problems (plist-get after :inconsistencies))
           (admitted (not load-error))
           (owners-delta (- owners-after owners-before))
           (pending-delta (- pending-after pending-before))
           (ok t))
      (when problems
        (setq ok nil))
      (if admitted
          (unless (= owners-delta 1)
            (setq ok nil))
        (unless (and (= owners-delta 0) (= pending-delta 0))
          (setq ok nil)))
      (when (and (equal expect "reject") admitted)
        ;; corrupt-truncate must never be admitted; an admitted corrupted
        ;; copy is a FAIL even though nothing here looks inconsistent.
        (setq ok nil))
      (princ (format "CORPUS %s.%s status:%s owners-delta:%d pending-delta:%d inconsistencies:%d detail:%s\n"
                      label check-kind (if admitted "admitted" "rejected")
                      owners-delta pending-delta (length problems)
                      (if load-error (format "%S" (car load-error)) "none")))
      (cons (if ok "PASS" "FAIL") (if ok 0 1)))))

(defun nelisp-eln-crash-corpus-batch--run ()
  (let* ((run-dir (or (getenv "RUN_DIR") (error "RUN_DIR unset")))
         (chunk-file (or (getenv "NELISP_ELN_CRASH_CORPUS_BATCH_FILE")
                          (error "NELISP_ELN_CRASH_CORPUS_BATCH_FILE unset")))
         (crash-after (getenv "NELISP_ELN_CRASH_CORPUS_BATCH_CRASH_AFTER"))
         (crash-after-n (and crash-after (string-to-number crash-after)))
         (lines
          (with-temp-buffer
            (insert-file-contents chunk-file)
            (split-string (buffer-string) "\n" t)))
         (idx 0)
         (any-fail nil))
    (dolist (line lines)
      (when (and crash-after-n (= idx crash-after-n))
        (princ (format "CORPUS batch-crash-inject idx:%d\n" idx))
        (kill-emacs 139))
      (let ((fields (nelisp-eln-crash-corpus-batch--split-tsv-line line)))
        (if (/= (length fields) 4)
            (progn
              (princ (format "CORPUS malformed-chunk-line idx:%d line:%S\n" idx line))
              (setq any-fail t))
          (let* ((label (nth 0 fields))
                 (check-kind (nth 1 fields))
                 (artifact (nth 2 fields))
                 (expect (nth 3 fields))
                 (result
                  (condition-case err
                      (nelisp-eln-crash-corpus-batch--run-one
                       label check-kind artifact expect)
                    ;; A bug in this driver's own bookkeeping (not the
                    ;; `load' attempt, already caught inside `--run-one')
                    ;; must not silently lose the rest of the chunk: report
                    ;; THIS check as FAIL and keep going, same principle as
                    ;; every other "never hide a failure" path in this
                    ;; file.
                    (t (princ (format "CORPUS %s.%s driver-error:%S\n"
                                       label check-kind err))
                       (cons "FAIL" "driver-error")))))
            (nelisp-eln-crash-corpus-batch--write-result
             run-dir label check-kind (car result) (cdr result))
            (unless (equal (car result) "PASS") (setq any-fail t)))))
      (setq idx (1+ idx)))
    (kill-emacs (if any-fail 1 0))))

(nelisp-eln-crash-corpus-batch--run)

;;; nelisp-eln-crash-corpus-batch-driver.el ends here
