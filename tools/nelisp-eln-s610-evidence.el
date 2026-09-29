;;; nelisp-eln-s610-evidence.el --- S10.3 end-to-end evidence -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ledger criterion S10.3 asks for the genuine byte-compile-form artifact,
;; on the binary, to pass the S6.10 measurement and the handler scenarios
;; (host-identical transcripts, forced handler mode, tampered slots, file
;; mutants).  Those stages take about four minutes together, far more than the
;; meter's 55 s cap, so (exactly like S6.22, tools/nelisp-eln-s6-corpus.el):
;;
;;   - `nelisp-eln-s610-batch-regenerate' (make eln-s610-evidence) runs every
;;     stage as its own process and writes an evidence file: per stage the
;;     command digest, exit code, status, the result lines, timestamps; at top
;;     level the binary sha256, the pinned .eln sha256, and the digest of every
;;     harness/test/source file the stages depend on.
;;   - `nelisp-eln-s610-batch-validate' (the S10.3 cmd) checks that evidence
;;     against the current binary, artifact and sources, requires every stage
;;     row to be a real PASS, and then runs the S6.22 corpus validator live
;;     (which now includes the byte-compile-form row).  Stale, missing, FAIL or
;;     tampered evidence fails.
;;
;; The stages themselves are test/nelisp-eln-s610-smoke.sh {scenarios,forced,
;; tamper,mutation} and the S6.10 ledger cmd (read from the ledger).
;; test/nelisp-eln-s610-e2e.sh runs the same stages directly, in one go.
;;
;; Environment: ELN_PROGRESS_BIN (binary under test), ELN_S10_EVIDENCE
;; (default target/progress/s610-e2e-evidence.json), ELN_S6_EVIDENCE (the
;; S6.22 evidence, default target/progress/s6-corpus-evidence.json),
;; ELN_S10_TIMEOUT (seconds per stage, default 900).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(let ((corpus (expand-file-name
               "nelisp-eln-s6-corpus.el"
               (file-name-directory
                (or load-file-name buffer-file-name default-directory)))))
  (load corpus nil t))

(defconst nelisp-eln-s610-schema "nelisp-eln-s610-e2e-evidence-v1")

(defconst nelisp-eln-s610-eln-sha256
  "7109fe7ea0c4cd7b8b4cdaf20ce3e9cf3deea16f71361fcdba7c4f6c8889730d"
  "sha256 of the pinned genuine gnu-byte-compile-form.eln.")

(defconst nelisp-eln-s610-root nelisp-eln-s6-corpus--root)

(defconst nelisp-eln-s610-stages
  '(("s610-measure" nil
     ("\\`S6_MEASURE_RESULT function=byte-compile-form status=PASS "))
    ("scenarios" "scenarios"
     ("\\`NELISP-ELN-S610-SCENARIOS-PASS pushes=0 landings=0\\'"
      "\\`transcript lines: [1-9][0-9]* identical\\'"))
    ("forced" "forced"
     ("\\`NELISP-ELN-S610-FORCED-PASS pushes=5 landings=4\\'"
      "\\`transcript lines: [1-9][0-9]* identical\\'"
      "\\`S10_RED_CONTROL_DIFFERS=PASS\\'"))
    ("tamper" "tamper"
     ("\\`NELISP-ELN-S610-TAMPER-PASS\\'"))
    ("mutation" "mutation"
     ("\\`NELISP-ELN-S610-MUTATION-PASS mutants=[1-9][0-9]+\\'")))
  "(STAGE SMOKE-MODE REQUIRED-RESULT-REGEXPS).
SMOKE-MODE nil means the S6.10 cmd of the ledger.")

(defconst nelisp-eln-s610-harness-files
  '("test/nelisp-eln-s610-smoke.sh" "test/nelisp-eln-s610-driver.el"
    "test/nelisp-eln-s610-scenarios.el" "test/nelisp-eln-s610-host.el"
    "test/nelisp-eln-s610-e2e.sh" "test/lib/nelisp-boot-args.sh"
    "test/fixtures/s6-corpus/byte-compile-form.wrapper.el"
    "tools/nelisp-eln-s610-evidence.el")
  "Files whose content decides what the stages verify (besides lisp/).")

(defun nelisp-eln-s610--source-digest (root)
  "Digest of the harness files, lisp/nelisp-eln-*.el and the nl-ffi sources."
  (secure-hash
   'sha256
   (mapconcat
    (lambda (rel)
      (let ((file (expand-file-name rel root)))
        (format "%s=%s" rel
                (if (file-readable-p file)
                    (nelisp-eln-s6-corpus--file-sha256 file)
                  "missing"))))
    (append nelisp-eln-s610-harness-files
            (mapcar (lambda (f) (file-relative-name f root))
                    (sort (append
                           (directory-files (expand-file-name "lisp" root) t
                                            "\\`nelisp-eln-.*\\.el\\'")
                           (directory-files (expand-file-name "packages/nl-ffi/src" root)
                                            t "\\.el\\'"))
                          #'string<)))
    "\n")))

(defun nelisp-eln-s610--eln-file ()
  (nelisp-eln-s6-corpus--env
   "NELISP_S10_ELN"
   (expand-file-name
    "~/.cache/tmp/s6-survey-lex/byte-compile-form/overlay/eln/31.1-ba35c031/gnu-byte-compile-form.eln")))

(defun nelisp-eln-s610--stage-cmd (stage ledger)
  "The shell command of STAGE (a `nelisp-eln-s610-stages' entry)."
  (if (nth 1 stage)
      (format "sh test/nelisp-eln-s610-smoke.sh %s" (nth 1 stage))
    (nth 2 (assq 'byte-compile-form (nelisp-eln-s6-corpus-ledger-cmds ledger)))))

(defun nelisp-eln-s610--cmd-digest (cmd root)
  (nelisp-eln-s6-corpus--cmd-digest cmd root))

(defun nelisp-eln-s610--result-lines (output)
  (cl-remove-if-not
   (lambda (l) (string-match-p "\\`\\(S6_MEASURE_RESULT\\|NELISP-ELN-S610\\|S10_\\|transcript lines\\)" l))
   (split-string output "\n" t)))

(defun nelisp-eln-s610--stage-problems (stage lines)
  "Problem strings for STAGE given its recorded result LINES."
  (let (problems)
    (dolist (re (nth 2 stage))
      (unless (cl-some (lambda (l) (string-match-p re l)) lines)
        (push (format "%s: no result line matches %s" (car stage) re) problems)))
    (when (null (nth 1 stage))
      (let* ((line (cl-find-if (lambda (l) (string-prefix-p "S6_MEASURE_RESULT " l)) lines))
             (pairs (and line (nelisp-eln-s6-corpus--parse-result-line line)))
             (raw (nelisp-eln-s6-corpus--int pairs "native_raw_calls"))
             (disp (nelisp-eln-s6-corpus--int pairs "native_dispatch_calls")))
        (unless (and raw disp (> raw 0) (> disp 0))
          (push "s610-measure: raw/dispatch native calls are not both > 0" problems))))
    (nreverse problems)))

;;;; Regeneration

(defun nelisp-eln-s610--run-stage (stage cmd root binary timeout)
  (let* ((default-directory root)
         (process-environment (append (list (concat "ELN_PROGRESS_BIN=" binary)
                                            (concat "NELISP_BIN=" binary))
                                      process-environment))
         (started (float-time))
         exit output)
    (message "s610: running %s" (car stage))
    (with-temp-buffer
      (setq exit (call-process "timeout" nil t nil (number-to-string timeout)
                               "sh" "-c" cmd))
      (setq output (buffer-string)))
    (let* ((finished (float-time))
           (lines (nelisp-eln-s610--result-lines output))
           (problems (nelisp-eln-s610--stage-problems stage lines))
           (status (if (and (eql exit 0) (null problems)) "PASS" "FAIL")))
      `((stage . ,(car stage))
        (cmd . ,cmd)
        (cmd_digest . ,(nelisp-eln-s610--cmd-digest cmd root))
        (exit_code . ,exit)
        (status . ,status)
        (reason . ,(if (equal status "PASS") :null
                     (string-join
                      (cons (format "exit %s" exit)
                            (append problems
                                    (list (string-join
                                           (last (split-string output "\n" t) 6) " | "))))
                      "; ")))
        (result_lines . ,(vconcat lines))
        (started_at . ,(nelisp-eln-s6-corpus--iso started))
        (finished_at . ,(nelisp-eln-s6-corpus--iso finished))
        (duration_s . ,(round (- finished started)))))))

(defun nelisp-eln-s610-regenerate (evidence-file binary &optional ledger root timeout)
  "Run every S10.3 stage with BINARY, write EVIDENCE-FILE, return the rows."
  (let* ((root (or root nelisp-eln-s610-root))
         (ledger (or ledger (expand-file-name "tools/ai/eln-progress.org" root)))
         (timeout (or timeout 900))
         (eln (nelisp-eln-s610--eln-file))
         (rows (mapcar
                (lambda (stage)
                  (nelisp-eln-s610--run-stage
                   stage (or (nelisp-eln-s610--stage-cmd stage ledger)
                             (error "no command for stage %s" (car stage)))
                   root binary timeout))
                nelisp-eln-s610-stages))
         (evidence
          `((schema . ,nelisp-eln-s610-schema)
            (generated_at . ,(nelisp-eln-s6-corpus--iso (float-time)))
            (binary_path . ,binary)
            (binary_sha256 . ,(nelisp-eln-s6-corpus--file-sha256 binary))
            (eln_sha256 . ,(if (file-readable-p eln)
                               (nelisp-eln-s6-corpus--file-sha256 eln) "missing"))
            (source_sha256 . ,(nelisp-eln-s610--source-digest root))
            (stages . ,(vconcat rows)))))
    (make-directory (file-name-directory (expand-file-name evidence-file)) t)
    (with-temp-file evidence-file
      (insert (json-serialize evidence :null-object :null :false-object :json-false)
              "\n"))
    rows))

;;;; Validation

(defun nelisp-eln-s610-validate (evidence-file binary &optional ledger root s6-evidence)
  "Return a list of problem strings; nil means fresh, all stages PASS, S6.22 valid."
  (let* ((root (or root nelisp-eln-s610-root))
         (ledger (or ledger (expand-file-name "tools/ai/eln-progress.org" root)))
         problems evidence)
    (cl-flet ((problem (fmt &rest args) (push (apply #'format fmt args) problems)))
      (cond
       ((not (file-readable-p evidence-file))
        (problem "evidence file missing: %s (regenerate with make eln-s610-evidence)"
                 evidence-file))
       ((not (and binary (file-executable-p binary)))
        (problem "binary not executable: %s" binary))
       (t
        (setq evidence (condition-case err
                           (nelisp-eln-s6-corpus-read-evidence evidence-file)
                         (error (problem "evidence unreadable: %S" err) nil)))
        (when evidence
          (unless (equal (alist-get 'schema evidence) nelisp-eln-s610-schema)
            (problem "unknown evidence schema %S" (alist-get 'schema evidence)))
          (unless (equal (alist-get 'binary_sha256 evidence)
                         (nelisp-eln-s6-corpus--file-sha256 binary))
            (problem "stale: binary sha256 differs from evidence (evidence for %s)"
                     (alist-get 'binary_path evidence)))
          (let ((eln (nelisp-eln-s610--eln-file)))
            (unless (and (file-readable-p eln)
                         (equal (nelisp-eln-s6-corpus--file-sha256 eln)
                                nelisp-eln-s610-eln-sha256)
                         (equal (alist-get 'eln_sha256 evidence)
                                nelisp-eln-s610-eln-sha256))
              (problem "stale: the pinned .eln is missing, changed, or not what the evidence used")))
          (unless (equal (alist-get 'source_sha256 evidence)
                         (nelisp-eln-s610--source-digest root))
            (problem "stale: harness or lisp/ sources changed since evidence"))
          (let ((rows (alist-get 'stages evidence)))
            (dolist (stage nelisp-eln-s610-stages)
              (let* ((name (car stage))
                     (matches (cl-remove-if-not
                               (lambda (r) (equal (alist-get 'stage r) name)) rows))
                     (row (car matches))
                     (cmd (nelisp-eln-s610--stage-cmd stage ledger)))
                (cond
                 ((null matches) (problem "%s: no evidence row" name))
                 ((cdr matches) (problem "%s: duplicate evidence rows" name))
                 (t
                  (unless (equal (alist-get 'status row) "PASS")
                    (problem "%s: status %s (%s)" name (alist-get 'status row)
                             (alist-get 'reason row)))
                  (unless (eql (alist-get 'exit_code row) 0)
                    (problem "%s: exit code %s" name (alist-get 'exit_code row)))
                  (dolist (p (nelisp-eln-s610--stage-problems
                              stage (append (alist-get 'result_lines row) nil)))
                    (problem "%s" p))
                  (cond
                   ((null cmd) (problem "%s: no command (ledger S6.10 pending?)" name))
                   ((not (equal (nelisp-eln-s610--cmd-digest cmd root)
                                (alist-get 'cmd_digest row)))
                    (problem "%s: stale: command or its input files changed" name)))))))
            (dolist (r rows)
              (unless (assoc (alist-get 'stage r) nelisp-eln-s610-stages)
                (problem "unexpected evidence row: %s" (alist-get 'stage r)))))
          ;; The corpus half: S6.22 must validate now, live.
          (dolist (p (nelisp-eln-s6-corpus-validate
                      (or s6-evidence
                          (nelisp-eln-s6-corpus--env
                           "ELN_S6_EVIDENCE"
                           (expand-file-name "target/progress/s6-corpus-evidence.json" root)))
                      binary ledger root))
            (problem "S6.22: %s" p))))))
    (nreverse problems)))

;;;; Batch entry points

(defun nelisp-eln-s610--config ()
  (list (nelisp-eln-s6-corpus--env
         "ELN_S10_EVIDENCE"
         (expand-file-name "target/progress/s610-e2e-evidence.json" nelisp-eln-s610-root))
        (nelisp-eln-s6-corpus--env "ELN_PROGRESS_BIN" (getenv "NELISP_BIN"))
        (nelisp-eln-s6-corpus--env
         "ELN_S6_LEDGER"
         (expand-file-name "tools/ai/eln-progress.org" nelisp-eln-s610-root))))

(defun nelisp-eln-s610-batch-regenerate ()
  "Regenerate the S10.3 evidence; exit 0 only if every stage PASSes."
  (pcase-let ((`(,evidence ,binary ,ledger) (nelisp-eln-s610--config)))
    (unless (and binary (file-executable-p binary))
      (message "ELN_PROGRESS_BIN is not an executable binary: %s" binary)
      (kill-emacs 2))
    (let* ((rows (nelisp-eln-s610-regenerate
                  evidence binary ledger nil
                  (string-to-number (nelisp-eln-s6-corpus--env "ELN_S10_TIMEOUT" "900"))))
           (pass (cl-count "PASS" rows :key (lambda (r) (alist-get 'status r)) :test #'equal)))
      (dolist (r rows)
        (message "%-14s %-5s %ss %s" (alist-get 'stage r) (alist-get 'status r)
                 (alist-get 'duration_s r)
                 (let ((reason (alist-get 'reason r))) (if (stringp reason) reason ""))))
      (message "evidence: %s -- %d/%d PASS" evidence pass (length rows))
      (kill-emacs (if (= pass (length rows)) 0 1)))))

(defun nelisp-eln-s610-batch-validate ()
  "Validate the S10.3 evidence; exit 0 only if fresh, all stages PASS, S6.22 valid."
  (pcase-let ((`(,evidence ,binary ,ledger) (nelisp-eln-s610--config)))
    (let ((problems (nelisp-eln-s610-validate evidence binary ledger)))
      (if problems
          (progn
            (message "S10.3 FAIL: %d problem(s) against %s" (length problems) evidence)
            (dolist (p problems) (message "  - %s" p))
            (kill-emacs 1))
        (message "S10.3 PASS: %d fresh PASS stages + S6.22 19/19 (%s)"
                 (length nelisp-eln-s610-stages) evidence)
        (kill-emacs 0)))))

(provide 'nelisp-eln-s610-evidence)
;;; nelisp-eln-s610-evidence.el ends here
