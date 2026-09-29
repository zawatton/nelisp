;;; nelisp-eln-s6-corpus.el --- Corpus-wide S6 measurement evidence -*- lexical-binding: t; -*-

;; Copyright (C) 2026
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Ledger criterion S6.22 (tools/ai/eln-progress.org) asks that host/VM/native
;; execution parity and timing were really measured for the fixed-19 vendor
;; corpus.  Each function has its own S6.N criterion whose `cmd:' line runs
;; test/nelisp-eln-s6-measure.sh (20-50 s).  Nineteen of them cannot run
;; inside the meter's per-check cap, so:
;;
;;   - `nelisp-eln-s6-corpus-batch-regenerate' runs every function's own
;;     ledger cmd (taken from the ledger, never duplicated here) and writes an
;;     evidence file: per function the S6_MEASURE_RESULT line, status, the
;;     .eln sha256, the digest of the ledger cmd and its input files, the
;;     timestamp; at top level the binary sha256 and the harness digest.
;;   - `nelisp-eln-s6-corpus-batch-validate' (the S6.22 cmd) checks that
;;     evidence against the current binary, .eln files and ledger cmds and
;;     exits non-zero unless all 19 rows are fresh real PASS measurements.
;;
;; Environment: ELN_PROGRESS_BIN (binary under test), ELN_S6_EVIDENCE
;; (default target/progress/s6-corpus-evidence.json), ELN_S6_LEDGER (default
;; tools/ai/eln-progress.org), ELN_S6_JOBS (default 1: parallel runs disturb
;; the timings), ELN_S6_TIMEOUT (seconds per function, default 900).

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)

(defconst nelisp-eln-s6-corpus-schema "nelisp-eln-s6-corpus-evidence-v1")

(defconst nelisp-eln-s6-corpus-functions
  '(macroexp--all-forms macroexpand-1 macroexp-parse-body
    cconv-closure-convert cconv--convert-function cconv--set-diff
    byte-compile-lambda byte-compile-form byte-compile-make-closure
    byte-compile-if byte-compile-setq byte-compile-funcall
    byte-compile-constant zerop caar cadr fixnump bignump
    frame-configuration-p)
  "The fixed-19 vendor corpus, in ledger order (S6.3 .. S6.21).")

(defconst nelisp-eln-s6-corpus--root
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory)))))

(defun nelisp-eln-s6-corpus--env (name default)
  (let ((value (getenv name)))
    (if (and value (not (string-empty-p value))) value default)))

(defun nelisp-eln-s6-corpus--file-sha256 (file)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (secure-hash 'sha256 (current-buffer))))

(defun nelisp-eln-s6-corpus--iso (epoch)
  (format-time-string "%Y-%m-%dT%H:%M:%SZ" (seconds-to-time epoch) t))

;;;; Ledger parsing

(defun nelisp-eln-s6-corpus-ledger-cmds (ledger)
  "Return ((FUNCTION ID CMD-or-nil) ...) for every S6 per-function criterion.
CMD is nil when the criterion is still `pending' (or has no cmd line)."
  (let (rows)
    (with-temp-buffer
      (insert-file-contents ledger)
      (goto-char (point-min))
      (while (re-search-forward
              "^\\*\\* \\(S6\\.[0-9]+\\) Host/VM/JIT equality.* for `\\([^`]+\\)`"
              nil t)
        (let ((id (match-string 1))
              (fn (intern (match-string 2)))
              (end (save-excursion
                     (if (re-search-forward "^\\*+ " nil t)
                         (match-beginning 0)
                       (point-max))))
              cmd)
          (save-excursion
            (when (re-search-forward "^cmd: \\(.*\\)$" end t)
              (setq cmd (match-string 1))))
          (push (list fn id cmd) rows))))
    (nreverse rows)))

(defun nelisp-eln-s6-corpus--cmd-paths (cmd)
  "Return the files a measure CMD names via --eln/--source/--corpus/--wrapper."
  (let (paths)
    (dolist (flag '("--eln" "--source" "--corpus" "--wrapper"))
      (when (string-match
             (concat (regexp-quote flag) " +\\(?:\"\\([^\"]*\\)\"\\|\\([^ ]+\\)\\)")
             cmd)
        (push (cons flag (or (match-string 1 cmd) (match-string 2 cmd))) paths)))
    (nreverse paths)))

(defun nelisp-eln-s6-corpus--resolve (path root)
  (expand-file-name (replace-regexp-in-string
                     "\\$HOME\\|\\${HOME}" (getenv "HOME") path t t)
                    root))

(defun nelisp-eln-s6-corpus--eln-file (cmd root)
  (let ((entry (assoc "--eln" (nelisp-eln-s6-corpus--cmd-paths cmd))))
    (and entry (nelisp-eln-s6-corpus--resolve (cdr entry) root))))

(defun nelisp-eln-s6-corpus--cmd-digest (cmd root)
  "Digest of CMD text plus the content of every input file it names.
Missing input files contribute the marker \"missing\", so a vanished corpus
changes the digest."
  (secure-hash
   'sha256
   (concat
    cmd "\n"
    (mapconcat
    (lambda (entry)
      (let ((file (nelisp-eln-s6-corpus--resolve (cdr entry) root)))
        (format "%s=%s"
                (car entry)
                (if (file-readable-p file)
                    (nelisp-eln-s6-corpus--file-sha256 file)
                  "missing"))))
    (nelisp-eln-s6-corpus--cmd-paths cmd)
    "\n"))))

(defun nelisp-eln-s6-corpus--harness-digest (root)
  (secure-hash
   'sha256
   (mapconcat
    (lambda (rel)
      (let ((file (expand-file-name rel root)))
        (if (file-readable-p file) (nelisp-eln-s6-corpus--file-sha256 file) "missing")))
    '("test/nelisp-eln-s6-measure.sh" "test/nelisp-eln-s6-measure-driver.el")
    "\n")))

;;;; Running measurements

(defun nelisp-eln-s6-corpus--parse-result-line (line)
  "Parse an S6_MEASURE_RESULT LINE into an alist of (KEY . VALUE-STRING)."
  (let (pairs)
    (dolist (token (cdr (split-string line " ")))
      (if (string-match "\\`\\([a-z_0-9]+\\)=\\(.*\\)\\'" token)
          (push (cons (match-string 1 token) (match-string 2 token)) pairs)
        (when pairs
          (setcdr (car pairs) (concat (cdar pairs) " " token)))))
    (nreverse pairs)))

(defun nelisp-eln-s6-corpus--int (pairs key)
  (let ((value (cdr (assoc key pairs))))
    (and value (string-match-p "\\`[0-9]+\\'" value) (string-to-number value))))

(defun nelisp-eln-s6-corpus--row (fn id cmd root exit-code output started finished)
  "Build the evidence row for FN from a finished measurement."
  (let* ((line (car (last (cl-remove-if-not
                           (lambda (l) (string-prefix-p "S6_MEASURE_RESULT " l))
                           (split-string output "\n")))))
         (pairs (and line (nelisp-eln-s6-corpus--parse-result-line line)))
         (eln (and cmd (nelisp-eln-s6-corpus--eln-file cmd root)))
         (result-status (cdr (assoc "status" pairs)))
         (reason (cdr (assoc "reason" pairs)))
         (status
          (cond ((null cmd) "NOT_MEASURED")
                ((and (eq exit-code 0) line
                      (equal (cdr (assoc "function" pairs)) (symbol-name fn))
                      (equal result-status "PASS"))
                 "PASS")
                (t "FAIL")))
         (raw (nelisp-eln-s6-corpus--int pairs "native_raw_calls"))
         (dispatch (nelisp-eln-s6-corpus--int pairs "native_dispatch_calls")))
    (when (and (equal status "PASS")
               (not (and raw dispatch (> (+ raw dispatch) 0))))
      (setq status "FAIL" reason "no_native_calls_recorded"))
    `((function . ,(symbol-name fn))
      (ledger_id . ,id)
      (status . ,status)
      (reason . ,(cond ((null cmd) "ledger criterion is pending: no measurement command")
                       ((equal status "PASS") :null)
                       (t (or reason
                              (if line (format "measure_exit_%s" exit-code)
                                (format "no_result_line_exit_%s" exit-code))))))
      (exit_code . ,(or exit-code :null))
      (result_line . ,(or line :null))
      (eln_file . ,(or eln :null))
      (eln_sha256 . ,(if (and eln (file-readable-p eln))
                         (nelisp-eln-s6-corpus--file-sha256 eln)
                       :null))
      (cmd_digest . ,(if cmd (nelisp-eln-s6-corpus--cmd-digest cmd root) :null))
      (equality_host_vm
       . ,(cond ((not (member status '("PASS" "FAIL"))) :null)
                ((and reason (string-match-p "mismatch_host_vm" reason)) :json-false)
                ((equal status "PASS") t)
                (t :null)))
      (equality_host_native
       . ,(cond ((not (member status '("PASS" "FAIL"))) :null)
                ((and reason (string-match-p "mismatch_host_native" reason)) :json-false)
                ((equal status "PASS") t)
                (t :null)))
      (corpus_n . ,(or (nelisp-eln-s6-corpus--int pairs "corpus_n") :null))
      (host_ns_per_call . ,(or (nelisp-eln-s6-corpus--int pairs "host_ns_per_call") :null))
      (vm_ns_per_call . ,(or (nelisp-eln-s6-corpus--int pairs "vm_ns_per_call") :null))
      (native_ns_per_call . ,(or (nelisp-eln-s6-corpus--int pairs "native_ns_per_call") :null))
      (native_raw_calls . ,(or raw :null))
      (native_dispatch_calls . ,(or dispatch :null))
      (started_at . ,(if started (nelisp-eln-s6-corpus--iso started) :null))
      (finished_at . ,(nelisp-eln-s6-corpus--iso finished))
      (duration_s . ,(if started (round (- finished started)) 0)))))

(defun nelisp-eln-s6-corpus--run-all (jobs root binary timeout)
  "Run every S6 ledger cmd with at most JOBS in parallel.
Return an alist FUNCTION -> (EXIT-CODE OUTPUT STARTED FINISHED)."
  (let* ((ledger-rows (nelisp-eln-s6-corpus-ledger-cmds
                       (nelisp-eln-s6-corpus--env
                        "ELN_S6_LEDGER"
                        (expand-file-name "tools/ai/eln-progress.org" root))))
         (queue (cl-loop for fn in nelisp-eln-s6-corpus-functions
                         for row = (assq fn ledger-rows)
                         when (and row (nth 2 row)) collect row))
         (running nil)
         (results nil)
         (process-environment (cons (concat "ELN_PROGRESS_BIN=" binary)
                                    process-environment)))
    (while (or queue running)
      (while (and queue (< (length running) jobs))
        (let* ((row (pop queue))
               (fn (nth 0 row))
               (buffer (generate-new-buffer (format " *s6-%s*" fn)))
               (default-directory root)
               (proc (make-process
                      :name (format "s6-%s" fn) :buffer buffer :noquery t
                      :command (list "timeout" (number-to-string timeout)
                                     "sh" "-c" (nth 2 row))
                      :connection-type 'pipe)))
          (message "s6-corpus: started %s" fn)
          (push (list proc fn (float-time)) running)))
      (accept-process-output nil 0.2)
      (dolist (entry (copy-sequence running))
        (let ((proc (nth 0 entry)))
          (unless (process-live-p proc)
            (setq running (delq entry running))
            (let ((buffer (process-buffer proc)))
              (push (list (nth 1 entry) (process-exit-status proc)
                          (with-current-buffer buffer (buffer-string))
                          (nth 2 entry) (float-time))
                    results)
              (message "s6-corpus: finished %s exit=%s" (nth 1 entry)
                       (process-exit-status proc))
              (kill-buffer buffer))))))
    results))

(defun nelisp-eln-s6-corpus-regenerate (evidence-file binary &optional ledger root jobs timeout)
  "Run all fixed-19 measurements with BINARY and write EVIDENCE-FILE.
Return the list of evidence rows."
  (let* ((root (or root nelisp-eln-s6-corpus--root))
         (ledger (or ledger (expand-file-name "tools/ai/eln-progress.org" root)))
         (jobs (or jobs 1))
         (timeout (or timeout 900))
         (process-environment process-environment)
         (ledger-rows (nelisp-eln-s6-corpus-ledger-cmds ledger))
         (results (progn (setenv "ELN_S6_LEDGER" ledger)
                         (nelisp-eln-s6-corpus--run-all jobs root binary timeout)))
         (now (float-time))
         (rows
          (mapcar
           (lambda (fn)
             (let* ((ledger-row (assq fn ledger-rows))
                    (res (assq fn results)))
               (unless ledger-row
                 (error "Ledger %s has no S6 criterion for %s" ledger fn))
               (nelisp-eln-s6-corpus--row
                fn (nth 1 ledger-row) (nth 2 ledger-row) root
                (nth 1 res) (or (nth 2 res) "") (nth 3 res)
                (or (nth 4 res) now))))
           nelisp-eln-s6-corpus-functions))
         (evidence
          `((schema . ,nelisp-eln-s6-corpus-schema)
            (generated_at . ,(nelisp-eln-s6-corpus--iso now))
            (binary_path . ,binary)
            (binary_sha256 . ,(nelisp-eln-s6-corpus--file-sha256 binary))
            (binary_size . ,(file-attribute-size (file-attributes binary)))
            (harness_sha256 . ,(nelisp-eln-s6-corpus--harness-digest root))
            (functions . ,(vconcat rows)))))
    (make-directory (file-name-directory (expand-file-name evidence-file)) t)
    (with-temp-file evidence-file
      (insert (json-serialize evidence :null-object :null :false-object :json-false)
              "\n"))
    rows))

;;;; Validation

(defun nelisp-eln-s6-corpus-read-evidence (evidence-file)
  (with-temp-buffer
    (insert-file-contents evidence-file)
    (json-parse-string (buffer-string) :object-type 'alist
                       :array-type 'list :null-object :null
                       :false-object :json-false)))

(defun nelisp-eln-s6-corpus-validate (evidence-file binary &optional ledger root)
  "Return a list of problem strings; nil means fully fresh and all 19 PASS."
  (let* ((root (or root nelisp-eln-s6-corpus--root))
         (ledger (or ledger (expand-file-name "tools/ai/eln-progress.org" root)))
         problems evidence)
    (cl-flet ((problem (fmt &rest args) (push (apply #'format fmt args) problems)))
      (cond
       ((not (file-readable-p evidence-file))
        (problem "evidence file missing: %s (regenerate with make eln-s6-corpus-evidence)"
                 evidence-file))
       ((not (and binary (file-executable-p binary)))
        (problem "binary not executable: %s" binary))
       (t
        (setq evidence (condition-case err
                           (nelisp-eln-s6-corpus-read-evidence evidence-file)
                         (error (problem "evidence unreadable: %S" err) nil)))
        (when evidence
          (unless (equal (alist-get 'schema evidence) nelisp-eln-s6-corpus-schema)
            (problem "unknown evidence schema %S" (alist-get 'schema evidence)))
          (unless (equal (alist-get 'binary_sha256 evidence)
                         (nelisp-eln-s6-corpus--file-sha256 binary))
            (problem "stale: binary sha256 differs from evidence (evidence for %s)"
                     (alist-get 'binary_path evidence)))
          (unless (equal (alist-get 'harness_sha256 evidence)
                         (nelisp-eln-s6-corpus--harness-digest root))
            (problem "stale: measurement harness changed since evidence"))
          (let ((rows (alist-get 'functions evidence))
                (ledger-rows (nelisp-eln-s6-corpus-ledger-cmds ledger)))
            (dolist (fn nelisp-eln-s6-corpus-functions)
              (let* ((name (symbol-name fn))
                     (matches (cl-remove-if-not
                               (lambda (r) (equal (alist-get 'function r) name)) rows))
                     (row (car matches))
                     (ledger-row (assq fn ledger-rows))
                     (cmd (nth 2 ledger-row)))
                (cond
                 ((null matches) (problem "%s: no evidence row" name))
                 ((cdr matches) (problem "%s: duplicate evidence rows" name))
                 (t
                  (unless (equal (alist-get 'status row) "PASS")
                    (problem "%s: status %s (%s)" name (alist-get 'status row)
                             (alist-get 'reason row)))
                  (let* ((line (alist-get 'result_line row))
                         (pairs (and (stringp line)
                                     (nelisp-eln-s6-corpus--parse-result-line line)))
                         (eln (and cmd (nelisp-eln-s6-corpus--eln-file cmd root))))
                    (when (equal (alist-get 'status row) "PASS")
                      (unless (and pairs
                                   (equal (cdr (assoc "function" pairs)) name)
                                   (equal (cdr (assoc "status" pairs)) "PASS"))
                        (problem "%s: PASS row has no matching PASS result line" name))
                      (let ((raw (nelisp-eln-s6-corpus--int pairs "native_raw_calls"))
                            (disp (nelisp-eln-s6-corpus--int pairs "native_dispatch_calls")))
                        (unless (and raw disp (> (+ raw disp) 0))
                          (problem "%s: no native calls in result line" name)))
                      (unless (equal (cdr (assoc "eln_sha256" pairs))
                                     (alist-get 'eln_sha256 row))
                        (problem "%s: row eln sha256 differs from its result line" name)))
                    (cond
                     ((null cmd)
                      (problem "%s: ledger criterion %s has no cmd (pending)"
                               name (nth 1 ledger-row)))
                     ((not (and eln (file-readable-p eln)))
                      (problem "%s: .eln missing: %s" name eln))
                     ((not (equal (nelisp-eln-s6-corpus--file-sha256 eln)
                                  (alist-get 'eln_sha256 row)))
                      (problem "%s: stale/tampered: .eln sha256 differs from evidence" name))
                     ((not (equal (nelisp-eln-s6-corpus--cmd-digest cmd root)
                                  (alist-get 'cmd_digest row)))
                      (problem "%s: stale: ledger cmd or its input files changed" name))))))))
            (dolist (r rows)
              (unless (memq (intern-soft (alist-get 'function r))
                            nelisp-eln-s6-corpus-functions)
                (problem "unexpected evidence row: %s" (alist-get 'function r)))))))))
    (nreverse problems)))

;;;; Batch entry points

(defun nelisp-eln-s6-corpus--config ()
  (list (nelisp-eln-s6-corpus--env
         "ELN_S6_EVIDENCE"
         (expand-file-name "target/progress/s6-corpus-evidence.json"
                           nelisp-eln-s6-corpus--root))
        (nelisp-eln-s6-corpus--env "ELN_PROGRESS_BIN" (getenv "NELISP_BIN"))
        (nelisp-eln-s6-corpus--env
         "ELN_S6_LEDGER"
         (expand-file-name "tools/ai/eln-progress.org" nelisp-eln-s6-corpus--root))))

(defun nelisp-eln-s6-corpus-batch-regenerate ()
  "Regenerate the evidence file; exit 0 only if all 19 rows are PASS."
  (pcase-let ((`(,evidence ,binary ,ledger) (nelisp-eln-s6-corpus--config)))
    (unless (and binary (file-executable-p binary))
      (message "ELN_PROGRESS_BIN is not an executable binary: %s" binary)
      (kill-emacs 2))
    (let* ((rows (nelisp-eln-s6-corpus-regenerate
                  evidence binary ledger nil
                  (string-to-number (nelisp-eln-s6-corpus--env "ELN_S6_JOBS" "1"))
                  (string-to-number (nelisp-eln-s6-corpus--env "ELN_S6_TIMEOUT" "900"))))
           (pass (cl-count "PASS" rows :key (lambda (r) (alist-get 'status r)) :test #'equal)))
      (dolist (r rows)
        (message "%-28s %-12s %s" (alist-get 'function r) (alist-get 'status r)
                 (let ((reason (alist-get 'reason r))) (if (stringp reason) reason ""))))
      (message "evidence: %s -- %d/%d PASS" evidence pass (length rows))
      (kill-emacs (if (= pass (length rows)) 0 1)))))

(defun nelisp-eln-s6-corpus-batch-validate ()
  "Validate the evidence file; exit 0 only if fresh and all 19 PASS."
  (pcase-let ((`(,evidence ,binary ,ledger) (nelisp-eln-s6-corpus--config)))
    (let ((problems (nelisp-eln-s6-corpus-validate evidence binary ledger)))
      (if problems
          (progn
            (message "S6.22 FAIL: %d problem(s) against %s" (length problems) evidence)
            (dolist (p problems) (message "  - %s" p))
            (kill-emacs 1))
        (message "S6.22 PASS: 19/19 fresh PASS measurements (%s)" evidence)
        (kill-emacs 0)))))

(provide 'nelisp-eln-s6-corpus)
;;; nelisp-eln-s6-corpus.el ends here
