;;; vendor-repl-standalone-replay.el --- standalone REPL vendor load replay  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'standalone-source-normalize)

(defvar vendor-repl-standalone-reader nil
  "Path to target/nelisp or a compatible standalone reader binary.")

(defvar vendor-repl-standalone-bootstrap-repl nil
  "Path to the generated nemacs bootstrap REPL input.")

(defvar vendor-repl-standalone-prelude nil
  "Path to the standalone reader stdlib prelude.")

(defvar vendor-repl-standalone-files nil
  "Whitespace-separated string or list of vendor files to load in the REPL.")

(defvar vendor-repl-standalone-proof-form
  "(fboundp (quote emacs-keymap-define-key-after))"
  "Raw Lisp form that must be true after REPL load replay.")

(defvar vendor-repl-standalone-proof-form-file nil
  "Optional file containing the raw Lisp proof form.

When non-nil, this file takes precedence over
`vendor-repl-standalone-proof-form'.  It is intended for exact extracted
vendor forms whose docstrings or backquote syntax are awkward to pass through
shell and Make quoting.")

(defvar vendor-repl-standalone-detail-form "nil"
  "Raw Lisp form returning a diagnostic string when the proof form is false.")

(defvar vendor-repl-standalone-repo-root
  (expand-file-name ".." (file-name-directory
                          (or load-file-name buffer-file-name)))
  "Repository root.")

(defvar vendor-repl-standalone-keep-temp nil
  "When non-nil, keep generated REPL input/output files for diagnostics.")

(defvar vendor-repl-standalone-trace-forms nil
  "When non-nil, record per-form progress in the sentinel marker.")

(defvar vendor-repl-standalone-direct-character-limit 0
  "Minimum normalized source size emitted directly in generated REPL input.

By default every normalized vendor form is emitted directly.  The persistent
REPL replay is an accumulated load/evaluator diagnostic, and current standalone
runtime builds remain sensitive to repeated nested `nelisp--eval-source-string'
reads.  Raise this value when intentionally probing that source-reader path.")

(defvar vendor-repl-standalone-coalesce-file-forms nil
  "When non-nil, replay each vendor file as one `progn' of normalized forms.

This keeps the vendor source intact after host-side normalization while avoiding
per-form persistent REPL overhead for files whose individual top-level forms
have already been reduced.  Form tracing remains per-form and takes precedence
over coalescing.")

(defvar vendor-repl-standalone-internal-timeout-seconds nil
  "Optional timeout, in seconds, enforced inside this Emacs process.

GNU timeout kills this batch process before it can report the last sentinel or
clean up the child REPL.  This internal timeout is intended for diagnostic
runs: when set to a positive number, the child reader is polled from Emacs,
killed on timeout, and the marker/input/output paths are reported.")

(defvar vendor-repl-standalone-keep-temp-on-timeout t
  "When non-nil, preserve generated files after an internal timeout.")

(defconst vendor-repl-standalone--success
  "VENDOR-REPL-STANDALONE=ok"
  "Marker-file sentinel written by a successful REPL replay.")

(defconst vendor-repl-standalone--failure
  "VENDOR-REPL-STANDALONE=fail"
  "Marker-file sentinel written by a failed REPL replay proof.")

(defun vendor-repl-standalone--status-form (prefix)
  "Return a standalone form recording load status with PREFIX.

Keep this free of raw pointer reads.  The persistent REPL diagnostic should
not depend on low-level telemetry primitives, because those are not guaranteed
to be safe in every standalone-reader evaluator path."
  (format "(setq vendor-repl-load-status (concat %S (number-to-string vendor-standalone-load-ok-count)))\n"
          prefix))

(defun vendor-repl-standalone--true-name (file)
  "Return FILE as a canonical absolute path."
  (file-truename (expand-file-name file)))

(defun vendor-repl-standalone--repo-root ()
  "Return the canonical repository root for replay-generated paths."
  (file-name-as-directory
   (vendor-repl-standalone--true-name vendor-repl-standalone-repo-root)))

(defun vendor-repl-standalone--record-load-form (file marker)
  "Return REPL forms that load FILE and record the outcome."
  (let ((name (file-name-nondirectory file))
        (index 0)
        (sources (standalone-source-normalize-file-to-form-strings file)))
    (concat
     (vendor-repl-standalone--status-form
      (concat "start:" name ":count="))
     (format "(nl-write-file %S vendor-repl-load-status)\n" marker)
     (format "(setq load-file-name %S)\n" file)
     (format "(setq buffer-file-name %S)\n" file)
     (if (and vendor-repl-standalone-coalesce-file-forms
              (not vendor-repl-standalone-trace-forms))
         (vendor-repl-standalone--eval-source-form
          (vendor-repl-standalone--coalesced-source sources))
       (mapconcat (lambda (source)
                    (setq index (1+ index))
                    (vendor-repl-standalone--eval-source-form
                     source marker name index))
                  sources
                  ""))
     (vendor-repl-standalone--sync-provided-features-form sources)
     "(setq vendor-standalone-load-ok-count (1+ vendor-standalone-load-ok-count))\n"
     (vendor-repl-standalone--status-form
      (concat "ok:" name ":count="))
     (format "(nl-write-file %S vendor-repl-load-status)\n" marker)
     "")))

(defun vendor-repl-standalone--form-provided-features (form)
  "Return feature symbols provided by FORM."
  (let ((features nil))
    (cond
     ((and (consp form)
           (eq (car form) 'provide)
           (consp (cdr form))
           (consp (cadr form))
           (eq (caadr form) 'quote)
           (symbolp (cadadr form)))
      (push (cadadr form) features))
     ((consp form)
      (setq features
            (append (vendor-repl-standalone--form-provided-features (car form))
                    features))
      (when (consp (cdr form))
        (setq features
              (append (vendor-repl-standalone--form-provided-features (cdr form))
                      features)))))
    features))

(defun vendor-repl-standalone--source-provided-features (source)
  "Return feature symbols provided by normalized SOURCE."
  (condition-case _err
      (vendor-repl-standalone--form-provided-features (read source))
    (error nil)))

(defun vendor-repl-standalone--sync-provided-features-form (sources)
  "Return standalone forms re-providing features found in SOURCES."
  (mapconcat
   (lambda (feature)
     (format "(provide '%S)\n" feature))
   (delete-dups
    (apply #'append
           (mapcar #'vendor-repl-standalone--source-provided-features
                   sources)))
   ""))

(defun vendor-repl-standalone--coalesced-source (sources)
  "Return one pretty-printed `progn' source containing SOURCES."
  (vendor-repl-standalone--pretty-form
   (concat "(progn\n"
           (mapconcat (lambda (source)
                        (concat source
                                (unless (string-suffix-p "\n" source)
                                  "\n")))
                      sources
                      "")
           ")\n")))

(defun vendor-repl-standalone--eval-source-form (source &optional marker file-name index)
  "Return a standalone form that evaluates SOURCE through NeLisp's reader."
  (let* ((print-escape-newlines t)
         (direct-form (and (> (length source)
                              vendor-repl-standalone-direct-character-limit)
                           (concat source
                                   (unless (string-suffix-p "\n" source)
                                     "\n"))))
         (eval-form (or direct-form
                        (format "(nelisp--eval-source-string %s)\n"
                                (prin1-to-string source)))))
    (if (and vendor-repl-standalone-trace-forms
             marker file-name index)
        (concat
         (vendor-repl-standalone--status-form
          (format "form-start:%s:%d:count=" file-name index))
         (format "(nl-write-file %S vendor-repl-load-status)\n" marker)
         eval-form
         (vendor-repl-standalone--status-form
          (format "form-ok:%s:%d:count=" file-name index))
         (format "(nl-write-file %S vendor-repl-load-status)\n" marker))
      eval-form)))

(defun vendor-repl-standalone--files ()
  "Return normalized absolute vendor file list."
  (cond
   ((stringp vendor-repl-standalone-files)
    (mapcar #'vendor-repl-standalone--true-name
            (split-string vendor-repl-standalone-files "[ \t\n]+" t)))
   ((listp vendor-repl-standalone-files)
    (mapcar #'vendor-repl-standalone--true-name vendor-repl-standalone-files))
   (t nil)))

(defun vendor-repl-standalone--load-paths ()
  "Return the load paths needed for standalone REPL vendor replay."
  (let ((root (vendor-repl-standalone--repo-root)))
    (cl-remove-if-not
     #'file-directory-p
     (list (expand-file-name "src" root)
           (expand-file-name "scripts" root)
           (expand-file-name "vendor/compat" root)
           (expand-file-name "vendor/cond-let" root)
           (expand-file-name "vendor/llama" root)
           (expand-file-name "vendor/transient/lisp" root)
           (expand-file-name "vendor/dash.el" root)
           (expand-file-name "vendor/with-editor/lisp" root)
           (expand-file-name "vendor/magit/lisp" root)
           (expand-file-name "vendor/emacs-lisp" root)
           (expand-file-name "vendor/emacs-lisp/emacs-lisp"
                             root)
           (expand-file-name "vendor/emacs-lisp/vc"
                             root)))))

(defun vendor-repl-standalone--read-file (file)
  "Return FILE contents as a string."
  (with-temp-buffer
    (insert-file-contents file)
    (buffer-string)))

(defun vendor-repl-standalone--pretty-form (source)
  "Return SOURCE as a multi-line form string when it is readable."
  (condition-case _err
      (pp-to-string (read source))
    (error source)))

(defun vendor-repl-standalone--proof-form-source ()
  "Return the configured raw proof form source text."
  (if (and vendor-repl-standalone-proof-form-file
           (not (string= vendor-repl-standalone-proof-form-file "")))
      (vendor-repl-standalone--read-file
       vendor-repl-standalone-proof-form-file)
    vendor-repl-standalone-proof-form))

(defun vendor-repl-standalone--read-all-forms (source)
  "Return every top-level Lisp form read from SOURCE, in file order.
Unlike a single `read', this walks the whole string so a
`vendor-repl-standalone-proof-form-file' with more than one top-level form
(for example a helper `defvar' followed by the actual proof expression) is
not silently truncated to its first form."
  (with-temp-buffer
    (insert source)
    (goto-char (point-min))
    (let (forms)
      (while (not (eobp))
        (condition-case _err
            (push (read (current-buffer)) forms)
          (end-of-file (goto-char (point-max)))))
      (nreverse forms))))

(defun vendor-repl-standalone--proof-forms ()
  "Return the configured proof source as a list of top-level forms."
  (vendor-repl-standalone--read-all-forms
   (vendor-repl-standalone--proof-form-source)))

(defun vendor-repl-standalone--detail-form ()
  "Return the configured detail form as a single Lisp expression."
  (car (vendor-repl-standalone--read-all-forms
        vendor-repl-standalone-detail-form)))

(defun vendor-repl-standalone--form-line (form)
  "Return FORM printed as one physical line of standalone-readable source.

The standalone reader's --repl loop reads and evaluates one physical line at
a time with no continuation support, so any embedded newline shreds FORM into
unrelated fragments (see NeLisp Doc 156 section 7).  `prin1-to-string' (unlike
`pp-to-string') never inserts formatting newlines on its own; binding
`print-escape-newlines' also forces any newline that appears *inside* a
printed string literal to come out as the two-character escape \"\\n\" rather
than a literal line break.  The final `replace-regexp-in-string' is a
defensive backstop only: with the bindings above there should be no raw
newline left to strip."
  (let* ((print-escape-newlines t)
         (printed (prin1-to-string form)))
    (replace-regexp-in-string "[\n\r]" " " printed)))

(defun vendor-repl-standalone--proof-scaffold (marker)
  "Return standalone REPL forms that evaluate the proof and update MARKER.

Every emitted form is single-line (see `vendor-repl-standalone--form-line').
This reader's `condition-case'/`ignore-errors' discards the protected body's
return value unconditionally, even when no error is signaled, so the proof
value cannot be captured through it.  Instead: `vendor-repl-proof-value' is
initialized to nil on its own line, then assigned on a separate single-line
`setq' that wraps the proof form in `prog1' together with a companion
`vendor-repl-proof-evaluated' flag.  If the proof form itself signals, that
whole physical line errors out (tolerated by the REPL's per-line error
handling) before either side effect runs, so the value/evaluated pair simply
stays at its nil initializer instead of going stale --- an errored proof and
a cleanly-false proof both leave `vendor-repl-proof-value' nil, but only a
clean evaluation sets `vendor-repl-proof-evaluated' to t, which is reported in
the failure detail text for diagnosis.

When the configured proof source has more than one top-level form (only
reachable through `vendor-repl-standalone-proof-form-file'), every form but
the last is emitted as its own single-line statement for side effects only,
and just the FINAL form's value is captured as the proof value."
  (let* ((forms (vendor-repl-standalone--proof-forms))
         (setup-forms (butlast forms))
         (final-form (car (last forms))))
    (concat
     (format "(nl-write-file %S %S)\n" marker "proof:start")
     "(setq vendor-repl-proof-value nil)\n"
     "(setq vendor-repl-proof-evaluated nil)\n"
     (mapconcat (lambda (form)
                  (concat (vendor-repl-standalone--form-line form) "\n"))
                setup-forms
                "")
     (vendor-repl-standalone--form-line
      (list 'setq 'vendor-repl-proof-value
            (list 'prog1 final-form
                  (list 'setq 'vendor-repl-proof-evaluated t))))
     "\n"
     (vendor-repl-standalone--form-line
      (list 'if 'vendor-repl-proof-value
            (list 'nl-write-file marker vendor-repl-standalone--success)
            (list 'nl-write-file marker
                  (list 'format
                        (concat vendor-repl-standalone--failure
                                " detail=%s evaluated=%s")
                        (vendor-repl-standalone--detail-form)
                        'vendor-repl-proof-evaluated))))
     "\n")))

(defun vendor-repl-standalone--write-input (files marker output)
  "Write standalone-reader REPL input for FILES to OUTPUT."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file output
      (insert ";;; standalone vendor REPL replay probe\n")
      (insert (format "(setq nelisp-emacs-vendor-root %S)\n"
                      (expand-file-name "vendor"
                                        (vendor-repl-standalone--repo-root))))
      (insert (format "(setq load-path '%S)\n"
                      (vendor-repl-standalone--load-paths)))
      (when vendor-repl-standalone-prelude
        (dolist (source (standalone-source-normalize-file-to-form-strings
                         vendor-repl-standalone-prelude))
          (insert (vendor-repl-standalone--eval-source-form source))))
      (insert (vendor-repl-standalone--read-file
               vendor-repl-standalone-bootstrap-repl))
      (unless (bolp)
        (insert "\n"))
      (insert (format "(setq vendor-standalone-load-file-count %d)\n"
                      (length files)))
      (insert "(setq vendor-standalone-load-ok-count 0)\n")
      (insert "(setq vendor-repl-load-status \"\")\n")
      (dolist (file files)
        (insert (vendor-repl-standalone--record-load-form file marker)))
      (insert (format "(setq vendor-repl-standalone-marker-file %S)\n"
                      marker))
      (insert (vendor-repl-standalone--proof-scaffold marker))
      (insert ",quit\n"))))

(defun vendor-repl-standalone--call-reader-sync (tmp out)
  "Run the standalone reader on TMP, writing combined output to OUT."
  (list
   (call-process
    "/bin/sh" nil (list out t) nil
    "-c" "exec \"$1\" --repl --no-prompt --no-print < \"$2\""
    "vendor-repl-standalone"
    vendor-repl-standalone-reader
    tmp)
   nil))

(defun vendor-repl-standalone--call-reader-with-timeout (tmp out timeout)
  "Run the standalone reader on TMP, preserving progress if TIMEOUT expires."
  (let* ((buffer (generate-new-buffer " *vendor-repl-standalone*"))
         (process (make-process
                   :name "vendor-repl-standalone"
                   :buffer buffer
                   :command
                   (list "/bin/sh" "-c"
                         "exec \"$1\" --repl --no-prompt --no-print < \"$2\" 2>&1"
                         "vendor-repl-standalone"
                         vendor-repl-standalone-reader
                         tmp)
                   :connection-type 'pipe
                   :noquery t))
         (deadline (+ (float-time) timeout))
         exit timed-out)
    (unwind-protect
        (progn
          (while (and (process-live-p process)
                      (< (float-time) deadline))
            (accept-process-output process 0.1))
          (when (process-live-p process)
            (setq timed-out t)
            (kill-process process)
            (accept-process-output process 1.0))
          (setq exit (if timed-out 124 (process-exit-status process)))
          (with-current-buffer buffer
            (write-region (point-min) (point-max) out nil 'silent))
          (list exit timed-out))
      (when (process-live-p process)
        (kill-process process))
      (kill-buffer buffer))))

(defun vendor-repl-standalone--call-reader (tmp out)
  "Run the configured standalone reader on TMP, writing output to OUT."
  (if (and (numberp vendor-repl-standalone-internal-timeout-seconds)
           (> vendor-repl-standalone-internal-timeout-seconds 0))
      (vendor-repl-standalone--call-reader-with-timeout
       tmp out vendor-repl-standalone-internal-timeout-seconds)
    (vendor-repl-standalone--call-reader-sync tmp out)))

(defun vendor-repl-standalone--run (files)
  "Run standalone reader REPL on generated input for FILES."
  (let ((tmp (make-temp-file "nemacs-vendor-repl-standalone-" nil ".repl"))
        (out (make-temp-file "nemacs-vendor-repl-standalone-" nil ".out"))
        (marker (make-temp-file "nemacs-vendor-repl-standalone-" nil ".sentinel"))
        (start (float-time))
        exit elapsed output sentinel timed-out)
    (delete-file marker)
    (unwind-protect
        (progn
          (vendor-repl-standalone--write-input files marker tmp)
          (with-temp-file marker
            (insert "reader:start"))
          (pcase-let ((`(,reader-exit ,reader-timed-out)
                       (vendor-repl-standalone--call-reader tmp out)))
            (setq exit reader-exit)
            (setq timed-out reader-timed-out))
          (setq elapsed (- (float-time) start))
          (setq output (vendor-repl-standalone--read-file out))
          (setq sentinel (and (file-exists-p marker)
                              (vendor-repl-standalone--read-file marker)))
          (list exit elapsed output sentinel tmp out marker timed-out))
      (unless (or vendor-repl-standalone-keep-temp
                  (and timed-out
                       vendor-repl-standalone-keep-temp-on-timeout))
        (dolist (file (list tmp out marker))
          (when (file-exists-p file)
            (delete-file file)))))))

(defun vendor-repl-standalone-batch ()
  "Load vendor files through a persistent standalone-reader REPL."
  (unless (and vendor-repl-standalone-reader
               (file-executable-p vendor-repl-standalone-reader))
    (error "vendor-repl-standalone-reader is not executable: %S"
           vendor-repl-standalone-reader))
  (unless (and vendor-repl-standalone-bootstrap-repl
               (file-readable-p vendor-repl-standalone-bootstrap-repl))
    (error "vendor-repl-standalone-bootstrap-repl is not readable: %S"
           vendor-repl-standalone-bootstrap-repl))
  (when (and vendor-repl-standalone-prelude
             (not (file-readable-p vendor-repl-standalone-prelude)))
    (error "vendor-repl-standalone-prelude is not readable: %S"
           vendor-repl-standalone-prelude))
  (let ((files (vendor-repl-standalone--files)))
    (unless files
      (error "vendor-repl-standalone-files is empty"))
    (dolist (file files)
      (unless (file-readable-p file)
        (error "vendor REPL load file is not readable: %S" file)))
    (princ (format "vendor-repl-standalone files=%S proof=%s detail=%s status=start\n"
                   files vendor-repl-standalone-proof-form
                   vendor-repl-standalone-detail-form))
    (pcase-let ((`(,exit ,elapsed ,output ,sentinel ,tmp ,out ,marker ,timed-out)
                 (vendor-repl-standalone--run files)))
      (if (and (numberp exit)
               (= exit 0)
               (equal sentinel vendor-repl-standalone--success))
          (princ (format "vendor-repl-standalone files=%S proof=%s detail=%s status=done elapsed=%S exit=%S\n"
                         files vendor-repl-standalone-proof-form
                         vendor-repl-standalone-detail-form elapsed exit))
        (princ (format "vendor-repl-standalone files=%S proof=%s detail=%s status=fail elapsed=%S exit=%S timed-out=%S sentinel=%S expected-sentinel=%S input=%S output=%S marker=%S\n"
                       files vendor-repl-standalone-proof-form
                       vendor-repl-standalone-detail-form elapsed exit
                       timed-out
                       sentinel
                       vendor-repl-standalone--success
                       tmp out marker))
        (princ output)
        (unless (string-suffix-p "\n" output)
          (princ "\n"))
        (kill-emacs 1)))))

(provide 'vendor-repl-standalone-replay)

;;; vendor-repl-standalone-replay.el ends here
