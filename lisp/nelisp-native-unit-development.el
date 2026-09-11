;;; nelisp-native-unit-development.el --- rebuild user native functions in the REPL -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-native-unit)

(defconst nelisp-native-unit-development-timeout 120
  "Default seconds allowed for a bounded compiler subprocess before it is
killed as a timeout. Used by both
`nelisp-native-unit-development-run-bounded' and, through it,
`nelisp-native-unit-rebuild-and-reload'.")

(defconst nelisp-native-unit-development--timeout-min 1
  "Lower bound accepted for an explicit TIMEOUT argument, in seconds.")

(defconst nelisp-native-unit-development--timeout-max 600
  "Upper bound accepted for an explicit TIMEOUT argument, in seconds.")

(defconst nelisp-native-unit-development-output-limit 65536
  "Default maximum combined stdout+stderr bytes captured from a bounded
subprocess. Exceeding it kills the subprocess and rejects the run; the
budget is shared across both streams, not per-stream.")

(defconst nelisp-native-unit-development--reason-limit 1024
  "Maximum bytes of any single reason/diagnostic string this file returns.
A subprocess's output is bounded by the output budget already, but a
formatted reason embedding it is truncated again to this smaller bound
before it is ever put in a returned plist.")

(defconst nelisp-native-unit-development--poll-interval 0.05
  "Seconds `nelisp-native-unit-development--run-subprocess' waits per poll.")

(defun nelisp-native-unit-development--bytes (path)
  "Read at most 2 MiB of raw source bytes."
  (unless (and (file-regular-p path) (file-readable-p path))
    (error "Unreadable native source: %s" path))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally path nil 0 (1+ (* 2 1024 1024)))
    (when (> (- (point-max) (point-min)) (* 2 1024 1024))
      (error "Native source exceeds 2 MiB"))
    (buffer-string)))

(defun nelisp-native-unit-development--budget-prefix (text budget)
  "Return the longest character prefix of TEXT that fits in BUDGET bytes.
Never splits a multibyte character: the cut point is always a character
boundary because it is chosen over character-indexed substrings, not by
slicing raw bytes."
  (if (<= budget 0) ""
    (let ((lo 0) (hi (min (length text) budget)))
      (while (< lo hi)
        (let ((mid (/ (+ lo hi 1) 2)))
          (if (<= (string-bytes (substring text 0 mid)) budget)
              (setq lo mid) (setq hi (1- mid)))))
      (substring text 0 lo))))

(defun nelisp-native-unit-development--kill (process)
  "Delete PROCESS if it is non-nil. Safe to call more than once."
  (when process (ignore-errors (delete-process process))))

(defun nelisp-native-unit-development--wait (process seconds)
  "Call `accept-process-output' on PROCESS for SECONDS.
A `quit' (C-g) during the wait is cancellation, not an error to propagate:
swallow it, clear `quit-flag' so it cannot re-fire on the next checkpoint
inside this same call, and return `:quit'. Otherwise return nil."
  (condition-case nil
      (progn (accept-process-output process seconds) nil)
    (quit (setq quit-flag nil) :quit)))

(defun nelisp-native-unit-development--artifact-produced-p (path)
  "Return non-nil when PATH exists and is non-empty.
Native-unit-specific: a caller with no such artifact concept (see
`nelisp-native-unit-development-run-bounded') has no use for this and does
not call it. PATH is created empty by `make-temp-file' before the compiler
subprocess ever runs, so mere existence proves nothing; a compiler that is
killed or that fails leaves it empty."
  (let ((attrs (and path (ignore-errors (file-attributes path)))))
    (and attrs (> (or (file-attribute-size attrs) 0) 0))))

(defun nelisp-native-unit-development--cleanup-temp-files (artifact snapshot)
  "Delete ARTIFACT and SNAPSHOT if either exists.
Return the list of paths verified removed, in (artifact snapshot) order.
Native-unit-specific bookkeeping: `nelisp-native-unit-development-run-bounded'
creates no files of its own and has nothing to clean up here."
  (let (deleted)
    (dolist (file (list artifact snapshot))
      (when (and file (file-exists-p file))
        (ignore-errors (delete-file file))
        (unless (file-exists-p file) (push file deleted))))
    (nreverse deleted)))

(defun nelisp-native-unit-development--validate-timeout (timeout)
  "Return TIMEOUT, or the default, validated to a sane range.
Signal an error outside
[`nelisp-native-unit-development--timeout-min' ..
`nelisp-native-unit-development--timeout-max'] seconds."
  (let ((value (or timeout nelisp-native-unit-development-timeout)))
    (unless (and (numberp value)
                 (<= nelisp-native-unit-development--timeout-min value
                     nelisp-native-unit-development--timeout-max))
      (error "TIMEOUT must be a number in [%d, %d] seconds, got: %S"
             nelisp-native-unit-development--timeout-min
             nelisp-native-unit-development--timeout-max value))
    value))

(defun nelisp-native-unit-development--compile-phase
    (deadline-expired output-limited exit-code produced-artifact-p)
  "Classify a finished subprocess run from process-independent signals only.

DEADLINE-EXPIRED and OUTPUT-LIMITED are decided from the wall clock and the
byte budget respectively -- never from sampling `process-live-p', which can
read either way once the child has already been reaped (this is the exact
mistake this function exists to avoid; see
`nelisp-native-unit-development--run-subprocess'). EXIT-CODE is the
process's exit status when, and only when, it is known to have exited
normally (nil otherwise: still running when killed, killed by a signal, or
never started). PRODUCED-ARTIFACT-P means whatever the caller's domain
means by a trustworthy successful record; a caller with no more specific
notion than the exit code itself passes t unconditionally, deferring
entirely to EXIT-CODE (see `nelisp-native-unit-development-run-bounded'); a
caller that also expects a concrete output artifact passes its own check
(see `nelisp-native-unit-rebuild-and-reload' and
`nelisp-native-unit-development--artifact-produced-p').

A successful completion record requires both an exact zero EXIT-CODE and
PRODUCED-ARTIFACT-P; a deadline that expired without one always wins over a
nonzero or absent exit code, because a process that ran past its budget is
a timeout regardless of what status it eventually reports once reaped. A
deadline that expired despite a valid successful completion record is
forgiven: the wait loop merely noticed the finish late.

Return one of `:success', `:compiler-error', `:output-limit', `:timeout'.
Cancellation (an explicit predicate or a `quit') is decided by the caller
before this function is consulted, and is not one of its inputs."
  (let ((success (and (eql exit-code 0) produced-artifact-p)))
    (cond
     (output-limited :output-limit)
     ((and deadline-expired (not success)) :timeout)
     (success :success)
     (t :compiler-error))))

(defun nelisp-native-unit-development--run-subprocess
    (command args timeout cancel-predicate output-budget)
  "Run COMMAND with ARGS as a bounded, cancellable subprocess. Generic: this
function knows nothing about artifacts or native units; see
`nelisp-native-unit-development-run-bounded' for the public wrapper and
`nelisp-native-unit-rebuild-and-reload' for the native-unit-specific caller
that layers its own artifact check on top of this function's raw signals.

TIMEOUT is the deadline in seconds, already validated. CANCEL-PREDICATE,
called with no arguments on every poll, and a `quit' (C-g) both cancel the
run; see `nelisp-native-unit-development--wait'. OUTPUT-BUDGET is the shared
stdout+stderr byte budget.

Return a plist:
  (:cancelled BOOL :deadline-expired BOOL :output-limited BOOL
   :exit-code N-or-nil :stdout STR :stderr STR
   :stdout-bytes N :stderr-bytes N
   :elapsed-seconds F :timeout-seconds N)
`:stdout'/`:stderr' are each already bounded by OUTPUT-BUDGET. Callers derive
a final phase by consulting `:cancelled' first, then
`nelisp-native-unit-development--compile-phase' on the remaining fields with
whatever PRODUCED-ARTIFACT-P notion fits their own domain.

On return there is no live child process and no live stderr pipe process,
regardless of how this call ended: normal completion, timeout, output
limit, or cancellation through CANCEL-PREDICATE or a `quit'."
  (let* ((start (float-time)) (deadline (+ start timeout))
         (used 0) limited cancelled process stderr-pipe stdout stderr)
    (unwind-protect
        (progn
          (setq stderr-pipe
                (make-pipe-process
                 :name "nelisp-native-unit-development-subprocess-stderr"
                 :noquery t :coding 'utf-8-unix
                 :filter
                 (lambda (_pipe text)
                   (unless limited
                     (let* ((room (max 0 (- output-budget used)))
                            (part (nelisp-native-unit-development--budget-prefix
                                   text room)))
                       (setq stderr (concat stderr part)
                             used (+ used (string-bytes part)))
                       (unless (= (string-bytes part) (string-bytes text))
                         (setq limited t)
                         (nelisp-native-unit-development--kill process)))))))
          (setq process
                (make-process
                 :name "nelisp-native-unit-development-subprocess"
                 :noquery t :connection-type 'pipe :coding 'utf-8-unix
                 :stderr stderr-pipe :command (cons command args)
                 :filter
                 (lambda (_process text)
                   (unless limited
                     (let* ((room (max 0 (- output-budget used)))
                            (part (nelisp-native-unit-development--budget-prefix
                                   text room)))
                       (setq stdout (concat stdout part)
                             used (+ used (string-bytes part)))
                       (unless (= (string-bytes part) (string-bytes text))
                         (setq limited t)
                         (nelisp-native-unit-development--kill process)))))))
          (while (and (process-live-p process) (not limited) (not cancelled)
                      (< (float-time) deadline))
            (if (and cancel-predicate (funcall cancel-predicate))
                (setq cancelled t)
              (when (eq :quit (nelisp-native-unit-development--wait
                               process nelisp-native-unit-development--poll-interval))
                (setq cancelled t))))
          ;; The deadline is judged from the clock captured the instant the
          ;; wait loop exits, never by re-sampling `process-live-p': on some
          ;; platforms the child can already have been reaped by the time a
          ;; later liveness check runs, which misreports a genuine timeout as
          ;; not one (or vice versa). See
          ;; `nelisp-native-unit-development--compile-phase'.
          (let* ((elapsed (- (float-time) start))
                 (deadline-expired (>= elapsed timeout)))
            (when (or cancelled limited deadline-expired)
              (nelisp-native-unit-development--kill process))
            (let ((tries 25))
              (while (and (> tries 0) (eq (process-status process) 'run))
                (nelisp-native-unit-development--wait process 0.02)
                (setq tries (1- tries))))
            (let* ((status (process-status process))
                   (exit-code (and (eq status 'exit) (process-exit-status process))))
              (list :cancelled (and cancelled t)
                    :deadline-expired deadline-expired
                    :output-limited (and limited t)
                    :exit-code exit-code
                    :stdout (or stdout "") :stderr (or stderr "")
                    :stdout-bytes (string-bytes (or stdout ""))
                    :stderr-bytes (string-bytes (or stderr ""))
                    :elapsed-seconds elapsed :timeout-seconds timeout))))
      (nelisp-native-unit-development--kill process)
      (nelisp-native-unit-development--kill stderr-pipe))))

(defun nelisp-native-unit-development-run-bounded
    (command args &optional timeout cancel-predicate output-budget)
  "Run COMMAND with ARGS as a bounded, cancellable subprocess and classify it.

Generic: this function knows nothing about native units, artifacts, sources
or staging. It exists so any bounded-subprocess call site in this tree can
share one implementation of the deadline, cancellation and output-limit
guards, and the one pure phase-decision function that classifies a finished
run without ever re-sampling `process-live-p'
(`nelisp-native-unit-development--compile-phase'). It deletes no files;
COMMAND's own output files, if any, are entirely the caller's to create and
clean up, on every return including a rejection.

TIMEOUT defaults to `nelisp-native-unit-development-timeout' and is
validated to
[`nelisp-native-unit-development--timeout-min' ..
`nelisp-native-unit-development--timeout-max'] seconds -- the same range
`nelisp-native-unit-rebuild-and-reload' enforces, since it validates through
this same function. CANCEL-PREDICATE, called with no arguments on every
poll, and a `quit' (C-g) both cancel the run. OUTPUT-BUDGET defaults to
`nelisp-native-unit-development-output-limit' combined stdout+stderr bytes.

Return a plist:
  (:phase PHASE :exit-code N-or-nil
   :stdout STR :stderr STR :stdout-bytes N :stderr-bytes N
   :output-truncated BOOL :elapsed-seconds F :timeout-seconds N
   :cancelled BOOL :deadline-expired BOOL :output-limited BOOL)
PHASE is one of:
  `:complete'     -- COMMAND exited with status zero.
  `:failed'       -- COMMAND ran to a nonzero exit, or never produced one.
  `:timeout'      -- TIMEOUT elapsed with no successful completion.
  `:output-limit' -- the combined stdout+stderr budget was exceeded.
  `:cancelled'    -- CANCEL-PREDICATE or a `quit' ended the run early.
The trailing three plist entries are the same raw signals PHASE was derived
from, exposed for a caller (such as `nelisp-native-unit-rebuild-and-reload')
that layers its own, more specific completion check on top of the bare exit
code via `nelisp-native-unit-development--compile-phase' directly.
`:stdout'/`:stderr' are already bounded by OUTPUT-BUDGET; a caller embedding
them in a user-facing message must still truncate further (this file's own
native-unit caller uses `nelisp-native-unit-development--reason-limit' for
that; see `nelisp-native-unit-development--reject-run').

On return there is no live child process and no live stderr pipe process,
regardless of how the call ended."
  (let* ((timeout (nelisp-native-unit-development--validate-timeout timeout))
         (budget (or output-budget nelisp-native-unit-development-output-limit))
         (run (nelisp-native-unit-development--run-subprocess
               command args timeout cancel-predicate budget))
         (phase
          (if (plist-get run :cancelled) :cancelled
            (pcase (nelisp-native-unit-development--compile-phase
                    (plist-get run :deadline-expired) (plist-get run :output-limited)
                    (plist-get run :exit-code) t)
              (:success :complete)
              (:compiler-error :failed)
              (other other)))))
    (list :phase phase :exit-code (plist-get run :exit-code)
          :stdout (plist-get run :stdout) :stderr (plist-get run :stderr)
          :stdout-bytes (plist-get run :stdout-bytes)
          :stderr-bytes (plist-get run :stderr-bytes)
          :output-truncated (plist-get run :output-limited)
          :elapsed-seconds (plist-get run :elapsed-seconds)
          :timeout-seconds (plist-get run :timeout-seconds)
          :cancelled (plist-get run :cancelled)
          :deadline-expired (plist-get run :deadline-expired)
          :output-limited (plist-get run :output-limited))))

(defun nelisp-native-unit-development--default-command
    (root script snapshot artifact binary)
  "Build the production (COMMAND . ARGS) pair for the compiler subprocess.
ROOT is the repository root, SCRIPT the compiler entry file inside it,
SNAPSHOT the frozen source copy, ARTIFACT the output path, and BINARY the
running executable's digest. Honours the `EMACS' environment variable
exactly as before this file gained a bounded runner."
  (cons (or (getenv "EMACS") "emacs")
        (list "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
              "-L" (expand-file-name "lisp" root)
              "-L" (expand-file-name "src" root)
              "-L" (expand-file-name "scripts" root)
              "-l" script "-f" "nelisp-native-unit-compile-command"
              snapshot artifact binary)))

(defvar nelisp-native-unit-development--command-function
  #'nelisp-native-unit-development--default-command
  "Function of (ROOT SCRIPT SNAPSHOT ARTIFACT BINARY) building the compiler
subprocess as (COMMAND . ARGS). Exists so tests can exercise
`nelisp-native-unit-rebuild-and-reload' end to end against a stub program (a
hung sleep, a flood of stdout, a nonzero exit) without touching the
production path or requiring the native runtime. Production code must not
rebind this; it always resolves to
`nelisp-native-unit-development--default-command'.")

(defun nelisp-native-unit-development--reject (phase reason artifact snapshot)
  "Build a rejected result plist at PHASE with REASON.
Deletes ARTIFACT and SNAPSHOT if either exists and reports what was removed."
  (list :status 'rejected :phase phase
        :reason (nelisp-native-unit-development--budget-prefix
                 (or reason "") nelisp-native-unit-development--reason-limit)
        :artifact artifact :snapshot snapshot
        :deleted (nelisp-native-unit-development--cleanup-temp-files
                  artifact snapshot)))

(defun nelisp-native-unit-development--reject-run (phase run artifact snapshot)
  "Build a rejected result plist from a finished
`nelisp-native-unit-development-run-bounded' RUN at PHASE.
PHASE is the already-remapped outer phase (`:compile' in place of the
internal `:compiler-error', or `:timeout'/`:output-limit'/`:cancelled' as
is). Every string embedded here is bounded well under RUN's own
already-bounded stdout/stderr, per
`nelisp-native-unit-development--reason-limit'."
  (let* ((exit-code (plist-get run :exit-code))
         (elapsed (plist-get run :elapsed-seconds))
         (timeout (plist-get run :timeout-seconds))
         (reason
          (cond
           ((eq phase :timeout)
            (format "Compiler exceeded the %s second timeout (%.1fs elapsed)"
                    timeout elapsed))
           ((eq phase :cancelled) "Compile was cancelled")
           ((eq phase :output-limit)
            (format "Compiler output exceeded the %d byte budget"
                    nelisp-native-unit-development-output-limit))
           (t (format "Compiler failed (%S): %s%s" exit-code
                      (or (plist-get run :stdout) "")
                      (or (plist-get run :stderr) ""))))))
    (append
     (list :status 'rejected :phase phase
           :reason (nelisp-native-unit-development--budget-prefix
                    reason nelisp-native-unit-development--reason-limit)
           :timeout-seconds timeout :elapsed-seconds elapsed
           :stdout-bytes (plist-get run :stdout-bytes)
           :stderr-bytes (plist-get run :stderr-bytes)
           :output-truncated (plist-get run :output-truncated)
           :artifact artifact :snapshot snapshot)
     (list :deleted (nelisp-native-unit-development--cleanup-temp-files
                     artifact snapshot)))))

(defun nelisp-native-unit-rebuild-and-reload
    (source &optional unit-id exports repository timeout cancel-predicate)
  "Recompile SOURCE and publish UNIT-ID, or create a unit when UNIT-ID is nil.
EXPORTS names the stable public entries at creation; nil selects all entries.
Existing unit contracts cannot change. REPOSITORY defaults to the current
directory. SOURCE is compiled in a bounded, cancellable host subprocess
(`nelisp-native-unit-development-run-bounded') from an immutable snapshot;
the original file is checked again before publication.

TIMEOUT bounds the subprocess in seconds and defaults to
`nelisp-native-unit-development-timeout'; it is validated to
[`nelisp-native-unit-development--timeout-min' ..
`nelisp-native-unit-development--timeout-max'] seconds. CANCEL-PREDICATE,
called with no arguments while waiting, and a `quit' signal (C-g) both abort
the compile as a cancellation. Exceeding TIMEOUT, exceeding the combined
stdout+stderr budget (`nelisp-native-unit-development-output-limit'), a
nonzero compiler exit, an exit that produced no real artifact, or the
source changing underneath the compile all reject without staging or
publishing; every rejection this way deletes the temporary artifact and
snapshot files it created and reports which ones under `:deleted'.

This command is explicit and does not retry application calls. Return the
publication result plist."
  (let ((phase :source) artifact snapshot)
    (condition-case err
        (let* ((timeout (nelisp-native-unit-development--validate-timeout timeout))
               (root (file-name-as-directory
                      (expand-file-name (or repository default-directory))))
               (path (expand-file-name source root))
               (bytes (nelisp-native-unit-development--bytes path))
               (hash (secure-hash 'sha256 bytes))
               (binary (nelisp-native-load--running-binary-sha256))
               (script (expand-file-name "scripts/nelisp-native-unit-compile.el" root)))
          (unless (and (stringp binary) (file-readable-p script))
            (error "Native identity or checkout compiler is unavailable"))
          (setq artifact (make-temp-file "nelisp-native-unit-" nil ".nelr")
                snapshot (concat artifact ".el") phase :compile)
          (let ((coding-system-for-write 'no-conversion))
            (with-temp-file snapshot
              (set-buffer-multibyte nil) (insert bytes)))
          (let* ((command-and-args
                  (funcall nelisp-native-unit-development--command-function
                           root script snapshot artifact binary))
                 (run (nelisp-native-unit-development-run-bounded
                       (car command-and-args) (cdr command-and-args)
                       timeout cancel-predicate))
                 ;; The generic helper judges success from the bare exit
                 ;; code alone (it has no artifact concept). Re-derive the
                 ;; native-unit-specific verdict through the same pure
                 ;; function, now with the real artifact check, so "exit
                 ;; zero but nothing was actually written" is not mistaken
                 ;; for success here.
                 (run-phase
                  (if (plist-get run :cancelled) :cancelled
                    (nelisp-native-unit-development--compile-phase
                     (plist-get run :deadline-expired) (plist-get run :output-limited)
                     (plist-get run :exit-code)
                     (nelisp-native-unit-development--artifact-produced-p artifact)))))
            (if (not (eq run-phase :success))
                (nelisp-native-unit-development--reject-run
                 (if (eq run-phase :compiler-error) :compile run-phase)
                 run artifact snapshot)
              (unless (equal hash (secure-hash 'sha256
                                              (nelisp-native-unit-development--bytes path)))
                (error "Native source changed while compiling"))
              (setq phase :stage)
              (let ((staged (nelisp-native-unit-stage artifact unit-id exports)))
                (if (not (eq (plist-get staged :status) 'staged))
                    (append staged
                            (list :artifact artifact :snapshot snapshot
                                  :deleted (nelisp-native-unit-development--cleanup-temp-files
                                            artifact snapshot)))
                  (setq phase :publish)
                  (unless (equal hash (secure-hash 'sha256
                                                  (nelisp-native-unit-development--bytes path)))
                    (nelisp-native-unit-discard (plist-get staged :candidate-id))
                    (error "Native source changed while staging"))
                  (append (plist-put (nelisp-native-unit-publish
                                      (plist-get staged :candidate-id))
                                     :source-sha256 hash)
                          (list :source path
                                :artifact artifact :snapshot snapshot)))))))
      (error (nelisp-native-unit-development--reject
              phase (error-message-string err) artifact snapshot)))))

(provide 'nelisp-native-unit-development)
