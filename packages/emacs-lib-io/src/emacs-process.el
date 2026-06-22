;;; emacs-process.el --- Process / subprocess substrate (Track I)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track I (2026-05-03) — Layer 2.
;;
;; γ-stage substrate for the Emacs process API.  Two-mode design:
;;
;; - Under host Emacs the unprefixed names (`call-process',
;;   `start-process', etc.) stay bound to the host C primitives;
;;   our bridge gate skips, and the prefixed substrate functions
;;   are thin pass-throughs to whatever the unprefixed name
;;   currently resolves to (= host's C impl).
;;
;; - Under standalone NeLisp the substrate dispatches to registered
;;   standalone primitives first, then to the loaded `nelisp-process'
;;   facade when available.
;;
;; The two-mode test guards against recursion via
;; `indirect-function' equality: if the unprefixed name's resolved
;; function is the same as the substrate (= we ARE the host's
;; binding), we signal rather than recurse.
;;
;; Bridged API (γ MVP):
;;   - call-process / call-process-region
;;   - start-process / make-process (= placeholder for async)
;;   - processp / process-list / process-status /
;;     process-exit-status / process-buffer / process-name
;;   - process-send-string / process-send-eof / delete-process
;;   - shell-command / shell-command-to-string
;;
;; Variables: shell-command-switch, shell-file-name.
;;
;; Out of scope:
;;   - filter / sentinel callbacks during async runs (= deferred
;;     until we have an event loop integrated with the command
;;     loop)
;;   - process-coding-system handling
;;   - network processes / make-network-process

;;; Code:

(require 'cl-lib)
(require 'emacs-standalone)

(define-error 'emacs-process-error "Process error")
(define-error 'emacs-process-not-implemented
  "Process operation not implemented in this environment"
  'emacs-process-error)

(declare-function nelisp-call-process "nelisp-process"
                  (program &optional infile destination display &rest args))
(declare-function nelisp-call-process-region "nelisp-process"
                  (start end program &optional delete destination display
                         &rest args))
(declare-function nelisp-process-call-process "nelisp-process"
                  (program &optional infile destination display &rest args))
(declare-function nelisp-process-call-process-region "nelisp-process"
                  (start end program &optional delete destination display
                         &rest args))
(declare-function nelisp-process-start "nelisp-process" (program &rest args))
(declare-function nelisp-process-wait "nelisp-process" (proc))
(declare-function nelisp-process-read-output "nelisp-process" (proc n))

;;;; --- delegate plumbing ---------------------------------------------

(defconst emacs-process--nelisp-delegates
  '((call-process
     nelisp-process-call-process
     nelisp-call-process)
    (call-process-region
     nelisp-process-call-process-region
     nelisp-call-process-region))
  "Mapping from Emacs process API names to `nelisp-process' facades.
The `nelisp-process-*' names are the current package-shaped primitive
surface; the legacy `nelisp-*' names remain as compatibility fallbacks.")

(defvar emacs-process--native-primitives
  (let (alist)
    (dolist (sym '(call-process call-process-region make-process start-process
                   start-process-shell-command process-file))
      (when (and (fboundp sym)
                 ;; `subrp'/`indirect-function' are host-Emacs builtins absent
                 ;; under standalone NeLisp image replay; this host-only subr
                 ;; capture must degrade to an empty alist rather than abort the
                 ;; bridge-image `progn' load (every form after it -- including
                 ;; the GUI bridge session runtime -- would otherwise be lost).
                 (fboundp 'subrp) (fboundp 'indirect-function)
                 (subrp (indirect-function sym)))
        (push (cons sym (symbol-function sym)) alist)))
    alist)
  "True native (subr) process primitives captured at first load.
Captured before any bridge/preload `fset' can alias the unprefixed name
to a wrapper that routes back into this facade.  `emacs-process--delegate'
prefers these in host mode so a leaked wrapper (e.g. one installed by the
runtime-image process preload, which would re-enter `emacs-process-*' and
recurse) never shadows the real primitive.  `defvar' (not `defconst') so a
re-load after a wrapper leak keeps the original subr capture.")

(defvar emacs-process--fallback-processes nil
  "Process objects created by the standalone synchronous fallback.")

(defvar emacs-process--fallback-next-pid 10000
  "Synthetic pid counter for standalone fallback process objects.")

(defconst emacs-process--fallback-tag 'emacs-process-fallback
  "Vector tag used for standalone fallback process objects.")

(defvar emacs-process--native-process-metadata nil
  "Metadata alist for native NeLisp process objects.")

(defun emacs-process--fallback-process-p (object)
  "Return non-nil when OBJECT is a fallback process vector."
  (and (vectorp object)
       (<= 10 (length object))
       (eq (aref object 0) emacs-process--fallback-tag)))

(defun emacs-process--native-process-p (object)
  "Return non-nil when OBJECT is a native NeLisp process object."
  (and (fboundp 'nelisp-process-object-p)
       (ignore-errors (nelisp-process-object-p object))))

(defun emacs-process--process-object-p (object)
  "Return non-nil when OBJECT is a process object owned here."
  (or (emacs-process--fallback-process-p object)
      (emacs-process--native-process-p object)))

(defun emacs-process--native-start-available-p ()
  "Return non-nil when native NeLisp async process start exists."
  (or (fboundp 'nelisp-process-start-process)
      (fboundp 'nelisp-process-start)))

(defun emacs-process--native-metadata-cell (process)
  "Return metadata cell for PROCESS."
  (assoc process emacs-process--native-process-metadata))

(defun emacs-process--native-metadata (process key)
  "Return metadata KEY for PROCESS."
  (plist-get (cdr (emacs-process--native-metadata-cell process)) key))

(defun emacs-process--native-put-metadata (process plist)
  "Store PLIST metadata for PROCESS."
  (let ((cell (emacs-process--native-metadata-cell process)))
    (if cell
        (setcdr cell plist)
      (push (cons process plist) emacs-process--native-process-metadata)))
  process)

(defun emacs-process--native-set-metadata (process key value)
  "Set metadata KEY to VALUE for PROCESS."
  (let ((cell (emacs-process--native-metadata-cell process)))
    (if cell
        (setcdr cell (plist-put (cdr cell) key value))
      (emacs-process--native-put-metadata process (list key value))))
  value)

(defun emacs-process--native-status-code (process)
  "Return native integer status code for PROCESS."
  (if (fboundp 'nelisp-process-status)
      (nelisp-process-status process)
    3))

(defun emacs-process--native-status-symbol (process)
  "Return Emacs status symbol for native PROCESS."
  (let ((code (emacs-process--native-status-code process)))
    (cond
     ((= code 0) 'run)
     ((= code 1) 'exit)
     ((= code 2) 'signal)
     (t 'closed))))

(defun emacs-process--native-exit-status (process)
  "Return native PROCESS exit status."
  (if (fboundp 'nelisp-process-exit-status)
      (nelisp-process-exit-status process)
    0))

(defun emacs-process--native-start (name buffer command filter sentinel)
  "Start native NeLisp COMMAND and attach Emacs metadata."
  (let* ((launcher (cond
                    ((fboundp 'nelisp-process-start-process)
                     'nelisp-process-start-process)
                    ((fboundp 'nelisp-process-start)
                     'nelisp-process-start)
                    (t nil)))
         (process (and launcher command
                       (apply launcher command))))
    (when process
      (emacs-process--native-put-metadata
       process
       (list :name name
             :buffer (emacs-process--fallback-buffer buffer)
             :command command
             :filter filter
             :sentinel sentinel
             :sentinel-fired nil
             :deleted nil)))
    process))

(defun emacs-process--native-drain-output (process)
  "Drain native PROCESS output into buffer/filter."
  (let ((observed nil)
        (chunk t)
        (buffer (emacs-process--native-metadata process :buffer))
        (filter (emacs-process--native-metadata process :filter)))
    (while (and (fboundp 'nelisp-process-read-output) chunk)
      (setq chunk (nelisp-process-read-output process 4096))
      (when (and (stringp chunk) (> (length chunk) 0))
        (setq observed t)
        (when buffer
          (with-current-buffer buffer
            (goto-char (point-max))
            (insert chunk)))
        (when (functionp filter)
          (funcall filter process chunk))))
    observed))

(defun emacs-process--native-maybe-fire-sentinel (process)
  "Fire native PROCESS sentinel once after exit or signal."
  (let ((status (emacs-process--native-status-symbol process)))
    (if (or (eq status 'run)
            (emacs-process--native-metadata process :sentinel-fired))
        nil
      (let ((sentinel (emacs-process--native-metadata process :sentinel))
            (event (if (eq status 'exit)
                       "finished\n"
                     (format "exited abnormally with code %s\n"
                             (emacs-process--native-exit-status process)))))
        (emacs-process--native-set-metadata process :sentinel-fired t)
        (when (functionp sentinel)
          (funcall sentinel process event))
        t))))

(defun emacs-process--native-live-processes ()
  "Return known native processes not marked deleted."
  (let ((processes nil))
    (dolist (cell emacs-process--native-process-metadata)
      (unless (plist-get (cdr cell) :deleted)
        (push (car cell) processes)))
    (nreverse processes)))

(defun emacs-process--native-accept (processes)
  "Drain output and sentinel events from PROCESSES."
  (let ((observed nil))
    (dolist (process processes)
      (when (emacs-process--native-process-p process)
        (when (emacs-process--native-drain-output process)
          (setq observed t))
        (when (emacs-process--native-maybe-fire-sentinel process)
          (setq observed t))))
    observed))

(defun emacs-process--native-delete (process)
  "Delete native PROCESS and mark metadata deleted."
  (when (fboundp 'nelisp-process-delete)
    (nelisp-process-delete process))
  (emacs-process--native-set-metadata process :deleted t)
  process)

(defun emacs-process--fallback-process-deleted-p (process)
  "Return non-nil when fallback PROCESS has been deleted."
  (and (emacs-process--fallback-process-p process)
       (aref process 8)))

(defun emacs-process--fallback-buffer (buffer)
  "Resolve BUFFER designator for fallback process output."
  (cond
   ((null buffer) nil)
   ((and (fboundp 'bufferp) (bufferp buffer)) buffer)
   ((and (stringp buffer) (fboundp 'get-buffer-create))
    (get-buffer-create buffer))
   (t buffer)))

(defun emacs-process--fallback-sentinel-event (status)
  "Return a process sentinel event string for exit STATUS."
  (if (and (integerp status) (= status 0))
      "finished\n"
    (format "exited abnormally with code %s\n" status)))

(defun emacs-process--fallback-make-process (&rest plist)
  "Create a fallback process object and run its command synchronously.

This is intentionally not the final async substrate.  It gives
standalone NeLisp a process-shaped object for APIs such as
`async-shell-command', `process-status', and process sentinels while
the lower event loop / pipe implementation is still being integrated."
  (let* ((name (or (plist-get plist :name) "process"))
         (buffer (emacs-process--fallback-buffer (plist-get plist :buffer)))
         (command (plist-get plist :command))
         (sentinel (plist-get plist :sentinel))
         (filter (plist-get plist :filter))
         (pid emacs-process--fallback-next-pid)
         (process (vector emacs-process--fallback-tag
                          name buffer command 'run nil filter sentinel
                          nil pid))
         (status 1))
    (setq emacs-process--fallback-next-pid
          (+ emacs-process--fallback-next-pid 1))
    (push process emacs-process--fallback-processes)
    (setq status
          (condition-case nil
              (if (and (consp command) (car command))
                  (apply #'emacs-process-call-process
                         (car command) nil buffer nil (cdr command))
                1)
            (error 1)))
    (aset process 4 'exit)
    (aset process 5 status)
    (when (functionp sentinel)
      (funcall sentinel process
               (emacs-process--fallback-sentinel-event status)))
    process))

(defun emacs-process--delegate-p (sym)
  "Return non-nil if SYM has a callable host binding distinct from us.
Avoids infinite recursion when the bridge has aliased the
unprefixed name to one of our substrate functions."
  (and (fboundp sym)
       (let ((our-prefixed
              (intern-soft (concat "emacs-process-"
                                   (symbol-name sym)))))
         (or (null our-prefixed)
             (not (eq (indirect-function sym)
                      (indirect-function our-prefixed)))))))

(defun emacs-process--nelisp-delegate (sym)
  "Return the `nelisp-process' delegate for SYM, or nil."
  (let ((cell (assq sym emacs-process--nelisp-delegates))
        (found nil))
    ;; Only pull in `nelisp-process' when SYM is actually one we can
    ;; delegate (CELL non-nil) and no candidate is bound yet.  Without the
    ;; CELL guard an unmapped SYM (e.g. `make-process') would still trigger
    ;; the require, loading the vendor package and leaking its host-elisp
    ;; `nelisp-call-process'/`nelisp-make-process' globally -- which then
    ;; forms a delegation cycle in later tests.
    (when (and cell
               (not (catch 'available
                      (dolist (candidate (cdr cell))
                        (when (fboundp candidate)
                          (throw 'available t)))
                      nil)))
      (require 'nelisp-process nil t))
    (catch 'done
      (dolist (candidate (cdr cell))
        (when (fboundp candidate)
          (setq found candidate)
          (throw 'done nil))))
    found))

(defun emacs-process--delegate (sym args)
  "Apply SYM to ARGS through the host binding or standalone primitive.

Lookup order:
  1. host-mode + host has a non-shadow binding → apply host.
  2. a standalone primitive is registered for SYM → dispatch.
  3. a loaded `nelisp-process' facade is available → dispatch.
  4. otherwise signal `emacs-process-not-implemented'.

Steps 2 and 3 are what let NeLisp replace the host primitive while
keeping this file as the Emacs-shaped compatibility boundary."
  (cond
   ;; Host mode: use the true native subr captured at load, never the live
   ;; unprefixed binding -- a bridge/preload may have aliased it to a wrapper
   ;; that re-enters this facade (infinite recursion).  Force-mode skips this.
   ((and (not (emacs-standalone-mode-p))
         (assq sym emacs-process--native-primitives))
    (apply (cdr (assq sym emacs-process--native-primitives)) args))
   ((and (not (emacs-standalone-mode-p))
         (emacs-process--delegate-p sym))
    (apply (indirect-function sym) args))
   ((emacs-standalone-has-primitive-p sym)
    (emacs-standalone-call-primitive sym args))
   ((emacs-process--nelisp-delegate sym)
    (apply (emacs-process--nelisp-delegate sym) args))
   (t (signal 'emacs-process-not-implemented (list sym)))))

;;;###autoload
(defun emacs-process-delegate (sym args)
  "Apply SYM to ARGS through the host binding or standalone primitive."
  (emacs-process--delegate sym args))

;;;; --- synchronous: call-process / call-process-region --------------

(defun emacs-process--standalone-capture-available-p ()
  "Return non-nil when the nelisp-process async capture primitives exist.
These (`nelisp-process-start' / `-wait' / `-read-output') let the
standalone reader run a child with its stdout on a pipe and read it
back -- which the synchronous `nelisp-process-call-process' facade does
not do (it returns only an exit code and leaks stdout to the parent)."
  (and (fboundp 'nelisp-process-start)
       (fboundp 'nelisp-process-wait)
       (fboundp 'nelisp-process-read-output)))

(defun emacs-process--call-process-target-buffer (destination)
  "Resolve the stdout buffer for call-process DESTINATION, or nil to discard.
Covers the part of the Emacs DESTINATION contract we support: nil / 0
discard; t = current buffer; a string names a buffer; a buffer object is
used directly; a list uses its car (the stdout destination)."
  (cond
   ((null destination) nil)
   ((eq destination 0) nil)
   ((eq destination t) (current-buffer))
   ((stringp destination) (get-buffer-create destination))
   ((consp destination)
    (emacs-process--call-process-target-buffer (car destination)))
   ((and (fboundp 'bufferp) (not (bufferp destination))) (current-buffer))
   (t destination)))

(defun emacs-process--standalone-call-process (program destination args)
  "Run PROGRAM with ARGS synchronously via the nelisp-process async
primitives, capturing stdout and inserting it per call-process
DESTINATION.  Returns the child's integer exit code.

This is the standalone-reader path that gives `call-process' real output
capture: the synchronous `nelisp-process-call-process' facade only
returns an exit code and leaks the child's stdout to the parent.

`nelisp-process-read-output' is non-blocking (it returns nil when no
data is buffered *yet*, not only at EOF), so we `nelisp-process-wait'
for the child to exit first and then drain the buffered stdout to the
nil EOF marker -- the ordering the reader's own process smoke uses.
Stdin redirection (INFILE) is not handled here; the caller routes INFILE
cases to the generic delegate instead.

Caveat: draining after exit assumes the child's total stdout fits the
reader's stdout buffer; multi-megabyte streaming output is out of scope
for this synchronous path."
  (let* ((proc (apply #'nelisp-process-start program args))
         (rc (nelisp-process-wait proc))
         (out "")
         (chunk nil))
    (while (setq chunk (nelisp-process-read-output proc 65536))
      (setq out (concat out chunk)))
    (let ((target (emacs-process--call-process-target-buffer destination)))
      (when (and target (> (length out) 0))
        (with-current-buffer target (insert out)))
      (if (integerp rc) rc 0))))

(defun emacs-process-call-process (program &optional infile destination
                                           display &rest args)
  "Synchronous program execution.  See `call-process' for semantics.

On the standalone reader, when stdin is not redirected (INFILE nil) and
the nelisp-process async capture primitives are available, route through
`emacs-process--standalone-call-process' so stdout is actually captured
into DESTINATION (the synchronous facade returns only an exit code and
leaks stdout to the parent).

Otherwise, when no host binding and no standalone primitive exists,
degrade GRACEFULLY: return a non-zero exit code (1, = \"program failed\")
instead of signalling `emacs-process-not-implemented'.  Load-time
feature/tool detection in vendor packages (e.g. org.el probing for
external tools via `(eq 0 (call-process ...))') then treats the tool as
unavailable and proceeds, rather than aborting the whole load."
  (if (and (emacs-standalone-mode-p)
           (null infile)
           (emacs-process--standalone-capture-available-p))
      (emacs-process--standalone-call-process program destination args)
    (condition-case nil
        (emacs-process--delegate 'call-process
                                 (cons program (cons infile (cons destination
                                                                  (cons display args)))))
      (emacs-process-not-implemented 1))))

(defun emacs-process-call-process-region (start end program &optional
                                                delete buffer display
                                                &rest args)
  "Synchronous program execution with input from a buffer region."
  (emacs-process--delegate 'call-process-region
                           (append (list start end program delete buffer display)
                                   args)))

;;;; --- asynchronous: start-process / make-process -------------------

(defun emacs-process-start-process (name buffer program &rest program-args)
  "Start PROGRAM in BUFFER asynchronously.  Returns the process object."
  (or (and (emacs-standalone-mode-p)
           (emacs-process--native-start-available-p)
           (ignore-errors
             (emacs-process--native-start
              name buffer (cons program program-args) nil nil)))
      (condition-case nil
          (emacs-process--delegate
           'start-process
           (cons name (cons buffer (cons program program-args))))
        (emacs-process-not-implemented
         (apply #'emacs-process--fallback-make-process
                (list :name name
                      :buffer buffer
                      :command (cons program program-args)))))))

(defun emacs-process-make-process (&rest plist)
  "Start a process described by PLIST (= keyword/value pairs)."
  (or (and (emacs-standalone-mode-p)
           (emacs-process--native-start-available-p)
           (ignore-errors
             (emacs-process--native-start
              (or (plist-get plist :name) "process")
              (plist-get plist :buffer)
              (plist-get plist :command)
              (plist-get plist :filter)
              (plist-get plist :sentinel))))
      (condition-case nil
          (emacs-process--delegate 'make-process plist)
        (emacs-process-not-implemented
         (apply #'emacs-process--fallback-make-process plist)))))

;;;; --- predicates / accessors ---------------------------------------

(defun emacs-process-processp (object)
  "Return non-nil if OBJECT is a process."
  (cond
   ((emacs-process--fallback-process-p object) t)
   ((emacs-process--native-process-p object) t)
   ((and (not (emacs-standalone-mode-p))
         (emacs-process--delegate-p 'processp))
    (funcall (indirect-function 'processp) object))
   (t nil)))

(defun emacs-process-process-list ()
  "Return the list of currently-active processes."
  (cond
   ((and (not (emacs-standalone-mode-p))
         (emacs-process--delegate-p 'process-list))
    (funcall (indirect-function 'process-list)))
   (t
    (let ((processes nil))
      (dolist (process (emacs-process--native-live-processes))
        (push process processes))
      (dolist (process emacs-process--fallback-processes)
        (unless (emacs-process--fallback-process-deleted-p process)
          (push process processes)))
      (nreverse processes)))))

(defun emacs-process-process-status (process)
  "Return PROCESS's status symbol."
  (cond
   ((emacs-process--fallback-process-p process) (aref process 4))
   ((emacs-process--native-process-p process)
    (emacs-process--native-status-symbol process))
   (t (emacs-process--delegate 'process-status (list process)))))

(defun emacs-process-process-exit-status (process)
  "Return PROCESS's exit-status integer."
  (cond
   ((emacs-process--fallback-process-p process) (or (aref process 5) 0))
   ((emacs-process--native-process-p process)
    (emacs-process--native-exit-status process))
   (t (emacs-process--delegate 'process-exit-status (list process)))))

(defun emacs-process-process-buffer (process)
  "Return PROCESS's associated buffer."
  (cond
   ((emacs-process--fallback-process-p process) (aref process 2))
   ((emacs-process--native-process-p process)
    (emacs-process--native-metadata process :buffer))
   (t (emacs-process--delegate 'process-buffer (list process)))))

(defun emacs-process-process-name (process)
  "Return PROCESS's name string."
  (cond
   ((emacs-process--fallback-process-p process) (aref process 1))
   ((emacs-process--native-process-p process)
    (emacs-process--native-metadata process :name))
   (t (emacs-process--delegate 'process-name (list process)))))

(defun emacs-process-process-command (process)
  "Return PROCESS's command (program + args) as a list."
  (cond
   ((emacs-process--fallback-process-p process) (aref process 3))
   ((emacs-process--native-process-p process)
    (emacs-process--native-metadata process :command))
   (t (emacs-process--delegate 'process-command (list process)))))

(defun emacs-process-process-live-p (process)
  "Return non-nil if PROCESS is alive (status = run/open/listen/connect/stop)."
  (cond
   ((emacs-process--process-object-p process)
    (memq (emacs-process-process-status process)
          '(run open listen connect stop)))
   ((and (not (emacs-standalone-mode-p))
         (emacs-process--delegate-p 'process-live-p))
    (funcall (indirect-function 'process-live-p) process))
   (t
    ;; Standalone fallback: derive from process-status if available.
    (let ((s (and (or (fboundp 'process-status)
                      (fboundp 'emacs-process-process-status))
                  (emacs-process-process-status process))))
      (memq s '(run open listen connect stop))))))

(defun emacs-process-process-id (process)
  "Return PROCESS's OS pid integer (or nil if not yet running)."
  (cond
   ((emacs-process--fallback-process-p process) (aref process 9))
   ((emacs-process--native-process-p process)
    (and (fboundp 'nelisp-process-pid)
         (nelisp-process-pid process)))
   (t (emacs-process--delegate 'process-id (list process)))))

(defun emacs-process-process-mark (process)
  "Return PROCESS's filter mark (used by buffer-attached output)."
  (if (emacs-process--process-object-p process)
      nil
    (emacs-process--delegate 'process-mark (list process))))

(defun emacs-process-set-process-filter (process filter)
  "Install FILTER as PROCESS's stdout/stderr callback."
  (cond
   ((emacs-process--fallback-process-p process)
    (aset process 6 filter)
    filter)
   ((emacs-process--native-process-p process)
    (emacs-process--native-set-metadata process :filter filter))
   (t (emacs-process--delegate 'set-process-filter (list process filter)))))

(defun emacs-process-set-process-sentinel (process sentinel)
  "Install SENTINEL as PROCESS's lifecycle callback."
  (cond
   ((emacs-process--fallback-process-p process)
    (aset process 7 sentinel)
    sentinel)
   ((emacs-process--native-process-p process)
    (emacs-process--native-set-metadata process :sentinel sentinel))
   (t (emacs-process--delegate 'set-process-sentinel
                               (list process sentinel)))))

(defun emacs-process-accept-process-output (&optional process seconds millisec just-this-one)
  "Block until PROCESS produces output or SECONDS pass.

Same calling convention as Emacs's `accept-process-output'.  When
the host primitive is available, delegate.  Otherwise the
substrate returns nil when only synchronous fallback processes exist."
  (condition-case nil
      (if (or (emacs-process--native-process-p process)
              (and (null process)
                   emacs-process--native-process-metadata))
          (emacs-process--native-accept
           (if process
               (list process)
             (emacs-process--native-live-processes)))
        (emacs-process--delegate 'accept-process-output
                                 (list process seconds millisec just-this-one)))
    (emacs-process-not-implemented nil)))

(defun emacs-process-signal-process (process-or-pid signum)
  "Send SIGNUM (number or symbol) to PROCESS-OR-PID."
  (if (emacs-process--fallback-process-p process-or-pid)
      (progn
        (aset process-or-pid 4 'signal)
        (aset process-or-pid 5 1)
        process-or-pid)
    (if (emacs-process--native-process-p process-or-pid)
        (emacs-process--native-delete process-or-pid)
      (emacs-process--delegate 'signal-process (list process-or-pid signum)))))

(defun emacs-process-kill-process (process)
  "Send SIGKILL to PROCESS.

Equivalent to `(signal-process PROCESS \\='KILL)'; provided as a
top-level alias for parity with the Emacs API."
  (cond
   ((emacs-process--process-object-p process)
    (emacs-process-signal-process process 'KILL))
   ((emacs-process--delegate-p 'kill-process)
    (funcall (indirect-function 'kill-process) process))
   (t
    (emacs-process-signal-process process 'KILL))))

;;;; --- I/O + lifecycle ----------------------------------------------

(defun emacs-process-process-send-string (process string)
  "Send STRING to PROCESS's stdin."
  (if (emacs-process--process-object-p process)
      nil
    (emacs-process--delegate 'process-send-string (list process string))))

(defun emacs-process-process-send-eof (&optional process)
  "Send EOF to PROCESS's stdin."
  (if (emacs-process--process-object-p process)
      nil
    (emacs-process--delegate 'process-send-eof (list process))))

(defun emacs-process-delete-process (process)
  "Kill PROCESS."
  (if (emacs-process--fallback-process-p process)
      (progn
        (aset process 8 t)
        process)
    (if (emacs-process--native-process-p process)
        (emacs-process--native-delete process)
      (emacs-process--delegate 'delete-process (list process)))))

;;;; --- shell-command / shell-command-to-string ----------------------

(defvar emacs-process-shell-file-name "/bin/sh"
  "Substrate-internal mirror of `shell-file-name'.")

(defvar emacs-process-shell-command-switch "-c"
  "Substrate-internal mirror of `shell-command-switch'.")

(defun emacs-process-shell-command (command &optional output-buffer error-buffer)
  "Execute COMMAND through the shell.

When the host's `shell-command' is available, delegate to it
unchanged.  Otherwise build a `call-process' invocation manually."
  (cond
   ((emacs-process--delegate-p 'shell-command)
    (funcall (indirect-function 'shell-command)
             command output-buffer error-buffer))
   (t
    (apply #'emacs-process-call-process
           emacs-process-shell-file-name
           nil
           (or output-buffer t)
           nil
           (list emacs-process-shell-command-switch command)))))

(defun emacs-process-shell-command-to-string (command)
  "Run COMMAND through the shell, return its stdout as a string."
  (cond
   ((emacs-process--delegate-p 'shell-command-to-string)
    (funcall (indirect-function 'shell-command-to-string) command))
   (t
    (let* ((std-buf (and (fboundp 'generate-new-buffer)
                         (generate-new-buffer " *shell-cmd*"))))
      (unwind-protect
          (progn
            (apply #'emacs-process-call-process
                   emacs-process-shell-file-name
                   nil std-buf nil
                   (list emacs-process-shell-command-switch command))
            (and std-buf (with-current-buffer std-buf (buffer-string))))
        (when std-buf (kill-buffer std-buf)))))))

(provide 'emacs-process)

;;; emacs-process.el ends here
