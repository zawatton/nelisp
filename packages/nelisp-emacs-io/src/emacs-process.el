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
;; - Under standalone NeLisp the substrate currently signals
;;   `emacs-process-not-implemented' — a future phase will wire
;;   NeLisp's process primitive (`anvil-process-spawn' etc.) here.
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

;;;; --- delegate plumbing ---------------------------------------------

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

(defun emacs-process--delegate (sym args)
  "Apply SYM to ARGS through the host binding or standalone primitive.

Lookup order:
  1. host-mode + host has a non-shadow binding → apply host.
  2. a standalone primitive is registered for SYM → dispatch.
  3. otherwise signal `emacs-process-not-implemented'.

Step 2 is what lets a future NeLisp primitive (= via
`emacs-standalone-register-primitive') replace the signal without
touching this file."
  (cond
   ((and (not (emacs-standalone-mode-p))
         (emacs-process--delegate-p sym))
    (apply (indirect-function sym) args))
   ((emacs-standalone-has-primitive-p sym)
    (emacs-standalone-call-primitive sym args))
   (t (signal 'emacs-process-not-implemented (list sym)))))

;;;; --- synchronous: call-process / call-process-region --------------

(defun emacs-process-call-process (program &optional infile destination
                                           display &rest args)
  "Synchronous program execution.  See `call-process' for semantics.

When no host binding and no standalone primitive exists, degrade
GRACEFULLY: return a non-zero exit code (1, = \"program failed\")
instead of signalling `emacs-process-not-implemented'.  Load-time
feature/tool detection in vendor packages (e.g. org.el probing for
external tools via `(eq 0 (call-process ...))') then treats the tool
as unavailable and proceeds, rather than aborting the whole load.
A real implementation (via the NeLisp OS surface fork/execve) can
later replace this through `emacs-standalone-register-primitive'."
  (condition-case nil
      (emacs-process--delegate 'call-process
                               (cons program (cons infile (cons destination
                                                                (cons display args)))))
    (emacs-process-not-implemented 1)))

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
  (emacs-process--delegate 'start-process
                           (cons name (cons buffer (cons program program-args)))))

(defun emacs-process-make-process (&rest plist)
  "Start a process described by PLIST (= keyword/value pairs)."
  (emacs-process--delegate 'make-process plist))

;;;; --- predicates / accessors ---------------------------------------

(defun emacs-process-processp (object)
  "Return non-nil if OBJECT is a process."
  (cond
   ((emacs-process--delegate-p 'processp)
    (funcall (indirect-function 'processp) object))
   (t nil)))

(defun emacs-process-process-list ()
  "Return the list of currently-active processes."
  (cond
   ((emacs-process--delegate-p 'process-list)
    (funcall (indirect-function 'process-list)))
   (t nil)))

(defun emacs-process-process-status (process)
  "Return PROCESS's status symbol."
  (emacs-process--delegate 'process-status (list process)))

(defun emacs-process-process-exit-status (process)
  "Return PROCESS's exit-status integer."
  (emacs-process--delegate 'process-exit-status (list process)))

(defun emacs-process-process-buffer (process)
  "Return PROCESS's associated buffer."
  (emacs-process--delegate 'process-buffer (list process)))

(defun emacs-process-process-name (process)
  "Return PROCESS's name string."
  (emacs-process--delegate 'process-name (list process)))

(defun emacs-process-process-command (process)
  "Return PROCESS's command (program + args) as a list."
  (emacs-process--delegate 'process-command (list process)))

(defun emacs-process-process-live-p (process)
  "Return non-nil if PROCESS is alive (status = run/open/listen/connect/stop)."
  (cond
   ((emacs-process--delegate-p 'process-live-p)
    (funcall (indirect-function 'process-live-p) process))
   (t
    ;; Standalone fallback: derive from process-status if available.
    (let ((s (and (or (fboundp 'process-status)
                      (fboundp 'emacs-process-process-status))
                  (emacs-process-process-status process))))
      (memq s '(run open listen connect stop))))))

(defun emacs-process-process-id (process)
  "Return PROCESS's OS pid integer (or nil if not yet running)."
  (emacs-process--delegate 'process-id (list process)))

(defun emacs-process-process-mark (process)
  "Return PROCESS's filter mark (used by buffer-attached output)."
  (emacs-process--delegate 'process-mark (list process)))

(defun emacs-process-set-process-filter (process filter)
  "Install FILTER as PROCESS's stdout/stderr callback."
  (emacs-process--delegate 'set-process-filter (list process filter)))

(defun emacs-process-set-process-sentinel (process sentinel)
  "Install SENTINEL as PROCESS's lifecycle callback."
  (emacs-process--delegate 'set-process-sentinel (list process sentinel)))

(defun emacs-process-accept-process-output (&optional process seconds millisec just-this-one)
  "Block until PROCESS produces output or SECONDS pass.

Same calling convention as Emacs's `accept-process-output'.  When
the host primitive is available, delegate.  Otherwise the
substrate currently signals `emacs-process-not-implemented' —
async I/O without an event loop is out of γ-MVP scope."
  (emacs-process--delegate 'accept-process-output
                           (list process seconds millisec just-this-one)))

(defun emacs-process-signal-process (process-or-pid signum)
  "Send SIGNUM (number or symbol) to PROCESS-OR-PID."
  (emacs-process--delegate 'signal-process (list process-or-pid signum)))

(defun emacs-process-kill-process (process)
  "Send SIGKILL to PROCESS.

Equivalent to `(signal-process PROCESS \\='KILL)'; provided as a
top-level alias for parity with the Emacs API."
  (cond
   ((emacs-process--delegate-p 'kill-process)
    (funcall (indirect-function 'kill-process) process))
   (t
    (emacs-process-signal-process process 'KILL))))

;;;; --- I/O + lifecycle ----------------------------------------------

(defun emacs-process-process-send-string (process string)
  "Send STRING to PROCESS's stdin."
  (emacs-process--delegate 'process-send-string (list process string)))

(defun emacs-process-process-send-eof (&optional process)
  "Send EOF to PROCESS's stdin."
  (emacs-process--delegate 'process-send-eof (list process)))

(defun emacs-process-delete-process (process)
  "Kill PROCESS."
  (emacs-process--delegate 'delete-process (list process)))

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
