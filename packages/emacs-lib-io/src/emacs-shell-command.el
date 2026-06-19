;;; emacs-shell-command.el --- Interactive shell-command layer  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Interactive shell-command UI for docs/design/02-v01-daily-driver.org
;; §3.4.1 (M4.1).  This layer intentionally stays thin:
;;
;; - sync commands dispatch through the existing process substrate's
;;   `call-process' / `call-process-region';
;; - async commands dispatch through `make-process';
;; - the module owns only the UI-level buffer selection, replacement,
;;   status messaging, and key installation for M-! / M-| / M-&.

;;; Code:

(require 'cl-lib)
(require 'emacs-process-builtins)

(defgroup emacs-shell-command nil
  "Interactive shell-command helpers."
  :group 'processes)

(defconst emacs-shell-command-output-buffer-name "*Shell Output*"
  "Default output buffer for synchronous shell commands.")

(defconst emacs-shell-command-async-buffer-name "*Async Shell Command*"
  "Default output buffer for asynchronous shell commands.")

(defvar emacs-shell-command-last-async-process nil
  "Most recent process started by `async-shell-command'.")

;;;; GUI bridge adapter -------------------------------------------------

(defvar emacs-shell-command-gui-backend nil
  "PLIST of GUI bridge shell/project backend callbacks.
The runtime owns shell/project command semantics; the backend owns
transport files and bridge buffer state.")

;;;###autoload
(defun emacs-shell-command-gui-register-backend (&rest backend)
  "Register BACKEND plist for GUI shell/project command helpers."
  (setq emacs-shell-command-gui-backend backend))

(defun emacs-shell-command-gui--backend-call (key &rest args)
  "Call GUI shell backend function KEY with ARGS when registered."
  (let ((fn (and emacs-shell-command-gui-backend
                 (plist-get emacs-shell-command-gui-backend key))))
    (when fn
      (apply fn args))))

(defun emacs-shell-command-gui--arg ()
  "Return current GUI bridge shell command argument."
  (or (emacs-shell-command-gui--backend-call :arg) ""))

(defun emacs-shell-command-gui--set-arg (arg)
  "Set current GUI bridge shell command ARG."
  (emacs-shell-command-gui--backend-call :set-arg arg))

(defun emacs-shell-command-gui--set-status (status)
  "Set GUI bridge STATUS."
  (emacs-shell-command-gui--backend-call :set-status status))

(defun emacs-shell-command-gui--transport-path (name)
  "Return bridge transport path NAME."
  (or (emacs-shell-command-gui--backend-call :transport-path name) ""))

(defun emacs-shell-command-gui--write-file (path text)
  "Write TEXT to PATH through the GUI backend."
  (emacs-shell-command-gui--backend-call :write-file path text))

(defun emacs-shell-command-gui--read-file (path)
  "Read PATH through the GUI backend."
  (or (emacs-shell-command-gui--backend-call :read-file path) ""))

(defun emacs-shell-command-gui--save-current-buffer-state ()
  "Ask the GUI backend to persist current buffer state."
  (emacs-shell-command-gui--backend-call :save-current-buffer-state))

(defun emacs-shell-command-gui--select-buffer (name read-only)
  "Ask GUI backend to select buffer NAME with READ-ONLY flag."
  (emacs-shell-command-gui--backend-call :select-buffer name read-only))

(defun emacs-shell-command-gui--buffer-string ()
  "Return current GUI bridge buffer string."
  (or (emacs-shell-command-gui--backend-call :buffer-string) ""))

(defun emacs-shell-command-gui--set-buffer-string (text)
  "Set current GUI bridge buffer string to TEXT."
  (emacs-shell-command-gui--backend-call :set-buffer-string text))

(defun emacs-shell-command-gui--set-compilation-buffer-string (text)
  "Set GUI bridge compilation buffer string to TEXT."
  (emacs-shell-command-gui--backend-call
   :set-compilation-buffer-string text))

(defun emacs-shell-command-gui--show-compilation-buffer ()
  "Ask GUI backend to show the compilation buffer."
  (emacs-shell-command-gui--backend-call :show-compilation-buffer))

(defun emacs-shell-command-gui--project-command-directory ()
  "Return current GUI project command directory."
  (or (emacs-shell-command-gui--backend-call :project-command-directory)
      "."))

(defun emacs-shell-command-gui-shell-quote-argument (text)
  "Quote TEXT for POSIX shell use."
  (let ((index 0)
        (out "'")
        (text (or text "")))
    (while (< index (length text))
      (let ((ch (aref text index)))
        (if (= ch ?')
            (setq out (concat out "'\\''"))
          (setq out (concat out (substring text index (1+ index))))))
      (setq index (1+ index)))
    (concat out "'")))

(defun emacs-shell-command-gui-project-shell-command-text (&optional command)
  "Return shell COMMAND wrapped in a cd to the current project directory."
  (let ((directory (emacs-shell-command-gui--project-command-directory)))
    (concat "cd "
            (emacs-shell-command-gui-shell-quote-argument directory)
            " && "
            (or command (emacs-shell-command-gui--arg)))))

(defun emacs-shell-command-gui--call-process-available-p ()
  "Return non-nil when a GUI bridge process substrate is available."
  (or (fboundp 'call-process)
      (and emacs-shell-command-gui-backend
           (plist-get emacs-shell-command-gui-backend :call-process))))

(defun emacs-shell-command-gui--call-process (&rest args)
  "Call the GUI bridge process substrate with ARGS."
  (let ((fn (and emacs-shell-command-gui-backend
                 (plist-get emacs-shell-command-gui-backend :call-process))))
    (if fn
        (apply fn args)
      (apply #'call-process args))))

(defun emacs-shell-command-gui--status-to-bridge (status)
  "Publish STATUS to the GUI bridge and return STATUS."
  (emacs-shell-command-gui--set-status
   (if (and (integerp status) (= status 0)) "ok" "error"))
  status)

(defun emacs-shell-command-gui-shell-command-core (command output-name
                                                         unavailable-text)
  "Run shell COMMAND into OUTPUT-NAME and current GUI output buffer.
UNAVAILABLE-TEXT is used when no process substrate is available."
  (let ((output-file
         (emacs-shell-command-gui--transport-path output-name))
        (status 1))
    (emacs-shell-command-gui--save-current-buffer-state)
    (emacs-shell-command-gui--write-file output-file "")
    (emacs-shell-command-gui--select-buffer "*Shell Command Output*" t)
    (if (and (emacs-shell-command-gui--call-process-available-p)
             (not (equal command "")))
        (setq status
              (emacs-shell-command-gui--call-process
               "/bin/sh" nil nil nil "-c"
               (concat "exec > " output-file " 2>&1\n"
                       command)))
      (setq status 127))
    (if (equal command "")
        (emacs-shell-command-gui--set-buffer-string "")
      (if (emacs-shell-command-gui--call-process-available-p)
          (emacs-shell-command-gui--set-buffer-string
           (emacs-shell-command-gui--read-file output-file))
        (emacs-shell-command-gui--set-buffer-string unavailable-text)))
    (emacs-shell-command-gui--status-to-bridge status)
    (emacs-shell-command-gui--save-current-buffer-state)
    status))

;;;###autoload
(defun emacs-shell-command-gui-shell-command ()
  "Run `shell-command' through the GUI bridge backend."
  (emacs-shell-command-gui-shell-command-core
   (emacs-shell-command-gui--arg)
   "nemacs-shell-command-output"
   "shell-command unavailable: no call-process substrate\n"))

;;;###autoload
(defun emacs-shell-command-gui-project-shell-command ()
  "Run `project-shell-command' through the GUI bridge backend."
  (let ((original (emacs-shell-command-gui--arg))
        (directory (emacs-shell-command-gui--project-command-directory))
        (status 1))
    (unwind-protect
        (progn
          (emacs-shell-command-gui--set-arg
           (emacs-shell-command-gui-project-shell-command-text original))
          (setq status (emacs-shell-command-gui-shell-command))
          (emacs-shell-command-gui--set-buffer-string
           (concat "Project directory: "
                   directory
                   "\n"
                   (emacs-shell-command-gui--buffer-string)))
          (emacs-shell-command-gui--save-current-buffer-state)
          status)
      (emacs-shell-command-gui--set-arg original))))

;;;###autoload
(defun emacs-shell-command-gui-async-shell-command ()
  "Run `async-shell-command' through the GUI bridge backend."
  (let ((command (emacs-shell-command-gui--arg))
        (output-file
         (emacs-shell-command-gui--transport-path
          "nemacs-async-shell-command-output"))
        (status 1))
    (if (and (not (equal command ""))
             (emacs-shell-command-gui--backend-call
              :async-native-available-p))
        (or (emacs-shell-command-gui--backend-call
             :async-start-native command)
            0)
      (progn
        (emacs-shell-command-gui--save-current-buffer-state)
        (emacs-shell-command-gui--write-file output-file "")
        (emacs-shell-command-gui--select-buffer
         emacs-shell-command-async-buffer-name t)
        (if (and (emacs-shell-command-gui--call-process-available-p)
                 (not (equal command "")))
            (setq status
                  (emacs-shell-command-gui--call-process
                   "/bin/sh" nil output-file nil "-c"
                   command))
          (setq status 127))
        (if (emacs-shell-command-gui--call-process-available-p)
            (emacs-shell-command-gui--set-buffer-string
             (emacs-shell-command-gui--read-file output-file))
          (emacs-shell-command-gui--set-buffer-string
           "async-shell-command unavailable: no call-process substrate\n"))
        (emacs-shell-command-gui--status-to-bridge status)
        (emacs-shell-command-gui--save-current-buffer-state)
        status))))

;;;###autoload
(defun emacs-shell-command-gui-project-async-shell-command ()
  "Run `project-async-shell-command' through the GUI bridge backend."
  (let ((original (emacs-shell-command-gui--arg))
        (directory (emacs-shell-command-gui--project-command-directory))
        (status 1))
    (unwind-protect
        (progn
          (emacs-shell-command-gui--set-arg
           (emacs-shell-command-gui-project-shell-command-text original))
          (setq status (emacs-shell-command-gui-async-shell-command))
          (emacs-shell-command-gui--set-buffer-string
           (concat "Project directory: "
                   directory
                   "\n"
                   (emacs-shell-command-gui--buffer-string)))
          (emacs-shell-command-gui--save-current-buffer-state)
          status)
      (emacs-shell-command-gui--set-arg original))))

;;;###autoload
(defun emacs-shell-command-gui-project-compile ()
  "Run `project-compile' through the GUI bridge backend."
  (let* ((command (emacs-shell-command-gui--arg))
         (directory (emacs-shell-command-gui--project-command-directory))
         (output-file
          (emacs-shell-command-gui--transport-path
           "nemacs-project-compile-output"))
         (status 1))
    (emacs-shell-command-gui--save-current-buffer-state)
    (emacs-shell-command-gui--write-file output-file "")
    (if (and (emacs-shell-command-gui--call-process-available-p)
             (not (equal command "")))
        (setq status
              (emacs-shell-command-gui--call-process
               "/bin/sh" nil nil nil "-c"
               (concat "cd "
                       (emacs-shell-command-gui-shell-quote-argument
                        directory)
                       " && exec > "
                       output-file
                       " 2>&1\n"
                       command)))
      (setq status 127))
    (emacs-shell-command-gui--set-compilation-buffer-string
     (concat "Project directory: "
             directory
             "\n"
             "Compile command: "
             command
             "\n"
             "Exit status: "
             (number-to-string status)
             "\n\n"
             (if (emacs-shell-command-gui--call-process-available-p)
                 (emacs-shell-command-gui--read-file output-file)
               "project-compile unavailable: no call-process substrate\n")))
    (if (equal command "")
        (emacs-shell-command-gui--set-status "error")
      (emacs-shell-command-gui--set-status "ok"))
    (emacs-shell-command-gui--show-compilation-buffer)
    status))

;;;###autoload
(defun emacs-shell-command-gui-project-interactive-shell-buffer
    (name kind prompt)
  "Show project shell placeholder buffer NAME for KIND with PROMPT."
  (let ((directory (emacs-shell-command-gui--project-command-directory)))
    (emacs-shell-command-gui--save-current-buffer-state)
    (emacs-shell-command-gui--select-buffer name nil)
    (emacs-shell-command-gui--set-buffer-string
     (concat "Project directory: "
             directory
             "\n"
             kind
             " process is not attached yet; this buffer preserves the project shell target for the GUI bridge.\n\n"
             prompt))
    (emacs-shell-command-gui--backend-call
     :set-point (length (emacs-shell-command-gui--buffer-string)))
    (emacs-shell-command-gui--set-status "ok")
    (emacs-shell-command-gui--save-current-buffer-state)
    (emacs-shell-command-gui--backend-call :apply-display-prefix-same-window)
    name))

;; Snapshot the host runtime's `shell-command' before this module
;; installs its polyfill.  Host Emacs's C `shell-command-to-string'
;; calls `shell-command' internally with `t' as OUTPUT-BUFFER (=
;; "insert at point in current buffer"), which our polyfill cannot
;; honour without the C subr's buffer-narrowing setup.  Saving the
;; original lets us delegate transparently in that one shape.
(defvar emacs-shell-command--orig-shell-command
  (and (fboundp 'shell-command)
       (subrp (symbol-function 'shell-command))
       (symbol-function 'shell-command))
  "Original `shell-command' subr captured at load time, or nil.")

(defun emacs-shell-command--read-command (prompt)
  "Read a shell command with PROMPT."
  (if (fboundp 'read-shell-command)
      (read-shell-command prompt)
    (read-string prompt)))

(defun emacs-shell-command--buffer-name (buffer default-name)
  "Resolve BUFFER designator, defaulting to DEFAULT-NAME."
  (cond
   ((null buffer) default-name)
   ((bufferp buffer) (buffer-name buffer))
   ((stringp buffer) buffer)
   (t (signal 'wrong-type-argument (list '(or null bufferp stringp) buffer)))))

(defun emacs-shell-command--get-buffer (buffer default-name)
  "Return output buffer designated by BUFFER or DEFAULT-NAME."
  (get-buffer-create
   (emacs-shell-command--buffer-name buffer default-name)))

(defun emacs-shell-command--prepare-buffer (buffer erase-p)
  "Prepare BUFFER for command output.
When ERASE-P is non-nil, erase first.  Return BUFFER."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (when erase-p
        (erase-buffer))
      (goto-char (point-max))))
  buffer)

(defun emacs-shell-command--message (fmt &rest args)
  "Emit a shell-command status message using FMT and ARGS."
  (apply #'message (concat "Shell command: " fmt) args))

(defun emacs-shell-command--exit-summary (command status)
  "Return a concise summary string for COMMAND and STATUS."
  (format "`%s' exited with status %s" command status))

(defun emacs-shell-command--call-shell (command destination &optional error-buffer)
  "Run COMMAND synchronously, sending stdout to DESTINATION.
ERROR-BUFFER is currently accepted for interface parity but is not
split from stdout in this MVP."
  (ignore error-buffer)
  (call-process shell-file-name nil destination nil
                shell-command-switch command))

(defun emacs-shell-command--call-shell-region (start end command destination)
  "Run COMMAND synchronously on region START..END into DESTINATION."
  (call-process-region start end shell-file-name nil destination nil
                       shell-command-switch command))

(defun emacs-shell-command--replace-region (start end text)
  "Replace region START..END with TEXT."
  (save-excursion
    (goto-char start)
    (delete-region start end)
    (insert text)))

(defun emacs-shell-command--async-sentinel-fn (process event)
  "Process sentinel for async shell commands.
Recovers OUTPUT-BUFFER + COMMAND via `process-buffer' / `process-name'
instead of closure capture, since NeLisp closure semantics differ from
host Emacs and earlier closure-based sentinel raised
\"Symbol's value as variable is void: output-buffer\" at sentinel firing."
  (when (memq (process-status process) '(exit signal))
    (let ((output-buffer (process-buffer process))
          (proc-name (process-name process)))
      (when (and output-buffer (buffer-live-p output-buffer))
        (with-current-buffer output-buffer
          (let ((inhibit-read-only t))
            (goto-char (point-max))
            (unless (bolp)
              (insert "\n"))
            (insert
             (format "[Process %s %s]" proc-name (string-trim-right event)))
            (unless (bolp)
              (insert "\n")))))
      (let ((status (process-exit-status process)))
        (when (and (integerp status) (/= status 0))
          (let ((command (if (and (stringp proc-name)
                                  (string-match "\\`async-shell-command<\\(.*\\)>\\'"
                                                proc-name))
                             (match-string 1 proc-name)
                           proc-name)))
            (emacs-shell-command--message "%s"
                                          (emacs-shell-command--exit-summary
                                           command status))))))))

(defun emacs-shell-command--make-process (command output-buffer &optional error-buffer)
  "Start COMMAND asynchronously, writing into OUTPUT-BUFFER.
ERROR-BUFFER is accepted for interface parity but is ignored in this
MVP because the host process bridge does not yet split stderr."
  (ignore error-buffer)
  (let ((proc-name (format "async-shell-command<%s>" command)))
    (make-process
     :name proc-name
     :buffer output-buffer
     :command (list shell-file-name shell-command-switch command)
     :noquery t
     :sentinel #'emacs-shell-command--async-sentinel-fn)))

;;;###autoload
(defun shell-command (command &optional output-buffer error-buffer)
  "Execute COMMAND synchronously and display its output.

Output is written to `*Shell Output*' unless OUTPUT-BUFFER names a
different target.  Return the exit status integer from
`call-process'.  When the exit status is non-zero, emit a message and
leave the output in the destination buffer."
  (interactive
   (list (emacs-shell-command--read-command "Shell command: ")))
  (if (and (eq output-buffer t) emacs-shell-command--orig-shell-command)
      ;; Host C `shell-command-to-string' passes `t' to mean "insert
      ;; output at point in the current buffer," which our polyfill
      ;; cannot honour without reproducing the C subr's narrowing.
      ;; Delegate that one shape to the original subr so the host's
      ;; `shell-command-to-string' keeps working when our polyfill
      ;; shadows the public symbol.
      (funcall emacs-shell-command--orig-shell-command
               command output-buffer error-buffer)
    (let* ((buffer (emacs-shell-command--prepare-buffer
                    (emacs-shell-command--get-buffer
                     output-buffer
                     emacs-shell-command-output-buffer-name)
                    t))
           (status (emacs-shell-command--call-shell command buffer error-buffer)))
      (when (and (integerp status) (/= status 0))
        (emacs-shell-command--message "%s"
                                      (emacs-shell-command--exit-summary
                                       command status)))
      (when (called-interactively-p 'interactive)
        (display-buffer buffer))
      status)))

;;;###autoload
(defun shell-command-on-region (start end command
                                      &optional output-buffer replace-flag
                                      error-buffer display-error-buffer
                                      region-noncontiguous-p)
  "Send region START..END to COMMAND on standard input.

Command output goes to `*Shell Output*' unless OUTPUT-BUFFER is
specified.  When REPLACE-FLAG is non-nil, replace the region with the
command output.  Return the exit status integer."
  (interactive
   (list (region-beginning)
         (region-end)
         (emacs-shell-command--read-command "Shell command on region: ")
         nil
         current-prefix-arg))
  (ignore error-buffer display-error-buffer region-noncontiguous-p)
  (let* ((buffer (emacs-shell-command--prepare-buffer
                  (emacs-shell-command--get-buffer
                   output-buffer
                   emacs-shell-command-output-buffer-name)
                  t))
         (status (emacs-shell-command--call-shell-region start end command buffer)))
    (when replace-flag
      (emacs-shell-command--replace-region
       start end
       (with-current-buffer buffer
         (buffer-string))))
    (when (and (integerp status) (/= status 0))
      (emacs-shell-command--message "%s"
                                    (emacs-shell-command--exit-summary
                                     command status)))
    (when (and (called-interactively-p 'interactive)
               (not replace-flag))
      (display-buffer buffer))
    status))

;;;###autoload
(defun async-shell-command (command &optional output-buffer error-buffer)
  "Execute COMMAND asynchronously and return the process object.

Output is appended to `*Async Shell Command*' unless OUTPUT-BUFFER is
supplied."
  (interactive
   (list (emacs-shell-command--read-command "Async shell command: ")))
  (let* ((buffer (emacs-shell-command--prepare-buffer
                  (emacs-shell-command--get-buffer
                   output-buffer
                   emacs-shell-command-async-buffer-name)
                  nil))
         (process (emacs-shell-command--make-process
                   command buffer error-buffer)))
    (setq emacs-shell-command-last-async-process process)
    (when (called-interactively-p 'interactive)
      (display-buffer buffer))
    process))

(defun emacs-shell-command--install-bindings ()
  "Install M-! / M-| / M-& into `current-global-map'."
  (let ((map (and (fboundp 'current-global-map) (current-global-map))))
    (when (and map (fboundp 'define-key) (fboundp 'kbd))
      (define-key map (kbd "M-!") #'shell-command)
      (define-key map (kbd "M-|") #'shell-command-on-region)
      (define-key map (kbd "M-&") #'async-shell-command))))

(emacs-shell-command--install-bindings)

(provide 'emacs-shell-command)

;;; emacs-shell-command.el ends here
