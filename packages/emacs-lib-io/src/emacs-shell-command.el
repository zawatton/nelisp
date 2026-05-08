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

(defun emacs-shell-command--async-sentinel (command output-buffer)
  "Build a process sentinel for COMMAND writing to OUTPUT-BUFFER."
  (lambda (process event)
    (when (memq (process-status process) '(exit signal))
      (with-current-buffer output-buffer
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (unless (bolp)
            (insert "\n"))
          (insert
           (format "[Process %s %s]" (process-name process) (string-trim-right event)))
          (unless (bolp)
            (insert "\n"))))
      (let ((status (process-exit-status process)))
        (when (and (integerp status) (/= status 0))
          (emacs-shell-command--message "%s"
                                        (emacs-shell-command--exit-summary
                                         command status)))))))

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
     :sentinel (emacs-shell-command--async-sentinel command output-buffer))))

;;;###autoload
(defun shell-command (command &optional output-buffer error-buffer)
  "Execute COMMAND synchronously and display its output.

Output is written to `*Shell Output*' unless OUTPUT-BUFFER names a
different target.  Return the exit status integer from
`call-process'.  When the exit status is non-zero, emit a message and
leave the output in the destination buffer."
  (interactive
   (list (emacs-shell-command--read-command "Shell command: ")))
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
    status))

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
