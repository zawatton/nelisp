;;; emacs-process-builtins.el --- Process bridges  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track I (2026-05-03) — Layer 2.
;;
;; Bridges the Emacs unprefixed process API to the substrate in
;; `emacs-process.el'.  Function definitions use a host-aware install
;; gate: host Emacs keeps its C builtins, while standalone NeLisp
;; overwrites bootstrap stubs with the pure-Elisp process substrate.
;; Variables are still gated on `unless (boundp ...)' so host-owned
;; special variables win.
;;
;; Bridged today:
;;   - call-process / call-process-region / process-file
;;   - start-process / start-file-process / make-process
;;   - processp / process-list / process-status /
;;     process-exit-status / process-buffer / process-name
;;   - process-send-string / process-send-eof / delete-process
;;   - shell-command / shell-command-to-string
;;   - shell-file-name / shell-command-switch
;;
;; Deferred:
;;   - filter / sentinel callbacks
;;   - process-coding-system handling
;;   - network processes

;; Shim audit 2026-09-29: intentionally shadows native NeLisp definitions -- process primitives are backed by the nemacs process/event-loop bridge.
;;; Code:

(require 'emacs-process)

;;;; --- function bridges ----------------------------------------------

(defun emacs-process-builtins--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed as an unprefixed bridge."
  (or (and (fboundp 'emacs-standalone-mode-p)
           (emacs-standalone-mode-p))
      (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(defun emacs-process-builtins--check-string (value)
  "Require VALUE to be a string, using the public Emacs error predicate."
  (unless (stringp value)
    (signal 'wrong-type-argument (list 'stringp value))))

(defun emacs-process-builtins--check-call-arguments (program infile destination args)
  "Validate the string arguments to synchronous program execution."
  (emacs-process-builtins--check-string program)
  (when infile
    (emacs-process-builtins--check-string infile))
  (dolist (arg args)
    (emacs-process-builtins--check-string arg))
  (let ((output (if (consp destination) (car destination) destination))
        (stderr (and (consp destination) (consp (cdr destination))
                     (car (cdr destination)))))
    (unless (or (null output) (eq output t) (integerp output)
                (bufferp output))
      (emacs-process-builtins--check-string output))
    (unless (or (null stderr) (eq stderr t))
      (emacs-process-builtins--check-string stderr))))

(defun emacs-process-builtins--region-position (position)
  "Return POSITION as an integer, checking marker and integer arguments."
  (cond ((integerp position) position)
        ((markerp position)
         (or (marker-position position)
             (error "Marker does not point anywhere")))
        (t (signal 'wrong-type-argument
                   (list 'integer-or-marker-p position)))))

(defun emacs-process-builtins--pipe-p (process)
  "Recognize the standalone prelude's pipe connection representation."
  (and (vectorp process) (= (length process) 7)
       (eq (aref process 0) 'pipe-process)))

(defun emacs-process-builtins--resolve-process (process)
  "Resolve PROCESS as a process, process name, buffer, or buffer name."
  (cond
   ((or (processp process) (emacs-process-builtins--pipe-p process)) process)
   ((or (null process) (bufferp process) (stringp process))
    (or (and (stringp process) (get-process process))
        (let ((buffer (cond ((null process) (current-buffer))
                            ((bufferp process) process)
                            (t (get-buffer process)))))
          (unless buffer
            (error "Process %s does not exist" process))
          (or (catch 'found
                (dolist (candidate (process-list))
                  (when (eq (process-buffer candidate) buffer)
                    (throw 'found candidate))))
              (error "Buffer %s has no process" (buffer-name buffer))))))
   (t (signal 'wrong-type-argument (list 'processp process)))))

(defun emacs-process-builtins--initialize-command (process command)
  "Retain the requested COMMAND and the initial output marker for PROCESS."
  (when (emacs-process--process-object-p process)
    (emacs-process--native-set-metadata process :command command)
    (let ((marker (make-marker)) (buffer (process-buffer process)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer (set-marker marker (point-max) buffer)))
      (emacs-process--native-set-metadata process :output-marker marker)))
  process)

(when (emacs-process-builtins--install-function-p 'call-process)
  (defun call-process (program &optional infile destination display &rest args)
    "Run PROGRAM synchronously with INFILE, DESTINATION, DISPLAY and ARGS."
    (emacs-process-builtins--check-call-arguments program infile destination args)
    (apply #'emacs-process-call-process program infile destination display args)))

(when (emacs-process-builtins--install-function-p 'call-process-region)
  (defun call-process-region (start end program &optional delete buffer display
                                   &rest args)
    "Run PROGRAM with input from START..END, or the string START."
    (let (size)
      (cond
       ((stringp start) (setq size (length start)))
       ((null start) (setq size (- (point-max) (point-min))))
       (t
        (let ((begin (emacs-process-builtins--region-position start))
              (finish (emacs-process-builtins--region-position end)))
          (unless (and (<= (point-min) begin) (<= begin (point-max))
                       (<= (point-min) finish) (<= finish (point-max)))
            (signal 'args-out-of-range (list (current-buffer) begin finish)))
          (setq size (abs (- finish begin))))))
      ;; GNU's nonempty-input path validates PROGRAM before spawning the
      ;; child; the empty-input path uses call-process's string predicate.
      (when (and (> size 0) (not (stringp program)))
        (error "Invalid argument 3 of operation ‘call-process-region’"))
      (emacs-process-builtins--check-call-arguments program nil buffer args)
      (apply #'emacs-process-call-process-region
             start end program delete buffer display args))))

(when (emacs-process-builtins--install-function-p 'process-file)
  (defalias 'process-file #'emacs-process-process-file))

(when (emacs-process-builtins--install-function-p 'start-process)
  (defun start-process (name buffer program &rest args)
    "Start PROGRAM with ARGS and retain its original command spelling."
    (emacs-process-builtins--initialize-command
     (apply #'emacs-process-start-process name buffer program args)
     (cons program args))))

(when (emacs-process-builtins--install-function-p 'start-file-process)
  (defun start-file-process (name buffer program &rest args)
    "Start PROGRAM with ARGS, allowing file name handlers."
    (emacs-process-builtins--initialize-command
     (apply #'emacs-process-start-file-process name buffer program args)
     (cons program args))))

(when (emacs-process-builtins--install-function-p 'make-process)
  (defun make-process (&rest args)
    "Create a subprocess described by the keyword options in ARGS."
    (emacs-process-builtins--initialize-command
     (apply #'emacs-process-make-process args) (plist-get args :command))))

(when (emacs-process-builtins--install-function-p 'processp)
  (defalias 'processp #'emacs-process-processp))

(when (emacs-process-builtins--install-function-p 'process-list)
  (defalias 'process-list #'emacs-process-process-list))

(when (emacs-process-builtins--install-function-p 'process-status)
  (defalias 'process-status #'emacs-process-process-status))

(when (emacs-process-builtins--install-function-p 'process-exit-status)
  (defalias 'process-exit-status #'emacs-process-process-exit-status))

(when (emacs-process-builtins--install-function-p 'process-buffer)
  (defalias 'process-buffer #'emacs-process-process-buffer))

(when (emacs-process-builtins--install-function-p 'process-name)
  (defalias 'process-name #'emacs-process-process-name))

(when (emacs-process-builtins--install-function-p 'process-command)
  (defalias 'process-command #'emacs-process-process-command))

(when (emacs-process-builtins--install-function-p 'process-live-p)
  (defalias 'process-live-p #'emacs-process-process-live-p))

(when (emacs-process-builtins--install-function-p 'process-id)
  (defalias 'process-id #'emacs-process-process-id))

(when (emacs-process-builtins--install-function-p 'process-mark)
  (defalias 'process-mark #'emacs-process-process-mark))

(when (emacs-process-builtins--install-function-p 'set-process-filter)
  (defalias 'set-process-filter #'emacs-process-set-process-filter))

(when (emacs-process-builtins--install-function-p 'set-process-sentinel)
  (defalias 'set-process-sentinel #'emacs-process-set-process-sentinel))

(when (emacs-process-builtins--install-function-p 'accept-process-output)
  (defalias 'accept-process-output #'emacs-process-accept-process-output))

(when (emacs-process-builtins--install-function-p 'signal-process)
  (defalias 'signal-process #'emacs-process-signal-process))

(when (emacs-process-builtins--install-function-p 'kill-process)
  (defun kill-process (&optional process current-group)
    "Send SIGKILL to PROCESS, optionally addressing CURRENT-GROUP."
    (setq process (emacs-process-builtins--resolve-process process))
    (when (or (emacs-process-builtins--pipe-p process)
              (emacs-process--network-process-p process))
      (error "Process %s is not a subprocess"
             (if (emacs-process-builtins--pipe-p process)
                 (aref process 1)
               (process-name process))))
    (unless (memq (process-status process) '(run stop))
      (error "Process %s is not active" (process-name process)))
    (if (and (emacs-process--native-process-p process)
             (fboundp 'emacs-process-posix--available-p)
             (emacs-process-posix--available-p))
        (emacs-process-send-control-signal process 'KILL current-group)
      (emacs-process-signal-process process 'KILL))
    process))

(when (emacs-process-builtins--install-function-p 'process-send-string)
  (defalias 'process-send-string #'emacs-process-process-send-string))

(when (emacs-process-builtins--install-function-p 'process-send-eof)
  (defun process-send-eof (&optional process)
    "Close PROCESS's outgoing stream and return the process."
    (setq process (emacs-process-builtins--resolve-process process))
    (if (and (fboundp 'emacs-cc-pipe-process-1--pipe-p)
             (emacs-cc-pipe-process-1--pipe-p process))
        (emacs-cc-pipe-process-1--send-eof process)
      (emacs-process-process-send-eof process)
      process)))

(when (emacs-process-builtins--install-function-p 'delete-process)
  (defalias 'delete-process #'emacs-process-delete-process))

(when (emacs-process-builtins--install-function-p 'shell-command)
  (defalias 'shell-command #'emacs-process-shell-command))

(when (emacs-process-builtins--install-function-p 'shell-command-to-string)
  (defalias 'shell-command-to-string
    #'emacs-process-shell-command-to-string))

;;;; --- process-lines family (subr.el) ---------------------------------

(unless (fboundp 'process-lines-handling-status)
  (defun process-lines-handling-status (program status-handler &rest args)
    "Run PROGRAM with ARGS and return its output as a list of lines.
STATUS-HANDLER, when non-nil, is called with the exit status; when nil a
non-zero exit status signals an error."
    ;; Capture through the buffer-free facade: `with-temp-buffer', `eobp'
    ;; and `forward-line' are macros or layer-specific primitives that break
    ;; when the session swaps between buffer layers.
    (let* ((result (emacs-process-capture-output program args))
           (status (car result))
           (text (cdr result)))
      (if status-handler
          (funcall status-handler status)
        (unless (eq status 0)
          (error "%s exited with status %s" program status)))
      (let ((start 0)
            (lines nil))
        (while (< start (length text))
          (let ((nl (string-match "\n" text start)))
            (setq lines (cons (substring text start (or nl (length text)))
                              lines))
            (setq start (if nl (1+ nl) (length text)))))
        (nreverse lines)))))

(unless (fboundp 'process-lines)
  (defun process-lines (program &rest args)
    "Run PROGRAM with ARGS; return output lines, error on non-zero status."
    (apply #'process-lines-handling-status program nil args)))

(unless (fboundp 'process-lines-ignore-status)
  (defun process-lines-ignore-status (program &rest args)
    "Run PROGRAM with ARGS; return output lines, ignoring the exit status."
    (apply #'process-lines-handling-status program #'ignore args)))

;;;; --- variable bridges ----------------------------------------------

(unless (boundp 'shell-file-name)
  (defvar shell-file-name (emacs-process-resolve-shell-file-name)
    "Track I bridge: path to the shell used by `shell-command'."))

(unless (boundp 'shell-command-switch)
  (defvar shell-command-switch "-c"
    "Track I bridge: the shell flag that invokes a single command."))

(unless (boundp 'explicit-shell-file-name)
  (defvar explicit-shell-file-name nil
    "The explicitly requested inferior shell, or nil for the default shell."))


;;;; --- A19 follow-up: filter/sentinel getters + plist/buffer/query/region --
(when (emacs-process-builtins--install-function-p 'process-filter)
  (defalias 'process-filter #'emacs-process-process-filter))
(when (emacs-process-builtins--install-function-p 'process-sentinel)
  (defalias 'process-sentinel #'emacs-process-process-sentinel))
(when (emacs-process-builtins--install-function-p 'set-process-buffer)
  (defalias 'set-process-buffer #'emacs-process-set-process-buffer))
(when (emacs-process-builtins--install-function-p 'process-plist)
  (defalias 'process-plist #'emacs-process-process-plist))
(when (emacs-process-builtins--install-function-p 'set-process-plist)
  (defalias 'set-process-plist #'emacs-process-set-process-plist))
(when (emacs-process-builtins--install-function-p 'process-query-on-exit-flag)
  (defalias 'process-query-on-exit-flag
    #'emacs-process-process-query-on-exit-flag))
(when (emacs-process-builtins--install-function-p 'set-process-query-on-exit-flag)
  (defalias 'set-process-query-on-exit-flag
    #'emacs-process-set-process-query-on-exit-flag))
(when (emacs-process-builtins--install-function-p 'process-send-region)
  (defalias 'process-send-region #'emacs-process-process-send-region))

(provide 'emacs-process-builtins)

;;; emacs-process-builtins.el ends here
