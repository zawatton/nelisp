;;; nelisp-process-buffer.el --- Buffer channels for standalone processes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Lisp-owned markers and buffer pipe channels over the existing native
;; subprocess transport. Separate stderr uses a private spool because the
;; current native spawn primitive merges fd 1 and fd 2. No new native API is
;; added. The fixed POSIX shell wrapper receives all paths and commands as
;; argv values, never interpolated shell code.

;;; Code:
;; Provided by the marker and process modules at run time.
(declare-function nelisp-process-pid "nelisp-runtime")
(declare-function nelisp-process-object-p "nelisp-runtime")
(declare-function nelisp-buffer-size "nelisp-runtime")
(declare-function nelisp-set-marker "nelisp-runtime")
(declare-function nelisp-marker-position "nelisp-runtime")
(declare-function nelisp-marker-buffer "nelisp-runtime")
(declare-function nelisp-marker-p "nelisp-runtime")
(declare-function nelisp-marker--make "nelisp-runtime")
(defvar nelisp-process-adapter--live)
(defvar nelisp-process-buffer--channels nil)
(defvar nelisp-process-buffer--original-get (symbol-function 'process-get))
(defvar nelisp-process-buffer--original-put (symbol-function 'process-put))
(defvar nelisp-process-buffer--original-p (symbol-function 'processp))
(defvar nelisp-process-buffer--original-status (symbol-function 'process-status))
(defvar nelisp-process-buffer--original-live (symbol-function 'process-live-p))
(defvar nelisp-process-buffer--original-buffer (symbol-function 'process-buffer))
(defvar nelisp-process-buffer--original-exit (symbol-function 'process-exit-status))

;; Extension point for other process representations.  A unit that owns
;; its own process objects registers (PREDICATE . FUNCTION) per public name;
;; the public functions below consult the handlers, newest first, before
;; falling through to their own behavior.  A PREDICATE of nil matches every
;; call; otherwise it receives the first argument.  FUNCTION is called as
;; (FUNCTION NEXT ARGS...), where NEXT continues with the older handlers.
(defvar nelisp-process-kind-handlers nil
  "Alist of (NAME . ((PREDICATE . FUNCTION) ...)), newest handler first.")

(defun nelisp-process-kind-register (name predicate function)
  "Extend public process function NAME with FUNCTION for matching calls."
  (let* ((cell (assq name nelisp-process-kind-handlers))
         (kept nil))
    (unless cell
      (setq cell (list name))
      (setq nelisp-process-kind-handlers
            (cons cell nelisp-process-kind-handlers)))
    (dolist (h (cdr cell))
      (unless (eq (cdr h) function) (push h kept)))
    (setcdr cell (cons (cons predicate function) (nreverse kept))))
  function)

(defun nelisp-process-kind--run (handlers args base)
  (while (and handlers (car (car handlers))
              (not (funcall (car (car handlers)) (car args))))
    (setq handlers (cdr handlers)))
  (if handlers
      (let ((rest (cdr handlers)))
        (apply (cdr (car handlers))
               (lambda (&rest more) (nelisp-process-kind--run rest more base))
               args))
    (apply base args)))

(defun nelisp-process-kind--call (name args base)
  "Call NAME's registered handlers on ARGS, falling through to BASE."
  (nelisp-process-kind--run (cdr (assq name nelisp-process-kind-handlers))
                            args base))

(defun nelisp-process-buffer-p (object)
  "Return non-nil for a Lisp-owned buffer pipe channel."
  (and (vectorp object) (= (length object) 6)
       (eq (aref object 0) 'nelisp-buffer-pipe)))

(defun nelisp-process-buffer-get (process key)
  "Read KEY from PROCESS, preserving native and network metadata."
  (if (nelisp-process-buffer-p process)
      (plist-get (aref process 4) key)
    (funcall nelisp-process-buffer--original-get process key)))

(defun nelisp-process-buffer-put (process key value)
  "Store KEY VALUE on PROCESS, preserving native and network metadata."
  (if (nelisp-process-buffer-p process)
      (aset process 4 (plist-put (aref process 4) key value))
    (funcall nelisp-process-buffer--original-put process key value))
  value)

(defun nelisp-process-buffer-make (&rest options)
  "Create a buffer pipe channel accepting queued writes and metadata.
This channel has no child pid or externally usable OS descriptor."
  (let* ((buffer (plist-get options :buffer))
         (buffer (if (stringp buffer) (get-buffer-create buffer) buffer))
         (channel (vector 'nelisp-buffer-pipe
                          (or (plist-get options :name) "pipe")
                          'open buffer options "")))
    (push channel nelisp-process-buffer--channels)
    channel))

(defun nelisp-process-buffer-mark (process)
  "Return the stable output marker associated with PROCESS."
  (or (process-get process :output-marker)
      (let* ((buffer (process-buffer process))
             (marker (nelisp-marker--make)))
        (when (and buffer (buffer-live-p buffer))
          (with-current-buffer buffer (set-marker marker (point-max) buffer)))
        (process-put process :output-marker marker)
        marker)))

(defun nelisp-process-buffer-insert (process text)
  "Insert TEXT at PROCESS's marker, preserving a point away from it."
  (let ((buffer (process-buffer process)))
    (when (and buffer (buffer-live-p buffer))
      (with-current-buffer buffer
        (let* ((marker (process-mark process))
               (position (marker-position marker))
               (moving (= (point) position))
               (saved (nelisp-marker--make)))
          (set-marker saved (point) buffer)
          (unwind-protect
              (progn
                (goto-char position)
                (insert text)
                (set-marker marker (point) buffer)
                (goto-char (marker-position (if moving marker saved))))
            (set-marker saved nil)))))))

(defun nelisp-process-buffer-drain (channel)
  "Deliver pending bytes to CHANNEL's filter or output buffer."
  (let ((text (aref channel 5))
        (owner (process-get channel :owner)))
    (when owner (nelisp-process-buffer-drain-stderr owner))
    (setq text (aref channel 5))
    (aset channel 5 "")
    (when (> (length text) 0)
      (let ((filter (process-get channel :filter)))
        (if filter (funcall filter channel text)
          (nelisp-process-buffer-insert channel text)))
      t)))

(defun nelisp-process-buffer-delete (channel)
  "Close CHANNEL and detach it from the buffer association registry."
  (nelisp-process-buffer-drain channel)
  (aset channel 2 'closed)
  (setq nelisp-process-buffer--channels
        (delq channel nelisp-process-buffer--channels))
  nil)

(defun nelisp-process-buffer-stderr-command (command stderr &optional name)
  "Return (ARGV CHANNEL SPOOL) for COMMAND with a separate STDERR sink.
NAME is the owning process name; GNU calls a stderr buffer's channel
\"NAME stderr\"."
  (if (null stderr) (list command nil nil)
    (let* ((channel (if (nelisp-process-buffer-p stderr) stderr
                      (nelisp-process-buffer-make :name (if name (format "%s stderr" name) "stderr")
                                                  :buffer stderr)))
           (shell (or (executable-find "sh") (error "Separate stderr requires POSIX sh")))
           (mktemp (or (executable-find "mktemp")
                       (error "Separate stderr requires mktemp")))
           (spool
            (with-temp-buffer
              (unless (eq (call-process mktemp nil t nil "-p"
                                       temporary-file-directory "nelisp-stderr.XXXXXXXXXX") 0)
                (error "Cannot allocate private stderr spool"))
              (string-trim (buffer-string)))))
      (unless (file-exists-p spool) (error "Stderr spool was not created"))
      (list (append (list shell "-c" "err=$1; shift; exec \"$@\" 2>\"$err\""
                          "nelisp-stderr" spool) command)
            channel spool))))

(defun nelisp-process-buffer-drain-stderr (process)
  "Deliver newly spooled stderr before PROCESS's completion sentinel."
  (let ((path (process-get process :stderr-spool))
        (channel (process-get process :stderr-channel)))
    (when (and path channel (file-exists-p path))
      (let* ((text (with-temp-buffer (insert-file-contents path) (buffer-string)))
             (offset (or (process-get process :stderr-offset) 0)))
        (when (> (length text) offset)
          (process-put process :stderr-offset (length text))
          (let ((chunk (substring text offset)))
            ;; Direct dispatch avoids recursive owner polling on this channel.
            (let ((filter (process-get channel :filter)))
              (if filter (funcall filter channel chunk)
                (nelisp-process-buffer-insert channel chunk)))
            t))))))

(defun nelisp-process-buffer-close-stderr (process)
  "Remove PROCESS's private spool and close its owned stderr channel."
  (let ((path (process-get process :stderr-spool))
        (channel (process-get process :stderr-channel)))
    (when path
      (when (file-exists-p path) (delete-file path))
      (process-put process :stderr-spool nil))
    (when channel
      (let ((was-open (eq (aref channel 2) 'open)))
        (nelisp-process-buffer-delete channel)
        ;; GNU notifies a pipe's sentinel (default: inserts a message) when
        ;; the child's output ends.
        (when was-open
          (let ((sentinel (process-get channel :sentinel)))
            (if (functionp sentinel)
                (funcall sentinel channel "finished\n")
              (nelisp-process-buffer-insert
               channel (format "\nProcess %s finished\n" (aref channel 1))))))))))

(when (fboundp 'nl-ffi-call)
  ;; Channel markers use the runtime's marker API and follow ordinary edits.
  (defun process-mark (process)
    (nelisp-process-kind--call 'process-mark (list process)
                               #'nelisp-process-buffer-mark))
  (defun make-pipe-process (&rest options)
    (nelisp-process-kind--call 'make-pipe-process options
                               #'nelisp-process-buffer-make))
  (defun process-get (process key)
    (nelisp-process-kind--call 'process-get (list process key)
                               #'nelisp-process-buffer-get))
  (defun process-put (process key value)
    (nelisp-process-kind--call 'process-put (list process key value)
                               #'nelisp-process-buffer-put))
  (defun processp (object)
    (nelisp-process-kind--call
     'processp (list object)
     (lambda (object)
       (or (nelisp-process-buffer-p object)
           (funcall nelisp-process-buffer--original-p object)))))
  (defun process-status (process)
    (nelisp-process-kind--call
     'process-status (list process)
     (lambda (process)
       (if (nelisp-process-buffer-p process) (aref process 2)
         (funcall nelisp-process-buffer--original-status process)))))
  (defun process-live-p (process)
    (if (nelisp-process-buffer-p process) (eq (aref process 2) 'open)
      (funcall nelisp-process-buffer--original-live process)))
  (defun process-buffer (process)
    (nelisp-process-kind--call
     'process-buffer (list process)
     (lambda (process)
       (if (nelisp-process-buffer-p process) (aref process 3)
         (funcall nelisp-process-buffer--original-buffer process)))))
  (defun process-exit-status (process)
    (nelisp-process-kind--call
     'process-exit-status (list process)
     (lambda (process)
       (if (nelisp-process-buffer-p process) 0
         (funcall nelisp-process-buffer--original-exit process)))))
  (unless (fboundp 'process-id)
    (defun process-id (process)
      "Return the existing native subprocess pid, or nil for a buffer pipe."
      (cond ((nelisp-process-buffer-p process) nil)
            ((nelisp-process-object-p process) (nelisp-process-pid process))
            ((processp process) nil)
            (t (signal 'wrong-type-argument (list 'processp process))))))
  (defun set-process-buffer (process buffer)
    "Attach PROCESS to BUFFER without changing the owning buffer API."
    (nelisp-process-kind--call
     'set-process-buffer (list process buffer)
     (lambda (process buffer)
       (let ((buffer (if (stringp buffer) (get-buffer-create buffer) buffer)))
         (unless (or (null buffer) (bufferp buffer))
           (signal 'wrong-type-argument (list 'bufferp buffer)))
         (when (nelisp-process-buffer-p process) (aset process 3 buffer))
         (process-put process :buffer buffer)
         (process-put process :output-marker nil)
         (process-mark process)
         buffer))))
  (defun get-buffer-process (buffer)
    "Return the live buffer channel or native subprocess owning BUFFER."
    (let ((target (and buffer (get-buffer buffer))))
      (when (and target (buffer-live-p target))
        (catch 'found
          ;; C-core processes live in the public registry.  Native buffer
          ;; channels and adapter transports also retain their own owners.
          (dolist (process (append (process-list)
                                  nelisp-process-buffer--channels
                                  nelisp-process-adapter--live))
            (when (eq target (process-buffer process))
              (throw 'found process)))
          nil)))))

(provide 'nelisp-process-buffer)
;;; nelisp-process-buffer.el ends here
