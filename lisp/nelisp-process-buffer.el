;;; nelisp-process-buffer.el --- Buffer channels for standalone processes -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:
;; Lisp-owned markers and buffer pipe channels over the existing native
;; subprocess transport. Separate stderr uses a private spool because the
;; current native spawn primitive merges fd 1 and fd 2. No new native API is
;; added. The fixed POSIX shell wrapper receives all paths and commands as
;; argv values, never interpolated shell code.

;;; Code:
(defvar nelisp-process-buffer--channels nil)
(defvar nelisp-process-buffer--original-get (symbol-function 'process-get))
(defvar nelisp-process-buffer--original-put (symbol-function 'process-put))
(defvar nelisp-process-buffer--original-p (symbol-function 'processp))
(defvar nelisp-process-buffer--original-status (symbol-function 'process-status))
(defvar nelisp-process-buffer--original-live (symbol-function 'process-live-p))
(defvar nelisp-process-buffer--original-buffer (symbol-function 'process-buffer))
(defvar nelisp-process-buffer--original-exit (symbol-function 'process-exit-status))
(defvar nelisp-process-buffer--original-markerp (symbol-function 'markerp))

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

(defun nelisp-process-buffer-stderr-command (command stderr)
  "Return (ARGV CHANNEL SPOOL) for COMMAND with a separate STDERR sink."
  (if (null stderr) (list command nil nil)
    (let* ((channel (if (nelisp-process-buffer-p stderr) stderr
                      (nelisp-process-buffer-make :name "stderr" :buffer stderr)))
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
    (when channel (nelisp-process-buffer-delete channel))))

(when (fboundp 'nl-ffi-call)
  ;; Use the runtime's existing marker representation and edit tracking.
  (defun markerp (object)
    (or (nelisp-marker-p object)
        (funcall nelisp-process-buffer--original-markerp object)))
  (defun marker-position (marker)
      (unless (nelisp-marker-p marker)
        (signal 'wrong-type-argument (list 'markerp marker)))
      (and (nelisp-marker-buffer marker) (nelisp-marker-position marker)))
  (defun marker-buffer (marker)
      (unless (nelisp-marker-p marker)
        (signal 'wrong-type-argument (list 'markerp marker)))
      (nelisp-marker-buffer marker))
  (when (or (not (fboundp 'set-marker)) (get 'set-marker 'emacs-stub-bulk))
    (defun set-marker (marker position &optional buffer)
      "Place MARKER in BUFFER, defaulting to the current buffer."
      (unless (nelisp-marker-p marker)
        (signal 'wrong-type-argument (list 'markerp marker)))
      (when (nelisp-marker-p position)
        (setq position (marker-position position)))
      (unless (or (null position) (integerp position))
        (signal 'wrong-type-argument (list 'integer-or-marker-p position)))
      (let ((target (or buffer (current-buffer))))
        (unless (bufferp target) (signal 'wrong-type-argument (list 'bufferp target)))
        (if (or (null position) (not (buffer-live-p target)))
            (nelisp-set-marker marker nil)
          (nelisp-set-marker marker
                             (max 1 (min (1+ (nelisp-buffer-size target)) position)) target)))
      marker))
  (dolist (name '(markerp marker-position marker-buffer set-marker))
    (put name 'emacs-stub-bulk nil))
  (defalias 'process-mark #'nelisp-process-buffer-mark)
  (defalias 'make-pipe-process #'nelisp-process-buffer-make)
  (defalias 'process-get #'nelisp-process-buffer-get)
  (defalias 'process-put #'nelisp-process-buffer-put)
  (defun processp (object)
    (or (nelisp-process-buffer-p object)
        (funcall nelisp-process-buffer--original-p object)))
  (defun process-status (process)
    (if (nelisp-process-buffer-p process) (aref process 2)
      (funcall nelisp-process-buffer--original-status process)))
  (defun process-live-p (process)
    (if (nelisp-process-buffer-p process) (eq (aref process 2) 'open)
      (funcall nelisp-process-buffer--original-live process)))
  (defun process-buffer (process)
    (if (nelisp-process-buffer-p process) (aref process 3)
      (funcall nelisp-process-buffer--original-buffer process)))
  (defun process-exit-status (process)
    (if (nelisp-process-buffer-p process) 0
      (funcall nelisp-process-buffer--original-exit process)))
  (unless (fboundp 'process-id)
    (defun process-id (process)
      "Return the existing native subprocess pid, or nil for a buffer pipe."
      (cond ((nelisp-process-buffer-p process) nil)
            ((nelisp-process-object-p process) (nelisp-process-pid process))
            ((processp process) nil)
            (t (signal 'wrong-type-argument (list 'processp process))))))
  (defun set-process-buffer (process buffer)
    "Attach PROCESS to BUFFER without changing the owning buffer API."
    (let ((buffer (if (stringp buffer) (get-buffer-create buffer) buffer)))
      (unless (or (null buffer) (bufferp buffer))
        (signal 'wrong-type-argument (list 'bufferp buffer)))
      (when (nelisp-process-buffer-p process) (aset process 3 buffer))
      (process-put process :buffer buffer)
      (process-put process :output-marker nil)
      (process-mark process)
      buffer))
  (defun get-buffer-process (buffer)
    "Return the live buffer channel or native subprocess owning BUFFER."
    (let ((buffer (cond ((null buffer) (current-buffer))
                        ((stringp buffer) (get-buffer buffer))
                        (t buffer))) found)
      (dolist (channel nelisp-process-buffer--channels)
        (when (and (eq buffer (process-buffer channel)) (process-live-p channel))
          (setq found channel)))
      (unless found
        (dolist (process nelisp-process-adapter--live)
          (when (eq buffer (process-buffer process)) (setq found process))))
      found)))

(provide 'nelisp-process-buffer)
;;; nelisp-process-buffer.el ends here
