;;; emacs-cc-pipe-process-1.el --- Pipe process object ownership -*- lexical-binding: t; -*-

;;; Commentary:

;; The event provider owns the first fourteen slots.  These pipe connections
;; add a contact plist and a persistent marker in slots 14 and 15.
;; Slots 16 through 18 own the outgoing fd and the two peer descriptors.
;; Like GNU, the incoming and outgoing streams are separate OS pipes.
;; Load this unit after the LIB process and event providers.

;;; Code:

(defun emacs-cc-pipe-process-1--pipe ()
  "Return a fresh (READ-FD . WRITE-FD), or nil if pipe creation fails.
Use the bundled FFI adapter instead of the optional pipe-process module."
  (let ((buf (nl-ffi-malloc 8)))
    (unwind-protect
        (let ((rc (emacs-network-ffi--call "pipe" [:sint32 :pointer] buf)))
          (when (and (integerp rc) (= rc 0))
            (cons (nl-ffi-read-i32 buf 0) (nl-ffi-read-i32 buf 4))))
      (nl-ffi-free buf))))

(defun emacs-cc-pipe-process-1--poll (fds timeout-ms)
  "Return (FD . REVENTS) entries ready within TIMEOUT-MS milliseconds.
Poll through the bundled FFI adapter without loading the optional event
loop, whose compatibility definitions would replace this unit's dispatcher."
  (if (null fds)
      (progn
        (when (> timeout-ms 0)
          (emacs-network-ffi--call
           "usleep" [:sint32 :sint32] (* 1000 timeout-ms)))
        nil)
    ;; Linux struct pollfd is i32 fd, i16 events, i16 revents.
    (let* ((n (length fds)) (buf (nl-ffi-malloc (* n 8))) (i 0))
      (unwind-protect
          (progn
            (dolist (fd fds)
              (let ((off (* i 8)))
                (nl-ffi-write-i32 buf off fd)
                (nl-ffi-write-i16 buf (+ off 4) 1) ; POLLIN
                (nl-ffi-write-i16 buf (+ off 6) 0))
              (setq i (1+ i)))
            (let ((rc (emacs-network-ffi--call
                       "poll" [:sint32 :pointer :sint32 :sint32]
                       buf n timeout-ms))
                  (ready nil))
              (when (and (integerp rc) (> rc 0))
                (setq i 0)
                (dolist (fd fds)
                  (let ((revents (nl-ffi-read-i16 buf (+ (* i 8) 6))))
                    ;; HUP/ERR also require dispatch to observe EOF/errors.
                    (unless (zerop revents) (push (cons fd revents) ready)))
                  (setq i (1+ i))))
              (nreverse ready)))
        (nl-ffi-free buf)))))

(defun emacs-cc-pipe-process-1--pipe-p (process)
  "Recognize pipe connections with contact and marker storage."
  (and (vectorp process) (>= (length process) 16)
       (eq (aref process 0) :emacs-process-events)
       (eq (aref process 3) 'pipe)))

(defun emacs-cc-pipe-process-1--get-process (name)
  "Return a registered process named NAME, or NAME if it is a process."
  (cond
   ((processp name) name)
   ;; Keep the prelude's existing internal representation accepted.
   ((and (vectorp name) (= (length name) 7)
         (eq (aref name 0) 'pipe-process)) name)
   ((not (stringp name)) (signal 'wrong-type-argument (list 'stringp name)))
   (t (catch 'found
        (dolist (process (process-list))
          (when (equal (process-name process) name)
            (throw 'found process)))
        nil))))

(defun emacs-cc-pipe-process-1--unique-name (name)
  "Return an unused process name derived from NAME."
  (let ((candidate name) (suffix 0))
    (while (get-process candidate)
      (setq suffix (1+ suffix)
            candidate (format "%s<%d>" name suffix)))
    candidate))

(defun emacs-cc-pipe-process-1--make-pipe-process (&rest args)
  "Create and register a pipe process object with the options in ARGS."
  (when args
    (unless (= (% (length args) 2) 0)
      (signal 'malformed-keyword-arg-list nil))
    (let ((name (plist-get args :name)))
      (unless (stringp name) (error ":name value not a string"))
      (let* ((buffer (if (plist-member args :buffer)
                         (plist-get args :buffer) name))
             (buffer (cond ((null buffer) nil)
                           ((bufferp buffer) buffer)
                           (t (unless (stringp buffer)
                                (signal 'wrong-type-argument (list 'stringp buffer)))
                              (get-buffer-create buffer))))
             (properties (copy-sequence (plist-get args :plist)))
             (coding (or (plist-get args :coding)
                         (if (boundp 'default-process-coding-system)
                             default-process-coding-system
                           '(utf-8-unix . utf-8-unix)))))
        (if (consp coding)
            (progn (check-coding-system (car coding))
                   (check-coding-system (cdr coding)))
          (check-coding-system coding))
        (let* ((marker (make-marker))
               (outgoing (emacs-cc-pipe-process-1--pipe))
               (incoming (and outgoing (emacs-cc-pipe-process-1--pipe)))
               (process
                (vector :emacs-process-events
                        (emacs-cc-pipe-process-1--unique-name name)
                        (if incoming (car incoming) -1) 'pipe
                        (if (plist-get args :stop) 'stop 'open)
                        (or (plist-get args :filter) 'internal-default-process-filter)
                        (or (plist-get args :sentinel) 'internal-default-process-sentinel)
                        buffer properties
                        (if (consp coding) (cons (car coding) (cdr coding))
                          (cons coding coding))
                        nil nil "" (not (plist-get args :noquery))
                        (copy-sequence args) marker
                        (and outgoing (cdr outgoing))
                        (and outgoing (car outgoing))
                        (and incoming (cdr incoming)))))
          (unless incoming
            (when outgoing
              (emacs-network-ffi--close (car outgoing))
              (emacs-network-ffi--close (cdr outgoing)))
            (signal 'file-error (list "Creating pipe" "Cannot create pipe")))
          (emacs-network-ffi--set-nonblocking (car incoming))
          (emacs-network-ffi--set-nonblocking (cdr outgoing))
          (when (buffer-live-p buffer)
            (with-current-buffer buffer (set-marker marker (point-max) buffer)))
          (emacs-process-events--register process)
          process)))))

(defun emacs-cc-pipe-process-1--process-contact (process &optional key no-block)
  "Return PROCESS's contact information or the field specified by KEY."
  (if (emacs-cc-pipe-process-1--pipe-p process)
      (let ((contact (aref process 14)))
        (cond ((null key) t)
              ((eq key t) (copy-sequence contact))
              (t (plist-get contact key))))
    (unless (or (processp process)
                (and (vectorp process) (= (length process) 7)
                     (eq (aref process 0) 'pipe-process)))
      (signal 'wrong-type-argument (list 'processp process)))
    (funcall #'emacs-cc-pipe-process-1--original-process-contact
             process key no-block)))

(defun emacs-cc-pipe-process-1--process-mark (process)
  "Return the persistent marker belonging to PROCESS."
  (if (emacs-cc-pipe-process-1--pipe-p process)
      (aref process 15)
    (unless (processp process)
      (signal 'wrong-type-argument (list 'processp process)))
    (if (emacs-process--process-object-p process)
        (or (emacs-process--native-metadata process :output-marker)
            (progn
              (emacs-process-builtins--initialize-command
               process (emacs-process-process-command process))
              (emacs-process--native-metadata process :output-marker)))
      (funcall #'emacs-cc-pipe-process-1--original-process-mark process))))

(defun emacs-cc-pipe-process-1--set-process-buffer (process buffer)
  "Assign BUFFER to PROCESS, retaining its marker identity."
  (let ((result (funcall #'emacs-cc-pipe-process-1--original-set-process-buffer
                         process buffer)))
    (when (emacs-cc-pipe-process-1--pipe-p process)
      (aset process 14 (plist-put (aref process 14) :buffer buffer))
      ;; Nil disconnects output, but GNU leaves the marker at its old place.
      (when (and (buffer-live-p buffer)
                 (not (eq buffer (marker-buffer (aref process 15)))))
        (with-current-buffer buffer
          (set-marker (aref process 15) (point-max) buffer))))
    result))

(defun emacs-cc-pipe-process-1--set-process-filter (process function)
  "Set PROCESS's filter and update its contact field."
  (let ((result (funcall #'emacs-cc-pipe-process-1--original-set-process-filter
                         process function)))
    (when (emacs-cc-pipe-process-1--pipe-p process)
      (aset process 14 (plist-put (aref process 14) :filter result)))
    result))

(defun emacs-cc-pipe-process-1--set-process-sentinel (process function)
  "Set PROCESS's sentinel and update its contact field."
  (let ((result (funcall #'emacs-cc-pipe-process-1--original-set-process-sentinel
                         process function)))
    (when (emacs-cc-pipe-process-1--pipe-p process)
      (aset process 14 (plist-put (aref process 14) :sentinel result)))
    result))

(defun emacs-cc-pipe-process-1--process-command (process)
  "Return PROCESS's original command, or nil for a pipe or network."
  (unless (processp process)
    (signal 'wrong-type-argument (list 'processp process)))
  (if (emacs-process--process-object-p process)
      (emacs-process-process-command process)
    nil))

(defun emacs-cc-pipe-process-1--default-filter (process text)
  "Insert TEXT at PROCESS's marker, leaving its live buffer current.
Preserve the buffer's point unless it follows the process marker."
  (unless (processp process)
    (signal 'wrong-type-argument (list 'processp process)))
  (unless (stringp text) (signal 'wrong-type-argument (list 'stringp text)))
  (let ((buffer (process-buffer process)) (marker (process-mark process)))
    (when (buffer-live-p buffer)
      ;; The primitive selects the process buffer even when called directly.
      ;; Callers needing restoration must save their current buffer.
      (set-buffer buffer)
      (let ((follow (= (point) (or (marker-position marker) (point-max))))
            (inhibit-read-only t))
        (save-restriction
          (widen)
          (save-excursion
            (goto-char (or (marker-position marker) (point-max)))
            (insert text)
            (when marker (set-marker marker (point) buffer)))
          (when follow (goto-char marker))))))
  nil)

(defun emacs-cc-pipe-process-1--send-eof (process)
  "Replace the outgoing pipe with /dev/null, leaving input open."
  (unless (memq (process-status process) '(open stop))
    (error "Process %s not running: finished\n" (process-name process)))
  (let* ((path (nl-ffi-malloc 10))
         (fd (unwind-protect
                 (progn
                   (nl-ffi-write-bytes-at path 0 "/dev/null\0")
                   (emacs-network-ffi--call
                    "open" [:sint32 :pointer :sint32] path 1))
               (nl-ffi-free path))))
    (unless (and (integerp fd) (>= fd 0))
      (signal 'file-error (list "Opening /dev/null")))
    (emacs-network-ffi--close (aref process 16))
    (aset process 16 fd))
  process)

(defun emacs-cc-pipe-process-1--accept-output
    (&optional process seconds millisec just-this-one)
  "Wait for real descriptor input and deliver it through process filters."
  (when (and process (not (processp process)))
    (signal 'wrong-type-argument (list 'processp process)))
  (unless (or (null seconds) (numberp seconds))
    (signal 'wrong-type-argument (list 'numberp seconds)))
  (unless (or (null millisec) (integerp millisec))
    (signal 'wrong-type-argument (list 'fixnump millisec)))
  (let* ((budget (and (or seconds millisec)
                      (max 0 (+ (or seconds 0) (/ (or millisec 0) 1000.0)))))
         (deadline (and budget (+ (float-time) budget)))
         (children (cond
                    ((and process (emacs-process--native-process-p process))
                     (list process))
                    ((or (null process) (null just-this-one))
                     (emacs-process--native-live-processes))))
         (waiting t) (any nil))
    (while (and waiting (not any))
      (let* ((fds (if (and process just-this-one)
                      (unless (emacs-process--process-object-p process)
                        (let ((fd (process-id-fd process)))
                          (and (>= fd 0)
                               (not (eq (process-status process) 'stop))
                               (not (eq (process-filter process) t)) (list fd))))
                    (emacs-process-events--all-fds)))
             (remaining (and deadline (max 0 (- deadline (float-time)))))
             (timeout (if remaining (truncate (* remaining 1000)) -1)))
        ;; Native subprocesses have their own descriptors and owner.  Poll
        ;; them between short event-loop waits instead of losing their output
        ;; when PROCESS is nil or another connection is selected.
        (when children
          (when (emacs-process--native-accept children)
            (when (or (null process) (memq process children)) (setq any t)))
          (setq timeout (if (< timeout 0) 10 (min timeout 10))))
        (dolist (entry (emacs-cc-pipe-process-1--poll fds (if any 0 timeout)))
          (let ((proc (emacs-process-events--lookup-by-fd (car entry))))
            (when proc
              (if (eq (emacs-process-events--get proc 3) 'network-server)
                  (when (emacs-process-events--accept-child proc)
                    (unless process (setq any t)))
                (when (emacs-process-events--read-and-dispatch proc)
                  (when (or (null process) (eq process proc)) (setq any t)))))))
        (when children
          (when (emacs-process--native-accept children)
            (when (or (null process) (memq process children)) (setq any t))))
        (setq waiting (and (or fds children)
                           (or (null deadline) (< (float-time) deadline))
                           (or (null process)
                               (memq (process-status process)
                                     '(run open listen connect stop)))))))
    any))

(unless (fboundp 'make-pipe-process)
  (defun make-pipe-process (&rest args)
    "Create a pipe process object with keyword options ARGS."
    (apply #'emacs-cc-pipe-process-1--make-pipe-process args)))

(unless (fboundp 'process-contact)
  (defun process-contact (process &optional key no-block)
    "Return PROCESS's contact information or its field KEY."
    (emacs-cc-pipe-process-1--process-contact process key no-block)))

(unless (fboundp 'process-mark)
  (defun process-mark (process)
    "Return PROCESS's output marker."
    (emacs-cc-pipe-process-1--process-mark process)))

;; Only the NeLisp runtime replaces existing owners; GNU keeps its subrs.
;; Capture each delegate once so reloading this unit cannot capture itself.
(when (fboundp 'nelisp--repr)
  (unless (fboundp 'emacs-cc-pipe-process-1--original-process-contact)
    (fset 'emacs-cc-pipe-process-1--original-process-contact
          (symbol-function 'process-contact)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-process-mark)
    (fset 'emacs-cc-pipe-process-1--original-process-mark
          (symbol-function 'process-mark)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-set-process-buffer)
    (fset 'emacs-cc-pipe-process-1--original-set-process-buffer
          (symbol-function 'set-process-buffer)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-set-process-filter)
    (fset 'emacs-cc-pipe-process-1--original-set-process-filter
          (symbol-function 'set-process-filter)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-set-process-sentinel)
    (fset 'emacs-cc-pipe-process-1--original-set-process-sentinel
          (symbol-function 'set-process-sentinel)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-process-command)
    (fset 'emacs-cc-pipe-process-1--original-process-command
          (symbol-function 'process-command)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-default-filter)
    (fset 'emacs-cc-pipe-process-1--original-default-filter
          (symbol-function 'internal-default-process-filter)))
  (fset 'make-pipe-process (symbol-function 'emacs-cc-pipe-process-1--make-pipe-process))
  (fset 'process-contact (symbol-function 'emacs-cc-pipe-process-1--process-contact))
  (fset 'process-mark (symbol-function 'emacs-cc-pipe-process-1--process-mark))
  (fset 'set-process-buffer (symbol-function 'emacs-cc-pipe-process-1--set-process-buffer))
  (fset 'set-process-filter (symbol-function 'emacs-cc-pipe-process-1--set-process-filter))
  (fset 'set-process-sentinel (symbol-function 'emacs-cc-pipe-process-1--set-process-sentinel))
  (fset 'get-process (symbol-function 'emacs-cc-pipe-process-1--get-process))
  (fset 'process-command (symbol-function 'emacs-cc-pipe-process-1--process-command))
  (fset 'internal-default-process-filter (symbol-function 'emacs-cc-pipe-process-1--default-filter))
  (fset 'accept-process-output (symbol-function 'emacs-cc-pipe-process-1--accept-output)))

(provide 'emacs-cc-pipe-process-1)
;;; emacs-cc-pipe-process-1.el ends here
