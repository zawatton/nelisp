;;; emacs-cc-pipe-process-1.el --- Pipe process object ownership -*- lexical-binding: t; -*-

;;; Commentary:

;; The NeLisp core owns the one pipe representation (`nelisp-process-buffer-make',
;; returned by `make-pipe-process').  This unit keeps the shared accept-output
;; wait, `get-process', `process-command' and the default filter, and adds
;; stateless GNU-visible delegates that read the core channel (process-type,
;; process-name, process-contact, process-list membership, delete-process,
;; set-process-filter and process-filter).  Load it after the LIB process and
;; event providers.

;;; Code:

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

(defun emacs-cc-pipe-process-1--core-pipe-p (object)
  "Return non-nil when OBJECT is a core buffer pipe channel."
  (and (fboundp 'nelisp-process-buffer-p) (nelisp-process-buffer-p object)))

(defun emacs-cc-pipe-process-1--process-contact (next process &optional key no-block)
  "Return PROCESS's contact information or the field specified by KEY.
NEXT continues with the previous owner."
  (unless (or (processp process)
              (and (vectorp process) (= (length process) 7)
                   (eq (aref process 0) 'pipe-process)))
    (signal 'wrong-type-argument (list 'processp process)))
  (funcall next process key no-block))

(defun emacs-cc-pipe-process-1--core-call (name args)
  "Run public process function NAME on core buffer pipe ARGS.
Stateless: every answer is read from, or written to, the core channel."
  (let ((process (car args)))
    (cond
     ((eq name 'processp) t)
     ((eq name 'process-name) (aref process 1))
     ((eq name 'process-type) 'pipe)
     ((eq name 'process-id) nil)
     ((eq name 'process-plist) nil)
     ((eq name 'process-filter)
      (or (process-get process :filter) 'internal-default-process-filter))
     ((eq name 'set-process-filter)
      (process-put process :filter (cadr args))
      (or (cadr args) 'internal-default-process-filter))
     ((eq name 'process-sentinel)
      (or (process-get process :sentinel) 'internal-default-process-sentinel))
     ((eq name 'set-process-sentinel)
      (process-put process :sentinel (cadr args))
      (or (cadr args) 'internal-default-process-sentinel))
     ((eq name 'delete-process)
      (nelisp-process-buffer-delete process))
     (t (let ((base (get name 'emacs-process-events--base)))
          (if base (apply base args)
            (signal 'void-function (list name))))))))

(defun emacs-cc-pipe-process-1--create-core-pipe (_next &rest args)
  "Create a pipe with the core owner, `nelisp-process-buffer-make'.
The first call loads the core unit, which then installs its own
`make-pipe-process' over this dispatcher."
  (apply #'nelisp-process-buffer-make args))

(defun emacs-cc-pipe-process-1--notify-close (original process)
  "Run ORIGINAL (the core stderr close) and notify a pipe that it closed.
GNU calls the pipe's sentinel, or inserts the default message, when the
pipe is closed by the end of the child's output.  The core closes the
channel silently, so read the channel state before and after.  Stateless:
nothing is stored by this unit."
  (let* ((channel (process-get process :stderr-channel))
         (was-open (and channel (eq (aref channel 2) 'open))))
    (prog1 (funcall original process)
      (when (and was-open (eq (aref channel 2) 'closed))
        (let ((sentinel (process-get channel :sentinel)))
          (if (functionp sentinel)
              (funcall sentinel channel "finished\n")
            (nelisp-process-buffer-insert
             channel (format "\nProcess %s finished\n" (aref channel 1)))))))))

;; The core defines `nelisp-process-buffer-close-stderr' when it loads
;; (lazily); advise it once then.  A core patch makes this unnecessary.
(defun emacs-cc-pipe-process-1--advise-core ()
  "Install the close notification on the core stderr close, once."
  (when (and (fboundp 'advice-add) (fboundp 'nelisp-process-buffer-close-stderr)
             (featurep 'nelisp-process-buffer)
             (not (advice-member-p #'emacs-cc-pipe-process-1--notify-close
                                   'nelisp-process-buffer-close-stderr)))
    (advice-add 'nelisp-process-buffer-close-stderr :around
                #'emacs-cc-pipe-process-1--notify-close)))

(when (fboundp 'nelisp--repr)
  (let ((entry (assq 'nelisp-process-buffer after-load-alist)))
    (if entry
        (unless (memq #'emacs-cc-pipe-process-1--advise-core (cdr entry))
          (setcdr entry (cons #'emacs-cc-pipe-process-1--advise-core (cdr entry))))
      (push (list 'nelisp-process-buffer #'emacs-cc-pipe-process-1--advise-core)
            after-load-alist)))
  (when (featurep 'nelisp-process-buffer)
    (emacs-cc-pipe-process-1--advise-core)))

(defun emacs-cc-pipe-process-1--process-list (next)
  "Add live core buffer pipes to the list NEXT returns."
  (let ((list (funcall next)))
    (dolist (channel (and (boundp 'nelisp-process-buffer--channels)
                          nelisp-process-buffer--channels))
      (unless (memq channel list) (setq list (append list (list channel)))))
    list))

(defun emacs-cc-pipe-process-1--core-contact (_next process &optional key _no-block)
  "Read the contact of core buffer pipe PROCESS from its channel."
  (let ((contact (aref process 4)))
    (cond ((null key) t)
          ((eq key t) (copy-sequence contact))
          ((eq key :name) (aref process 1))
          (t (plist-get contact key)))))

(defun emacs-cc-pipe-process-1--process-mark (next process)
  "Return the persistent marker belonging to PROCESS.
NEXT continues with the previous owner for processes that are not events
vectors."
  (unless (processp process)
    (signal 'wrong-type-argument (list 'processp process)))
  (if (emacs-process--process-object-p process)
      (or (emacs-process--native-metadata process :output-marker)
          (progn
            (emacs-process-builtins--initialize-command
             process (emacs-process-process-command process))
            (emacs-process--native-metadata process :output-marker)))
    (funcall next process)))

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

(unless (fboundp 'process-contact)
  (defun process-contact (process &optional key no-block)
    "Return PROCESS's contact information or its field KEY."
    (emacs-cc-pipe-process-1--process-contact nil process key no-block)))

(unless (fboundp 'process-mark)
  (defun process-mark (process)
    "Return PROCESS's output marker."
    (emacs-cc-pipe-process-1--process-mark nil process)))

;; Only the NeLisp runtime replaces existing owners; GNU keeps its subrs.
;; Capture each delegate once so reloading this unit cannot capture itself.
(when (fboundp 'nelisp--repr)
  (unless (fboundp 'emacs-cc-pipe-process-1--original-process-command)
    (fset 'emacs-cc-pipe-process-1--original-process-command
          (symbol-function 'process-command)))
  (unless (fboundp 'emacs-cc-pipe-process-1--original-default-filter)
    (fset 'emacs-cc-pipe-process-1--original-default-filter
          (symbol-function 'internal-default-process-filter)))
  ;; The names that several layers extend are installed once, by the single
  ;; dispatcher in emacs-process-events.el; register this unit's handlers.
  (when (fboundp 'emacs-process-events-register-handler)
    (emacs-process-events-register-handler
     'process-contact nil #'emacs-cc-pipe-process-1--process-contact)
    (emacs-process-events-register-handler
     'process-contact #'emacs-cc-pipe-process-1--core-pipe-p
     #'emacs-cc-pipe-process-1--core-contact)
    (emacs-process-events-register-handler
     'make-pipe-process nil #'emacs-cc-pipe-process-1--create-core-pipe)
    (emacs-process-events-register-handler
     'process-list nil #'emacs-cc-pipe-process-1--process-list)
    (emacs-process-events-register-handler
     'process-mark nil #'emacs-cc-pipe-process-1--process-mark))
  ;; `accept-process-output' is owned by emacs-process-builtins' shared wait.
  (dolist (entry '((get-process . emacs-cc-pipe-process-1--get-process)
                   (process-command . emacs-cc-pipe-process-1--process-command)
                   (internal-default-process-filter
                    . emacs-cc-pipe-process-1--default-filter)))
    (fset (car entry) (symbol-function (cdr entry)))))

(provide 'emacs-cc-pipe-process-1)
;;; emacs-cc-pipe-process-1.el ends here
