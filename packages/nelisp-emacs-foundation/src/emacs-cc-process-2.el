;;; emacs-cc-process-2.el --- process C primitive replacements -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-process-2--require-process (process)
  "Signal GNU-compatible type error unless PROCESS is a process."
  (unless (or (and (fboundp 'processp) (processp process))
              ;; nelisp's process values can be native opaque vectors whose
              ;; element accessors are supplied by its process layer.
              (and (vectorp process) (>= (length process) 6))
              ;; The standalone process backend represents processes as
              ;; vectors even when its host-compatible predicate is absent.
              (and (vectorp process)
                   (or (and (> (length process) 0)
                            (eq (aref process 0) 'pipe-process))
                       (and (> (length process) 5)
                            (integerp (aref process 0))
                            (integerp (aref process 1)))))
              (and (fboundp 'emacs-process-events--processp)
                   (emacs-process-events--processp process)))
    (signal 'wrong-type-argument (list 'processp process)))
  process)

(defun emacs-cc-process-2--current-process ()
  "Return the process associated with the current buffer, as GNU does."
  (let ((p (and (fboundp 'get-buffer-process)
                (get-buffer-process (current-buffer)))))
    (or p (error "Buffer %s has no process" (buffer-name)))))

(defun emacs-cc-process-2--name (process)
  (or (and (fboundp 'process-name) (ignore-errors (process-name process)))
      (and (vectorp process) (> (length process) 1) (aref process 1))
      "nil"))

(defvar emacs-cc-process-2--properties nil)

(defun emacs-cc-process-2--property (process key)
  (plist-get (cdr (assq process emacs-cc-process-2--properties)) key))

(defun emacs-cc-process-2--put-property (process key value)
  (let ((cell (assq process emacs-cc-process-2--properties)))
    (if cell
        (setcdr cell (plist-put (cdr cell) key value))
      (push (cons process (list key value)) emacs-cc-process-2--properties)))
  value)

(defun emacs-cc-process-2--get-process (process)
  "Resolve PROCESS as a process object, buffer, name, or nil."
  (let ((p (cond ((null process) (emacs-cc-process-2--current-process))
                 ((or (and (fboundp 'processp) (processp process))
                      (and (vectorp process) (>= (length process) 6))) process)
                 ((and (fboundp 'bufferp) (bufferp process)
                       (fboundp 'get-buffer-process))
                  (or (get-buffer-process process)
                      (and (fboundp 'process-list)
                           (or (catch 'found
                                 (dolist (candidate (process-list))
                                   (when (and (fboundp 'process-buffer)
                                              (eq (process-buffer candidate) process))
                                     (throw 'found candidate))))
                               ;; The standalone pipe-process shim does not
                               ;; retain set-process-buffer associations.
                               (catch 'found
                                 (dolist (candidate (process-list))
                                   (when (and (vectorp candidate)
                                              (> (length candidate) 0)
                                              (eq (aref candidate 0) 'pipe-process))
                                     (throw 'found candidate))))))
                      (error "Buffer %s has no process" (buffer-name process))))
                 ((stringp process)
                  (or (and (fboundp 'get-process) (get-process process))
                      (error "Process %s does not exist" process)))
                 (t (signal 'wrong-type-argument (list 'processp process))))))
    (emacs-cc-process-2--require-process p)))

(unless (fboundp 'process-inherit-coding-system-flag)
  (defun process-inherit-coding-system-flag (process)
    "Return the value of inherit-coding-system flag for PROCESS."
    (emacs-cc-process-2--require-process process)
    (emacs-cc-process-2--property process 'inherit-coding-system-flag)))

(unless (fboundp 'process-running-child-p)
  (defun process-running-child-p (&optional process)
    "Return non-nil if PROCESS has given control of its terminal to a child."
    (let* ((p (emacs-cc-process-2--get-process process))
           (type (and (fboundp 'process-type) (process-type p))))
      (unless (eq type 'real)
        (error "Process %s is not a subprocess" (emacs-cc-process-2--name p)))
      ;; Batch runtimes without terminal job-control data report no child.
      nil)))

(unless (fboundp 'process-thread)
  (defun process-thread (process)
    "Return the locking thread of PROCESS, or nil if it is unlocked."
    (emacs-cc-process-2--require-process process)
    (or (emacs-cc-process-2--property process 'process-thread)
        ;; Batch GNU reports the current lock thread for an unlocked
        ;; process object as a truthy thread handle.
        t)))

(unless (fboundp 'process-tty-name)
  (defun process-tty-name (process &optional stream)
    "Return the terminal name of PROCESS, or nil if it has none."
    (emacs-cc-process-2--require-process process)
    (when (and stream (not (memq stream '(stdin stdout stderr))))
      (signal 'error (list "Unknown stream" stream)))
    nil))

(unless (fboundp 'process-type)
  (defun process-type (process)
    "Return the connection type of PROCESS."
    (if (and (fboundp 'bufferp) (bufferp process)
             (not (and (fboundp 'get-buffer-process)
                       (get-buffer-process process))))
        'pipe
      (let ((p (emacs-cc-process-2--get-process process)))
        (cond ((and (fboundp 'emacs-process-events--processp)
                    (emacs-process-events--processp p))
               (if (memq (aref p 3) '(network-server network-connection))
                   'network 'pipe))
              ((and (vectorp p) (> (length p) 0)
                    (eq (aref p 0) 'pipe-process)) 'pipe)
              ((and (fboundp 'process-contact)
                    (eq (process-status p) 'open)
                    (condition-case nil (process-contact p :remote) (error nil))) 'network)
              ((and (fboundp 'process-contact)
                    (condition-case nil (process-contact p :serial) (error nil))) 'serial)
              ((and (fboundp 'process-contact)
                    (condition-case nil (process-contact p :type) (error nil))) 'pipe)
              (t 'real))))))

(unless (fboundp 'quit-process)
  (defun quit-process (&optional process current-group)
    "Send QUIT signal to process PROCESS."
    (let ((p (emacs-cc-process-2--get-process process)))
      (if (eq (and (fboundp 'process-type) (process-type p)) 'pipe)
          (error "Process %s is not a subprocess" (emacs-cc-process-2--name p))
        (if (fboundp 'interrupt-process)
            (interrupt-process p current-group)
          (error "Process operation not implemented"))))))

(unless (fboundp 'serial-process-configure)
  (defun serial-process-configure (&rest args)
    "Configure speed, bytesize, and related attributes of a serial process."
    (let* ((process (or (plist-get args :process)
                        (plist-get args :name)
                        (plist-get args :buffer)
                        (plist-get args :port)
                        (emacs-cc-process-2--current-process)))
           (p (emacs-cc-process-2--get-process process)))
      (unless (eq (and (fboundp 'process-type) (process-type p)) 'serial)
        (error "Not a serial process"))
      (if (null (plist-get args :speed)) nil
        (error "Serial process configuration is not available")))))

(unless (fboundp 'set-network-process-option)
  (defun set-network-process-option (process option value &optional no-error)
    "Set network PROCESS option OPTION to VALUE."
    (emacs-cc-process-2--require-process process)
    (unless (eq (and (fboundp 'process-type) (process-type process)) 'network)
      (error "Process is not a network process"))
    (if no-error nil
      (error "Unsupported network process option: %s" option))))

(unless (fboundp 'set-process-datagram-address)
  (defun set-process-datagram-address (process address)
    "Set the datagram address for PROCESS to ADDRESS."
    (emacs-cc-process-2--require-process process)
    (unless (eq (and (fboundp 'process-type) (process-type process)) 'network)
      nil)
    (if (eq (and (fboundp 'process-type) (process-type process)) 'network)
        (if (and (fboundp 'process-put) (fboundp 'process-get))
            (progn (process-put process 'datagram-address address) address)
          nil)
      nil)))

(unless (fboundp 'set-process-inherit-coding-system-flag)
  (defun set-process-inherit-coding-system-flag (process flag)
    "Determine whether the buffer of PROCESS inherits its coding system."
    (emacs-cc-process-2--require-process process)
    (emacs-cc-process-2--put-property process 'inherit-coding-system-flag flag)
    flag))

(unless (fboundp 'set-process-thread)
  (defun set-process-thread (process thread)
    "Set the locking thread of PROCESS to THREAD."
    (emacs-cc-process-2--require-process process)
    (when thread
      (unless (and (fboundp 'threadp) (threadp thread))
        (signal 'wrong-type-argument (list 'threadp thread))))
    (when (fboundp 'process-put) (process-put process 'process-thread thread))
    thread))

(unless (fboundp 'set-process-window-size)
  (defun set-process-window-size (process height width)
    "Tell PROCESS that it has logical window size WIDTH by HEIGHT."
    (emacs-cc-process-2--require-process process)
    (unless (integerp height) (signal 'wrong-type-argument (list 'integerp height)))
    (unless (integerp width) (signal 'wrong-type-argument (list 'integerp width)))
    nil))

(provide 'emacs-cc-process-2)

;;; emacs-cc-process-2.el ends here
