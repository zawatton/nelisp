;;; emacs-cc-process-1.el --- process.c primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-process-1--process (process)
  "Resolve PROCESS like the process.c entry points in batch mode."
  (let ((p (or process (and (fboundp 'get-buffer-process)
                            (get-buffer-process (current-buffer))))))
    (cond ((and (fboundp 'processp) (processp p)) p)
          ((and (bufferp p) (fboundp 'get-buffer-process)
                (get-buffer-process p)) (get-buffer-process p))
          ((stringp p)
           (let ((b (get-buffer p)))
             (or (and b (get-buffer-process b))
                 (and (fboundp 'get-process) (get-process p)))))
          (t nil))))

(defun emacs-cc-process-1--signal-process-error (process)
  (cond ((null process) (error "Buffer %s has no process" (buffer-name)))
        ((stringp process) (error "Process %s does not exist" process))
        (t (signal 'wrong-type-argument (list 'processp process)))))

(unless (fboundp 'continue-process)
  (defun continue-process (&optional process current-group)
    "Continue process PROCESS.  May be process or name of one.
See function `interrupt-process' for more details on usage.
If PROCESS is a network or serial process, resume handling of incoming
traffic."
    (let ((p (emacs-cc-process-1--process process)))
      (if p (if (fboundp 'emacs-process-continue-process)
                (emacs-process-continue-process p current-group)
              (error "Process %s cannot be continued" (process-name p)))
        (emacs-cc-process-1--signal-process-error process)))))

(unless (fboundp 'internal-default-interrupt-process)
  (defun internal-default-interrupt-process (&optional process current-group)
    "Default function to interrupt process PROCESS.
It shall be the last element in list `interrupt-process-functions'.
See function `interrupt-process' for more details on usage."
    (let ((p (emacs-cc-process-1--process process)))
      (if p (if (fboundp 'emacs-process-signal-process)
                (emacs-process-signal-process p 'SIGINT current-group)
              (error "Process %s cannot be signaled" (process-name p)))
        (emacs-cc-process-1--signal-process-error process)))))

(unless (fboundp 'internal-default-process-filter)
  (defun internal-default-process-filter (proc text)
    "Function used as default process filter.
This inserts the process's output into its buffer, if there is one.
Otherwise it discards the output."
    (unless (and (fboundp 'processp) (processp proc))
      (signal 'wrong-type-argument (list 'processp proc)))
    (unless (stringp text) (signal 'wrong-type-argument (list 'stringp text)))
    (let ((buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert text))))))

(unless (fboundp 'internal-default-process-sentinel)
  (defun internal-default-process-sentinel (proc msg)
    "Function used as default sentinel for processes.
This inserts a status message into the process's buffer, if there is one."
    (unless (and (fboundp 'processp) (processp proc))
      (signal 'wrong-type-argument (list 'processp proc)))
    (unless (stringp msg) (signal 'wrong-type-argument (list 'stringp msg)))
    (let ((buffer (process-buffer proc)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (goto-char (point-max))
          (insert msg))))))

(unless (fboundp 'internal-default-signal-process)
  (defun internal-default-signal-process (process sigcode &optional remote)
    "Default function to send PROCESS the signal with code SIGCODE.
It shall be the last element in list `signal-process-functions'.
See function `signal-process' for more details on usage."
    (let ((p (emacs-cc-process-1--process process)))
      (cond (p (if (fboundp 'signal-process)
                   (signal-process p sigcode remote)
                 (error "Process cannot be signaled")))
            ;; The default hook quietly declines unknown process names.
            ((stringp process) nil)
            (t (emacs-cc-process-1--signal-process-error process))))))

(unless (fboundp 'interrupt-process)
  (defun interrupt-process (&optional process current-group)
    "Interrupt process PROCESS.
PROCESS may be a process, a buffer, or the name of a process or buffer.
No arg or nil means current buffer's process.  Second arg CURRENT-GROUP
non-nil means send signal to the current process-group of the process's
controlling terminal rather than to the process's own process group.
If CURRENT-GROUP is `lambda', and if the shell owns the terminal, don't
send the signal.

This function calls the functions of `interrupt-process-functions' in
the order of the list, until one of them returns non-nil."
    (let ((p (emacs-cc-process-1--process process)))
      (if p (if (fboundp 'emacs-process-signal-process)
                (emacs-process-signal-process p 'SIGINT current-group)
              (error "Process cannot be interrupted"))
        (emacs-cc-process-1--signal-process-error process)))))

(unless (fboundp 'list-system-processes)
  (defun list-system-processes ()
    "Return a list of numerical process IDs of all running processes.
If this functionality is unsupported, return nil."
    (condition-case nil
        (delq nil (mapcar #'process-id (process-list)))
      (error nil))))

(unless (fboundp 'make-serial-process)
  (defun make-serial-process (&rest args)
    "Create and return a serial port process.

Arguments are specified as keyword/argument pairs.  See the GNU Emacs
manual for the supported :port, :speed, :name, :buffer and other options."
    (let ((port (plist-get args :port)) (speed (plist-get args :speed)))
      (unless port (error "Missing :port argument"))
      (unless (plist-member args :speed) (error "Missing :speed argument"))
      (if (fboundp 'emacs-process-make-serial-process)
          (apply #'emacs-process-make-serial-process args)
        (signal 'file-missing (list "Opening serial port"
                                    "No such file or directory" port))
        speed))))

(unless (fboundp 'network-interface-info)
  (defun network-interface-info (ifname)
    "Return information about network interface named IFNAME."
    (unless (stringp ifname) (signal 'wrong-type-argument (list 'stringp ifname)))
    nil))

(unless (fboundp 'network-interface-list)
  (defun network-interface-list (&optional full family)
    "Return an alist of network interfaces and their addresses.
FULL requests address, broadcast and mask information; FAMILY selects
nil, `ipv4' or `ipv6'."
    (when (and family (not (memq family '(ipv4 ipv6))))
      (error "Invalid address family: %s" family))
    (ignore full)
    nil))

(unless (fboundp 'network-lookup-address-info)
  (defun network-lookup-address-info (name &optional family hint)
    "Look up Internet Protocol address info of NAME.
FAMILY is nil, `ipv4' or `ipv6'; HINT supports `numeric'."
    (unless (stringp name) (signal 'wrong-type-argument (list 'stringp name)))
    (when (and family (not (memq family '(ipv4 ipv6))))
      (error "Invalid address family: %s" family))
    (when (and hint (not (eq hint 'numeric)))
      (error "Invalid lookup hint: %s" hint))
    (when (and (eq hint 'numeric) (fboundp 'string-to-number))
      (condition-case nil
          (let ((addr (and (fboundp 'make-network-process)
                           (make-network-process :name "cc-process-probe"
                                                 :family (or family 'ipv4)
                                                 :host name :service 0
                                                 :nowait t))))
            (when addr (delete-process addr)))
        (error nil)))
    nil))

(unless (fboundp 'process-datagram-address)
  (defun process-datagram-address (process)
    "Get the current datagram address associated with PROCESS."
    (unless (and (fboundp 'processp) (processp process))
      (signal 'wrong-type-argument (list 'processp process)))
    (if (fboundp 'process-contact) (process-contact process :remote) nil)))

(provide 'emacs-cc-process-1)
