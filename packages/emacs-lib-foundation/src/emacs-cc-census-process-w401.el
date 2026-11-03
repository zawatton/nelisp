;;; emacs-cc-census-process-w401.el --- Process lookup and D-Bus validation  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-process-w401--dbus-error (&rest data)
  "Signal a D-Bus error with DATA, installing its condition on demand."
  (unless (get 'dbus-error 'error-conditions)
    (define-error 'dbus-error "D-Bus error"))
  (signal 'dbus-error data))

(defun emacs-cc-census-process-w401--check-bus (bus)
  "Validate the symbol or address string BUS.
Address parsing and connection establishment require a D-Bus transport."
  (if (stringp bus)
      (if (string-match ":" bus)
          (emacs-cc-census-process-w401--dbus-error
           "D-Bus address parsing is unavailable in this runtime")
        (emacs-cc-census-process-w401--dbus-error
         "Address does not contain a colon"))
    (unless (symbolp bus)
      (signal 'wrong-type-argument (list 'symbolp bus)))
    (unless (memq bus '(:system :session :system-private :session-private))
      (emacs-cc-census-process-w401--dbus-error "Wrong bus name" bus))))

(unless (fboundp 'dbus--init-bus)
  (defun dbus--init-bus (bus &optional private)
    "Validate BUS before establishing a possibly PRIVATE D-Bus connection.
The standalone runtime currently has no D-Bus connection provider."
    (emacs-cc-census-process-w401--check-bus bus)
    ;; Do not manufacture a connection count or a unique bus name.
    (emacs-cc-census-process-w401--dbus-error
     "D-Bus connection establishment is unavailable in this runtime")))

(unless (fboundp 'dbus-get-unique-name)
  (defun dbus-get-unique-name (bus)
    "Return the unique D-Bus name of BUS, or signal when not connected."
    (emacs-cc-census-process-w401--check-bus bus)
    (emacs-cc-census-process-w401--dbus-error "No connection to bus" bus)))

(unless (fboundp 'dbus-message-internal)
  (defun dbus-message-internal (&rest args)
    "Validate a D-Bus message's arity and type before transport dispatch.
The standalone runtime currently has no D-Bus message provider."
    (when (< (length args) 4)
      (signal 'wrong-number-of-arguments
              (list 'dbus-message-internal (length args))))
    (let ((type (car args)))
      (unless (and (integerp type) (>= type 0))
        (signal 'wrong-type-argument (list 'wholenump type)))
      (when (> type 4)
        (emacs-cc-census-process-w401--dbus-error "Invalid message type" type))
      ;; Return/error messages validate their serial before the bus.
      (when (memq type '(2 3))
        (let ((serial (nth 3 args)))
          (unless (numberp serial)
            (signal 'wrong-type-argument (list 'numberp serial)))
          (unless (and (>= serial 0) (<= serial 4294967295)
                       (= serial (truncate serial)))
            (signal 'args-out-of-range (list serial 0 4294967295)))))
      (emacs-cc-census-process-w401--check-bus (nth 1 args))
      (emacs-cc-census-process-w401--dbus-error
       "D-Bus message dispatch is unavailable in this runtime"))))

(unless (fboundp 'get-buffer-process)
  (defun get-buffer-process (buffer)
    "Return a registered process associated with BUFFER, or nil.
BUFFER may be a buffer, a buffer name, or nil.  Nil has no process."
    (let ((target (and buffer (get-buffer buffer))))
      (when (and target (buffer-live-p target))
        (catch 'found
          ;; GNU also searches the process registry, rather than all buffers.
          (dolist (process (process-list))
            (when (eq (process-buffer process) target)
              (throw 'found process)))
          nil)))))

(provide 'emacs-cc-census-process-w401)
;;; emacs-cc-census-process-w401.el ends here
