;;; nemacs-next-session.el --- Session snapshot adapter for nemacs-next  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Thin app/session adapter for the nemacs-next protocol.  This module
;; reports editor state owned by reusable nelisp-emacs libraries; it does not
;; implement editing command semantics.

;;; Code:

(require 'nemacs-next)

(defconst nemacs-next-session-snapshot-version 0
  "Current nemacs-next snapshot payload version.")

(defconst nemacs-next-session-default-buffer-name "*scratch*"
  "Default buffer name used when the session has no current buffer yet.")

(defun nemacs-next-session--string-equal (a b)
  "Return non-nil when A and B name the same protocol atom."
  (let ((as (if (symbolp a) (symbol-name a) a))
        (bs (if (symbolp b) (symbol-name b) b)))
    (and (stringp as)
         (stringp bs)
         (string= as bs))))

(defun nemacs-next-session--buffer-name (buffer)
  "Return BUFFER's name using the reusable buffer API."
  (cond
   ((and buffer (fboundp 'nelisp-ec-buffer-name))
    (nelisp-ec-buffer-name buffer))
   ((and buffer (fboundp 'buffer-name))
    (buffer-name buffer))
   (t nil)))

(defun nemacs-next-session--with-buffer (buffer thunk)
  "Call THUNK with BUFFER current when possible."
  (if (and buffer (fboundp 'nelisp-ec-set-buffer))
      (let ((previous (and (fboundp 'nelisp-ec-current-buffer)
                           (nelisp-ec-current-buffer))))
        (unwind-protect
            (progn
              (nelisp-ec-set-buffer buffer)
              (funcall thunk))
          (when previous
            (nelisp-ec-set-buffer previous))))
    (funcall thunk)))

(defun nemacs-next-session-current-buffer-or-create (&optional name)
  "Return the current buffer, creating NAME when there is none.
This is session assembly only; buffer allocation is delegated to
`nelisp-ec-generate-new-buffer'."
  (or (and (fboundp 'nelisp-ec-current-buffer)
           (nelisp-ec-current-buffer))
      (let ((buffer (and (fboundp 'nelisp-ec-generate-new-buffer)
                         (nelisp-ec-generate-new-buffer
                          (or name nemacs-next-session-default-buffer-name)))))
        (when (and buffer (fboundp 'nelisp-ec-set-buffer))
          (nelisp-ec-set-buffer buffer))
        buffer)))

(defun nemacs-next-session-buffer-snapshot (&optional buffer)
  "Return a protocol snapshot plist for BUFFER or the current buffer.
The text, point, and size are read through reusable buffer APIs."
  (let ((target (or buffer
                    (and (fboundp 'nelisp-ec-current-buffer)
                         (nelisp-ec-current-buffer)))))
    (nemacs-next-session--with-buffer
     target
     (lambda ()
       (list :type 'snapshot
             :version nemacs-next-session-snapshot-version
             :protocol-version nemacs-next-protocol-version
             :buffer-name (nemacs-next-session--buffer-name target)
             :point (and (fboundp 'nelisp-ec-point)
                         (nelisp-ec-point))
             :point-min (and (fboundp 'nelisp-ec-point-min)
                             (nelisp-ec-point-min))
             :point-max (and (fboundp 'nelisp-ec-point-max)
                             (nelisp-ec-point-max))
             :size (and (fboundp 'nelisp-ec-buffer-size)
                        (nelisp-ec-buffer-size))
             :text (and (fboundp 'nelisp-ec-buffer-string)
                        (nelisp-ec-buffer-string)))))))

(defun nemacs-next-session-hello ()
  "Return a minimal protocol hello payload for frontend negotiation."
  (list :type 'hello
        :protocol-version nemacs-next-protocol-version
        :snapshot-version nemacs-next-session-snapshot-version
        :client-message-types (copy-sequence nemacs-next-client-message-types)
        :session-message-types (copy-sequence nemacs-next-session-message-types)
        :session-plan (nemacs-next-session-plan)))

(defun nemacs-next-session-error (code message &optional request)
  "Return a protocol error payload for CODE and MESSAGE.
REQUEST is included for diagnostics when supplied."
  (let ((payload (list :type 'error
                       :code code
                       :message message)))
    (if request
        (append payload (list :request request))
      payload)))

(defun nemacs-next-session--command-name (message)
  "Return the command name from MESSAGE."
  (or (plist-get message :name)
      (plist-get message :command)))

(defun nemacs-next-session--command-text (message)
  "Return command text payload from MESSAGE."
  (or (plist-get message :text)
      (plist-get message :char)
      (let ((args (plist-get message :args)))
        (and (listp args)
             (or (plist-get args :text)
                 (plist-get args :char))))))

(defun nemacs-next-session--command-arg (message key)
  "Return KEY from MESSAGE, falling back to MESSAGE's nested :args plist."
  (or (plist-get message key)
      (let ((args (plist-get message :args)))
        (and (listp args) (plist-get args key)))))

(defun nemacs-next-session--command-count (message &optional default)
  "Return an integer :count argument from MESSAGE, or DEFAULT (default 1)."
  (let ((count (nemacs-next-session--command-arg message :count)))
    (if (integerp count) count (or default 1))))

(defun nemacs-next-session--move-point (delta)
  "Move point by DELTA characters through the reusable movement API.
Return a buffer snapshot, or a structured `out-of-range' protocol error
when DELTA would move point outside the buffer/narrowing bounds."
  (nemacs-next-session-current-buffer-or-create)
  (condition-case _err
      (progn
        (nelisp-ec-forward-char delta)
        (nemacs-next-session-buffer-snapshot))
    (nelisp-ec-args-out-of-range
     (nemacs-next-session-error
      'out-of-range "move would leave buffer bounds"))))

(defun nemacs-next-session--goto-char (position)
  "Move point to POSITION through the reusable movement API.
Return a buffer snapshot, or a structured protocol error when POSITION is
missing or out of range."
  (nemacs-next-session-current-buffer-or-create)
  (if (integerp position)
      (condition-case _err
          (progn
            (nelisp-ec-goto-char position)
            (nemacs-next-session-buffer-snapshot))
        (nelisp-ec-args-out-of-range
         (nemacs-next-session-error
          'out-of-range "goto-char position out of range")))
    (nemacs-next-session-error
     'bad-command "goto-char requires an integer :position")))

(defun nemacs-next-session--delete-char (count)
  "Delete COUNT characters (negative = backward) through the reusable
editing API.  Return a buffer snapshot, or a structured `out-of-range'
protocol error when COUNT would delete past the buffer bounds."
  (nemacs-next-session-current-buffer-or-create)
  (condition-case _err
      (progn
        (nelisp-ec-delete-char count)
        (nemacs-next-session-buffer-snapshot))
    (nelisp-ec-args-out-of-range
     (nemacs-next-session-error
      'out-of-range "delete-char count out of range"))))

(defun nemacs-next-session-handle-command (message)
  "Handle a command protocol MESSAGE and return a response payload.
The supported M1/M2 commands are deliberately small and delegate editor
mutation to reusable buffer/editing APIs."
  (let ((name (nemacs-next-session--command-name message)))
    (cond
     ((or (nemacs-next-session--string-equal name 'snapshot)
          (nemacs-next-session--string-equal name "snapshot"))
      (nemacs-next-session-buffer-snapshot
       (nemacs-next-session-current-buffer-or-create)))
     ((or (nemacs-next-session--string-equal name 'create-buffer)
          (nemacs-next-session--string-equal name "create-buffer"))
      (let ((buffer (and (fboundp 'nelisp-ec-generate-new-buffer)
                         (nelisp-ec-generate-new-buffer
                          (or (plist-get message :buffer-name)
                              (plist-get message :name-arg)
                              nemacs-next-session-default-buffer-name)))))
        (when (and buffer (fboundp 'nelisp-ec-set-buffer))
          (nelisp-ec-set-buffer buffer))
        (nemacs-next-session-buffer-snapshot buffer)))
     ((or (nemacs-next-session--string-equal name 'insert-text)
          (nemacs-next-session--string-equal name "insert-text"))
      (let ((text (nemacs-next-session--command-text message)))
        (if (stringp text)
            (progn
              (nemacs-next-session-current-buffer-or-create)
              (nelisp-ec-insert text)
              (nemacs-next-session-buffer-snapshot))
          (nemacs-next-session-error
           'bad-command "insert-text requires a string :text" message))))
     ((or (nemacs-next-session--string-equal name 'forward-char)
          (nemacs-next-session--string-equal name "forward-char"))
      (nemacs-next-session--move-point
       (nemacs-next-session--command-count message 1)))
     ((or (nemacs-next-session--string-equal name 'backward-char)
          (nemacs-next-session--string-equal name "backward-char"))
      (nemacs-next-session--move-point
       (- (nemacs-next-session--command-count message 1))))
     ((or (nemacs-next-session--string-equal name 'goto-char)
          (nemacs-next-session--string-equal name "goto-char"))
      (nemacs-next-session--goto-char
       (nemacs-next-session--command-arg message :position)))
     ((or (nemacs-next-session--string-equal name 'delete-char)
          (nemacs-next-session--string-equal name "delete-char"))
      (nemacs-next-session--delete-char
       (nemacs-next-session--command-count message 1)))
     (t
      (nemacs-next-session-error
       'unknown-command
       (format "unknown command: %S" name)
       message)))))

(defun nemacs-next-session-handle-message (message)
  "Handle one protocol MESSAGE plist and return a response payload."
  (let ((type (plist-get message :type)))
    (cond
     ((or (nemacs-next-session--string-equal type 'hello)
          (nemacs-next-session--string-equal type "hello"))
      (nemacs-next-session-hello))
     ((or (nemacs-next-session--string-equal type 'snapshot)
          (nemacs-next-session--string-equal type "snapshot"))
      (nemacs-next-session-buffer-snapshot
       (nemacs-next-session-current-buffer-or-create)))
     ((or (nemacs-next-session--string-equal type 'command)
          (nemacs-next-session--string-equal type "command"))
      (nemacs-next-session-handle-command message))
     (t
      (nemacs-next-session-error
       'unknown-message
       (format "unknown message type: %S" type)
       message)))))

(provide 'nemacs-next-session)

;;; nemacs-next-session.el ends here
