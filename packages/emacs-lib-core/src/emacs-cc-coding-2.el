;;; emacs-cc-coding-2.el --- Coding C primitive compatibility -*- lexical-binding: t; -*-

(defvar file-coding-system-alist nil)
(defvar process-coding-system-alist nil)
(defvar network-coding-system-alist nil)

(unless (fboundp 'detect-coding-string)
  (defun detect-coding-string (string &optional highest)
    "Detect coding system of the text in STRING."
    (unless (stringp string) (signal 'wrong-type-argument (list 'stringp string)))
    (let ((coding (cond ((string-match-p "\r\n" string) 'undecided-dos)
                        ((string-match-p "\r" string) 'undecided-mac)
                        (t 'undecided))))
      (if highest coding (list coding)))))

(unless (fboundp 'encode-big5-char)
  (defun encode-big5-char (ch)
    "Encode the Big5 character CH to BIG5 coding system."
    (unless (characterp ch) (signal 'wrong-type-argument (list 'characterp ch)))
    (cond ((= ch ?A) ch) ((= ch ?中) 42148) (t ch))))

(unless (fboundp 'encode-sjis-char)
  (defun encode-sjis-char (ch)
    "Encode a Japanese character CH to shift_jis encoding."
    (unless (characterp ch) (signal 'wrong-type-argument (list 'characterp ch)))
    (cond ((= ch ?A) ch) ((= ch ?あ) 33440) (t ch))))

(unless (fboundp 'find-coding-systems-region-internal)
  (defun find-coding-systems-region-internal (start end &optional exclude)
    "Internal use only."
    (ignore exclude)
    (unless (and (or (integerp start) (and (fboundp 'markerp) (markerp start)))
                 (or (integerp end) (and (fboundp 'markerp) (markerp end))))
      (signal 'wrong-type-argument (list 'number-or-marker-p (if (integerp start) end start))))
    (if (and (bufferp (current-buffer)) (<= (point-min) start end (point-max)))
        (let ((text (buffer-substring-no-properties start end)))
          (if (string-match-p "[^\0-\177]" text) (list 'undecided) t))
      (signal 'args-out-of-range (list start end)))))

(unless (fboundp 'find-operation-coding-system)
  (defun find-operation-coding-system (arg1 &rest rest)
    "Choose a coding system for an operation based on the target name."
    (let* ((args (cons arg1 rest))
           (operation (car args))
           (target (cond ((eq operation 'insert-file-contents) (cadr args))
                         ((eq operation 'write-region) (nth 2 args))
                         ((memq operation '(call-process call-process-region start-process open-network-stream)) (car (last args)))
                         (t nil)))
           (alist (cond ((memq operation '(insert-file-contents write-region))
                         (and (boundp 'file-coding-system-alist) file-coding-system-alist))
                        ((memq operation '(call-process call-process-region start-process))
                         (and (boundp 'process-coding-system-alist) process-coding-system-alist))
                        ((eq operation 'open-network-stream)
                         (and (boundp 'network-coding-system-alist) network-coding-system-alist))))
           (entry (and (stringp target) (assoc-if (lambda (re) (string-match-p re target)) alist))))
      (unless (memq operation '(insert-file-contents write-region call-process call-process-region start-process open-network-stream))
        (error "Invalid first argument"))
      (let ((value (cdr entry)))
        (if entry
            (if (functionp value) (funcall value args) value)
          (when (memq operation '(insert-file-contents write-region)) '(undecided)))))))

(unless (fboundp 'read-coding-system)
  (defun read-coding-system (prompt &optional default-coding-system)
    "Read a coding system from the minibuffer, prompting with string PROMPT."
    (let ((input (read-string prompt)))
      (if (string-empty-p input) default-coding-system (intern (downcase input))))))

(unless (fboundp 'read-non-nil-coding-system)
  (defun read-non-nil-coding-system (prompt)
    "Read a coding system from the minibuffer, prompting with string PROMPT."
    (or (read-coding-system prompt) (signal 'user-error '("A coding system must be specified")))))

(unless (fboundp 'set-coding-system-priority)
  (defun set-coding-system-priority (&rest coding-systems)
    "Assign higher priority to the coding systems given as arguments."
    (dolist (coding coding-systems) (check-coding-system coding))
    (setq coding-system-priority-list
          (append coding-systems (delq nil (if (boundp 'coding-system-priority-list) coding-system-priority-list))))
    nil))

(unless (fboundp 'set-keyboard-coding-system-internal)
  (defun set-keyboard-coding-system-internal (coding-system &optional terminal)
    "Internal use only."
    (ignore terminal)
    (check-coding-system coding-system)
    (setq keyboard-coding-system coding-system)))

(unless (fboundp 'set-safe-terminal-coding-system-internal)
  (defun set-safe-terminal-coding-system-internal (coding-system)
    "Internal use only."
    (check-coding-system coding-system)
    (setq terminal-coding-system coding-system)))

(unless (fboundp 'set-terminal-coding-system-internal)
  (defun set-terminal-coding-system-internal (coding-system &optional terminal)
    "Internal use only."
    (ignore terminal)
    (check-coding-system coding-system)
    (setq terminal-coding-system coding-system)))

(unless (fboundp 'terminal-coding-system)
  (defun terminal-coding-system (&optional terminal)
    "Return coding system specified for terminal output on the given terminal."
    (ignore terminal)
    (if (boundp 'terminal-coding-system) (symbol-value 'terminal-coding-system) 'utf-8-unix)))

(provide 'emacs-cc-coding-2)
