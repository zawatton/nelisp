;;; nelisp-repl-session.el --- explicit REPL failure/replay records -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)

(defvar nelisp-repl-session--failures nil)
(defvar nelisp-repl-session--next-id 1)
(defvar nelisp-repl-session--records nil)
(defconst nelisp-repl-session-max-failures 64)
(defconst nelisp-repl-session-max-args 16)
(defconst nelisp-repl-session-max-records 128)
(defconst nelisp-repl-session-max-message-chars 4096)
(defconst nelisp-repl-session-max-form-bytes 65536)

(defun nelisp-repl-session--readable-p (value &optional seen)
  "Return non-nil if VALUE is finite and safely printable/readable."
  (cond
   ((or (null value) (symbolp value) (numberp value) (stringp value)
        (characterp value)) t)
   ((memq value seen) nil)
   ((consp value)
    (and (nelisp-repl-session--readable-p (car value) (cons value seen))
         (nelisp-repl-session--readable-p (cdr value) (cons value seen))))
   ((vectorp value)
    (let ((ok t) (i 0))
      (while (and ok (< i (length value)))
        (setq ok (nelisp-repl-session--readable-p (aref value i)
                                                  (cons value seen))
              i (1+ i)))
      ok))
   (t nil)))

(defun nelisp-repl-session--form-string (form)
  "Validate FORM and return its bounded printed representation."
  (unless (nelisp-repl-session--readable-p form)
    (error "REPL session refuses circular or unreadable value"))
  (let* ((print-length nil) (print-level nil) (read-eval nil)
         (text (prin1-to-string form)))
    (when (> (string-bytes text) nelisp-repl-session-max-form-bytes)
      (error "REPL session refuses unreadable or oversized form"))
    (unless (equal (read text) form)
      (error "REPL session form did not round-trip"))
    text))

(defun nelisp-repl-session--failure-data (data)
  "Copy readable diagnostic DATA, dropping cyclic or opaque condition data."
  (when (nelisp-repl-session--readable-p data)
    (copy-tree data)))

(defun nelisp-repl-session-call (function &rest args)
  "Call FUNCTION with ARGS, recording and re-signalling any error.
Only explicit calls through this wrapper are recorded.  Up to
`nelisp-repl-session-max-args' argument references and bounded diagnostic
text are retained for explicit retry; no variables or environment are dumped.
Argument values are retained by reference at failure time, so later mutation is
visible to an explicit retry; call `nelisp-repl-session-clear' to release them."
  (condition-case err
      (apply function args)
    (error
     (let ((id nelisp-repl-session--next-id))
       (setq nelisp-repl-session--next-id (1+ id))
       (let ((truncated (> (length args) nelisp-repl-session-max-args)))
         (push (list :id id :function function
                   :args (cl-subseq args 0 (min (length args)
                                                nelisp-repl-session-max-args))
                   :retryable (not truncated)
                   :truncated truncated
                   :condition (car err)
                   :data (nelisp-repl-session--failure-data (cdr err))
                   :message (let ((message (error-message-string err)))
                              (substring message 0
                                         (min (length message)
                                              nelisp-repl-session-max-message-chars))))
               nelisp-repl-session--failures))
       (when (> (length nelisp-repl-session--failures)
                nelisp-repl-session-max-failures)
         (setq nelisp-repl-session--failures
               (cl-subseq nelisp-repl-session--failures 0
                          nelisp-repl-session-max-failures))))
     (signal (car err) (cdr err)))))

(defun nelisp-repl-session-clear ()
  "Explicitly release retained failures, replay records, and retry IDs."
  (setq nelisp-repl-session--failures nil
        nelisp-repl-session--records nil
        nelisp-repl-session--next-id 1)
  nil)

(defun nelisp-repl-session-failures ()
  "Return copied records and argument lists, retaining argument value references.
Argument values can be cyclic and must not be recursively copied."
  (mapcar (lambda (entry)
            (let ((record (copy-sequence entry)))
              (plist-put record :args (copy-sequence (plist-get entry :args)))
              (plist-put record :data (copy-tree (plist-get entry :data)))
              record))
          (reverse nelisp-repl-session--failures)))

(defun nelisp-repl-session-retry (&optional id)
  "Explicitly retry failure ID; signal when ID is absent or ambiguous."
  (unless id (user-error "retry requires an explicit failure ID"))
  (let ((entry (cl-find id nelisp-repl-session--failures :key (lambda (x)
                                                               (plist-get x :id)))) )
    (unless entry (user-error "no recorded REPL failure with ID %S" id))
    (unless (plist-get entry :retryable)
      (user-error "REPL failure %S retained truncated arguments; retry is refused" id))
    (apply (plist-get entry :function) (plist-get entry :args))))

(defun nelisp-repl-session-record (form)
  "Explicitly register readable FORM for later export/replay."
  (when (>= (length nelisp-repl-session--records)
            nelisp-repl-session-max-records)
    (user-error "Replay record limit reached; export and clear before recording more"))
  (let ((text (nelisp-repl-session--form-string form)))
    (push (list :kind :form :form text) nelisp-repl-session--records)
    form))

(defun nelisp-repl-session-record-setting (symbol value)
  "Explicitly register SYMBOL/VALUE as a replayable setting."
  (unless (symbolp symbol) (error "setting name must be a symbol"))
  (nelisp-repl-session-record `(setq ,symbol (quote ,value))))

(defun nelisp-repl-session-record-load (path)
  "Explicitly register readable PATH for replay, retaining its file hash."
  (when (>= (length nelisp-repl-session--records)
            nelisp-repl-session-max-records)
    (user-error "Replay record limit reached; export and clear before recording more"))
  (unless (and (stringp path) (file-regular-p path) (file-readable-p path))
    (error "REPL session load path is not readable: %S" path))
  (let ((absolute (expand-file-name path)))
    (push (list :kind :load :path absolute
                :sha256 (with-temp-buffer
                          (insert-file-contents-literally absolute)
                          (secure-hash 'sha256 (buffer-string))))
          nelisp-repl-session--records))
  path)

(defun nelisp-repl-session-export (path)
  "Export explicit replay records to PATH as readable Elisp."
  (with-temp-file path
    (insert ";;; nelisp REPL session replay; explicit records only\n"
            ";;; load-sha256 metadata is for change review; replay does not auto-verify it.\n")
    (dolist (record (reverse nelisp-repl-session--records))
      (pcase (plist-get record :kind)
        (:form (insert (plist-get record :form) "\n"))
        (:load
         (let* ((export-directory (file-name-directory (expand-file-name path)))
                (source (plist-get record :path))
                (relative (if (fboundp 'file-relative-name)
                              (file-relative-name source export-directory)
                            (if (string-prefix-p export-directory source)
                                (substring source (length export-directory))
                              (error "Relative load paths require file-relative-name")))))
           (insert ";; load-sha256: " (plist-get record :sha256) "\n"
                   "(load (expand-file-name " (prin1-to-string relative)
                   " (file-name-directory load-file-name)) nil nil t)\n"))))))
  path)

(provide 'nelisp-repl-session)
;;; nelisp-repl-session.el ends here
