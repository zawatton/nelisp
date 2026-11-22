;;; emacs-cc-print-1.el --- C-core print primitives -*- lexical-binding: t; -*-

(defvar emacs-batch-stdio-pending-newline nil
  "Non-nil after batch stdout printing, until the next displayed message.
GNU separates stdout printing from the next stderr message with an extra
newline, even when the stdout text already ended in a newline.")

(defun emacs-cc-print-1--note-stdout (stream)
  "Remember batch printing to stdout through STREAM."
  (when (and (boundp 'noninteractive) noninteractive
             (let ((target (or stream standard-output)))
               (or (null target) (eq target t))))
    (setq emacs-batch-stdio-pending-newline t)))

(defun emacs-cc-print-1--stdout-around (original &rest args)
  "Track stdout output by `princ', `prin1', or `print'."
  (prog1 (apply original args)
    (emacs-cc-print-1--note-stdout (nth 1 args))))

(defun emacs-cc-print-1--terpri-around (original &rest args)
  "Track stdout output by `terpri'."
  (prog1 (apply original args)
    (emacs-cc-print-1--note-stdout (car args))))

;; Host Emacs owns this state in print.c; only the standalone needs the
;; compatibility tracking.  Buffer/function streams and string rendering
;; do not set the flag, and inhibited messages must not consume it.
(when (and (boundp 'noninteractive) noninteractive
           (fboundp 'nelisp--write-stdout-bytes))
  (dolist (printer '(princ prin1 print))
    (unless (advice-member-p #'emacs-cc-print-1--stdout-around printer)
      (advice-add printer :around #'emacs-cc-print-1--stdout-around)))
  (unless (advice-member-p #'emacs-cc-print-1--terpri-around 'terpri)
    (advice-add 'terpri :around #'emacs-cc-print-1--terpri-around)))

(defvar emacs-cc-print-1--debugging-output-file nil)
(defvar print-circle nil)
(defvar print-length nil)
(defvar print-level nil)
(when (fboundp 'nl-ffi-call)
  (defvar print-quoted t))
(defvar print-gensym nil)
(defvar print-continuous-numbering nil)
(defvar print-number-table nil)
(defvar print-charset-text-property 'default)
(defvar print-unreadable-function nil)
(when (fboundp 'nl-ffi-call)
  (defvar float-output-format nil))
(defvar print-integers-as-characters nil)

(defun emacs-cc-print-1--write-stderr-byte (byte)
  "Write BYTE to descriptor 2 without changing or flushing the stream."
  (let ((buffer (syscall-direct 9 0 8 3 #x22 -1 0)))
    (when (< buffer 0)
      (error "external-debugging-output: mmap scratch allocation failed: %S"
             buffer))
    (let ((primary nil) (cleanup nil))
      (unwind-protect
          (condition-case err
              (progn
          (ptr-write-u8 buffer 0 byte)
          (let ((done nil))
            (while (not done)
              (let ((written (syscall-direct 1 2 buffer 1 0 0 0)))
                (cond
                 ((= written -4) nil)
                 ((< written 0)
                  (signal 'emacs-cc-print-1--stderr-write-error
                          (list written)))
                 ((= written 0)
                  (error "external-debugging-output: write(stderr) made no progress"))
                 ((/= written 1)
                  (error "external-debugging-output: invalid write count: %S"
                         written))
                 (t (setq done t)))))))
            (t (setq primary err)))
        (condition-case err
            (let ((released (syscall-direct 11 buffer 8 0 0 0 0)))
              (unless (= released 0)
                (setq cleanup
                      (list 'error
                            (format "external-debugging-output: munmap failed: %S"
                                    released)))))
          (t (setq cleanup err))))
      (cond
       ((and primary cleanup)
        (signal (car primary)
                (append (cdr primary) (list :cleanup-failure cleanup))))
       (primary (signal (car primary) (cdr primary)))
       (cleanup (signal (car cleanup) (cdr cleanup)))))
  byte))

(define-error 'emacs-cc-print-1--stderr-write-error
  "external-debugging-output: write(stderr) failed" 'error)

(defun emacs-cc-print-1--internal-character-bytes (character)
  "Return CHARACTER's GNU multibyte representation as byte integers.
This follows GNU Emacs 31.1 `CHAR_STRING' and `char_string' in
src/character.h and src/character.c, including extended and byte8 chars."
  (cond
   ((<= character #x7f) (list character))
   ((<= character #x7ff)
    (list (logior #xc0 (ash character -6))
          (logior #x80 (logand character #x3f))))
   ((<= character #xffff)
    (list (logior #xe0 (ash character -12))
          (logior #x80 (logand (ash character -6) #x3f))
          (logior #x80 (logand character #x3f))))
   ((<= character #x1fffff)
    (list (logior #xf0 (ash character -18))
          (logior #x80 (logand (ash character -12) #x3f))
          (logior #x80 (logand (ash character -6) #x3f))
          (logior #x80 (logand character #x3f))))
   ((<= character #x3fff7f)
    (list #xf8
          (logior #x80 (logand (ash character -18) #xf))
          (logior #x80 (logand (ash character -12) #x3f))
          (logior #x80 (logand (ash character -6) #x3f))
          (logior #x80 (logand character #x3f))))
   (t
    (let ((byte (- character #x3fff00)))
      (list (logior #xc0 (logand (ash byte -6) 1))
            (logior #x80 (logand byte #x3f)))))))

(defun emacs-cc-print-1--write-character (character)
  "Write the Emacs character CHARACTER using the active output coding."
  (if (<= character 127)
      (emacs-cc-print-1--write-stderr-byte character)
    (let* ((text (string character))
           (coding (or coding-system-for-write locale-coding-system)))
      (if coding
          (let ((bytes (encode-coding-string text coding)) (index 0))
            (while (< index (length bytes))
              (emacs-cc-print-1--write-stderr-byte (aref bytes index))
              (setq index (1+ index))))
        (dolist (byte (emacs-cc-print-1--internal-character-bytes character))
          (emacs-cc-print-1--write-stderr-byte byte)))))
  character)

(defun emacs-cc-print-1--display-table-entry (character)
  "Return CHARACTER's display-table expansion, if one is active."
  (when (and (<= character #x3fffff)
             (boundp 'standard-display-table)
             (char-table-p standard-display-table)
             (eq (char-table-subtype standard-display-table) 'display-table)
             ;; The provider reports allocated capacity, not mutable subtype
             ;; metadata.  Unknown native layouts are not assumed compatible.
             (fboundp 'emacs-char-table-extra-slot-count)
             (eql (emacs-char-table-extra-slot-count standard-display-table) 6))
    (let ((entry (char-table-range standard-display-table character)))
      (and (vectorp entry) (not (stringp entry)) entry))))

(defun emacs-cc-print-1--external-debugging-output (character)
    "Write CHARACTER to stderr.
You can call `print' while debugging emacs, and pass this function
to make it write to the debugging output."
    (unless (fixnump character)
      (signal 'wrong-type-argument (list 'fixnump character)))
    (let* ((code (logand character #xffffffff))
           (display (emacs-cc-print-1--display-table-entry code)))
      (condition-case err
          (if display
              (dotimes (index (length display))
                (let ((entry (aref display index)))
                  (when (characterp entry)
                    (emacs-cc-print-1--write-character entry))))
            (let* ((resolved (char-resolve-modifiers code))
                   (resolved (logand resolved (lognot #x0fc00000))))
              (when (> resolved #x3fffff)
                (signal 'error
                        (list (format "Invalid character: %x" resolved))))
              (emacs-cc-print-1--write-character resolved)))
        (emacs-cc-print-1--stderr-write-error
         (if (plist-get (cddr err) :cleanup-failure)
             (signal (car err) (cdr err))
           nil)))
    character))

(unless (fboundp 'external-debugging-output)
  (defalias 'external-debugging-output
    #'emacs-cc-print-1--external-debugging-output))

(unless (fboundp 'flush-standard-output)
  (defun flush-standard-output ()
    "Flush standard-output.
This can be useful after using `princ' and the like in scripts."
    (and standard-output nil)))

(unless (fboundp 'print--preprocess)
  (defun print--preprocess (object)
    "Extract sharing info from OBJECT needed to print it.
Fills `print-number-table' if `print-circle' is non-nil.  Does nothing
if `print-circle' is nil."
    (when print-circle
      ;; The standalone printer owns its sharing table.  Calling it for its
      ;; side effect also handles circular structures without retaining state.
      (prin1-to-string object))
    nil))

(defun emacs-cc-print-1--collect-strings (object)
  "Return strings in OBJECT's list/vector traversal order, or nil on cycles."
  ;; gnu562: an atom has no cycle to detect; answer before building the two
  ;; tables and the walker (`prin1-to-string' of a symbol spent 74 us here).
  (if (not (or (consp object) (and (vectorp object) (not (stringp object)))))
      (and (stringp object) (list object))
    (let ((active (make-hash-table :test 'eq))
          (seen (make-hash-table :test 'eq))
          (strings nil)
          (cycle nil))
      (cl-labels ((walk (value)
                    (cond
                     ((stringp value) (setq strings (cons value strings)))
                     ((consp value)
                      (when (if print-circle (gethash value seen)
                              (gethash value active))
                        (setq cycle t))
                      (unless cycle
                        (puthash value t active)
                        (puthash value t seen)
                        (walk (car value))
                        (walk (cdr value))
                        (remhash value active)))
                     ((and (vectorp value) (not (stringp value)))
                      (when (if print-circle (gethash value seen)
                              (gethash value active))
                        (setq cycle t))
                      (unless cycle
                        (puthash value t active)
                        (puthash value t seen)
                        (dotimes (index (length value))
                          (walk (aref value index)))
                        (remhash value active))))))
        (walk object))
      (unless cycle (nreverse strings)))))

(defun emacs-cc-print-1--property-string (orig string)
  "Return STRING's readable property-vector form using ORIG for literals."
  (let ((runs (and (fboundp 'emacs-buffer-string-text-property)
                   (emacs-buffer-string-text-property 'runs string))))
    (if (null runs)
        (funcall orig string)
      (let ((parts (list "#(" (funcall orig string))))
        (dolist (run runs)
          (setq parts
                (nconc parts
                       (list " " (funcall orig (nth 0 run))
                             " " (funcall orig (nth 1 run)) " "
                             (prin1-to-string (nth 2 run)))))
        (setq parts (nconc parts (list ")"))))
        (apply #'concat parts)))))

(defun emacs-cc-print-1--next-string-token (text start)
  "Find the next quoted string token in printed TEXT after START.
Skip escaped syntax and vertical-bar-quoted symbol names."
  (let ((index start)
        (length (length text))
        (symbol-quoted nil)
        (token nil))
    (while (and (< index length) (null token))
      (let ((character (aref text index)))
        (cond
         ((eq character ?\\)
          (setq index (min length (+ index 2))))
         ((eq character ?|)
          (setq symbol-quoted (not symbol-quoted)
                index (1+ index)))
         ((and (not symbol-quoted) (eq character ?\x22))
          (let ((end (1+ index))
                (escaped nil))
            (while (and (< end length)
                        (or escaped (not (eq (aref text end) ?\x22))))
              (if escaped
                  (setq escaped nil)
                (when (eq (aref text end) ?\\)
                  (setq escaped t)))
              (setq end (1+ end)))
            (if (< end length)
                (setq token (cons index (1+ end)))
              (setq index length))))
         (t (setq index (1+ index))))))
    token))

(defun emacs-cc-print-1--replace-string-literals (orig object strings)
  "Print OBJECT with managed string leaves represented by their sidecar runs."
  (let ((printed (funcall orig object))
        (remaining strings)
        (cursor 0)
        (parts nil)
        (changed nil)
        (failed nil))
    (while (and remaining (not failed))
      (let* ((string (pop remaining))
             (literal (funcall orig string))
             (token (emacs-cc-print-1--next-string-token printed cursor)))
        (if (or (null token)
                (not (equal literal
                            (substring printed (car token) (cdr token)))))
            (setq failed t)
          (let ((position (car token))
                (end (cdr token)))
            (push (substring printed cursor position) parts)
            (push (emacs-cc-print-1--property-string orig string) parts)
            (when (and (fboundp 'emacs-buffer-string-text-property)
                       (emacs-buffer-string-text-property 'runs string))
              (setq changed t))
            (setq cursor end)))))
    (if (or failed (not changed))
        printed
      (push (substring printed cursor) parts)
      (apply #'concat (nreverse parts)))))

(defun emacs-cc-print-1--call-native (orig object noescape overrides)
  "Call ORIG for OBJECT while honoring NOESCAPE and print OVERRIDES."
  (cond
   ((and noescape (stringp object))
    (let ((copy (substring object 0)))
      (set-text-properties 0 (length copy) nil copy)
      copy))
   ((and (listp overrides)
         (or (assq 'length overrides) (assq 'level overrides)))
    (let ((print-length (if (assq 'length overrides)
                            (cdr (assq 'length overrides))
                          print-length))
          (print-level (if (assq 'level overrides)
                           (cdr (assq 'level overrides))
                         print-level)))
      (funcall orig object noescape)))
   ((or noescape overrides)
    (funcall orig object noescape overrides))
   (t (funcall orig object))))

(defun emacs-cc-print-1--prin1-to-string-managed
    (orig object noescape overrides)
  "Print OBJECT, whose strings carry managed properties, through ORIG."
  (let ((native (lambda (value)
                  (emacs-cc-print-1--call-native
                   orig value noescape overrides)))
        (leaves (if (stringp object)
                    (list object)
                  (emacs-cc-print-1--collect-strings object))))
    (if leaves
        (emacs-cc-print-1--replace-string-literals native object leaves)
      (funcall native object))))

(defun emacs-cc-print-1--prin1-to-string-around
    (orig object &optional noescape overrides)
  "Preserve managed properties and forward NOESCAPE and OVERRIDES.
The closure for managed strings lives in
`emacs-cc-print-1--prin1-to-string-managed': on the standalone VM a
function that builds a closure over its arguments runs several times
slower even when it takes the other branch."
  (if (or noescape overrides
          (not (fboundp 'emacs-buffer-string-text-property)))
      (emacs-cc-print-1--call-native orig object noescape overrides)
    (let ((strings (if (stringp object)
                       (list object)
                     (emacs-cc-print-1--collect-strings object)))
          (managed nil))
      (while strings
        (when (emacs-buffer-string-text-property 'runs (car strings))
          (setq managed t))
        (setq strings (cdr strings)))
      (if (not managed)
          (emacs-cc-print-1--call-native orig object noescape overrides)
        (emacs-cc-print-1--prin1-to-string-managed
         orig object noescape overrides)))))

(defun emacs-cc-print-1--prn-to-string-around
    (orig object escape &optional depth)
  "Print dead buffers as the literal killed-buffer marker.
Delegate every other value and all printer options to ORIG."
  (if (and (bufferp object)
           (not (buffer-live-p object)))
      "#<killed buffer>"
    (funcall orig object escape depth)))

(defun emacs-cc-print-1--prin1-with-overrides (orig object stream overrides)
  "Print OBJECT to STREAM with call-local OVERRIDES using ORIG.
Apply settings in order, including resets, before invoking the printer.
The underlying printer already reads dynamically bound print variables."
  (let ((print-length print-length)
        (print-level print-level)
        (print-circle print-circle)
        (print-quoted print-quoted)
        (print-gensym print-gensym)
        (print-continuous-numbering print-continuous-numbering)
        (print-number-table print-number-table)
        (print-escape-newlines print-escape-newlines)
        (print-escape-control-characters print-escape-control-characters)
        (print-escape-nonascii print-escape-nonascii)
        (print-escape-multibyte print-escape-multibyte)
        (print-charset-text-property print-charset-text-property)
        (print-unreadable-function print-unreadable-function)
        (float-output-format float-output-format)
        (print-integers-as-characters print-integers-as-characters)
        (settings '((length print-length nil)
                    (level print-level nil)
                    (circle print-circle nil)
                    (quoted print-quoted t)
                    (gensym print-gensym nil)
                    (continuous-numbering print-continuous-numbering nil)
                    (number-table print-number-table nil)
                    (escape-newlines print-escape-newlines nil)
                    (escape-control-characters print-escape-control-characters nil)
                    (escape-nonascii print-escape-nonascii nil)
                    (escape-multibyte print-escape-multibyte nil)
                    (charset-text-property print-charset-text-property default)
                    (unreadable-function print-unreadable-function nil)
                    (float-format float-output-format nil)
                    (integers-as-characters print-integers-as-characters nil)))
        (tail (if (eq overrides t) '(t) overrides)))
    (while (consp tail)
      (let ((entry (car tail)))
        (if (eq entry t)
            (dolist (setting settings)
              (set (nth 1 setting) (nth 2 setting)))
          (unless (consp entry)
            (signal 'wrong-type-argument (list 'consp entry)))
          (let ((setting (assq (car entry) settings)))
            (unless setting
              (signal 'wrong-type-argument (list 'symbolp nil)))
            (set (nth 1 setting) (cdr entry)))))
      (setq tail (cdr tail)))
    (when tail
      (signal 'wrong-type-argument (list 'consp overrides)))
    ;; Let ORIG report stream errors before rendering the object.
    (when (and stream (not (nelisp--valid-print-stream-p stream)))
      (funcall orig object stream nil))
    ;; GNU ignores non-integer limits and negative lengths.  Normalize
    ;; only while rendering so output callbacks still see the given values.
    (let ((text (let ((print-length (and (integerp print-length)
                                         (>= print-length 0) print-length))
                      (print-level (and (integerp print-level) print-level)))
                  (prin1-to-string object))))
      (princ text stream)
      object)))

(defun emacs-cc-print-1--prin1-around
    (orig object &optional stream overrides)
  "Preserve managed string properties when printing OBJECT to STREAM."
  (cond
   (overrides
    (emacs-cc-print-1--prin1-with-overrides orig object stream overrides))
   ((not (fboundp 'emacs-buffer-string-text-property))
    (funcall orig object stream overrides))
   (t
    (let ((strings (if (stringp object)
                       (list object)
                     (emacs-cc-print-1--collect-strings object)))
          (managed nil))
      (while strings
        (when (emacs-buffer-string-text-property 'runs (car strings))
          (setq managed t))
        (setq strings (cdr strings)))
      (if (not managed)
          (funcall orig object stream overrides)
        (princ (prin1-to-string object nil overrides) stream)
        object)))))

(when (and (fboundp 'nelisp--buffer-multibyte-p)
           (fboundp 'prin1-to-string))
  (advice-add 'prin1-to-string :around
              #'emacs-cc-print-1--prin1-to-string-around)
  (when (fboundp 'nelisp--prn-to-string)
    (unless (advice-member-p #'emacs-cc-print-1--prn-to-string-around
                             'nelisp--prn-to-string)
      (advice-add 'nelisp--prn-to-string :around
                  #'emacs-cc-print-1--prn-to-string-around)))
  (when (fboundp 'prin1)
    (advice-add 'prin1 :around #'emacs-cc-print-1--prin1-around)))

(unless (fboundp 'redirect-debugging-output)
  (defun redirect-debugging-output (file &optional append)
    "Redirect debugging output (stderr stream) to file FILE.
If FILE is nil, reset target to the initial stderr stream.
Optional arg APPEND non-nil (interactively, with prefix arg) means
append to existing target file."
    (unless (or (null file) (stringp file))
      (signal 'wrong-type-argument (list 'stringp file)))
    (when (and file (not (file-writable-p (or (file-name-directory file) default-directory))))
      (signal 'file-error (list "Opening output file" "Permission denied" file)))
    (when (and file append)
      (with-temp-buffer
        (insert-file-contents file)))
    (setq emacs-cc-print-1--debugging-output-file file)
    nil))

(provide 'emacs-cc-print-1)
