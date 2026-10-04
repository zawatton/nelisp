;;; emacs-cc-print-1.el --- C-core print primitives -*- lexical-binding: t; -*-

(defvar emacs-cc-print-1--debugging-output-file nil)
(defvar print-circle nil)
(defvar print-length nil)
(defvar print-level nil)
(defvar print-quoted t)
(defvar print-gensym nil)
(defvar print-continuous-numbering nil)
(defvar print-number-table nil)
(defvar print-charset-text-property 'default)
(defvar print-unreadable-function nil)
(defvar float-output-format nil)
(defvar print-integers-as-characters nil)

(defun emacs-cc-print-1--write-stderr-byte (byte)
  "Write BYTE to the process stderr through libc and return BYTE.
Use a duplicate of descriptor 2 so closing the stream leaves stderr open."
  (require 'nl-ffi)
  (require 'emacs-network-ffi)
  (let ((libc emacs-network-ffi-libc-path))
    (unless (and (stringp libc) (> (length libc) 0))
      (error "external-debugging-output: system libc path is unavailable"))
    (ffi:library libc)
    (let* ((call (lambda (name c-name args result values)
                   (funcall #'nl-ffi--invoke name c-name args result values)))
           (fd (funcall call 'external-debugging-output--dup "dup"
                        '(:sint32) :sint32 '(2)))
           (stream nil))
      (unless (and (integerp fd) (>= fd 0))
        (error "external-debugging-output: dup(stderr) failed: %S" fd))
      (unwind-protect
          (progn
            (setq stream (funcall call 'external-debugging-output--fdopen
                                  "fdopen" '(:sint32 :pointer) :pointer
                                  (list fd "w")))
            (unless (and (integerp stream) (/= stream 0))
              (error "external-debugging-output: fdopen(stderr) failed"))
            (unless (= (funcall call 'external-debugging-output--fputc
                                "fputc" '(:sint32 :pointer) :sint32
                                (list byte stream))
                       byte)
              (error "external-debugging-output: fputc failed")))
        (if (and (integerp stream) (/= stream 0))
            (funcall call 'external-debugging-output--fclose
                     "fclose" '(:pointer) :sint32 (list stream))
          (funcall call 'external-debugging-output--close
                   "close" '(:sint32) :sint32 (list fd))))
      byte)))

(unless (fboundp 'external-debugging-output)
  (defun external-debugging-output (character)
    "Write CHARACTER to stderr.
You can call `print' while debugging emacs, and pass this function
to make it write to the debugging output."
    (unless (integerp character)
      (signal 'wrong-type-argument (list 'fixnump character)))
    (when (> character #x10ffff)
      (signal 'error (list (format "Invalid character: %x" character))))
    (mapc #'emacs-cc-print-1--write-stderr-byte
          (append (encode-coding-string (string character) 'utf-8) nil))
    character))

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
    (unless cycle (nreverse strings))))

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

(defun emacs-cc-print-1--prin1-to-string-around
    (orig object &optional noescape overrides)
  "Preserve managed properties and forward NOESCAPE and OVERRIDES."
  (let ((native (lambda (value)
                  (emacs-cc-print-1--call-native
                   orig value noescape overrides))))
    (if (or noescape overrides
            (not (fboundp 'emacs-buffer-string-text-property)))
        (funcall native object)
      (let* ((strings (if (stringp object)
                          (list object)
                        (emacs-cc-print-1--collect-strings object)))
             (managed nil))
        (while strings
          (when (emacs-buffer-string-text-property 'runs (car strings))
            (setq managed t))
          (setq strings (cdr strings)))
        (if (not managed)
            (funcall native object)
          (let ((leaves (if (stringp object)
                            (list object)
                          (emacs-cc-print-1--collect-strings object))))
            (if leaves
                (emacs-cc-print-1--replace-string-literals
                 native object leaves)
              (funcall native object))))))))

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
