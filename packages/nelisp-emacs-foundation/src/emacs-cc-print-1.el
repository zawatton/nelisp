;;; emacs-cc-print-1.el --- C-core print primitives -*- lexical-binding: t; -*-

(defvar emacs-cc-print-1--debugging-output-file nil)
(defvar print-circle nil)

(unless (fboundp 'external-debugging-output)
  (defun external-debugging-output (character)
    "Write CHARACTER to stderr.
You can call `print' while debugging emacs, and pass this function
to make it write to the debugging output."
    (unless (integerp character)
      (signal 'wrong-type-argument (list 'fixnump character)))
    (when (> character #x10ffff)
      (signal 'error (list (format "Invalid character: %x" character))))
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
