;;; emacs-cc-filelock-1.el --- File locking C primitive replacements -*- lexical-binding: t; -*-

(defun emacs-cc-filelock-1--buffer-file (file)
  "Return FILE, or the current buffer's visited file."
  (or file (and (bufferp (current-buffer)) (buffer-file-name))))

(unless (fboundp 'lock-buffer)
  (defun lock-buffer (&optional file)
    "Lock FILE, if current buffer is modified.
FILE defaults to current buffer's visited file,
or else nothing is done if current buffer isn't visiting a file.

If the option `create-lockfiles' is nil, this does nothing."
    (when (and file (not (stringp file)))
      (signal 'wrong-type-argument (list 'stringp file)))
    (when (and (or (null file) (stringp file))
               (bound-and-true-p create-lockfiles)
               (buffer-modified-p)
               (emacs-cc-filelock-1--buffer-file file)
               (fboundp 'lock-file))
      (lock-file (emacs-cc-filelock-1--buffer-file file)))))

(unless (fboundp 'unlock-buffer)
  (defun unlock-buffer ()
    "Unlock the file visited in the current buffer.
If the buffer is not modified, this does nothing because the file
should not be locked in that case.  It also does nothing if the
current buffer is not visiting a file, or is not locked.  Handles file
system errors by calling `display-warning' and continuing as if the
error did not occur."
    (when (and (buffer-modified-p)
               (emacs-cc-filelock-1--buffer-file nil)
               (fboundp 'unlock-file))
      (condition-case err
          (unlock-file (emacs-cc-filelock-1--buffer-file nil))
        (file-error
         (display-warning 'files (error-message-string err)))))))

(provide 'emacs-cc-filelock-1)
