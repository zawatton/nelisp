;;; emacs-cc-xfaces-2.el --- xfaces C primitive compatibility -*- lexical-binding: t; -*-

(unless (fboundp 'x-family-fonts)
  (defun x-family-fonts (&optional family frame)
    "Return a list of available fonts of family FAMILY on FRAME."
    (when (and family (not (stringp family)))
      (signal 'wrong-type-argument (list 'stringp family)))
    (let ((target (or frame (selected-frame))))
      (unless (frame-live-p target)
        (signal 'wrong-type-argument (list 'frame-live-p target)))
      ;; GNU batch builds without a window system have no font families.
      nil)))

(unless (fboundp 'x-list-fonts)
  (defun x-list-fonts (pattern &optional face frame maximum width)
    "Return a list of the names of available fonts matching PATTERN."
    (ignore face frame maximum width)
    ;; This is the result from GNU Emacs built in batch mode without a
    ;; window system; it signals before validating PATTERN.
    (signal 'error (list "Window system is not in use or not initialized"))))

(unless (fboundp 'x-load-color-file)
  (defun x-load-color-file (filename)
    "Create an alist of color entries from an external file."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (when (file-readable-p filename)
      (let (colors)
        (with-temp-buffer
          (insert-file-contents filename)
          (goto-char (point-min))
          (while (not (eobp))
            (let* ((line (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))
                  (fields (split-string line "[ \t]+" t)))
              (when (>= (length fields) 4)
                (let ((red (string-to-number (nth 0 fields)))
                      (green (string-to-number (nth 1 fields)))
                      (blue (string-to-number (nth 2 fields)))
                      (name (mapconcat #'identity (nthcdr 3 fields) " ")))
                  (push (cons name (logior (ash red 16) (ash green 8) blue))
                        colors)))
              (forward-line 1)))
        colors)))))

(provide 'emacs-cc-xfaces-2)
