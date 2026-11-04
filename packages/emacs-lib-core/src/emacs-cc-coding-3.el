;;; emacs-cc-coding-3.el --- Coding C-core replacements -*- lexical-binding: t; -*-

(defun emacs-cc-coding-3--unencodable-p (character coding-system)
  "Return non-nil if CHARACTER cannot round-trip through CODING-SYSTEM."
  (let ((name (if (symbolp coding-system) (symbol-name coding-system) "")))
    (cond
     ((or (equal name "ascii") (equal name "us-ascii")) (> character 127))
     ((or (equal name "latin-1") (equal name "iso-latin-1")) (> character 255))
     ((or (equal name "utf-8") (equal name "utf-8-unix")
          (equal name "utf-8-dos") (equal name "utf-8-mac")
          (equal name "no-conversion")) nil)
     (t
      (let* ((text (char-to-string character))
             (encoded (encode-coding-string text coding-system t))
             (decoded (decode-coding-string encoded coding-system t)))
        (not (equal text decoded)))))))

(unless (fboundp 'unencodable-char-position)
  (defun unencodable-char-position (start end coding-system &optional count string)
    "Return position of first un-encodable character in a region.
START and END specify the region and CODING-SYSTEM specifies the
encoding to check.  Return nil if CODING-SYSTEM does encode the region.

If optional 4th argument COUNT is non-nil, it specifies at most how
many un-encodable characters to search.  In this case, the value is a
list of positions.

If optional 5th argument STRING is non-nil, it is a string to search
for un-encodable characters.  In that case, START and END are indexes
to the string and treated as in `substring'."
    (unless (or (null string) (stringp string))
      (signal 'wrong-type-argument (list 'stringp string)))
    (when (and string
               (null start) (setq start 0)) nil)
    (when (and string
               (null end) (setq end (length string))) nil)
    (when (and (not string) (null start) (setq start (point-min))) nil)
    (when (and (not string) (null end) (setq end (point-max))) nil)
    (when (and string
               (or (not (integerp start)) (not (integerp end))
                   (< start 0) (< end start) (> end (length string))))
      (signal 'args-out-of-range (list string start end)))
    (when (and (not string)
               (or (not (integerp start)) (not (integerp end))
                   (< start (point-min)) (> end (point-max)) (> start end)))
      (signal 'args-out-of-range (list (current-buffer) start end)))
    (unless (or (memq coding-system '(ascii us-ascii latin-1 iso-latin-1
                                           utf-8 utf-8-unix utf-8-dos
                                           utf-8-mac no-conversion))
                (and (fboundp 'coding-system-p)
                     (coding-system-p coding-system)))
      (signal 'coding-system-error (list coding-system)))
    (let ((index start) (found nil) (remaining count))
      (while (and (< index end) (not (and count (<= remaining 0))))
        (let ((character (if string (aref string index)
                           (char-after index))))
          (when (and character
                     (emacs-cc-coding-3--unencodable-p character coding-system))
            (if count
                (setq found (cons index found)
                      remaining (1- remaining))
              (setq found index
                    index end))))
        (setq index (1+ index)))
      (if count (nreverse found) found))))

(provide 'emacs-cc-coding-3)
