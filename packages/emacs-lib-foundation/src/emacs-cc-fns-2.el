;;; emacs-cc-fns-2.el --- GNU fns.c primitives, unit 2  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-fns-2--hash-string (string)
  "Return a deterministic integer hash of STRING."
  (let ((h 5381) (i 0))
    (while (< i (length string))
      (setq h (logand 2147483647 (+ (* h 33) (aref string i)))
            i (1+ i)))
    h))

(unless (fboundp 'internal--hash-table-index-size)
  (defun internal--hash-table-index-size (hash-table)
    "Index size of HASH-TABLE.  Internal use only."
    (unless (hash-table-p hash-table)
      (signal 'wrong-type-argument (list 'hash-table-p hash-table)))
    (if (and (fboundp 'hash-table-size)
             (condition-case nil (hash-table-size hash-table) (error nil)))
        (hash-table-size hash-table)
      (let ((n (hash-table-count hash-table)))
        (max 8 (1+ (* n 2)))))))

(unless (fboundp 'load-average)
  (defun load-average (&optional use-floats)
    "Return list of 1 minute, 5 minute and 15 minute load averages.
When USE-FLOATS is non-nil, return unscaled floats."
    (let ((file (and (fboundp 'file-readable-p) (file-readable-p "/proc/loadavg"))))
      (unless file (error "Could not obtain load average"))
      (with-temp-buffer
        (insert-file-contents "/proc/loadavg")
        (let* ((parts (split-string (buffer-string) "[ \t\n]+" t))
               (vals (mapcar 'string-to-number (butlast parts (- (length parts) 3)))))
          (mapcar (lambda (x) (if use-floats x (truncate (* x 100)))) vals))))))

(unless (fboundp 'locale-info)
  (defun locale-info (item)
    "Access locale data ITEM when available."
    (cond
      ((eq item 'codeset) (or (and (boundp 'locale-coding-system)
                         (symbolp locale-coding-system)
                         (symbol-name locale-coding-system)) "UTF-8"))
      ((eq item 'days) ["Sunday" "Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"])
      ((eq item 'months) ["January" "February" "March" "April" "May" "June"
                          "July" "August" "September" "October" "November" "December"])
      (t nil))))

(unless (fboundp 'secure-hash-algorithms)
  (defun secure-hash-algorithms ()
    "Return the algorithms supported by `secure-hash'."
    '(md5 sha1 sha224 sha256 sha384 sha512)))

(unless (fboundp 'sxhash-equal-including-properties)
  (defun sxhash-equal-including-properties (obj)
    "Return an integer hash code for OBJ suitable for equal-including-properties."
    (if (and (fboundp 'sxhash-equal) (not (stringp obj)))
        (sxhash-equal obj)
      (let ((text (if (stringp obj)
                      (concat (prin1-to-string (substring-no-properties obj))
                              (prin1-to-string
                               (let ((i 0) (props nil))
                                 (while (< i (length obj))
                                   (push (text-properties-at i obj) props)
                                   (setq i (1+ i)))
                                 (nreverse props))))
                    (prin1-to-string obj))))
        (if (fboundp 'sxhash-equal)
            (sxhash-equal text)
          (emacs-cc-fns-2--hash-string text))))))

(provide 'emacs-cc-fns-2)
;;; emacs-cc-fns-2.el ends here
