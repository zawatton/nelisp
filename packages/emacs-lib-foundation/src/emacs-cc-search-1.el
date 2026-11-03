;;; emacs-cc-search-1.el --- POSIX search primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-search-1--string-match (regexp string start inhibit)
  (let ((m (nelisp-rx-string-match regexp string (or start 0))))
    (when m
      (unless inhibit
        (nelisp-ec-rx-match-data-to-ec 0 m))
      (plist-get m :start))))

(unless (fboundp 'newline-cache-check)
  (defun newline-cache-check (&optional buffer)
    "Check the newline cache of BUFFER against buffer contents.

BUFFER defaults to the current buffer.  Value is nil when no cache exists."
    (unless (or (null buffer) (bufferp buffer))
      (signal 'wrong-type-argument (list 'bufferp buffer)))
    nil))

(unless (fboundp 'posix-looking-at)
  (defun posix-looking-at (regexp &optional inhibit-modify)
    "Return t if text after point matches REGEXP according to Posix rules."
    (unless (stringp regexp)
      (signal 'wrong-type-argument (list 'stringp regexp)))
    (if inhibit-modify
        (save-match-data (and (looking-at regexp) t))
      (and (looking-at regexp) t))))

(unless (fboundp 'posix-search-backward)
  (defun posix-search-backward (regexp &optional bound noerror count)
    "Search backward from point for match for REGEXP according to Posix rules."
    (let ((n (or count 1)) (result nil))
      (if (< n 0)
          (posix-search-forward regexp bound noerror (- n))
        (while (> n 0)
          (setq result (re-search-backward regexp bound noerror))
          (unless result (setq n 0))
          (when result (setq n (1- n))))
        result))))

(unless (fboundp 'posix-search-forward)
  (defun posix-search-forward (regexp &optional bound noerror count)
    "Search forward from point for match for REGEXP according to Posix rules."
    (let ((n (or count 1)) (result nil))
      (if (< n 0)
          (posix-search-backward regexp bound noerror (- n))
        (while (> n 0)
          (setq result (re-search-forward regexp bound noerror))
          (unless result (setq n 0))
          (when result (setq n (1- n))))
        result))))

(unless (fboundp 'posix-string-match)
  (defun posix-string-match (regexp string &optional start inhibit-modify)
    "Return index of start of first match for Posix REGEXP in STRING, or nil."
    (unless (stringp regexp) (signal 'wrong-type-argument (list 'stringp regexp)))
    (unless (stringp string) (signal 'wrong-type-argument (list 'stringp string)))
    (emacs-cc-search-1--string-match regexp string start inhibit-modify)))

(unless (fboundp 're--describe-compiled)
  (defun re--describe-compiled (regexp &optional raw)
    "Return a string describing the compiled form of REGEXP."
    (unless (stringp regexp) (signal 'wrong-type-argument (list 'stringp regexp)))
    (if raw
        ;; Expose a stable textual representation when the runtime has no
        ;; GNU regexp bytecode object.
        (format "%S" (nelisp-rx-compile regexp))
      (signal 'error (list "Not available: rebuild with --enable-checking")))))

(provide 'emacs-cc-search-1)
;;; emacs-cc-search-1.el ends here
