;;; emacs-cc-search-1.el --- POSIX search primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-search-1--string-match (regexp string start inhibit)
  (let ((m (nelisp-rx-string-match regexp string (or start 0))))
    (when m
      (unless inhibit
        (nelisp-ec-rx-match-data-to-ec 0 m))
      (plist-get m :start))))

(defvar cache-long-scans t)
(defvar emacs-cc-search-1--newline-cache nil
  "Buffer-local newline cache: t before its first diagnostic snapshot.
A snapshot contains the modification tick, accessible bounds and positions.")
(make-variable-buffer-local 'cache-long-scans)
(make-variable-buffer-local 'emacs-cc-search-1--newline-cache)
(put 'emacs-cc-search-1--newline-cache 'permanent-local t)

(defun emacs-cc-search-1--cache-buffer ()
  "Return the base buffer that owns the current buffer's newline cache."
  (or (buffer-base-buffer) (current-buffer)))

(defun emacs-cc-search-1--start-newline-scan (&rest _args)
  "Record newline cache creation or removal after successful line motion."
  (let ((enabled cache-long-scans)
        (indirect (buffer-base-buffer)))
    (with-current-buffer (emacs-cc-search-1--cache-buffer)
      ;; An indirect buffer cannot toggle a cache against its base policy.
      (when (or (not indirect) (eq (not enabled) (not cache-long-scans)))
        (if enabled
            (unless emacs-cc-search-1--newline-cache
              (setq emacs-cc-search-1--newline-cache t))
          (setq emacs-cc-search-1--newline-cache nil))))))

(defun emacs-cc-search-1--newline-positions ()
  "Scan accessible text for newline character positions without line motion."
  (let* ((start (point-min))
         (text (buffer-substring-no-properties start (point-max)))
         (index 0) (positions nil))
    (while (< index (length text))
      (when (= (aref text index) 10)
        (push (+ start index) positions))
      (setq index (1+ index)))
    (vconcat (nreverse positions))))

;; The host keeps its C cache.  Standalone line primitives can be prebound,
;; so attach to their public entry points after the buffer shims load.
(when (fboundp 'nelisp--repr)
  (dolist (function '(forward-line line-beginning-position line-end-position
                     beginning-of-line end-of-line))
    (advice-add function :after #'emacs-cc-search-1--start-newline-scan)))

(unless (fboundp 'newline-cache-check)
  (defun newline-cache-check (&optional buffer)
    "Return cached and scanned newline position vectors for BUFFER.

BUFFER defaults to the current buffer.  Return nil if no cache exists
or `cache-long-scans' is nil.  Line motion creates the cache lazily."
    (unless (or (null buffer) (bufferp buffer))
      (signal 'wrong-type-argument (list 'bufferp buffer)))
    (when (buffer-live-p (or buffer (current-buffer)))
      (with-current-buffer (or buffer (current-buffer))
        (with-current-buffer (emacs-cc-search-1--cache-buffer)
          (when (and cache-long-scans emacs-cc-search-1--newline-cache)
            (let* ((tick (buffer-chars-modified-tick))
                   (start (point-min)) (end (point-max))
                   (cache emacs-cc-search-1--newline-cache))
              (unless (and (consp cache) (equal (car cache) tick)
                           (= (nth 1 cache) start) (= (nth 2 cache) end))
                (setq cache (list tick start end
                                  (emacs-cc-search-1--newline-positions))
                      emacs-cc-search-1--newline-cache cache))
              (vector (copy-sequence (nth 3 cache))
                      (emacs-cc-search-1--newline-positions)))))))))

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
