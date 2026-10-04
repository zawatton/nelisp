;;; emacs-cc-census-process-w301.el --- Descriptor, mutex and process queries  -*- lexical-binding: t; -*-

;; File descriptors belong to the runtime, rather than to a buffer.
(defvar emacs-cc-census-process-w301--fds nil)

(defun emacs-cc-census-process-w301--dbus-error (filename)
  "Signal the D-Bus error for a file that could not be opened."
  (unless (get 'dbus-error 'error-conditions)
    (define-error 'dbus-error "D-Bus error"))
  (signal 'dbus-error (list "Cannot open file" filename)))

(unless (fboundp 'dbus--fd-open)
  (defun dbus--fd-open (filename)
    "Open FILENAME read-only, reusing an already registered descriptor."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (let* ((filename (expand-file-name filename))
           (registered (rassoc filename emacs-cc-census-process-w301--fds)))
      (if registered
          (car registered)
        ;; The standalone's path syscall accepts a NUL-terminated filename.
        ;; Linux open(2), O_RDONLY | O_CLOEXEC, with no creation mode.
        (let ((fd (nelisp--syscall-path-int 2 filename #o2000000)))
          (when (<= fd 0)
            (emacs-cc-census-process-w301--dbus-error filename))
          (setq emacs-cc-census-process-w301--fds
                (cons (cons fd filename) emacs-cc-census-process-w301--fds))
          fd)))))

(unless (fboundp 'dbus--fd-close)
  (defun dbus--fd-close (fd)
    "Close registered descriptor FD, returning whether closing succeeded."
    (unless (integerp fd)
      (signal 'wrong-type-argument (list 'fixnump fd)))
    (let ((registered (assq fd emacs-cc-census-process-w301--fds)))
      (when registered
        (setq emacs-cc-census-process-w301--fds
              (delq registered emacs-cc-census-process-w301--fds))
        (= (syscall-direct 3 fd 0 0 0 0 0) 0)))))

(unless (fboundp 'dbus--registered-fds)
  (defun dbus--registered-fds ()
    "Return an alist of open D-Bus descriptors and their filenames.
The list and each association are copied, protecting the registry."
    (let ((fds emacs-cc-census-process-w301--fds) (result nil))
      (while fds
        (setq result (cons (cons (caar fds) (cdar fds)) result)
              fds (cdr fds)))
      (nreverse result))))

(defun emacs-cc-census-process-w301--check-mutex (mutex)
  "Validate the fallback mutex object MUTEX."
  (unless (mutexp mutex)
    (signal 'wrong-type-argument (list 'mutexp mutex))))

(unless (fboundp 'mutex-lock)
  (defun mutex-lock (mutex)
    "Acquire MUTEX recursively for the current thread."
    (emacs-cc-census-process-w301--check-mutex mutex)
    ;; The fallback thread implementation runs synchronously.  Slot 2,
    ;; initially nil, holds (OWNER . RECURSION-COUNT) while locked.
    (let ((state (aref mutex 2)) (thread (current-thread)))
      (cond
       ((null state) (aset mutex 2 (cons thread 1)))
       ((eq (car state) thread) (setcdr state (1+ (cdr state))))
       (t (signal 'nelisp-unsupported-primitive (list 'mutex-lock)))))
    nil))

(unless (fboundp 'mutex-unlock)
  (defun mutex-unlock (mutex)
    "Release one acquisition of MUTEX by the current thread."
    (emacs-cc-census-process-w301--check-mutex mutex)
    (let ((state (aref mutex 2)))
      (unless (and state (eq (car state) (current-thread)))
        (signal 'error (list "Cannot unlock mutex owned by another thread")))
      (if (= (cdr state) 1)
          (aset mutex 2 nil)
        (setcdr state (1- (cdr state)))))
    nil))

(defun emacs-cc-census-process-w301--read-file (filename)
  "Read FILENAME without decoding, or return nil if it is unavailable."
  (condition-case nil
      (with-temp-buffer
        (set-buffer-multibyte nil)
        (insert-file-contents-literally filename)
        (buffer-string))
    ((file-error file-missing) nil)))

(defun emacs-cc-census-process-w301--command-line (text comm)
  "Format /proc command line TEXT with GNU's whitespace escaping."
  (let ((end (length text)) (index 0) (pieces nil))
    (while (and (> end 0) (= (aref text (1- end)) 0))
      (setq end (1- end)))
    (if (= end 0)
        (concat "[" comm "]")
      (while (< index end)
        (let ((char (aref text index)))
          (when (or (memq char '(9 10 11 12 13 32)) (= char 92))
            (setq pieces (cons "\\" pieces)))
          (setq pieces (cons (char-to-string (if (= char 0) 32 char)) pieces)))
        (setq index (1+ index)))
      (apply #'concat (nreverse pieces)))))

(defun emacs-cc-census-process-w301--linux-attributes (pid)
  "Read Linux procfs attributes of PID.
Unavailable fields are omitted, as with GNU's platform-dependent alist."
  (let* ((directory (concat "/proc/" (number-to-string pid)))
         (stat (emacs-cc-census-process-w301--read-file
                (concat directory "/stat")))
         (attributes nil))
    (when stat
      ;; A command name can contain spaces and parentheses.  The final
      ;; closing parenthesis separates it from the numeric fields.
      (let ((open (string-match "(" stat)) (close (1- (length stat))))
        (while (and (> close 0) (/= (aref stat close) 41))
          (setq close (1- close)))
        (when (and open (> close open))
          (let* ((comm (substring stat (1+ open) close))
                 (fields (split-string (substring stat (+ close 2)) "[ \t\n]+" t))
                 (indices '((ppid . 1) (pgrp . 2) (sess . 3) (tpgid . 5)
                            (minflt . 7) (cminflt . 8) (majflt . 9)
                            (cmajflt . 10) (pri . 15) (nice . 16)
                            (thcount . 17)))
                 (status (emacs-cc-census-process-w301--read-file
                          (concat directory "/status")))
                 (command (emacs-cc-census-process-w301--read-file
                           (concat directory "/cmdline"))))
            (setq attributes (list (cons 'comm comm)))
            (when (>= (length fields) 22)
              (setq attributes (cons (cons 'state (car fields)) attributes))
              (dolist (entry indices)
                (setq attributes
                      (cons (cons (car entry)
                                  (string-to-number (nth (cdr entry) fields)))
                            attributes)))
              (setq attributes
                    (cons (cons 'vsize (/ (string-to-number (nth 20 fields)) 1024))
                          attributes)))
            (when status
              (dolist (entry '((euid . "Uid") (egid . "Gid") (rss . "VmRSS")))
                (when (string-match
                       (concat "^" (cdr entry) ":[ \t]+\\([0-9]+\\)"
                               (if (eq (car entry) 'rss) ""
                                 "[ \t]+\\([0-9]+\\)"))
                       status)
                  (setq attributes
                        (cons (cons (car entry)
                                    (string-to-number
                                     (match-string (if (eq (car entry) 'rss) 1 2)
                                                   status)))
                              attributes)))))
            (when command
              (setq attributes
                    (cons (cons 'args
                                (emacs-cc-census-process-w301--command-line
                                 command comm))
                          attributes)))))))
    attributes))

(unless (fboundp 'process-attributes)
  (defun process-attributes (pid)
    "Return a platform-dependent alist of operating system attributes of PID."
    (let ((handler (find-file-name-handler default-directory 'process-attributes)))
      (if handler
          (funcall handler 'process-attributes pid)
        (unless (numberp pid)
          (signal 'wrong-type-argument (list 'numberp pid)))
        (unless (and (>= pid -2147483648) (<= pid 2147483647)
                     (or (integerp pid) (= pid (truncate pid))))
          (signal 'error
                  (list "Not an in-range integer, integral float, or cons of integers")))
        (emacs-cc-census-process-w301--linux-attributes (truncate pid))))))

(provide 'emacs-cc-census-process-w301)
;;; emacs-cc-census-process-w301.el ends here
