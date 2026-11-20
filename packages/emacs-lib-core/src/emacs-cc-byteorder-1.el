;;; emacs-cc-byteorder-1.el --- Byte order primitive -*- lexical-binding: t; -*-

(defun emacs-cc-byteorder-1--decode-htonl (value)
  "Decode the result of libc htonl applied to one."
  (cond
   ((equal value 1) ?B)
   ((equal value #x01000000) ?l)
   (t (error "byteorder: unexpected htonl result %S" value))))

(defun emacs-cc-byteorder-1--linux-x86-64-p ()
  "Whether the raw syscall numbers below are valid on this host."
  (and (eq system-type 'gnu/linux)
       (equal system-configuration "x86_64-pc-linux-gnu")))

(defun emacs-cc-byteorder-1--observe ()
  "Return GNU's byteorder code by observing a native integer in memory."
  (unless (emacs-cc-byteorder-1--linux-x86-64-p)
    (error "byteorder: raw memory observation supports Linux x86_64 only"))
  (let ((buffer (syscall-direct 9 0 8 3 #x22 -1 0)))
    (when (< buffer 0)
      (error "byteorder: mmap scratch allocation failed: %S" buffer))
    (let ((value nil) (primary nil) (cleanup nil))
      (unwind-protect
          (condition-case err
              (progn
                (ptr-write-u64 buffer 0 1)
                (let ((byte (ptr-read-u8 buffer 0)))
                  (setq value
                        (cond ((= byte 1) ?l)
                              ((= byte 0) ?B)
                              (t (error "byteorder: unexpected first byte %S"
                                        byte))))))
            (t (setq primary err)))
        (condition-case err
            (let ((released (syscall-direct 11 buffer 8 0 0 0 0)))
              (unless (= released 0)
                (setq cleanup
                      (list 'error
                            (format "byteorder: munmap scratch release failed: %S"
                                    released)))))
          (t (setq cleanup err))))
      (cond
       ((and primary cleanup)
        (signal (car primary)
                (append (cdr primary) (list :cleanup-failure cleanup))))
       (primary (signal (car primary) (cdr primary)))
       (cleanup (signal (car cleanup) (cdr cleanup)))
       (t value)))))

(unless (fboundp 'byteorder)
  (defun byteorder (&rest arguments)
    "Return 66 for big-endian machines or 108 for little-endian machines."
    (when arguments
      (signal 'wrong-number-of-arguments
              (list 'byteorder (length arguments))))
    (emacs-cc-byteorder-1--observe)))

(provide 'emacs-cc-byteorder-1)
;;; emacs-cc-byteorder-1.el ends here
