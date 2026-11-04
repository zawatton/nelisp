;;; emacs-cc-signal-names-1.el --- signal-names compatibility -*- lexical-binding: t; -*-

(unless (fboundp 'signal-names)
  (defun signal-names (&rest arguments)
    "Return the platform signal abbreviations in GNU Emacs order.

The Linux/glibc implementation derives names from `sigabbrev_np' and
the runtime realtime-signal limit.  A libc without those APIs signals
its ordinary FFI resolution error rather than returning a guessed list."
    (unless (null arguments)
      (signal 'wrong-number-of-arguments
              (list 'signal-names (length arguments))))
    (require 'emacs-network-ffi)
    (require 'nl-ffi)
    (ffi:library emacs-network-ffi-libc-path)
    (let* ((minimum (nl-ffi--invoke
                     'signal-names "__libc_current_sigrtmin" nil :sint32 nil))
           (maximum (nl-ffi--invoke
                     'signal-names "__libc_current_sigrtmax" nil :sint32 nil))
           (number maximum)
           names)
      (unless (and (integerp minimum) (integerp maximum)
                   (> minimum 0) (>= maximum minimum))
        (signal 'error (list "libc returned invalid realtime signal limits"
                             minimum maximum)))
      (while (> number 0)
        (let ((name
               (if (>= number minimum)
                   (let ((from-minimum (- number minimum))
                         (from-maximum (- maximum number)))
                     (if (< from-maximum from-minimum)
                         (if (= from-maximum 0) "RTMAX"
                           (format "RTMAX-%d" from-maximum))
                       (if (= from-minimum 0) "RTMIN"
                         (format "RTMIN+%d" from-minimum))))
                 (let ((pointer (nl-ffi--invoke
                                'signal-names "sigabbrev_np" '(:sint32) :pointer
                                (list number))))
                   (and (integerp pointer) (> pointer 0)
                        (nl-ffi-get-string pointer))))))
          (when name
            (push name names)))
        (setq number (1- number)))
      (append (nreverse names) '("EXIT")))))

(provide 'emacs-cc-signal-names-1)
;;; emacs-cc-signal-names-1.el ends here
