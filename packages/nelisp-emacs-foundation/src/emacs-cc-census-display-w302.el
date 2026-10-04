;;; emacs-cc-census-display-w302.el --- Timed waits without redisplay  -*- lexical-binding: t; -*-

;;; Commentary:

;; The window probes in this lane require fixes to the window provider:
;; native buffer acceptance, parent access, and dedication/combination
;; setters.  Leave those primitives installed by the bundle unchanged.

;;; Code:

(defun emacs-cc-census-display-w302--sleep (seconds)
  "Wait for positive SECONDS using the existing OS sleep primitive."
  (when (> seconds 0)
    (let* ((whole (truncate seconds))
           (fraction (truncate (* (- seconds whole) 1000000000.0)))
           (timespec (alloc-bytes 16 8)))
      (ptr-write-u64 timespec 0 whole)
      (ptr-write-u64 timespec 8 fraction)
      (nl-nanosleep timespec))))

(unless (fboundp 'sleep-for)
  (defun sleep-for (seconds &optional milliseconds)
    "Pause without redisplay for SECONDS plus MILLISECONDS / 1000.
SECONDS must be a number; MILLISECONDS must be nil or a fixnum.
Non-positive durations return immediately.  Service timers and process
output when their providers are installed, and always return nil."
    (unless (numberp seconds)
      (signal 'wrong-type-argument (list 'numberp seconds)))
    (unless (or (null milliseconds) (fixnump milliseconds))
      (signal 'wrong-type-argument (list 'fixnump milliseconds)))
    (let ((duration (+ seconds (/ (or milliseconds 0) 1000.0))))
      (when (> duration 0)
        (let* ((deadline (+ (float-time) duration))
               (remaining duration))
          (while (> remaining 0)
            (cond
             ((fboundp 'nelisp-process-adapter--wait)
              (nelisp-process-adapter--wait nil remaining nil nil))
             ((fboundp 'nelisp-async-core-sit-for)
              (nelisp-async-core-sit-for remaining))
             (t (emacs-cc-census-display-w302--sleep remaining)))
            ;; Process output and interrupted system calls can return
            ;; early.  They do not end a sleep before its deadline.
            (setq remaining (- deadline (float-time)))))))
    nil))

(provide 'emacs-cc-census-display-w302)
;;; emacs-cc-census-display-w302.el ends here
