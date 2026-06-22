;;; bootstrap-step-walk.el --- stepwise emacs-init require diagnostic -*- lexical-binding: t; -*-

;;; Code:

(defvar bootstrap-step-walk-features
  '(nelisp-emacs
    emacs-font-lock-builtins
    emacs-syntax-table
    emacs-elisp-mode))

(defvar bootstrap-step-walk-limit nil)

(defvar bootstrap-step-walk-print-timing t
  "Non-nil means include elapsed time in progress output when available.")

(defun bootstrap-step-walk--now ()
  "Return a timestamp suitable for elapsed reporting, or nil."
  (and (fboundp 'float-time)
       (ignore-errors (float-time))))

(defun bootstrap-step-walk--elapsed-field (start end)
  "Return a printable elapsed field for START and END."
  (let ((elapsed (and bootstrap-step-walk-print-timing
                      (numberp start)
                      (numberp end)
                      (- end start))))
    (if elapsed
        (format " elapsed=%S" elapsed)
      "")))

(defun bootstrap-step-walk-run ()
  "Require `bootstrap-step-walk-features' one by one with progress output."
  (let ((index 0))
    (catch 'done
      (dolist (feature bootstrap-step-walk-features)
        (when (and bootstrap-step-walk-limit
                   (>= index bootstrap-step-walk-limit))
          (throw 'done nil))
        (princ (format "bootstrap-step index=%S feature=%S status=start\n"
                       index feature))
        (let ((start (bootstrap-step-walk--now)))
          (condition-case err
              (progn
                (require feature)
                (princ (format "bootstrap-step index=%S feature=%S status=done%s\n"
                               index feature
                               (bootstrap-step-walk--elapsed-field
                                start (bootstrap-step-walk--now)))))
            (error
             (princ (format "bootstrap-step index=%S feature=%S status=error err=%S%s\n"
                            index feature err
                            (bootstrap-step-walk--elapsed-field
                             start (bootstrap-step-walk--now))))
             (signal (car err) (cdr err)))))
        (setq index (1+ index)))))
  (princ "bootstrap-step summary=done\n"))

(provide 'bootstrap-step-walk)

;;; bootstrap-step-walk.el ends here
