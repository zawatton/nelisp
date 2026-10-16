;;; diag-native-startup.el --- evaluate the native startup companion without its guard  -*- lexical-binding: t; -*-

;; Diagnostic driver for test/support/diag-windows-f1b.py (not a gate).
;; DIAG_MODE=raw   evaluate the companion with `nelisp--eval-source-string'
;;                 outside any condition-case, so an error prints the original
;;                 backtrace instead of the re-signal in `nelisp-native-startup-ensure'.
;; DIAG_MODE=forms read and evaluate the companion form by form in Lisp,
;;                 printing the index and head of the first failing form.

(let* ((path (expand-file-name (concat invocation-name ".native-startup.el") invocation-directory))
       (source (with-temp-buffer (insert-file-contents path) (buffer-string)))
       (mode (getenv "DIAG_MODE")))
  (princ (format "DIAG-STARTUP mode=%s size=%d\n" mode (length source)))
  (if (equal mode "raw")
      (progn (nelisp--eval-source-string source)
             (princ "DIAG-STARTUP-RAW-OK\n"))
    (let ((pos 0) (index 0) (done nil))
      (while (not done)
        (let ((read (condition-case nil (read-from-string source pos) (end-of-file nil))))
          (if (null read)
              (setq done t)
            (setq pos (cdr read) index (1+ index))
            (condition-case err
                (eval (car read) t)
              (error
               (princ (format "DIAG-STARTUP-FORM-FAIL index=%d end=%d head=%S second=%S error=%S\n"
                              index pos (car-safe (car read))
                              (car-safe (cdr-safe (car read))) err))
               (signal (car err) (cdr err))))
            (when (= 0 (% index 200))
              (princ (format "DIAG-STARTUP-PROGRESS index=%d end=%d\n" index pos)))))))
      (princ (format "DIAG-STARTUP-FORMS-OK forms=%d\n" index)))))

;;; diag-native-startup.el ends here
