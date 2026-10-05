;;; b88-buffer-timing.el --- Time staged parity forms in runner order -*- lexical-binding: t; -*-
;; Run after --cold-load-from, with C_CORE_SELECTED_PROBES pointing to the
;; audit area's selected.el.  P rows preserve the certifying driver's output.
(load (getenv "C_CORE_SELECTED_PROBES") nil t)
(let ((index 0) (total 0.0))
  (dolist (entry c-core-parity--staged-entries)
    (dolist (form (cdr entry))
      (let* ((start (float-time))
             (value (condition-case err
                        (prin1-to-string (eval form t))
                      (error (prin1-to-string
                              (list 'ERR (car err) (cdr err))))))
             (elapsed (- (float-time) start)))
        (setq index (1+ index) total (+ total elapsed))
        (princ (format "T| %d | %s | %.6f\n" index (car entry) elapsed))
        (princ (format "P| %s | %s\n" (car entry) value)))))
  (princ (format "T-DONE| %d | %.6f\n" index total)))
(princ "P-DONE\n")
t
;;; b88-buffer-timing.el ends here
