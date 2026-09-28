;;; nelisp-maxmin-source-probe.el --- Isolated max/min source probe -*- lexical-binding: t; -*-

;; Load only the max/min definitions from the source stdlib.  Loading the
;; complete file would intentionally replace arithmetic primitives in this
;; Host Emacs process, which is unnecessary for this focused differential.

(let ((wanted '(nelisp--maxmin-nan-p nelisp--maxmin-int-float-order
                nelisp--maxmin-order min max)))
  (with-temp-buffer
    (insert-file-contents "lisp/nelisp-stdlib.el")
    (goto-char (point-min))
    (let (form)
      (while (setq form (condition-case nil (read (current-buffer))
                          (end-of-file nil)))
        (when (and (eq (car-safe form) 'defun)
                   (memq (nth 1 form) wanted))
          (eval form)))))
  (prin1 (eval (car (read-from-string (getenv "NELISP_PROBE_FORM"))))))

;;; nelisp-maxmin-source-probe.el ends here
