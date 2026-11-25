;;; par-dedup-subr.el --- GNU subr.el predicates through native adapters -*- lexical-binding: t; -*-

(defun dedup-subr--live (process)
  (condition-case err
      (process-live-p process)
    (error (list 'error (car err)))))

(let ((pipe (make-pipe-process :name "dedup-probe-pipe" :noquery t)))
  (unwind-protect
      (progn
        (prin1 (list 'process-live-p (dedup-subr--live nil)
                     (dedup-subr--live (process-name pipe))
                     (process-status pipe) (dedup-subr--live pipe)))
        (terpri))
    (delete-process pipe))
  (prin1 (list 'process-deleted (dedup-subr--live pipe)))
  (terpri))

(with-temp-buffer
  (insert "abc")
  (let ((whole (buffer-narrowed-p)))
    (narrow-to-region 2 4)
    (prin1 (list 'buffer-narrowed-p whole (buffer-narrowed-p)))
    (terpri)))
