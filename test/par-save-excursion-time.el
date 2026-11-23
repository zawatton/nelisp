;;; -*- lexical-binding: t; -*-
;; save-excursion cost row (F424).  Load, then call (sx-time-row) from a SEPARATE top-level form (outside `load').
;; Prints t when an empty-body save-excursion costs <= 0.6x the pre-F424 cost on the NeLisp image
;; (11.4 us -> limit 6.84 us; GNU is far below).
(defun sx-time-row ()
  (with-temp-buffer
    (insert "abcdefghij") (goto-char 5)
    (let* ((n 20000) (i 0) (t0 (float-time)))
      (while (< i n) (save-excursion (goto-char 3)) (setq i (1+ i)))
      (let ((per (/ (- (float-time) t0) n)))
        (princ (format "T1 save-excursion <= 0.6x old (6.84 us)  %S\n" (<= per 6.84e-6)))
        (message "   (mean %.3f us)" (* per 1e6))))))
