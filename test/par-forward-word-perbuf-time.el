;;; -*- lexical-binding: t; -*-
;; Per-buffer forward-word cost row (F426).  Load, then call (fw-time-row) from a SEPARATE top-level form
;; (outside `load').  Prints t when forward-word in a no-property buffer, after the global property flag was
;; set by another buffer, costs <= 0.3x the old cost (old 67.7 us in save-excursion; limit 20.3 us).
(defun fw-time-row ()
  (with-temp-buffer (insert "x") (put-text-property 1 2 'face 'bold))
  (with-temp-buffer
    (insert (mapconcat #'identity (make-list 200 "foo-bar baz_qux (quux) 12.5") " "))
    (let* ((n 5000) (i 0) (t0 (float-time)))
      (while (< i n) (save-excursion (goto-char 2800) (forward-word 1)) (setq i (1+ i)))
      (let ((per (/ (- (float-time) t0) n)))
        (princ (format "T1 forward-word in save-excursion <= 0.3x old (20.3 us)  %S\n" (<= per 20.3e-6)))
        (message "   (mean %.3f us)" (* per 1e6))))))
