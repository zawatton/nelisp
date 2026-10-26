;;; -*- lexical-binding: t; -*-
;; Scaling check: replace-match in a re-search-forward loop must be linear.
;; Prints "RATIO <t4k/t1k>" ; exits 0 iff ratio < 8 (linear ~4, quadratic ~16).
(defun rm-run (n)
  (with-temp-buffer
    (dotimes (j 4) (insert "foo boo woo "))
    (let ((a 0) (start (float-time)))
      (goto-char (point-min))
      (while (< a n)
        (goto-char (point-min))
        (while (and (< a n) (re-search-forward "[oO]" nil t))
          (replace-match (if (= (% a 2) 0) "O" "o") t t) (setq a (1+ a))))
      (- (float-time) start))))
(rm-run 100)
(let* ((t1 (rm-run 1000)) (t4 (rm-run 4000)) (r (/ t4 (max t1 1e-6))))
  (princ (format "t1k=%.3f t4k=%.3f RATIO %.2f\n" t1 t4 r))
  (kill-emacs (if (< r 8.0) 0 1)))
