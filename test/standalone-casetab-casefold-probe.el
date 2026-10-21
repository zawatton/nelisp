;;; standalone-casetab-casefold-probe.el --- Library case-fold timing gate -*- lexical-binding: t -*-

;; Run on a library image.  The coordinator's reproduction uses this same
;; subject, pattern and 50-call sample; each mode is warmed before timing.
(let ((subject (apply #'concat (make-list 40 "Alpha Beta coding: utf-8 Gamma ")))
      (count 50)
      plain folded)
  (dolist (fold '(nil t))
    (let ((case-fold-search fold))
      (unless (equal (string-match "coding: *utf" subject) 11)
        (error "Case-fold timing probe returned the wrong match"))
      (let ((started (float-time)))
        (dotimes (_ count) (string-match "coding: *utf" subject))
        (let ((elapsed (/ (* 1000 (- (float-time) started)) count)))
          (if fold (setq folded elapsed) (setq plain elapsed))
          (princ (format "case-fold %s: %.6f ms/match\n" fold elapsed))))))
  (let ((ratio (/ folded (max plain 0.000001))))
    (princ (format "CASETAB-CASEFOLD %s ratio=%.6f limit=1.5\n"
                   (if (<= ratio 1.5) "PASS" "FAIL") ratio))
    (kill-emacs (if (<= ratio 1.5) 0 1))))

;;; standalone-casetab-casefold-probe.el ends here
