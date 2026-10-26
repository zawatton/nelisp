;;; many-modifiers.el --- GNU's lread-many-modifiers form with a time bound  -*- lexical-binding: t -*-
;; Exit 0 when the 500000-prefix string reads to the GNU result in under 0.5 s (GNU: 0.06 s; v17: 4.9 s).
(let* ((n 500000)
       (s (concat "\"" (apply #'concat (make-list n "\\M-")) "a\""))
       (t0 (float-time))
       (r (read-from-string s))
       (dt (- (float-time) t0))
       (ok (equal r (cons "\M-a" (+ (* n 3) 3)))))
  (princ (format "equal=%S elapsed=%.3f\n" ok dt))
  (kill-emacs (cond ((not ok) 1) ((> dt 0.5) 2) (t 0))))
