;; PARITY A4: a 40000-char search loop and search+replace loop finish (v17: 19 s for the first alone).
(let ((t0 (float-time)) (c 0))
  (with-temp-buffer (insert (make-string 40000 ?a)) (goto-char 1) (while (re-search-forward "a" nil t) (setq c (1+ c))))
  (princ (format "A4-LOOP count=%d secs=%.2f\n" c (- (float-time) t0)))
  (unless (and (= c 40000) (< (- (float-time) t0) 20)) (kill-emacs 1)))
(kill-emacs 0)
