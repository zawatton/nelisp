;;; -*- lexical-binding: t; -*-
;; Per-call driver: OP=gtp|nspc|pt|atp|rtp|rtp0|tpa|none, BENCH_N trips, 1000 intervals.
(defvar tpd-b (with-current-buffer (generate-new-buffer " tpd") (dotimes (k 2500) (insert "abcdefghijklmnopqrstuvwxyz0123456789ABCD"))
  (dotimes (k 1000) (put-text-property (+ 1 (* k 100)) (+ 50 (* k 100)) 'face (if (= 0 (% k 2)) 'bold 'italic))) (current-buffer)))
(defun tpd-run (op n)
  (let ((a 0))
    (with-current-buffer tpd-b
      (cond
       ((eq op 'gtp) (dotimes (i n) (setq a (+ a (if (get-text-property (+ 20 (* 100 (% i 500))) 'face) 1 0)))))
       ((eq op 'nspc) (dotimes (i n) (setq a (+ a (next-single-property-change (+ 20 (* 100 (% i 500))) 'face)))))
       ((eq op 'pt) (dotimes (i n) (put-text-property 101 120 'face (if (= 0 (% i 2)) 'bold 'italic)) (setq a (1+ a))))
       ((eq op 'atp) (dotimes (i n) (add-text-properties 101 120 (list 'face (if (= 0 (% i 2)) 'bold 'italic) 'mouse-face 'highlight)) (setq a (1+ a))))
       ((eq op 'rtp0) (dotimes (i n) (remove-text-properties 101 120 '(help-echo nil)) (setq a (1+ a))))
       ((eq op 'rtp) (dotimes (i n) (put-text-property 101 120 'help-echo "x") (remove-text-properties 101 120 '(help-echo nil)) (setq a (1+ a))))
       ((eq op 'tpa) (dotimes (i n) (setq a (+ a (length (text-properties-at (+ 20 (* 100 (% i 500)))))))))
       ((eq op 'none) (dotimes (i n) (setq a (+ a 1))))))
    a))
(let* ((n (string-to-number (getenv "BENCH_N"))) (op (intern (getenv "OP"))))
  (tpd-run op 2)
  (let* ((start (float-time)) (r (tpd-run op n)) (seconds (- (float-time) start)))
    (princ (format "BENCH-CHECK n=%d result=%S elapsed=%.9f\n" n r seconds))))
(kill-emacs 0)
