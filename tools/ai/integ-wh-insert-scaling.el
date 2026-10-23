;;; integ-wh-insert-scaling.el --- the cost of a run of propertized inserts scales linearly  -*- lexical-binding: t; -*-

;; NeLisp only (it reads the buffer's interval list).  A buffer built by N propertized inserts at its end has N intervals
;; (GNU keeps one per insert and so does this) and each insert must not walk them: doubling N must about double the time.
;; Two series, N = 200 ... 3,200: the same plist every time, and a distinct plist value with plain text between (the
;; no-props speed test's shape).  Each time is the best of three runs after a warm-up run; the verdict is the worst ratio of
;; consecutive doublings against a bound of 2.6 (the walk made it 3.0 to 3.7: 0.063, 0.194, 0.665 and 2.433 s at 200, 400,
;; 800 and 1,600 on batch D).  Prints one line per N and a final INSERT-SCALING PASS or FAIL line.

(defun iwh-sc-run (series n)
  "Build a buffer with N inserts of SERIES, answer (SECONDS . INTERVALS)."
  (let ((t0 (float-time)) count)
    (with-temp-buffer
      (if (eq series 'same)
          (dotimes (_ n) (insert (propertize "xy" 'face 'bold)))
        (dotimes (i n) (insert (propertize "line of text " 'p i)) (insert "plain\n")))
      (setq count (length (nelisp-buffer-text-properties nelisp--current-buffer))))
    (cons (- (float-time) t0) count)))

(defun iwh-sc-best (series n)
  (let ((best nil) (count 0) (i 0))
    (while (< i 3)
      (let ((r (iwh-sc-run series n)))
        (setq count (cdr r))
        (when (or (null best) (< (car r) best)) (setq best (car r))))
      (setq i (1+ i)))
    (cons best count)))

(let ((worst 0.0) (bad nil) (sizes '(200 400 800 1600 3200)))
  (dolist (series '(same mixed))
    (iwh-sc-run series 200)             ; warm-up
    (let ((prev nil))
      (dolist (n sizes)
        (let* ((r (iwh-sc-best series n)) (ratio (and prev (/ (car r) (car prev)))))
          (princ (format "INSERT-SCALING %s n=%d seconds=%.4f intervals=%d%s\n" series n (car r) (cdr r)
                         (if ratio (format " ratio=%.2f" ratio) "")))
          (unless (= (cdr r) (if (eq series 'same) n n)) (setq bad (format "%s n=%d: %d intervals" series n (cdr r))))
          (when (and ratio (> ratio worst)) (setq worst ratio))
          (setq prev r)))))
  (princ (if (and (not bad) (<= worst 2.6))
             (format "INSERT-SCALING PASS worst-ratio=%.2f\n" worst)
           (format "INSERT-SCALING FAIL worst-ratio=%.2f %s\n" worst (or bad "")))))
