;;; textprop1-scale.el --- text-property scaling probes  -*- lexical-binding: t; -*-
;; Env: PROBE (name), M (interval count / loop count).  Prints "RESULT PROBE M seconds check".
;; Setup is outside the timed window.
(defconst tsp-chunk (concat "abcdefghijklmnopqrstuvwxyz0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789abcdefghijklmnopqrst" "uvwxyzAB"))
(defun tsp-setup-buf (m)
  "A buffer of 100*M characters carrying M intervals of alternating faces."
  (let ((b (generate-new-buffer " tsp")))
    (with-current-buffer b
      (dotimes (_ m) (insert tsp-chunk))
      (dotimes (k m)
        (put-text-property (1+ (* k 100)) (+ 50 (* k 100)) 'face (if (= 0 (% k 2)) 'bold 'italic))))
    b))
(let* ((probe (getenv "PROBE")) (m (string-to-number (getenv "M"))) (t0 0.0) (res nil))
  (cond
   ((string= probe "a1-loop")
    ;; the survey A1 loop: M puts into a 200000-character string
    (let ((s (make-string 200000 ?a)))
      (put-text-property 0 200000 'face 'bold s)
      (setq t0 (float-time))
      (dotimes (k m) (put-text-property (* k 50) (+ 10 (* k 50)) 'face 'italic s))
      (setq res (length (format "%S" (next-single-property-change 0 'face s))))))
   ((string= probe "build")
    (let ((b (generate-new-buffer " tsp")) (n 0))
      (with-current-buffer b
        (dotimes (_ m) (insert tsp-chunk))
        (setq t0 (float-time))
        (dotimes (k m) (put-text-property (1+ (* k 100)) (+ 50 (* k 100)) 'face (if (= 0 (% k 2)) 'bold 'italic)))
        (setq res (buffer-size)))))
   ((string= probe "put-existing")
    (let ((b (tsp-setup-buf m)))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (i 400) (put-text-property (+ 10 (* 100 (% (* i 7) m))) (+ 60 (* 100 (% (* i 7) m))) 'x i))
        (setq res (buffer-size)))))
   ((string= probe "get")
    (let ((b (tsp-setup-buf m)) (c 0))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (i 2000) (when (get-text-property (+ 20 (* 100 (% (* i 13) m))) 'face) (setq c (1+ c))))
        (setq res c))))
   ((string= probe "props-at")
    (let ((b (tsp-setup-buf m)) (c 0))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (i 2000) (setq c (+ c (length (text-properties-at (+ 20 (* 100 (% (* i 13) m))))))))
        (setq res c))))
   ((string= probe "any")
    (let ((b (tsp-setup-buf m)) (c 0))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (_ 20) (setq c (+ c (or (text-property-any 1 (1+ (* 100 m)) 'face 'nosuch) 0))))
        (setq res c))))
   ((string= probe "not-all")
    (let ((b (tsp-setup-buf m)) (c 0))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (_ 20) (setq c (+ c (or (text-property-not-all 1 (1+ (* 100 m)) 'face nil) 0))))
        (setq res c))))
   ((string= probe "walk")
    (let ((b (tsp-setup-buf m)) (c 0))
      (with-current-buffer b
        (setq t0 (float-time))
        (let ((p 1)) (while (setq p (next-single-property-change p 'face)) (setq c (1+ c))))
        (let ((p (point-max))) (while (setq p (previous-single-property-change p 'face)) (setq c (1+ c))))
        (setq res c))))
   ((string= probe "insert-mid")
    (let ((b (tsp-setup-buf m)))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (i 400) (goto-char (+ 5 (* 100 (% (* i 7) m)))) (insert "z"))
        (setq res (buffer-size)))))
   ((string= probe "delete-mid")
    (let ((b (tsp-setup-buf m)))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (i 400) (let ((p (+ 5 (* 100 (% (* i 7) (- m 10)))))) (delete-region p (+ p 2))))
        (setq res (buffer-size)))))
   ((string= probe "bufstr")
    (let ((b (tsp-setup-buf m)) (c 0))
      (with-current-buffer b
        (setq t0 (float-time))
        (dotimes (i 20) (setq c (+ c (length (buffer-substring 1 (min (point-max) (+ 1 (* 100 m))))))))
        (setq res c))))
   (t (error "unknown probe %s" probe)))
  (princ (format "RESULT %s %d %.6f %S\n" probe m (- (float-time) t0) res)))
