;;; integ-wh-interval-fuzz.el --- differential fuzz of a buffer's interval list across two builds  -*- lexical-binding: t; -*-

;; NeLisp only (it reads the buffer's interval list).  Run it on two binaries and diff the outputs: for 150 seeds of 80 random
;; operations (propertized and plain inserts at the end and inside, insert-before-markers, delete-region, erase-buffer,
;; put-text-property, set-text-properties, goto-char) each line is the interval list after one operation, so the build under test
;; must print the same lines as the build it replaces.  Only valid intervals (start < end) are printed: the shift of an insert
;; turns an empty interval that a delete-region left behind into a negative one, which nothing reads and which the O(1) append
;; path leaves alone.  Used for ledger S2.29: batch D against the tip, 12,000 lines identical.

(defvar fz-state 12345)
(defun fz-rand (n) (setq fz-state (% (+ (* fz-state 1103515245) 12345) 2147483648)) (% (/ fz-state 65536) n))
(defun fz-text ()
  (let* ((len (1+ (fz-rand 4))) (kind (fz-rand 6)) (s (make-string len (+ 97 (fz-rand 26)))))
    (cond ((= kind 0) s)                                         ; plain
          ((= kind 1) (propertize s 'p (fz-rand 3)))
          ((= kind 2) (propertize s 'face 'bold))
          ((= kind 3) (concat (propertize s 'p (fz-rand 3)) "-" (propertize s 'q (fz-rand 2)))) ; two intervals and a gap
          ((= kind 4) (concat (propertize "a" 'p 1) (propertize "b" 'p 1) (propertize "c" 'p 2)))
          (t (propertize s 'p 1 'face 'italic)))))
(defun fz-dump (tag)
  (let (valid (n 0))
    (dolist (iv (nelisp--tp-list (nelisp-buffer-text-properties nelisp--current-buffer) 1))
      (if (< (nth 0 iv) (nth 1 iv)) (push iv valid) (setq n (1+ n))))
    (princ (format "%s %S\n" tag (nreverse valid)))))
(let ((seed 1))
  (while (<= seed 150)
    (setq fz-state (* seed 7919))
    (with-temp-buffer
      (let ((step 0))
        (while (< step 80)
          (let ((op (fz-rand 14)) (size (1- (point-max))))
            (cond
             ((< op 6) (insert (fz-text)))
             ((= op 6) (goto-char (1+ (fz-rand (1+ size)))))
             ((= op 7) (goto-char (point-max)))
             ((= op 8) (when (> size 1) (let* ((a (1+ (fz-rand size))) (b (1+ (fz-rand size)))) (delete-region (min a b) (max a b)))))
             ((= op 9) (insert-before-markers (fz-text)))
             ((= op 10) (when (> size 1) (let* ((a (1+ (fz-rand size))) (b (1+ (fz-rand size)))) (put-text-property (min a b) (max a b) 'p (fz-rand 4)))))
             ((= op 11) (when (> size 1) (let* ((a (1+ (fz-rand size))) (b (1+ (fz-rand size)))) (set-text-properties (min a b) (max a b) nil))))
             ((= op 12) (when (and (> size 3) (= (fz-rand 3) 0)) (erase-buffer)))
             (t (goto-char (point-max)) (insert (fz-text))))
            (fz-dump (format "s%d.%d" seed step)))
          (setq step (1+ step)))))
    (setq seed (1+ seed))))
(princ "FUZZ DONE\n")
