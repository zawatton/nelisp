;;; crash1/sortgc.el --- sort GC-stress corpus, self-checked against a bucket oracle  -*- lexical-binding: t -*-
;; GNU Emacs 31.1 itself segfaults intermittently on a keyed sort whose KEY conses
;; (observed 3 of 4 runs at n=20000/131072), so these cases are checked against an
;; oracle built without `sort': for keys in 0..999 the stable order is the original
;; order within each key bucket, ascending by key.
(defmacro crash1--case (id form)
  `(message "%s %S" ,id (condition-case e ,form (error (list 'ERR (car e) (cdr e))))))
(defun crash1--lcg (n range)
  "N pseudo-random integers in [0,RANGE) from a fixed LCG (same on both sides)."
  (let ((x 12345) (acc nil))
    (dotimes (_ n)
      (setq x (% (+ (* x 1103515245) 12345) 2147483648))
      (push (% (/ x 16) range) acc))
    acc))
(defun crash1--fold (seq)
  "Order-sensitive checksum of SEQ whose elements are integers or (KEY . INDEX)."
  (let ((h 7) (n 0))
    (mapc (lambda (e) (setq h (% (+ (* h 31) (if (consp e) (+ (* 1000 (car e)) (cdr e)) e)) 1000003) n (1+ n))) seq)
    (list n h)))
(defun crash1--pairs (n range)
  (let ((i -1)) (mapcar (lambda (k) (setq i (1+ i)) (cons k i)) (crash1--lcg n range))))
(defun crash1--sorted-p (seq pred)
  (let ((prev nil) (ok t) (first t))
    (mapc (lambda (e) (when (and (not first) (funcall pred e prev)) (setq ok nil)) (setq prev e first nil)) seq)
    ok))
(defun crash1--car< (a b) (< (car a) (car b)))

(defun crash1--oracle (pairs)
  "Stable ascending order of PAIRS (KEY . INDEX) by KEY, built by bucketing."
  (let ((buckets (make-vector 1000 nil)) (out nil))
    (dolist (p pairs) (aset buckets (car p) (cons p (aref buckets (car p)))))
    (let ((k 999))
      (while (>= k 0)
        (dolist (p (aref buckets k)) (push p out))   ; bucket is reversed, push re-reverses
        (setq k (1- k))))
    out))
(defun crash1--same (a b) (let ((ok (= (length a) (length b)))) (while (and ok a) (unless (and (eq (car a) (car b))) (setq ok nil)) (setq a (cdr a) b (cdr b))) ok))
(dolist (n '(140000 560000))
  (let* ((pairs (crash1--pairs n 1000)) (want (crash1--oracle pairs)) (calls 0))
    (crash1--case (format "gc-pred-%d" n)
      (crash1--same want
        (sort (copy-sequence pairs)
              :lessp (lambda (a b)
                       (setq calls (1+ calls))
                       (when (= 0 (% calls 50000)) (garbage-collect))
                       (let ((junk (list (car a) (car b) (make-string 8 ?x)))) (< (car junk) (cadr junk)))))))
    (setq calls 0)
    (crash1--case (format "gc-key-%d" n)
      (crash1--same want
        (sort (copy-sequence pairs)
              :key (lambda (p) (setq calls (1+ calls))
                     (when (= 0 (% calls 40000)) (garbage-collect))
                     (list (car p) (make-string 4 ?k)))
              :lessp (lambda (a b) (< (car a) (car b))))))
    (crash1--case (format "gc-vec-key-%d" n)
      (crash1--same want
        (append (sort (vconcat pairs)
                      :key (lambda (p) (setq calls (1+ calls))
                             (when (= 0 (% calls 40000)) (garbage-collect))
                             (list (car p) (make-string 4 ?k)))
                      :lessp (lambda (a b) (< (car a) (car b)))) nil)))))
