;;; standalone-gc-image-hole-check.el --- garbage in cold-image holes is collected -*- lexical-binding: t -*-

;; Usage: GC_CHECK=loop|cons|pause nelisp --cold-load-from BIN.cold \
;;          --load test/standalone-gc-image-hole-check.el
;;
;; A cold image carries the dumping run's freed blocks.  They are reused
;; for new allocations, and the collector must reclaim what is put there:
;; the image's own objects are permanent, the holes between them are not.
;; Live bytes are read after two collections (`nelisp--arena-stats'
;; field 4, :live-after-last-gc).
;;   loop   three runs of a 100000-iteration counting loop leave < 1 MiB
;;          of live growth in total (it was ~288 bytes per iteration)
;;   cons   200000 discarded conses leave < 1 MiB of live growth
;;   pause  after 2000000 iterations of churn, one collection takes at
;;          most 1.5 x (plus 0.05 s) what it took before the churn
;; Prints one GC-HOLE line and exits non-zero on failure.

(defun gc-hole--live ()
  (garbage-collect) (garbage-collect)
  (nth 4 (nelisp--arena-stats)))
(defun gc-hole--count (n)
  (let ((i 0)) (while (< i n) (setq i (1+ i)))))
(defun gc-hole--conses (n)
  (let ((i 0)) (while (< i n) (cons i 2) (setq i (1+ i)))))
(defun gc-hole--pause ()
  (garbage-collect)
  (let ((t0 (float-time)))
    (garbage-collect)
    (- (float-time) t0)))

(let* ((check (or (getenv "GC_CHECK") "loop"))
       (mib 1048576)
       (ok nil))
  (cond
   ((equal check "loop")
    (let ((b (gc-hole--live)))
      (dotimes (_ 3) (gc-hole--count 100000))
      (let ((d (- (gc-hole--live) b)))
        (setq ok (< d mib))
        (princ (format "GC-HOLE loop live-growth %d bytes (limit %d)\n" d mib)))))
   ((equal check "cons")
    (let ((b (gc-hole--live)))
      (gc-hole--conses 200000)
      (let ((d (- (gc-hole--live) b)))
        (setq ok (< d mib))
        (princ (format "GC-HOLE cons live-growth %d bytes (limit %d)\n" d mib)))))
   ((equal check "pause")
    (let ((before (min (gc-hole--pause) (gc-hole--pause))))
      (gc-hole--count 2000000)
      (let ((after (min (gc-hole--pause) (gc-hole--pause))))
        (setq ok (<= after (+ (* 1.5 before) 0.05)))
        (princ (format "GC-HOLE pause %.3f s after churn vs %.3f s before (limit 1.5x+0.05)\n"
                       after before))))))
  (princ (format "GC-HOLE %s %s\n" check (if ok "PASS" "FAIL")))
  (kill-emacs (if ok 0 1)))

;;; standalone-gc-image-hole-check.el ends here
