;;; gc-port-float-blocks.el --- Doc 223 typed float blocks self-test -*- lexical-binding: t; -*-

;; Floats are allocated from 32 KiB typed blocks of 2046 sixteen-byte cells (GNU float_block),
;; marked through a header bit, swept per block and ranked into the image dump like symbol cells.
;; Debug switches: (nelisp--debug-switch 44) answers (LIVE FREE BLOCKS ALLOCATED) float cells;
;; (nelisp--debug-switch 45) answers (SURVIVED TYPED): a float held only by native frames
;; survives a collection and is a float_block cell.
;; GCFB_MODE selects the run:
;;   run       allocation, marking, sweeping, free-list reuse, the conservative scan;
;;   dump      run, then dump the heap to GCFB_IMAGE;
;;   restore   under --cold-load-from GCFB_IMAGE: the dumped floats, now image objects, survive
;;             collections and new cells work beside them;
;;   redump    under --cold-load-from GCFB_IMAGE: more floats, then a second-generation dump to
;;             GCFB_IMAGE2;
;;   restore2  under --cold-load-from GCFB_IMAGE2: both populations.
;; Prints "GCFB MODE checks=N failures=F PASS|FAIL" last.
(defvar gcfb-mode nil)
(defvar gcfb-checks 0)
(defvar gcfb-failures 0)
(defvar gcfb-keep nil)
(defvar gcfb-keep2 nil)
(setq gcfb-mode (or (getenv "GCFB_MODE") "run") gcfb-checks 0 gcfb-failures 0)
(defun gcfb-check (ok label)
  (setq gcfb-checks (1+ gcfb-checks))
  (unless ok
    (setq gcfb-failures (1+ gcfb-failures))
    (princ (format "GCFB FAIL %s\n" label))))
(defun gcfb-stats () (nelisp--debug-switch 44))
(defun gcfb-gc () (garbage-collect) (garbage-collect))
(defun gcfb-fill (n base)
  (let ((v (make-vector n 0.0)) (i 0))
    (while (< i n) (aset v i (+ base i 0.25)) (setq i (1+ i)))
    v))
(defun gcfb-ok (v n base)
  (let ((good 0) (i 0))
    (while (< i n)
      (if (= (aref v i) (+ base i 0.25)) (setq good (1+ good)))
      (setq i (1+ i)))
    good))
(defun gcfb-churn (n)
  ;; Garbage floats; other values follow so no recorded result slot still names one.
  (let ((i 0) (x 0.0))
    (while (< i n) (setq x (* i 1.5)) (setq i (1+ i))))
  (list 1 2 3)
  (length (make-list 10 nil)))

(cond
 ((equal gcfb-mode "restore")
  (gcfb-check (vectorp gcfb-keep) "the kept vector came through the image")
  (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "image floats intact after the restore")
  (gcfb-gc)
  (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "image floats survive two collections")
  (let ((v (gcfb-fill 2500 5000.0)))
    (gcfb-gc)
    (gcfb-check (= (gcfb-ok v 2500 5000.0) 2500) "new floats beside the image floats")
    (gcfb-check (>= (nth 0 (gcfb-stats)) 2500) "live count after the restore")
    (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "image floats unaffected")))
 ((equal gcfb-mode "restore2")
  (gcfb-check (and (vectorp gcfb-keep) (vectorp gcfb-keep2)) "both vectors came through")
  (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "first generation intact")
  (gcfb-check (= (gcfb-ok gcfb-keep2 2500 7000.0) 2500) "second generation intact")
  (gcfb-gc)
  (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "first generation survives collections")
  (gcfb-check (= (gcfb-ok gcfb-keep2 2500 7000.0) 2500) "second generation survives collections"))
 ((equal gcfb-mode "redump")
  (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "image floats intact before the redump")
  (setq gcfb-keep2 (gcfb-fill 2500 7000.0))
  (gcfb-gc)
  (gcfb-check (= (gcfb-ok gcfb-keep2 2500 7000.0) 2500) "new floats before the redump")
  (gcfb-check (> (nelisp--arena-dump-image-stream (getenv "GCFB_IMAGE2")) 0) "second-generation dump")
  (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "first generation intact after the redump")
  (gcfb-check (= (gcfb-ok gcfb-keep2 2500 7000.0) 2500) "second generation intact after the redump")
  (gcfb-gc)
  (gcfb-check (= (gcfb-ok gcfb-keep2 2500 7000.0) 2500) "survives collections after the redump"))
 (t
  (gcfb-gc)
  (let* ((s0 (gcfb-stats)) (b0 (nth 2 s0)) (a0 (nth 3 s0)))
    (setq gcfb-keep (gcfb-fill 3000 1000.0))
    (gcfb-check (and (vectorp gcfb-keep) (= (length gcfb-keep) 3000)) "a vector of 3000")
    (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "fresh floats")
    (gcfb-gc)
    (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "floats survive two collections")
    (let ((s1 (gcfb-stats)))
      (gcfb-check (>= (nth 0 s1) 3000) (format "3000 live floats after the sweep: %S" s1))
      (gcfb-check (>= (- (nth 2 s1) b0) 1) (format "float blocks in use grew: %S %S" s0 s1))
      (gcfb-check (>= (- (nth 3 s1) a0) 3000) (format "3000 allocated: %S %S" s0 s1))
      ;; Sweeping and reuse, with the conservative scan off so no stale stack
      ;; word keeps the dropped population.
      (nelisp--debug-switch 28)
      (gcfb-churn 9000)
      (gcfb-gc)
      (let ((s2 (gcfb-stats)))
        (gcfb-check (< (nth 0 s2) (+ (nth 0 s1) 200)) (format "the garbage floats are not live: %S %S" s1 s2))
        (gcfb-check (>= (nth 1 s2) 1000) (format "free cells after the sweep: %S" s2))
        (let ((v (gcfb-fill 2000 3000.0)))
          (let ((s3 (gcfb-stats)))
            (gcfb-check (<= (nth 2 s3) (+ (nth 2 s2) 1)) (format "new floats reuse free cells: %S %S" s2 s3))
            (gcfb-check (= (gcfb-ok v 2000 3000.0) 2000) "reused cells")
            (gcfb-gc)
            (gcfb-check (= (gcfb-ok v 2000 3000.0) 2000) "reused cells survive")
            (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "kept floats unaffected"))))
      (nelisp--debug-switch 29)
      ;; Doc 223 conservative scan: a float held only by native frames survives and
      ;; is a float_block cell.
      (let ((r (nelisp--debug-switch 45)))
        (gcfb-check (equal r '(1 1)) (format "a float held only by the native stack survives: %S" r)))))
  (when (equal gcfb-mode "dump")
    (gcfb-gc)
    (gcfb-check (> (nelisp--arena-dump-image-stream (getenv "GCFB_IMAGE")) 0) "image dump")
    (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "floats intact after the dump")
    (gcfb-gc)
    (gcfb-check (= (gcfb-ok gcfb-keep 3000 1000.0) 3000) "floats survive collections after the dump"))))
(princ (format "GCFB %s checks=%d failures=%d %s\n" gcfb-mode gcfb-checks gcfb-failures
               (if (= gcfb-failures 0) "PASS" "FAIL")))
nil
