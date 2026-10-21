;;; gc-port-symbol-blocks.el --- Doc 226 D9 symbol_block self-test -*- lexical-binding: t; -*-

;; Drives the symbol_block self-test arms of `nelisp--debug-switch' on a
;; line where no Lisp operation makes a symbol cell yet:
;;   (nelisp--debug-switch 40 N)  a vector of N fresh uninterned symbol
;;                                cells, each holding its index in its four
;;                                edges (a legacy cons view, a direct cons,
;;                                a fixnum, a two-element list);
;;   (nelisp--debug-switch 41 V)  how many cells of V still hold exactly
;;                                that, with no collector bit set;
;;   (nelisp--debug-switch 42)    (LIVE FREE BLOCKS ALLOCATED) symbol cells.
;; The cells are never printed or handed to a symbol primitive: on this
;; line no symbol consumer reads a direct symbol WORD.
;; GCSB_MODE selects the run:
;;   run      allocation, marking, sweeping and free-list reuse;
;;   dump     run, then dump the heap to GCSB_IMAGE;
;;   restore  under --cold-load-from GCSB_IMAGE: the dumped cells, now image
;;            objects, survive collections, and new cells work beside them.
;; Prints "GCSB MODE checks=N failures=F PASS|FAIL" last.
;; A restored image carries the dumping run's values: reset the per-run
;; state with setq, and keep only the population (`defvar' leaves it).
(defvar gcsb-mode nil)
(defvar gcsb-checks 0)
(defvar gcsb-failures 0)
(defvar gcsb-keep nil)
(setq gcsb-mode (or (getenv "GCSB_MODE") "run") gcsb-checks 0 gcsb-failures 0)
(defun gcsb-check (ok label)
  (setq gcsb-checks (1+ gcsb-checks))
  (unless ok
    (setq gcsb-failures (1+ gcsb-failures))
    (princ (format "GCSB FAIL %s\n" label))))
(defun gcsb-make (n) (nelisp--debug-switch 40 n))
(defun gcsb-ok (v) (nelisp--debug-switch 41 v))
(defun gcsb-stats () (nelisp--debug-switch 42))
(defun gcsb-gc () (garbage-collect) (garbage-collect))
(defun gcsb-drop (n)
  ;; A population nothing keeps; other values follow it so no recorded
  ;; result slot still names it.
  (gcsb-make n)
  (list 1 2 3)
  (length (make-list 10 nil)))

(if (equal gcsb-mode "restore")
    (progn
      (gcsb-check (vectorp gcsb-keep) "the kept vector came through the image")
      (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "image cells intact after the restore")
      (gcsb-gc)
      (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "image cells survive two collections")
      (let ((v (gcsb-make 700)))
        (gcsb-gc)
        (gcsb-check (= (gcsb-ok v) 700) "new cells beside the image cells")
        (gcsb-check (= (nth 0 (gcsb-stats)) 700) "live count after the restore")
        (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "image cells unaffected")))
  (gcsb-gc)
  (let ((s0 (gcsb-stats)))
    (gcsb-check (equal (list (nth 0 s0) (nth 2 s0) (nth 3 s0)) '(0 0 0))
                "no symbol cell before the test")
    (setq gcsb-keep (gcsb-make 2000))
    (gcsb-check (and (vectorp gcsb-keep) (= (length gcsb-keep) 2000)) "a vector of 2000")
    (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "fresh cells")
    (gcsb-gc)
    (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "cells survive two collections")
    (let ((s1 (gcsb-stats)))
      (gcsb-check (= (nth 0 s1) 2000) "2000 live after the sweep")
      (gcsb-check (= (nth 2 s1) 3) "three blocks of 682 cells")
      (gcsb-check (= (nth 3 s1) 2000) "2000 allocated"))
    ;; Sweeping and reuse, with the conservative scan off so no stale stack
    ;; word keeps the dropped population.
    (nelisp--debug-switch 28)
    (gcsb-drop 1000)
    (gcsb-gc)
    (let ((s2 (gcsb-stats)))
      (gcsb-check (= (nth 0 s2) 2000) "the dropped 1000 are not live")
      ;; 46 cells ended the third block, 682 filled a fourth, 272 began a
      ;; fifth: all 1000 swept to the free list, no block wholly free
      ;; beyond the first block's worth (alloc.c:7050).
      (gcsb-check (= (nth 1 s2) 1000) "1000 free cells")
      (gcsb-check (= (nth 2 s2) 5) "five blocks, none wholly free beyond one block")
      (let ((v (gcsb-make 900)))
        (let ((s3 (gcsb-stats)))
          (gcsb-check (= (nth 2 s3) 5) "900 new cells reuse free cells, no new block")
          (gcsb-check (= (gcsb-ok v) 900) "reused cells")
          (gcsb-gc)
          (gcsb-check (= (gcsb-ok v) 900) "reused cells survive")
          (gcsb-check (= (nth 0 (gcsb-stats)) 2900) "2900 live")
          (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "kept cells unaffected"))))
    (nelisp--debug-switch 29))
  (when (equal gcsb-mode "dump")
    (gcsb-gc)
    (gcsb-check (> (nelisp--arena-dump-image-stream (getenv "GCSB_IMAGE")) 0) "image dump")
    (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "cells intact after the dump")
    (gcsb-gc)
    (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "cells survive collections after the dump")))
(princ (format "GCSB %s checks=%d failures=%d %s\n" gcsb-mode gcsb-checks gcsb-failures
               (if (= gcsb-failures 0) "PASS" "FAIL")))
nil
