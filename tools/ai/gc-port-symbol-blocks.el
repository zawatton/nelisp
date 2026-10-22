;;; gc-port-symbol-blocks.el --- Doc 226 D9 symbol_block self-test -*- lexical-binding: t; -*-

;; Drives the symbol_block self-test arms of `nelisp--debug-switch' on a
;; line where no Lisp operation makes a symbol cell yet:
;;   (nelisp--debug-switch 40 N)  a vector of N fresh uninterned symbol
;;                                cells, each holding its index in its four
;;                                edges (a legacy cons view, a direct cons,
;;                                a fixnum, a two-element list);
;;   (nelisp--debug-switch 41 V)  how many cells of V still hold exactly
;;                                that, with no collector bit set;
;;   (nelisp--debug-switch 42)    (LIVE FREE BLOCKS ALLOCATED) symbol cells;
;;   (nelisp--debug-switch 43)    (SURVIVED OUTSIDE): a cons held only by
;;                                native frames survives a collection, and
;;                                its typed group lies outside the chunks.
;; The cells are never printed or handed to a symbol primitive: on this
;; line no symbol consumer reads a direct symbol WORD.
;; Every count is relative to a baseline taken after a warm-up pass.  Where
;; interned symbols are objects (Doc 226 D9 stage 3) a name is a symbol-block
;; cell, created when it is first read, and the first call of a function
;; interns a few more (measured: seven cells at the first call of
;; `gcsb-check', whose `unless' is expanded then); the warm-up pass runs every
;; path of the test once with throwaway sizes, so none of that lands between
;; the baseline and the checks, and the expectations are LIVE0 + N, the blocks
;; the cells fill (682 per block) and N free cells after the drop.
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
(defvar gcsb-quiet nil)
(setq gcsb-mode (or (getenv "GCSB_MODE") "run") gcsb-checks 0 gcsb-failures 0 gcsb-quiet nil)
(defun gcsb-check (ok label)
  (setq gcsb-checks (1+ gcsb-checks))
  (unless ok
    (setq gcsb-failures (1+ gcsb-failures))
    (unless gcsb-quiet (princ (format "GCSB FAIL %s\n" label)))))
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

(defun gcsb-blocks-for (cells) (/ (+ cells 681) 682))

(defun gcsb-pass (n drop reuse)
  ;; One pass of allocation, marking, sweeping and reuse with N cells kept,
  ;; DROP swept and REUSE allocated again, every count relative to the
  ;; baseline taken after a collection.
  (setq gcsb-keep nil)
  ;; The conservative scan is off from the baseline to the end of the counts:
  ;; a stale native word (the warm-up's dead cells are in the frames' unwritten
  ;; slots) would keep a few cells live at the baseline and not later.
  (nelisp--debug-switch 28)
  (gcsb-gc)
  (let* ((s0 (gcsb-stats)) (live0 (nth 0 s0)) (blocks0 (nth 2 s0)) (alloc0 (nth 3 s0)))
    (setq gcsb-keep (gcsb-make n))
    (gcsb-check (and (vectorp gcsb-keep) (= (length gcsb-keep) n)) (format "a vector of %d" n))
    (gcsb-check (= (gcsb-ok gcsb-keep) n) "fresh cells")
    (gcsb-gc)
    (gcsb-check (= (gcsb-ok gcsb-keep) n) "cells survive two collections")
    (let ((s1 (gcsb-stats)))
      (gcsb-check (= (nth 0 s1) (+ live0 n))
                  (format "%d + %d live after the sweep: %S" live0 n s1))
      (gcsb-check (= (nth 3 s1) (+ alloc0 n)) (format "%d allocated" n))
      (gcsb-check (= (nth 2 s1) (max blocks0 (gcsb-blocks-for (+ live0 n))))
                  (format "blocks of 682 cells for %d live: %S" (+ live0 n) s1)))
    ;; Sweeping and reuse.
    (gcsb-drop drop)
    (gcsb-gc)
    (let ((s2 (gcsb-stats)))
      (gcsb-check (= (nth 0 s2) (+ live0 n)) (format "the dropped %d are not live: %S" drop s2))
      ;; All DROP cells swept to the free list; the cells fill
      ;; ceil((LIVE0 + N + DROP) / 682) blocks, and a wholly free block is
      ;; released only beyond one block's worth of free cells (alloc.c:7050).
      (gcsb-check (= (nth 1 s2) drop) (format "%d free cells: %S" drop s2))
      (gcsb-check (= (nth 2 s2) (max blocks0 (gcsb-blocks-for (+ live0 n drop))))
                  (format "blocks, none wholly free beyond one block: %S" s2))
      (let ((v (gcsb-make reuse)))
        (let ((s3 (gcsb-stats)))
          (gcsb-check (= (nth 2 s3) (nth 2 s2)) (format "%d new cells reuse free cells, no new block" reuse))
          (gcsb-check (= (gcsb-ok v) reuse) "reused cells")
          (gcsb-gc)
          (gcsb-check (= (gcsb-ok v) reuse) "reused cells survive")
          (gcsb-check (= (nth 0 (gcsb-stats)) (+ live0 n reuse)) "live count after the reuse")
          (gcsb-check (= (gcsb-ok gcsb-keep) n) "kept cells unaffected"))))
    (nelisp--debug-switch 29)
    ;; Doc 223 conservative scan: a cons held only by native frames, in a
    ;; typed group outside the chunks' address range, survives (debug
    ;; switch 43 answers (SURVIVED OUTSIDE)).  The scan envelope used to
    ;; cover only the chunks.
    (let ((r (nelisp--debug-switch 43)))
      (gcsb-check (equal r '(1 1))
                  (format "a cons held only by the native stack survives: %S" r)))))

(defun gcsb-warm ()
  ;; Every function and builtin of the test is used once, quietly, so that
  ;; whatever a first use interns exists before the baseline.
  (let ((checks gcsb-checks) (failures gcsb-failures))
    (setq gcsb-quiet t)
    (gcsb-pass 10 5 4)
    (setq gcsb-quiet nil gcsb-checks checks gcsb-failures failures gcsb-keep nil)))

(if (equal gcsb-mode "restore")
    (progn
      (gcsb-check (vectorp gcsb-keep) "the kept vector came through the image")
      (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "image cells intact after the restore")
      ;; The warm-up must not touch the restored population.
      (let ((kept gcsb-keep))
        (gcsb-warm)
        (setq gcsb-keep kept))
      (gcsb-gc)
      (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "image cells survive two collections")
      (nelisp--debug-switch 28)
      (gcsb-gc)
      (let ((live0 (nth 0 (gcsb-stats))))
        (let ((v (gcsb-make 700)))
          (gcsb-gc)
          (gcsb-check (= (gcsb-ok v) 700) "new cells beside the image cells")
          ;; Image cells are never counted (GNU counts no pdumper object),
          ;; so only the 700 new cells join the baseline.
          (gcsb-check (= (nth 0 (gcsb-stats)) (+ live0 700)) "live count after the restore")
          (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "image cells unaffected")))
      (nelisp--debug-switch 29))
  (gcsb-warm)
  (gcsb-pass 2000 1000 900)
  (when (equal gcsb-mode "dump")
    (gcsb-gc)
    (gcsb-check (> (nelisp--arena-dump-image-stream (getenv "GCSB_IMAGE")) 0) "image dump")
    (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "cells intact after the dump")
    (gcsb-gc)
    (gcsb-check (= (gcsb-ok gcsb-keep) 2000) "cells survive collections after the dump")))
(princ (format "GCSB %s checks=%d failures=%d %s\n" gcsb-mode gcsb-checks gcsb-failures
               (if (= gcsb-failures 0) "PASS" "FAIL")))
nil
