;;; gc-port-image-edges.el --- image objects reached only through precise edges survive collections  -*- lexical-binding: t; -*-

;; Doc 223, "Image objects as pdumper objects": a block of the restored image takes its mark in the
;; boot bitmap (pdumper_set_marked) and is traversed once per collection; its children, heap objects
;; included, must be marked through it.  The population is an N-element vector of items
;;   (I "I" [I 2I] 0.5I (2^70+I) (I 3I) SLOT)
;; kept only by the global `gcie-root' (a precise edge; `defvar' leaves the restored value).  SLOT is
;; (nil): the restore modes store a fresh heap object of a kind chosen by I into the image cons, so
;; that object is reachable only through an image object.
;; GCIE_MODE selects the run:
;;   run       build, collect with the conservative scan off and on, fill the slots, collect, check
;;             (the same checks on heap objects);
;;   dump      build, collect, dump the heap to GCIE_IMAGE, check again;
;;   restore   under --cold-load-from GCIE_IMAGE: the items are image objects: five collections with
;;             the scan off, slots filled with heap objects, five collections, three with the scan on,
;;             slots cleared, three collections; every phase checks every item and slot;
;;   redump    restore, fill the slots, then dump a second-generation image to GCIE_IMAGE2;
;;   restore2  under --cold-load-from GCIE_IMAGE2: items and slots are image objects; collect, check.
;; Prints "GCIE MODE checks=N failures=F PASS|FAIL" last.
(defvar gcie-mode nil)
(defvar gcie-checks 0)
(defvar gcie-failures 0)
(defvar gcie-n 3000)
(defvar gcie-root nil)
(setq gcie-mode (or (getenv "GCIE_MODE") "run") gcie-checks 0 gcie-failures 0)
(defun gcie-check (ok label)
  (setq gcie-checks (1+ gcie-checks))
  (unless ok
    (setq gcie-failures (1+ gcie-failures))
    (princ (format "GCIE FAIL %s\n" label))))
(defun gcie-gc (n) (dotimes (_ n) (garbage-collect)))
(defun gcie-item (i)
  (list i (number-to-string i) (vector i (* i 2)) (* i 0.5) (+ (ash 1 70) i) (list i (* i 3)) (list nil)))
(defun gcie-build ()
  (let ((v (make-vector gcie-n nil)) (i 0))
    (while (< i gcie-n) (aset v i (gcie-item i)) (setq i (1+ i)))
    v))
(defun gcie-fresh (i)
  (let ((k (% i 6)))
    (cond ((= k 0) (make-string (+ 8 (% i 40)) (+ 97 (% i 26))))
          ((= k 1) (make-vector 5 i))
          ((= k 2) (+ 0.25 i))
          ((= k 3) (+ (ash 1 66) i))
          ((= k 4) (list i (* i 7)))
          (t (symbol-name (make-symbol (format "u%d" i)))))))
(defun gcie-item-ok (item i)
  (and (= (nth 0 item) i) (equal (nth 1 item) (number-to-string i))
       (equal (nth 2 item) (vector i (* i 2))) (= (nth 3 item) (* i 0.5))
       (= (nth 4 item) (+ (ash 1 70) i)) (equal (nth 5 item) (list i (* i 3)))))
(defun gcie-verify (filled)
  ;; Number of items that fail: the item itself, and its slot (a fresh object when FILLED, else nil).
  (let ((bad 0) (i 0))
    (while (< i gcie-n)
      (let ((item (aref gcie-root i)))
        (unless (and (gcie-item-ok item i)
                     (if filled (equal (car (nth 6 item)) (gcie-fresh i)) (null (car (nth 6 item)))))
          (setq bad (1+ bad))))
      (setq i (1+ i)))
    bad))
(defun gcie-fill ()
  (let ((i 0)) (while (< i gcie-n) (setcar (nth 6 (aref gcie-root i)) (gcie-fresh i)) (setq i (1+ i)))))
(defun gcie-clear ()
  (let ((i 0)) (while (< i gcie-n) (setcar (nth 6 (aref gcie-root i)) nil) (setq i (1+ i)))))
(defun gcie-phase (label n filled)
  (gcie-gc n)
  (gcie-check (= (gcie-verify filled) 0) (format "%s: every item%s intact" label (if filled " and slot" ""))))

(cond
 ((equal gcie-mode "restore")
  (gcie-check (and (vectorp gcie-root) (= (length gcie-root) gcie-n)) "the population came through the image")
  (gcie-check (= (gcie-verify nil) 0) "restored items intact before any collection")
  (nelisp--debug-switch 28)
  (gcie-phase "image items, five collections, scan off" 5 nil)
  (gcie-fill)
  (gcie-phase "heap objects held only by image conses, five collections, scan off" 5 t)
  (nelisp--debug-switch 29)
  (gcie-phase "the same, scan on" 3 t)
  (gcie-clear)
  (gcie-phase "slots cleared" 3 nil))
 ((equal gcie-mode "restore2")
  (gcie-check (and (vectorp gcie-root) (= (length gcie-root) gcie-n)) "the population came through the second image")
  (gcie-check (= (gcie-verify t) 0) "restored items and slots intact before any collection")
  (nelisp--debug-switch 28)
  (gcie-phase "image items and slots, five collections, scan off" 5 t)
  (nelisp--debug-switch 29)
  (gcie-phase "the same, scan on" 3 t))
 ((equal gcie-mode "redump") nil)
 (t
  (setq gcie-root (gcie-build))
  (gcie-check (= (gcie-verify nil) 0) "built items intact")
  (nelisp--debug-switch 28)
  (gcie-phase "heap items, five collections, scan off" 5 nil)
  (nelisp--debug-switch 29)
  (when (equal gcie-mode "dump")
    (gcie-gc 2)
    (gcie-check (> (nelisp--arena-dump-image-stream (getenv "GCIE_IMAGE")) 0) "image dump")
    (gcie-check (= (gcie-verify nil) 0) "items intact after the dump")
    (gcie-gc 2)
    (gcie-check (= (gcie-verify nil) 0) "items intact after collections following the dump"))
  (when (equal gcie-mode "run")
    (nelisp--debug-switch 28)
    (gcie-fill)
    (gcie-phase "heap objects held by heap conses, five collections, scan off" 5 t)
    (nelisp--debug-switch 29))))
(when (equal gcie-mode "redump")
  (gcie-check (and (vectorp gcie-root) (= (length gcie-root) gcie-n)) "the population came through the image")
  (gcie-fill)
  (gcie-gc 2)
  (gcie-check (= (gcie-verify t) 0) "filled slots intact")
  (gcie-check (> (nelisp--arena-dump-image-stream (getenv "GCIE_IMAGE2")) 0) "second-generation dump")
  (gcie-check (= (gcie-verify t) 0) "filled slots intact after the second dump"))
(princ (format "GCIE %s checks=%d failures=%d %s\n" gcie-mode gcie-checks gcie-failures
               (if (= gcie-failures 0) "PASS" "FAIL")))
nil
