;;; gc-port-image-holes.el --- live-only images carry no hole map; older images still take that path  -*- lexical-binding: t; -*-

;; Doc 223, "Live-only image dump": the stream dump writes only included blocks with their marks
;; cleared, so its image has no free block and no hole run (pdumper has no hole map either).  The
;; writer stamps the trailer version 2 and the loader then builds no hole map and walks no free
;; blocks.  A version 1 image (an older writer) takes the old path.  Debug switches:
;;   (nelisp--debug-switch 46)  bit 0 a hole map exists, bit 1 the boot structures are built,
;;                              bit 2 a cold image is loaded;
;;   (nelisp--debug-switch 47)  how many 8-byte granules the hole map marks free;
;;   (nelisp--debug-switch 48)  1 when the hole scan marks exactly the free blocks of a synthetic
;;                              header run (live 48, free 64, live 32, free 32, free 32, live 48).
;; GCIH_EXPECT is liveonly (state 6), legacy (state 7: the older path also builds the map) or none
;; (state 0: no image was accepted).  Prints "GCIH EXPECT checks=N failures=F PASS|FAIL" and one
;; "GCIH probe ..." line that the harness compares between the modes.
(defvar gcih-checks 0)
(defvar gcih-failures 0)
(defvar gcih-expect (or (getenv "GCIH_EXPECT") "liveonly"))
(defun gcih-check (ok label)
  (setq gcih-checks (1+ gcih-checks))
  (unless ok
    (setq gcih-failures (1+ gcih-failures))
    (princ (format "GCIH FAIL %s\n" label))))
(garbage-collect)                       ; builds the boot structures of a loaded image
(let ((state (nelisp--debug-switch 46)) (holes (nelisp--debug-switch 47)))
  (gcih-check (= state (cond ((equal gcih-expect "liveonly") 6) ((equal gcih-expect "legacy") 7) (t 0)))
              (format "state %S for %s" state gcih-expect))
  (gcih-check (= holes 0) (format "no granule of the image is a hole: %S" holes)))
(gcih-check (= (nelisp--debug-switch 48) 1) "the hole scan marks exactly the free blocks of a synthetic run")
;; Behaviour that must not depend on how the image was loaded.
(let ((l nil))
  (dotimes (i 4000) (push (cons i (number-to-string i)) l))
  (garbage-collect)
  (garbage-collect)
  (princ (format "GCIH probe %S\n"
                 (list (length l) (cdr (nth 100 l)) (car (car (last l)))
                       (apply #'+ (mapcar #'car l))
                       (length (make-string 5000 ?a))
                       (let ((v (make-vector 300 0))) (dotimes (i 300) (aset v i (* i i))) (aref v 299))
                       (symbol-name 'gcih-expect)))))
(princ (format "GCIH %s checks=%d failures=%d %s\n" gcih-expect gcih-checks gcih-failures
               (if (= gcih-failures 0) "PASS" "FAIL")))
nil
