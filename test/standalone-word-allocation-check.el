;;; standalone-word-allocation-check.el --- Retained WORD fields -*- lexical-binding: t; -*-

;; A pointer view of nil/t/fixnum must not become an owned cons field.
;; Keep the population across top-level forms so evaluator roots have the
;; same lifetime at both collections. GNU runs the semantic checks too.
(defvar wa--keep (make-vector 4000 nil))
(defvar wa--native (fboundp 'nelisp--arena-stats))
(when wa--native (nelisp--debug-switch 28))
(garbage-collect)
(garbage-collect)
(defvar wa--before (if wa--native (nth 4 (nelisp--arena-stats)) 0))
(dotimes (i 4000)
  (aset wa--keep i (cons nil nil)))
;; Mutation must preserve the same representation as construction. In
;; particular, nreverse must not box the nil terminator of every source list.
(dotimes (i 4000)
  (setcar (aref wa--keep i) nil)
  (setcdr (aref wa--keep i) nil))
(garbage-collect)
(garbage-collect)
(when wa--native
  (let ((bytes (- (nth 4 (nelisp--arena-stats)) wa--before)))
    (unless (<= bytes (+ (* 4000 80) 4096))
      (error "WORD fields retained extra boxes: %d bytes for 4000 conses" bytes))))
(aset wa--keep 1 (cons t nil))
(aset wa--keep 2 (cons 2 nil))
(unless (equal (list (aref wa--keep 0) (aref wa--keep 1) (aref wa--keep 2))
               '((nil) (t) (2)))
  (error "Immediate cons fields changed"))
(let* ((child (list 1)) (cell (cons child child)) (v (vector cell)))
  (setcar child 9)
  (garbage-collect)
  (unless (and (eq (car cell) (cdr cell)) (eq cell (aref v 0))
               (equal cell '((9) 9)))
    (error "Heap fields lost sharing or reachability")))
;; The type WORD lives in the fixed-size record header, including when it
;; is immediate. Copying, collecting and reading it must preserve that WORD.
(dolist (type '(nil t 17 wa-type))
  (dolist (n '(0 1 8))
    (let* ((r (make-record type n nil)) (copy (copy-sequence r)))
      (garbage-collect)
      (unless (and (equal (aref r 0) type) (equal (aref copy 0) type))
        (error "Record type WORD changed")))))
(setq wa--keep (make-vector 4000 nil))
(garbage-collect)
(garbage-collect)
(setq wa--before (if wa--native (nth 4 (nelisp--arena-stats)) 0))
(dotimes (i 4000) (aset wa--keep i (make-marker)))
(garbage-collect)
(garbage-collect)
(when wa--native
  (let ((bytes (- (nth 4 (nelisp--arena-stats)) wa--before)))
    (unless (<= bytes (+ (* 4000 218) 4096))
      (error "Marker defaults retained scalar boxes: %d bytes" bytes))))
(setq wa--keep nil)
(when wa--native (nelisp--debug-switch 29))
(princ "WORD-ALLOCATION PASS\n")
