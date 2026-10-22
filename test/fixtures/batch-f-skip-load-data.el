;;; batch-f-skip-load-data.el --- forms for batch-f-skip-load.el -*- lexical-binding: t; -*-

;; Loaded by batch-f-skip-load.el.  A form with `declare' comes first so that a
;; load which stops after it evaluates none of the rest.
(defmacro bfs-m1 (x) (declare (indent 1)) x)
(put 'bfs-pa 'v 1)
(defun bfs-d1 (x) (declare (indent 1)) x)
(put 'bfs-pb 'v 2)
(defun bfs-d2 (x) "doc" (declare (indent 2) (pure t)) (list x x))
(defmacro bfs-m2 (&rest body) (declare (debug t) (indent 0)) `(progn ,@body))
(bfs-m2 (put 'bfs-pc 'v 3) (put 'bfs-pd 'v 4))
(defvar bfs-dv 5)
(put 'bfs-pe 'v bfs-dv)
(progn (defun bfs-d3 (x) (declare (indent 1)) (1+ x)) (put 'bfs-pf 'v (bfs-d3 6)))
(put 'bfs-pg 'v (bfs-m1 8))
