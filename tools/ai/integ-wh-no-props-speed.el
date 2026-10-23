;;; integ-wh-no-props-speed.el --- cost and retention of a run of propertized inserts  -*- lexical-binding: t; -*-

;; NeLisp only (it reads the identity table of string properties).  Prints one line:
;;   NO-PROPS-SPEED seconds=S n=800 table-growth=G
;; where G counts the entries the run added to `nelisp--tp-string-properties': one per `propertize' call is the floor.
;; A gap string that carried the inserted string's intervals made the run O(n^2) (800 inserts: 14.8 s against 0.35 s on
;; batch F) and kept every intermediate gap string in that table (growth 4503 for 800 inserts).

(let ((t0 (float-time)) (c0 (hash-table-count nelisp--tp-string-properties)))
  (with-temp-buffer
    (dotimes (i 800)
      (insert (propertize "line of text " 'p i))
      (insert "plain\n")))
  (princ (format "NO-PROPS-SPEED seconds=%.3f n=800 table-growth=%d\n"
                 (- (float-time) t0) (- (hash-table-count nelisp--tp-string-properties) c0))))
