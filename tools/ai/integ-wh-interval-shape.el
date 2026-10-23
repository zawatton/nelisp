;;; integ-wh-interval-shape.el --- the interval shape of a buffer built by propertized inserts  -*- lexical-binding: t; -*-

;; Run with `-l' on NeLisp and on GNU Emacs 31.1; both must print the same bytes.  GNU does not merge neighbouring
;; intervals whose plists are equal: graft_intervals_into_buffer has no merge step and copy_intervals keeps every interval
;; of the range, so (insert (propertize "a" 'face 'bold) (propertize "b" 'face 'bold)) prints
;; #("ab" 0 1 (face bold) 1 2 (face bold)), two intervals, and five inserts of "xy" print five.  Each case builds a buffer,
;; then prints its string with the intervals as `format' %S prints them and the plist at every position.  The cases that
;; append after the last interval are the ones the O(1) append path takes; the others (an insert before or inside the
;; list, a delete, an erase, a property change, a narrowed buffer) make the cached last cons stale or unused.

(defun iwh-is (tag setup &optional from to)
  "Run SETUP in a fresh buffer, then print the buffer string (or its FROM..TO part) with its intervals."
  (with-temp-buffer
    (funcall setup)
    (let* ((s (if from (buffer-substring from to) (buffer-string))) (i 0) r)
      (while (< i (length s)) (push (text-properties-at i s) r) (setq i (1+ i)))
      (princ (format "IS %s ## %S | %S\n" tag s (nreverse r))))))
(defmacro iwh-ib (&rest body) `(lambda () ,@body))

(iwh-is "two equal in one insert" (iwh-ib (insert (propertize "a" 'face 'bold) (propertize "b" 'face 'bold))))
(iwh-is "two equal in two inserts" (iwh-ib (insert (propertize "a" 'face 'bold)) (insert (propertize "b" 'face 'bold))))
(iwh-is "five equal" (iwh-ib (dotimes (_ 5) (insert (propertize "xy" 'face 'bold)))))
(iwh-is "equal runs, different runs, plain, equal"
        (iwh-ib (insert (propertize "a" 'face 'bold)) (insert (propertize "b" 'face 'bold))
                (insert (propertize "c" 'face 'italic)) (insert (propertize "d" 'face 'italic))
                (insert "e") (insert (propertize "f" 'face 'bold))))
(iwh-is "equal plists around plain text"
        (iwh-ib (insert (propertize "a" 'p 1)) (insert "-") (insert (propertize "b" 'p 1))))
(iwh-is "same value, different key order is still equal text" (iwh-ib (insert (propertize "ab" 'p 1 'q 2)) (insert (propertize "cd" 'p 1 'q 2))))
(iwh-is "a string with three intervals, then the same again"
        (iwh-ib (let ((s (concat (propertize "a" 'p 1) (propertize "b" 'p 1) "c" (propertize "d" 'p 2))))
                  (insert s) (insert s))))
(iwh-is "a string with intervals inserted in the middle of a plain buffer"
        (iwh-ib (insert "xxxx") (goto-char 3) (insert (concat (propertize "a" 'p 1) (propertize "b" 'p 1)))))
(iwh-is "insert-before-markers at the end"
        (iwh-ib (insert-before-markers (propertize "a" 'p 1)) (insert-before-markers (propertize "b" 'p 1))))
(iwh-is "delete the tail interval, then append"
        (iwh-ib (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 2)) (delete-region 3 5) (insert (propertize "e" 'p 3))))
(iwh-is "delete the tail interval, then plain text"
        (iwh-ib (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 2)) (delete-region 3 5) (insert "e")))
(iwh-is "delete across two intervals, then append"
        (iwh-ib (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 1)) (delete-region 2 4) (insert (propertize "e" 'p 1))))
(iwh-is "delete the first interval, then append"
        (iwh-ib (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 2)) (delete-region 1 3) (insert (propertize "e" 'p 3))))
(iwh-is "delete everything, then append"
        (iwh-ib (insert (propertize "ab" 'p 1)) (delete-region 1 3) (insert (propertize "e" 'p 3))))
(iwh-is "erase, then append twice"
        (iwh-ib (insert (propertize "ab" 'p 1)) (erase-buffer) (insert (propertize "e" 'p 3)) (insert (propertize "f" 'p 3))))
(iwh-is "put-text-property, then append"
        (iwh-ib (insert "abcd") (put-text-property 2 4 'p 9) (insert (propertize "e" 'p 3))))
(iwh-is "set-text-properties to nil, then append"
        (iwh-ib (insert (propertize "abcd" 'p 1)) (set-text-properties 1 5 nil) (insert (propertize "e" 'p 3))))
(iwh-is "insert in the middle, then append"
        (iwh-ib (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 2)) (goto-char 3) (insert (propertize "XY" 'p 3))
                (goto-char (point-max)) (insert (propertize "e" 'p 4))))
(iwh-is "insert at the start, then append"
        (iwh-ib (insert (propertize "ab" 'p 1)) (goto-char 1) (insert (propertize "XY" 'p 3))
                (goto-char (point-max)) (insert (propertize "e" 'p 4))))
(iwh-is "append in a narrowed buffer"
        (iwh-ib (insert (propertize "ab" 'p 1)) (insert (propertize "cd" 'p 2)) (narrow-to-region 2 4) (goto-char (point-max))
                (insert (propertize "e" 'p 4)) (widen)))
(iwh-is "append after a mid-buffer insert and a delete"
        (iwh-ib (dotimes (i 6) (insert (propertize "ab" 'p (% i 2)))) (goto-char 5) (insert (propertize "ZZ" 'p 7))
                (delete-region 2 4) (goto-char (point-max)) (dotimes (i 3) (insert (propertize "cd" 'p 1)))))
(iwh-is "a long run of equal inserts, cut out of the middle"
        (iwh-ib (dotimes (_ 40) (insert (propertize "ab" 'face 'bold)))) 5 40)
(iwh-is "a long run of distinct inserts and plain text"
        (iwh-ib (dotimes (i 25) (insert (propertize "xy" 'p i)) (insert "-"))) 3 60)
(iwh-is "a long run of two alternating plists"
        (iwh-ib (dotimes (i 30) (insert (propertize "x" 'p (% i 2))))))
(with-temp-buffer
  (dotimes (_ 300) (insert (propertize "xy" 'face 'bold)))
  (let ((s (format "%S" (buffer-string))))
    (princ (format "IS 300 equal inserts ## length %d md5 %s\n" (length s) (secure-hash (quote md5) s)))))
(princ "INTEG-WH-INTERVAL-SHAPE DONE\n")
