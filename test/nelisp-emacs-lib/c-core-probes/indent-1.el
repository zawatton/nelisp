(compute-motion
  (with-temp-buffer (insert "abcde")
    (compute-motion 1 '(0 . 0) 5 nil 80 nil nil))
  (compute-motion 1 '(0 . 0) (point-max) '(4 . 1) 80 nil nil)
  (with-temp-buffer
    (insert "ab\tcd\nefghijkl\nZ")
    (compute-motion 1 '(0 . 0) (point-max) nil 4 nil nil))
  (compute-motion nil '(0 . 0) 2 nil 80 nil nil)
  (compute-motion 1 nil 2 nil 80 nil nil))
(vertical-motion
  (with-temp-buffer (insert "one\ntwo\nthree") (goto-char 1)
    (list (vertical-motion 1) (point)))
  (with-temp-buffer (insert "one\ntwo\nthree") (goto-char 8)
    (list (vertical-motion -1) (point)))
  (with-temp-buffer (insert "abcdefgh\nxy") (goto-char 1)
    (list (vertical-motion '(4 . 0)) (point)))
  (vertical-motion nil)
  (vertical-motion 1 nil nil))
