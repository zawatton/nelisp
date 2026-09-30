(insert-before-markers-and-inherit
 (with-temp-buffer (insert "ab") (goto-char 2) (insert-before-markers-and-inherit "X") (buffer-string))
 (condition-case e (insert-before-markers-and-inherit nil) (error e)))
(insert-byte
 (with-temp-buffer (insert-byte 65 3) (buffer-string))
 (condition-case e (insert-byte 256 1) (error e)))
(message-box
 (message-box "box:%s" "ok")
 (condition-case e (message-box "%d" "x") (error e)))
(message-or-box
 (message-or-box "echo:%d" 7)
 (message-or-box ""))
(position-bytes
 (with-temp-buffer (insert "Aé中") (list (position-bytes 1) (position-bytes 2) (position-bytes 3) (position-bytes 4)))
 (condition-case e (position-bytes "x") (error e))
 (with-temp-buffer (insert "xy") (position-bytes 4)))
(replace-region-contents
 (with-temp-buffer (insert "abc") (replace-region-contents 2 3 "XY") (buffer-string))
 (condition-case e (replace-region-contents 1 2 nil) (error e))
 (with-temp-buffer (insert "abc") (replace-region-contents 1 3 "z" 0)))
(translate-region-internal
 (with-temp-buffer (insert "abc") (let ((table (make-string 256 0))) (aset table ?a ?A) (aset table ?b ?B) (aset table ?c ?C) (let ((n (translate-region-internal 1 4 table))) (list n (buffer-string)))))
 (condition-case e (translate-region-internal 1 2 nil) (error e))
 (with-temp-buffer (insert "abc") (translate-region-internal 1 2 "ABC")))
(transpose-regions
 (with-temp-buffer (insert "abcdef") (transpose-regions 1 3 4 6) (buffer-string))
 (condition-case e (transpose-regions 1 4 3 6) (error e))
 (with-temp-buffer (insert "abcdef") (transpose-regions 1 3 4 6 t) (buffer-string)))
