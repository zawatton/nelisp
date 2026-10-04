(current-buffer
 (bufferp (current-buffer))
 (let ((original (current-buffer)))
   (with-temp-buffer (eq original (current-buffer)))))
(point
 (with-temp-buffer (point))
 (with-temp-buffer (insert "abc") (point)))
(point-min
 (with-temp-buffer (point-min))
 (with-temp-buffer (insert "abc") (point-min)))
(point-max
 (with-temp-buffer (point-max))
 (with-temp-buffer (insert "abc") (point-max)))
(goto-char
 (with-temp-buffer (insert "abc") (goto-char 2))
 (with-temp-buffer (insert "abc") (goto-char 1) (point)))
(insert
 (with-temp-buffer (insert "ab" ?c) (buffer-string))
 (with-temp-buffer (list (insert "a") (buffer-string))))
(forward-line
 (with-temp-buffer
   (insert "a\nb\nc")
   (goto-char (point-min))
   (list (forward-line 1) (point)))
 (with-temp-buffer (forward-line 1))
 (with-temp-buffer (insert "abc") (goto-char 1)
   (list (forward-line 1) (point)))
 (with-temp-buffer (insert "abc")
   (list (forward-line 1) (point)))
 (with-temp-buffer
   (list (forward-line 2) (point)))
 (with-temp-buffer (insert "abc")
   (list (forward-line 2) (point)))
 (with-temp-buffer (insert "a\n") (goto-char 1)
   (list (forward-line 1) (point)))
 (with-temp-buffer (insert "a\n")
   (list (forward-line 1) (point)))
 (with-temp-buffer
   (list (forward-line -1) (point)))
 (with-temp-buffer (insert "abc")
   (list (forward-line -1) (point))))
(substring
 (substring "abc" 1)
 (substring "abc" 0 2))
(string-match
 (string-match "b" "abc")
 (string-match "z" "abc"))
(match-beginning
 (progn (string-match "b" "abc") (match-beginning 0))
 (progn (string-match "z" "abc") (match-beginning 0)))
(match-end
 (progn (string-match "b" "abc") (match-end 0))
 (progn (string-match "z" "abc") (match-end 0)))
(looking-at
 (with-temp-buffer
   (insert "abc")
   (goto-char 2)
   (looking-at "b"))
 (with-temp-buffer
   (insert "abc")
   (goto-char 2)
   (looking-at "z")))
(re-search-forward
 (with-temp-buffer
   (insert "abc")
   (goto-char (point-min))
   (re-search-forward "b" nil t))
 (with-temp-buffer
   (insert "abc")
   (goto-char (point-min))
   (re-search-forward "z" nil t)))
