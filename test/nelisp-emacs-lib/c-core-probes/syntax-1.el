(syntax-class-to-char
 (syntax-class-to-char 2)
 (syntax-class-to-char 15)
 (condition-case e (syntax-class-to-char 16) (error e))
 (condition-case e (syntax-class-to-char nil) (error e)))
(matching-paren
 (matching-paren ?\()
 (matching-paren ?\[)
 (matching-paren ?x)
 (condition-case e (matching-paren -1) (error e)))
(internal-describe-syntax-value
 (with-temp-buffer (internal-describe-syntax-value '(2)) (buffer-string))
 (with-temp-buffer (internal-describe-syntax-value '(4 . 41)) (buffer-string))
 (with-temp-buffer (internal-describe-syntax-value nil) (buffer-string))
 (with-temp-buffer (internal-describe-syntax-value 'invalid) (buffer-string)))
(backward-prefix-chars
 (with-temp-buffer (insert "abc") (goto-char (point-max)) (backward-prefix-chars) (point))
 (with-temp-buffer (modify-syntax-entry ?' ". 1p") (insert "'x") (goto-char (point-max)) (backward-prefix-chars) (point))
 (with-temp-buffer (insert "abc") (goto-char (point-min)) (backward-prefix-chars) (point)))
