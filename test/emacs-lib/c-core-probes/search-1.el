;;; search-1.el --- parity probes for search C primitives -*- lexical-binding: t; -*-

(newline-cache-check
 (newline-cache-check)
 (with-temp-buffer (insert "a\nb\n") (newline-cache-check)))

(posix-looking-at
 (with-temp-buffer (insert "abbb") (goto-char 1) (posix-looking-at "ab+"))
 (with-temp-buffer (insert "abbb") (goto-char 2) (posix-looking-at "a"))
 (posix-looking-at nil))

(posix-search-backward
 (with-temp-buffer (insert "a1 a22") (goto-char (point-max)) (posix-search-backward "a[0-9]+"))
 (with-temp-buffer (insert "a1 a22") (goto-char (point-max)) (posix-search-backward "a[0-9]+" nil t 2))
 (posix-search-backward "x" nil t))

(posix-search-forward
 (with-temp-buffer (insert "a1 a22") (goto-char 1) (posix-search-forward "a[0-9]+"))
 (with-temp-buffer (insert "a1 a22") (goto-char 1) (posix-search-forward "a[0-9]+" nil t 2))
 (posix-search-forward "x" nil t))

(posix-string-match
 (posix-string-match "a[0-9]+" "za22")
 (posix-string-match "a[0-9]+" "za22" 2 t)
 (posix-string-match "x" "za22")
 (posix-string-match nil "x"))

(re--describe-compiled
 (stringp (re--describe-compiled "a+"))
 (stringp (re--describe-compiled "a+" t))
 (condition-case e (re--describe-compiled nil) (error e)))

;;; search-1.el ends here
