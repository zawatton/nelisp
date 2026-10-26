;;; -*- lexical-binding: t; -*-
;; Root-cause check: replace-match must not leave markers in the buffer's marker list.
(with-temp-buffer
  (insert "foo boo woo ") (goto-char 1)
  (dotimes (_ 20) (re-search-forward "[oO]" nil t) (replace-match "o" t t))
  (let ((n (length (nelisp-buffer-markers nelisp--current-buffer))))
    (princ (format "markers=%d\n" n))
    (kill-emacs (if (= n 0) 0 1))))
