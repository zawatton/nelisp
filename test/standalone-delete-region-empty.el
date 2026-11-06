;;; standalone-delete-region-empty.el --- empty delete-region keeps the buffer unmodified -*- lexical-binding: t -*-

;; GNU Emacs: (delete-region P P) changes nothing, so an unmodified buffer
;; stays unmodified and point does not move.  Exit 0 on pass, 1 on failure.
(defvar dre--failures 0)
(defun dre--check (name want got)
  (unless (equal want got)
    (setq dre--failures (1+ dre--failures))
    (princ (format "FAIL %s: want %S got %S\n" name want got))))

(with-temp-buffer
  (insert "abc")
  (set-buffer-modified-p nil)
  (goto-char 2)
  (delete-region 2 2)
  (dre--check "empty-modified" nil (buffer-modified-p))
  (dre--check "empty-point" 2 (point))
  (dre--check "empty-text" "abc" (buffer-string)))

(with-temp-buffer
  (insert "abc")
  (set-buffer-modified-p nil)
  (delete-region 1 2)
  (dre--check "nonempty-modified" t (buffer-modified-p))
  (dre--check "nonempty-text" "bc" (buffer-string)))

(princ (if (= dre--failures 0) "PASS\n" "FAILED\n"))
(kill-emacs (if (= dre--failures 0) 0 1))
