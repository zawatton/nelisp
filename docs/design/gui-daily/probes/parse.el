;;; parse.el --- read every probe completely -*- lexical-binding: t; -*-
(dolist (p (directory-files "probes" t "\\.el$"))
  (with-temp-buffer
    (insert-file-contents p)
    (emacs-lisp-mode)
    (check-parens)
    (goto-char (point-min))
    (while (progn (forward-comment (point-max)) (< (point) (point-max)))
      (read (current-buffer))))
  (princ (format "%s parsed\n" p)))
