;;; strings-format-width.el --- GNU default format width data -*- lexical-binding: t; -*-
;; Regenerate with GNU Emacs 31.1:
;;   /usr/local/bin/emacs -Q --batch --load tools/ai/strings-format-width.el
;; This writes target/strings/format-width-data.el. Replace the marked data
;; block in scripts/nelisp-stdlib-prelude.el with that file's contents.
;; GNU character.c:344 lisp_string_width (auto_comp=false) and
;; buffer.h:1708 CHARACTER_WIDTH use this table for non-ASCII characters.
(unless (string-prefix-p "31.1" emacs-version)
  (error "GNU Emacs 31.1 is required"))
(let* ((root (expand-file-name "../.." (file-name-directory load-file-name)))
       (output (expand-file-name "target/strings/format-width-data.el" root)) ranges)
  (map-char-table
   (lambda (range width)
     (unless (= width 1)
       (push (list (if (consp range) (car range) range)
                   (if (consp range) (cdr range) range) (max 0 width)) ranges)))
   char-width-table)
  (make-directory (file-name-directory output) t)
  (setq ranges (nreverse ranges))
  (with-temp-file output
    (insert ";; BEGIN GNU 31.1 format width data; tools/ai/strings-format-width.el\n")
    (insert "(defconst nelisp--format-width-ranges\n  [")
    (dolist (row ranges)
      (insert (format "\n   %d %d %d" (nth 0 row) (nth 1 row) (nth 2 row))))
    (insert "]\n  \"GNU 31.1 non-default character widths, in ordered inclusive ranges.\")\n")
    (insert ";; END GNU 31.1 format width data\n"))
  (princ (format "FORMAT-WIDTH ranges=%d\n" (length ranges))))
