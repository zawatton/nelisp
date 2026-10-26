;;; gen-ucs-names.el --- write lisp/nelisp-ucs-names.txt from GNU Emacs's own character-name table  -*- lexical-binding: t -*-
;; usage: emacs -Q --batch -l tools/ai/reader1/gen-ucs-names.el -- OUTPUT   (GNU Emacs 31.1)
;; The table is exactly what `char-from-name' consults on GNU Emacs: the hash table `ucs-names'
;; (the `name' and `old-name' character properties over the ranges listed in mule-cmds.el, with
;; the LAMDA/LAMBDA and BELL special cases already applied).  Output format, ASCII only:
;;   "\n", then one "NAME<TAB>HEX\n" per name sorted by name, then "#RANGES\n" and one
;;   "PREFIX<TAB>START<TAB>END\n" per run of code points outside the table's ranges whose name is
;;   PREFIX-HEX (HEX the code in upper case, at least 4 digits): those are what GNU resolves through
;;   the "-HEX" fallback of `char-from-name'.
(require 'cl-lib)
(defconst gen--ranges
  ;; Copied from `ucs-names' (lisp/international/mule-cmds.el, Emacs 31.1).
  '((#x0000 . #x33FF) (#x4DC0 . #x4DFF) (#xA000 . #xD7FF) (#xFB00 . #x143FA) (#x14400 . #x14646)
    (#x16100 . #x16139) (#x16800 . #x16F9F) (#x16FE0 . #x16FF6) (#x1AFF0 . #x1B122) (#x1B132 . #x1B132)
    (#x1B150 . #x1B16F) (#x1B170 . #x1B2FF) (#x1BC00 . #x1BCAF) (#x1CC00 . #x1FFFF) (#xE0000 . #xE01FF)))
(defun gen--in-ranges-p (c) (cl-some (lambda (r) (and (>= c (car r)) (<= c (cdr r)))) gen--ranges))
(let* ((out (car (last command-line-args-left)))
       (rows nil) runs prefix start end)
  (maphash (lambda (k v) (push (cons k v) rows)) (ucs-names))
  (setq rows (sort rows (lambda (a b) (string< (car a) (car b)))))
  (dotimes (c (1+ #x10FFFF))
    (unless (gen--in-ranges-p c)
      (let* ((n (get-char-code-property c 'name))
             (p (and n (string-match "\\`\\(.+\\)-\\([0-9A-F]+\\)\\'" n)
                     (>= (length (match-string 2 n)) 4)
                     (string= (match-string 2 n) (format "%04X" c))
                     (match-string 1 n))))
        (if (and p (equal p prefix) (= c (1+ end)))
            (setq end c)
          (when prefix (push (list prefix start end) runs))
          (setq prefix p start c end c)))))
  (when prefix (push (list prefix start end) runs))
  (setq runs (nreverse runs))
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert "\n")
    (dolist (r rows) (insert (format "%s\t%X\n" (car r) (cdr r))))
    (insert "#RANGES\n")
    (dolist (r runs) (insert (format "%s\t%X\t%X\n" (nth 0 r) (nth 1 r) (nth 2 r))))
    (let ((coding-system-for-write 'no-conversion)) (write-region (point-min) (point-max) out)))
  (message "names=%d ranges=%d" (length rows) (length runs)))
