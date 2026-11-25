;;; -*- lexical-binding: t; -*-
;; The standard category table after GNU international/characters.el
;; (category.c + characters.el): mnemonics of sample characters across
;; scripts, docstrings, set sharing, and a modified copy.  Spec = GNU 31.1
;; (par-category.gnu).
(defmacro row (n f) `(princ (format "%-24s %S\n" ,n (condition-case e ,f (error (list 'ERR (car e)))))))
(defvar pc-chars '(?\s ?a ?Z ?0 ?_ ?\t #xA0 ?é ?ß ?Ω ?Ж ?א ?ا ?अ ?ก ?ກ ?ᄀ ?가 ?あ ?ア ?ｱ ?漢 ?〇 ?、 ?Ａ #x3400 #x20000 #x1F600 #x10FFFF))
(row "C1 mnemonics" (mapcar (lambda (c) (category-set-mnemonics (char-category-set c))) pc-chars))
(row "C2 docstrings" (mapcar (lambda (c) (let ((d (category-docstring c))) (and d (car (split-string d "\n"))))) '(?a ?j ?C ?| ?^ ?L ?R ?. ?\s)))
(row "C3 shared sets" (list (eq (char-category-set ?a) (char-category-set ?b)) (eq (char-category-set ?あ) (char-category-set ?い))))
(row "C4 set type" (list (bool-vector-p (char-category-set ?a)) (length (char-category-set ?a))))
(row "C5 unused" (get-unused-category))
(row "C6 copy modify" (let ((tb (copy-category-table)))
                        (define-category ?Z "probe" tb)
                        (modify-category-entry '(?a . ?c) ?Z tb)
                        (list (category-set-mnemonics (aref tb ?b))
                              (category-set-mnemonics (char-category-set ?b))
                              (category-docstring ?Z tb) (category-docstring ?Z))))
(row "C7 reset" (let ((tb (copy-category-table)))
                  (modify-category-entry '(#x3040 . #x309F) ?j tb t)
                  (list (category-set-mnemonics (aref tb ?あ)) (category-set-mnemonics (aref tb ?ア)))))
(row "C8 regexp \\cj" (list (string-match "\\cj+" "abcあいう漢x") (match-end 0) (string-match "\\Ca" "あa")))
(row "C9 runs" (let ((n 0)) (map-char-table (lambda (_k _v) (setq n (1+ n))) (standard-category-table)) n))
(row "C10 Unicode cases"
     (mapcar (lambda (c)
               (let ((set (char-category-set c)))
                 (list (category-set-mnemonics set)
                       (aref set ?.) (aref set ?^) (aref set ?L) (aref set ?R))))
             '(?A ?é ?あ ?ア ?漢 ?한 ?ก ?ا ?א #x0301 #x3099 #x0661 #x200f)))
(row "C11 word boundaries"
     (with-temp-buffer
       (insert "alpha漢字ひらがなカタカナ ABCかなカナ 한글 ไทย")
       (goto-char (point-min))
       (let (ends)
         (while (forward-word 1)
           (push (point) ends))
         (nreverse ends))))
(row "C12 CJK width"
     (let ((chars '(?a ?¡ ?α ?あ ?漢 ?한 #x0301 #x1f600))
           (char-width-table char-width-table)
           (cjk-ambiguous-chars-are-wide t))
       (use-default-char-width-table)
       (let ((widths (mapcar #'char-width chars)))
         (use-cjk-char-width-table 'ja_JP)
         (list widths (mapcar #'char-width chars)))))
(row "C13 Japanese"
     (progn
       (set-language-environment "Japanese")
       (list current-language-environment
             (char-width ?¡) (char-width ?あ))))
(row "C14 make table"
     (let ((table (make-category-table)))
       (with-temp-buffer
         (set-category-table table)
         (let ((initial (char-category-set ?a)))
           (define-category ?Z "fresh category")
           (modify-category-entry '(#xfffe . #x10002) ?Z)
           (list (category-table-p table)
                 (eq table (category-table))
                 (bool-vector-p initial) (category-set-mnemonics initial)
                 (mapcar (lambda (c) (category-set-mnemonics (char-category-set c)))
                         '(#xfffd #xfffe #xffff #x10000 #x10002 #x10003))
                 (category-docstring ?Z (standard-category-table)))))))
(row "C15 regexp live table"
     (with-temp-buffer
       (let ((table (make-category-table)) (regexp "\\cZ"))
         (set-category-table table)
         (define-category ?Z "regexp category")
         (modify-category-entry ?z ?Z)
         (list (string-match regexp "az")
               (progn (modify-category-entry ?z ?Z nil t)
                      (string-match regexp "az"))))))
