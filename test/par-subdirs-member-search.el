;;; par-subdirs-member-search.el --- short cons keys and bounded byte search -*- lexical-binding: t; -*-

(let* ((first (list -7 3))
       (second (list -7 4))
       (third (list 8 4))
       (items (list first second third)))
  (prin1 (list 'member-tail
               (eq (member (list -7 4) items) (cdr items))
               (member (list 9 4) items)
               (member (list -7 5) items)
               (eq (member (cons -7 4) (list first (cons -7 4))) nil)))
  (terpri)
  (setcar first 8)
  (setcar second 8)
  (prin1 (list 'member-live
               (eq (member (list 8 4) items) (cdr items))
               (member (list -7 4) items)
               (equal (member (list 2 4) (list (list 2.0 4) (list 2 4)))
                      '((2 4)))))
  (terpri))

(let ((search (if (fboundp 'nelisp--string-search)
                  #'nelisp--string-search
                #'string-search)))
  ;; Use only unibyte text so the public reference reports byte offsets too.
  (dolist (row (list (list "" "" 0)
                     (list "" "abcdef" 6)
                     (list "/" "1234567/12345678/." 0)
                     (list "/" "1234567/12345678/." 8)
                     (list "/../" "12345678/./abcdefghi/../" 0)
                     (list "aba" "12345678abbababa" 9)
                     (list "ab" "12345678a" 0)
                     (list "x" (make-string 129 ?y) 2)
                     (list (unibyte-string 255 128)
                           (concat (make-string 11 ?a) (unibyte-string 255 128)) 1)
                     (list (unibyte-string 0) "0123456789\0end" 3)))
    (prin1 (list 'search (apply search row)))
    (terpri)))
