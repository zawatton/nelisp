(case-table-p
 (case-table-p (make-char-table 'case-table))
 (case-table-p (make-char-table 'syntax-table))
 (case-table-p nil)
 (case-table-p 17)
 (case-table-p (make-vector 259 nil))
 (let ((table (make-char-table 'case-table)))
   (set-char-table-range table ?a ?z)
   (list (case-table-p table) (char-table-range table ?a))))
