(get-unicode-property-internal
 (let ((table (unicode-property-table-internal 'general-category)))
   (list (not (null (get-unicode-property-internal table ?A)))
         (get-unicode-property-internal table ?1)))
 (condition-case e (get-unicode-property-internal nil ?A) (error e)))
(optimize-char-table
 (let ((table (unicode-property-table-internal 'general-category)))
   (list (optimize-char-table table) (char-table-p table)))
 (condition-case e (optimize-char-table nil) (error e)))
(put-unicode-property-internal
 (let ((table (unicode-property-table-internal 'general-category)))
   (prog1 (progn (put-unicode-property-internal table ?A 'probe-value)
                 (eq (get-unicode-property-internal table ?A) 'probe-value))
     (put-unicode-property-internal table ?A 'Lu)))
 (condition-case e (put-unicode-property-internal nil ?A 'x) (error e)))
(unicode-property-table-internal
 (list (char-table-p (unicode-property-table-internal 'general-category))
       (null (unicode-property-table-internal 'nelisp-probe-unknown)))
 (condition-case e (unicode-property-table-internal nil) (error e)))
