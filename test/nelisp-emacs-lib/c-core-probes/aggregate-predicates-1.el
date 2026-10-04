;;; aggregate-predicates-1.el --- Aggregate representation predicates -*- lexical-binding: t; -*-

(bool-vector-p
 (mapcar #'bool-vector-p
         (list nil t 0 "s" '(a . b) [] [0] (make-vector 3 nil)
               (make-bool-vector 0 nil) (make-bool-vector 9 t)
               (make-hash-table) (make-char-table 'syntax-table)
               (record 'probe 1)))
 (condition-case err (bool-vector-p) (error err))
 (condition-case err (bool-vector-p nil t) (error err)))

(char-table-p
 (mapcar #'char-table-p
         (list nil t 0 "s" '(a . b) [] [0] (make-vector 3 nil)
               (make-bool-vector 0 nil) (make-bool-vector 9 t)
               (make-hash-table) (make-char-table 'syntax-table)
               (record 'probe 1)))
 (condition-case err (char-table-p) (error err))
 (condition-case err (char-table-p nil t) (error err)))

(hash-table-p
 (mapcar #'hash-table-p
         (list nil t 0 "s" '(a . b) [] [0] (make-vector 3 nil)
               (make-bool-vector 0 nil) (make-bool-vector 9 t)
               (make-hash-table) (make-char-table 'syntax-table)
               (record 'probe 1)))
 (condition-case err (hash-table-p) (error err))
 (condition-case err (hash-table-p nil t) (error err)))

(recordp
 (mapcar #'recordp
         (list nil t 0 "s" '(a . b) [] [0] (make-vector 3 nil)
               (make-bool-vector 0 nil) (make-bool-vector 9 t)
               (make-hash-table) (make-char-table 'syntax-table)
               (record 'probe 1)))
 (condition-case err (recordp) (error err))
 (condition-case err (recordp nil t) (error err)))
