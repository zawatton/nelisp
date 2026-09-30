(category-docstring
 (let* ((table (make-category-table)) (category (get-unused-category table))) (define-category category "probe category" table) (category-docstring category table))
 (condition-case e (category-docstring nil) (error (list (car e) (cdr e))))
 (let* ((table (make-category-table)) (category (get-unused-category table))) (define-category category "changed table" table) (list category (category-docstring category table))) )
(category-set-mnemonics
 (category-set-mnemonics (make-category-set "abc"))
 (condition-case e (category-set-mnemonics [t]) (error (list (car e) (cdr e))))
 (let ((set (make-category-set "xz"))) (list (aref set ?x) (aref set ?z) (category-set-mnemonics set))) )
(define-category
 (let* ((table (make-category-table)) (category (get-unused-category table))) (define-category category "probe category" table) (equal (category-docstring category table) "probe category"))
 (condition-case e (define-category 1 "bad") (error (list (car e) (cdr e))))
 (let ((category (get-unused-category))) (define-category category "dynamic category") (equal (category-docstring category) "dynamic category")) )
(get-unused-category
 (get-unused-category)
 (let ((table (make-category-table))) (let ((first (get-unused-category table))) (define-category first "taken" table) (list first (get-unused-category table))))
 (condition-case e (get-unused-category 3) (error (list (car e) (cdr e)))) )
(make-category-set
 (category-set-mnemonics (make-category-set "aZ"))
 (condition-case e (make-category-set nil) (error (list (car e) (cdr e))))
 (let ((set (make-category-set "xz"))) (list (aref set ?x) (aref set ?z) (aref set ?y))) )
