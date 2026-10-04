(sqlite-columns
 (condition-case e (sqlite-columns nil) (error e))
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-columns db) (error e)) (sqlite-close db))))
(sqlite-execute-batch
 (let ((db (sqlite-open nil))) (unwind-protect (sqlite-execute-batch db "create table t(x); insert into t values(7);") (sqlite-close db)))
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-execute-batch db nil) (error e)) (sqlite-close db)))
 (condition-case e (sqlite-execute-batch nil "select 1") (error e)))
(sqlite-finalize
 (condition-case e (sqlite-finalize nil) (error e))
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-finalize db) (error e)) (sqlite-close db))))
(sqlite-load-extension
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-load-extension db nil) (error e)) (sqlite-close db)))
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-load-extension db "missing-module") (error e)) (sqlite-close db)))
 (condition-case e (sqlite-load-extension nil "x") (error e)))
(sqlite-more-p
 (condition-case e (sqlite-more-p nil) (error e))
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-more-p db) (error e)) (sqlite-close db))))
(sqlite-next
 (condition-case e (sqlite-next nil) (error e))
 (let ((db (sqlite-open nil))) (unwind-protect (condition-case e (sqlite-next db) (error e)) (sqlite-close db))))
(sqlite-version
 (sqlite-version)
 (stringp (sqlite-version)))
