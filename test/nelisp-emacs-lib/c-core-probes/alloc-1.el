(garbage-collect-heapsize
 (let ((x (garbage-collect-heapsize))) (list (listp x) (and (listp x) (symbolp (caar x)))))
 (with-temp-buffer (insert (make-string 2000 ?x)) (let ((x (garbage-collect-heapsize))) (list (listp x) (bufferp (current-buffer))))) )
(garbage-collect-maybe
 (condition-case e (list (garbage-collect-maybe 1)) (error e))
 (condition-case e (garbage-collect-maybe nil) (error e))
 (condition-case e (garbage-collect-maybe "x") (error e)))
(make-finalizer
 (condition-case e (type-of (make-finalizer 'ignore)) (error e))
 (condition-case e (make-finalizer nil) (error e)))
(malloc-info
 (list (null (malloc-info)))
 (with-temp-buffer (insert "changed allocation state") (null (malloc-info))))
(malloc-trim
 (condition-case e (memq (malloc-trim 0) '(nil t)) (error e))
 (condition-case e (malloc-trim 4096) (error e))
 (condition-case e (malloc-trim -1) (error e)))
(memory-info
 (let ((x (memory-info))) (list (or (null x) (and (= (length x) 4) (integerp (nth 0 x)) (integerp (nth 1 x)) (integerp (nth 2 x)) (integerp (nth 3 x))))))
 (with-temp-buffer (insert "allocation state") (let ((x (memory-info))) (list (or (null x) (and (= (length x) 4) (integerp (nth 0 x)) (integerp (nth 1 x)) (integerp (nth 2 x)) (integerp (nth 3 x))))))))
