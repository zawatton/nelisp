(mutexp
 (mutexp nil)
 (mutexp [mutex])
 (let ((object (make-mutex "c-core-probe")))
   (list (type-of object) (mutexp object))))
(condition-variable-p
 (condition-variable-p nil)
 (let* ((mutex (make-mutex "c-core-probe"))
        (object (make-condition-variable mutex "c-core-probe")))
   (list (type-of object) (condition-variable-p object))))
(threadp
 (threadp nil)
 (let ((object (make-thread (lambda () nil) "c-core-probe")))
   (list (type-of object) (threadp object))))
