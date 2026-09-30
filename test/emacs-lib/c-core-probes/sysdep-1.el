(get-internal-run-time
 (let ((time (get-internal-run-time)))
   (and (listp time) (= (length time) 4)
        (not (memq nil (mapcar #'numberp time)))))
 (let ((before (get-internal-run-time)))
   (dotimes (_ 100000))
   (time-less-p before (get-internal-run-time))))
