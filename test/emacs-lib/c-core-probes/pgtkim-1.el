(pgtk-use-im-context
 (condition-case e (pgtk-use-im-context nil) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-use-im-context t) (error (list (car e) (cdr e))))
 (let ((b (get-buffer-create "*pgtk-im-probe*")))
   (unwind-protect
       (with-current-buffer b
         (erase-buffer)
         (insert "probe")
         (condition-case e (pgtk-use-im-context nil (selected-frame))
           (error (list (car e) (cdr e)))))
     (kill-buffer b)))
 (condition-case e (pgtk-use-im-context t 7) (error (list (car e) (cdr e)))))
