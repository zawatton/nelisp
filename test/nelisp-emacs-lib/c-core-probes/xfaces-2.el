(x-family-fonts
 (condition-case e (list (x-family-fonts) (x-family-fonts nil (selected-frame))) (error e))
 (condition-case e (x-family-fonts 17) (error e))
 (condition-case e (x-family-fonts "fixed" 17) (error e)))
(x-list-fonts
 (condition-case e (x-list-fonts "*") (error e))
 (condition-case e
     (let ((b (get-buffer-create " *xfaces-2-probe*")))
       (unwind-protect
           (with-current-buffer b
             (insert "changed")
             (put-text-property (point-min) (point-max) 'face 'default)
             (let ((w (split-window)))
               (unwind-protect
                   (condition-case err (x-list-fonts "fixed" nil nil 1 1) (error err))
                 (delete-window w))))
         (kill-buffer b)))
   (error e)))
(x-load-color-file
 (condition-case e
     (let ((file (make-temp-file "xfaces-2-color-")))
       (unwind-protect
           (progn
             (with-temp-file file (insert "255 0 0 probe-red\n"))
             (x-load-color-file file))
         (delete-file file)))
   (error e))
 (condition-case e (x-load-color-file nil) (error e)))
