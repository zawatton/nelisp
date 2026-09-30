(xw-color-defined-p
 (condition-case e (xw-color-defined-p "red") (error e))
 (let ((buffer (generate-new-buffer " *pgtkfns-4-color-defined*")) window)
   (unwind-protect
       (progn
         (with-current-buffer buffer (insert "color probe")
           (put-text-property 1 6 'face 'bold))
         (setq window (split-window))
         (condition-case e (xw-color-defined-p "#123456" (selected-frame)) (error e)))
     (when (and window (window-live-p window)) (delete-window window))
     (when (buffer-live-p buffer) (kill-buffer buffer))))
 (condition-case e (xw-color-defined-p "red" 3) (error e)))
(xw-color-values
 (condition-case e (xw-color-values "red") (error e))
 (let ((buffer (generate-new-buffer " *pgtkfns-4-color-values*")) window)
   (unwind-protect
       (progn
         (with-current-buffer buffer (insert "changed buffer"))
         (setq window (split-window))
         (condition-case e (xw-color-values "not-a-color" (selected-frame)) (error e)))
     (when (and window (window-live-p window)) (delete-window window))
     (when (buffer-live-p buffer) (kill-buffer buffer))))
 (condition-case e (xw-color-values nil 3) (error e)))
(xw-display-color-p
 (condition-case e (xw-display-color-p) (error e))
 (let ((buffer (generate-new-buffer " *pgtkfns-4-display-color*")) window)
   (unwind-protect
       (progn
         (with-current-buffer buffer (insert "terminal color"))
         (setq window (split-window))
         (condition-case e (xw-display-color-p nil) (error e)))
     (when (and window (window-live-p window)) (delete-window window))
     (when (buffer-live-p buffer) (kill-buffer buffer))))
 (condition-case e (xw-display-color-p 3) (error e)))
