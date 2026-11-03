(marker-last-position
 (marker-last-position (make-marker))
 (let* ((b (get-buffer-create " *marker-last-position*"))
        (m (with-current-buffer b (insert "abc") (copy-marker 3))))
   (prog1 (marker-last-position m) (kill-buffer b)))
 (condition-case e (marker-last-position nil) (error e)))
