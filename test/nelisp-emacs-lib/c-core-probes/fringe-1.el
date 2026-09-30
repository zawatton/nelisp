(fringe-bitmaps-at-pos
 (fringe-bitmaps-at-pos)
 (let ((w (split-window)))
   (unwind-protect
       (list (fringe-bitmaps-at-pos nil (selected-window)) (windowp w))
     (delete-window w)))
 (condition-case e (fringe-bitmaps-at-pos "x") (error e)))
