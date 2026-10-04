(visible-frame-list
 (length (visible-frame-list))
 (mapcar (lambda (frame) (eq frame (selected-frame)))
         (visible-frame-list))
 (condition-case err (visible-frame-list nil)
   (error (list (car err) (cdr err)))))
(tool-bar-pixel-width
 (tool-bar-pixel-width)
 (tool-bar-pixel-width (selected-frame))
 (condition-case err (tool-bar-pixel-width 'bad-frame)
   (error (list (car err) (cdr err)))))
(posn-at-x-y
 (let ((position (posn-at-x-y 0 0)))
   (list (nth 1 position) (nth 2 position) (nth 6 position)
         (nth 8 position) (nth 9 position)))
 (let ((position (posn-at-x-y 0 0 (selected-window))))
   (list (nth 1 position) (nth 2 position) (nth 6 position)))
 (condition-case err (posn-at-x-y 0.2 0)
   (error (list (car err) (cdr err)))))
