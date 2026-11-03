(frame-window-state-change
 (frame-window-state-change)
 (let ((f (selected-frame)))
   (set-frame-window-state-change f t)
   (prog1 (list (and (frame-window-state-change f) t) (and (frame-window-state-change) t))
     (set-frame-window-state-change f nil)))
 (condition-case e (frame-window-state-change 7) (error (list (car e) (cdr e))))
 (condition-case e (set-frame-window-state-change 7 t) (error (list (car e) (cdr e)))))
(handle-switch-frame
 (let ((f (selected-frame))) (handle-switch-frame f) (eq (selected-frame) f))
 (let ((f (selected-frame))) (list (null (handle-switch-frame f)) (and (framep (selected-frame)) t)))
 (let ((w (split-window))) (prog1 (list (windowp w) (and (framep (selected-frame)) t)) (delete-window w))))
(last-nonminibuffer-frame
 (and (framep (last-nonminibuffer-frame)) t)
 (let ((f (selected-frame))) (select-frame f) (and (framep (last-nonminibuffer-frame)) t))
 (condition-case e (progn (delete-other-windows) (and (framep (last-nonminibuffer-frame)) t)) (error (car e))))
(make-terminal-frame
 (condition-case e (make-terminal-frame nil) (error (list (car e) (cdr e))))
 (condition-case e (make-terminal-frame '((tty-type . "no-such-terminal"))) (error (list (car e) (cdr e)))))
(mouse-pixel-position
 (let ((p (mouse-pixel-position))) (list (and (framep (car p)) t) (cadr p) (cddr p)))
 (let ((w (split-window))) (prog1 (let ((p (mouse-pixel-position))) (list (and (framep (car p)) t) (cadr p) (cddr p))) (delete-window w))))
(mouse-position-in-root-frame
 (let ((p (mouse-position-in-root-frame))) (list (car p) (cdr p)))
 (let ((w (split-window))) (prog1 (let ((p (mouse-position-in-root-frame))) (list (car p) (cdr p))) (delete-window w))))
(old-selected-frame
 (and (framep (old-selected-frame)) t)
 (let ((f (selected-frame))) (select-frame f) (and (framep (old-selected-frame)) t)))
(previous-frame
 (eq (previous-frame) (selected-frame))
 (let ((f (selected-frame))) (list (and (framep (previous-frame f 'visible)) t) (eq (previous-frame f 0) f)))
 (condition-case e (previous-frame 7) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (prog1 (and (framep (previous-frame (selected-frame) w)) t) (delete-window w))))
(reconsider-frame-fonts
 (condition-case e (reconsider-frame-fonts (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (reconsider-frame-fonts 7) (error (list (car e) (cdr e)))))
(set-frame-size-and-position-pixelwise
 (set-frame-size-and-position-pixelwise (selected-frame) 80 24 0 0)
 (let ((f (selected-frame))) (prog1 (progn (set-frame-size-and-position-pixelwise f 640 384 12 34 2) (list (frame-parameter f 'left) (frame-parameter f 'top))) (set-frame-size-and-position-pixelwise f 80 24 0 0)))
 (condition-case e (set-frame-size-and-position-pixelwise 7 80 24 0 0) (error (list (car e) (cdr e))))
 (condition-case e (set-frame-size-and-position-pixelwise (selected-frame) 0 10 1 2) (error (list (car e) (cdr e)))))
(set-frame-window-state-change
 (set-frame-window-state-change)
 (let ((f (selected-frame))) (set-frame-window-state-change f t) (prog1 (frame-window-state-change f) (set-frame-window-state-change f nil)))
 (condition-case e (set-frame-window-state-change 7 t) (error (list (car e) (cdr e)))))
(set-mouse-pixel-position
 (set-mouse-pixel-position (selected-frame) 0 0)
 (set-mouse-pixel-position (selected-frame) 11 17)
 (condition-case e (set-mouse-pixel-position 7 1 2) (error (list (car e) (cdr e)))))
