(combine-windows
  (condition-case e (combine-windows (selected-window) (selected-window)) (error (car e)))
  (condition-case e (combine-windows nil nil) (error (car e))))
 (coordinates-in-window-p
  (coordinates-in-window-p '(0 . 0) (selected-window))
  (condition-case e (coordinates-in-window-p nil (selected-window)) (error (car e))))
 (delete-other-windows-internal
  (progn (delete-other-windows-internal) (one-window-p))
  (condition-case e (delete-other-windows-internal 'bad) (error (car e))))
 (delete-window-internal
  (condition-case e (delete-window-internal (selected-window)) (error (car e)))
  (condition-case e (delete-window-internal nil) (error (car e))))
 (frame-old-selected-window
  (windowp (frame-old-selected-window))
  (condition-case e (frame-old-selected-window 'bad) (error (car e))))
 (frame-root-window
  (windowp (frame-root-window))
  (condition-case e (frame-root-window 'bad) (error (car e))))
 (minibuffer-selected-window
  (windowp (minibuffer-selected-window))
  (null (minibuffer-selected-window)))
 (move-to-window-line
  (move-to-window-line nil)
  (condition-case e (move-to-window-line 'bad) (error (car e))))
 (old-selected-window
  (windowp (old-selected-window))
  (null (old-selected-window)))
 (other-window-for-scrolling
  (windowp (other-window-for-scrolling))
  (condition-case e (other-window-for-scrolling) (error (car e))))
 (resize-mini-window-internal
  (condition-case e (resize-mini-window-internal (selected-window)) (error (car e)))
  (condition-case e (resize-mini-window-internal nil) (error (car e))))
 (run-window-scroll-functions
  (progn (run-window-scroll-functions) t)
  (condition-case e (run-window-scroll-functions 'bad) (error (car e))))
