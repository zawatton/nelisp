(window-pixel-height
  (window-pixel-height)
  (let ((w (split-window nil 8))) (prog1 (list (window-pixel-height w) (windowp w)) (delete-window w)))
  (condition-case e (window-pixel-height 5) (error (list (car e) (cdr e)))))
 (window-pixel-left
  (window-pixel-left)
  (let ((w (split-window nil nil t))) (prog1 (list (window-pixel-left w) (window-pixel-left)) (delete-window w)))
  (condition-case e (window-pixel-left 5) (error (list (car e) (cdr e)))))
 (window-pixel-top
  (window-pixel-top)
  (let ((w (split-window nil 8))) (prog1 (list (window-pixel-top w) (window-pixel-top)) (delete-window w)))
  (condition-case e (window-pixel-top 5) (error (list (car e) (cdr e)))))
 (window-pixel-width
  (window-pixel-width)
  (let ((w (split-window nil nil t))) (prog1 (list (window-pixel-width w) (window-pixel-width)) (delete-window w)))
  (condition-case e (window-pixel-width 5) (error (list (car e) (cdr e)))))
 (window-prev-sibling
  (window-prev-sibling)
  (let ((w (split-window nil nil t))) (prog1 (list (windowp (window-prev-sibling w)) (eq nil (window-prev-sibling))) (delete-window w)))
  (condition-case e (window-prev-sibling 5) (error (list (car e) (cdr e)))))
 (window-resize-apply
  (window-resize-apply)
  (window-resize-apply nil t)
  (condition-case e (window-resize-apply 5) (error (list (car e) (cdr e)))))
 (window-resize-apply-total
  (window-resize-apply-total)
  (window-resize-apply-total nil t)
  (condition-case e (window-resize-apply-total 5) (error (list (car e) (cdr e)))))
 (window-right-divider-width
  (window-right-divider-width)
  (let ((w (split-window nil nil t))) (prog1 (window-right-divider-width w) (delete-window w)))
  (condition-case e (window-right-divider-width 5) (error (list (car e) (cdr e)))))
 (window-scroll-bar-height
  (window-scroll-bar-height)
  (let ((w (split-window nil 8))) (prog1 (window-scroll-bar-height w) (delete-window w)))
  (condition-case e (window-scroll-bar-height 5) (error (list (car e) (cdr e)))))
 (window-scroll-bars
  (window-scroll-bars)
  (let ((w (split-window nil 8))) (prog1 (list (windowp w) (window-scroll-bars w)) (delete-window w)))
  (condition-case e (window-scroll-bars 5) (error (list (car e) (cdr e)))))
 (window-scroll-bar-width
  (window-scroll-bar-width)
  (let ((w (split-window nil nil t))) (prog1 (window-scroll-bar-width w) (delete-window w)))
  (condition-case e (window-scroll-bar-width 5) (error (list (car e) (cdr e)))))
 (window-tab-line-height
  (window-tab-line-height)
  (let ((w (split-window nil 8))) (prog1 (window-tab-line-height w) (delete-window w)))
  (condition-case e (window-tab-line-height 5) (error (list (car e) (cdr e)))))
