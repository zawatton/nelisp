(window-pixel-height
  (window-pixel-height)
  (list (window-pixel-height) (windowp (selected-window)))
  (condition-case e (window-pixel-height 5) (error (list (car e) (cdr e)))))
 (window-pixel-left
  (window-pixel-left)
  (list (window-pixel-left) (window-pixel-left))
  (condition-case e (window-pixel-left 5) (error (list (car e) (cdr e)))))
 (window-pixel-top
  (window-pixel-top)
  (list (window-pixel-top) (window-pixel-top))
  (condition-case e (window-pixel-top 5) (error (list (car e) (cdr e)))))
 (window-pixel-width
  (window-pixel-width)
  (list (window-pixel-width) (window-pixel-width))
  (condition-case e (window-pixel-width 5) (error (list (car e) (cdr e)))))
 (window-prev-sibling
  (window-prev-sibling)
  (list (windowp (window-prev-sibling)) (eq nil (window-prev-sibling)))
  (condition-case e (window-prev-sibling 5) (error (list (car e) (cdr e)))))
 (window-resize-apply
  (window-resize-apply)
  (window-resize-apply nil nil)
  (condition-case e (window-resize-apply 5) (error (list (car e) (cdr e)))))
(window-resize-apply-total
  (functionp (symbol-function 'window-resize-apply-total))
  (functionp (symbol-function 'window-resize-apply-total))
  (condition-case e (window-resize-apply-total 5) (error (list (car e) (cdr e)))))
 (window-right-divider-width
  (window-right-divider-width)
  (window-right-divider-width)
  (condition-case e (window-right-divider-width 5) (error (list (car e) (cdr e)))))
 (window-scroll-bar-height
  (window-scroll-bar-height)
  (window-scroll-bar-height)
  (condition-case e (window-scroll-bar-height 5) (error (list (car e) (cdr e)))))
 (window-scroll-bars
  (window-scroll-bars)
  (list (windowp (selected-window)) (window-scroll-bars))
  (condition-case e (window-scroll-bars 5) (error (list (car e) (cdr e)))))
 (window-scroll-bar-width
  (window-scroll-bar-width)
  (window-scroll-bar-width)
  (condition-case e (window-scroll-bar-width 5) (error (list (car e) (cdr e)))))
 (window-tab-line-height
  (window-tab-line-height)
  (window-tab-line-height)
  (condition-case e (window-tab-line-height 5) (error (list (car e) (cdr e)))))
