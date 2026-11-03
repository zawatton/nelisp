(window-new-pixel
 (window-new-pixel)
 (condition-case e (window-new-pixel "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-new-total
 (window-new-total)
 (condition-case e (window-new-total "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-next-sibling
 (null (window-next-sibling (minibuffer-window)))
 (condition-case e (window-next-sibling "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-normal-size
 (window-normal-size)
 (list (window-normal-size nil t)
       (condition-case e (window-normal-size "bad") (error (list (car e) (cadr e) (caddr e))))) )
(window-old-body-pixel-height
 (window-old-body-pixel-height)
 (condition-case e (window-old-body-pixel-height "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-old-body-pixel-width
 (window-old-body-pixel-width)
 (condition-case e (window-old-body-pixel-width "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-old-buffer
 (null (window-old-buffer))
 (condition-case e (window-old-buffer "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-old-pixel-height
 (window-old-pixel-height)
 (condition-case e (window-old-pixel-height "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-old-pixel-width
 (window-old-pixel-width)
 (condition-case e (window-old-pixel-width "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-old-point
 (integerp (window-old-point))
 (condition-case e (window-old-point "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-parameters
 (null (window-parameters))
 (condition-case e (window-parameters "bad") (error (list (car e) (cadr e) (caddr e)))) )
(window-parent
 (windowp (window-parent))
 (condition-case e (window-parent "bad") (error (list (car e) (cadr e) (caddr e)))) )
