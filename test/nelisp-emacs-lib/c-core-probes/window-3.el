(window-configuration-frame
 (framep (window-configuration-frame (current-window-configuration)))
 (condition-case e (window-configuration-frame nil) (error (list (car e) (cdr e)))))
(window-cursor-info
 (null (window-cursor-info))
 (condition-case e (window-cursor-info 1) (error (list (car e) (cdr e)))))
(window-cursor-type
 (let ((v (window-cursor-type))) (or (null v) (eq v t) (symbolp v) (consp v)))
 (condition-case e (window-cursor-type 1) (error (list (car e) (cdr e)))))
(window-discard-buffer-from-window
 (let ((b (get-buffer-create " *window-3-probe*")))
   (prog1 (null (window-discard-buffer-from-window b (selected-window)))
     (kill-buffer b)))
 (condition-case e (window-discard-buffer-from-window (current-buffer) 1)
   (error (list (car e) (cdr e)))))
(window-fringes
 (window-fringes)
 (condition-case e (window-fringes 1) (error (list (car e) (cdr e)))))
(window-header-line-height
 (window-header-line-height)
 (condition-case e (window-header-line-height 1) (error (list (car e) (cdr e)))))
(window-left-child
 (null (window-left-child))
 (condition-case e (window-left-child 1) (error (list (car e) (cdr e)))))
(window-left-column
 (window-left-column)
 (condition-case e (window-left-column 1) (error (list (car e) (cdr e)))))
(window-line-height
 (window-line-height 'header-line)
 (condition-case e (window-line-height nil 1) (error (list (car e) (cdr e)))))
(window-lines-pixel-dimensions
 (null (window-lines-pixel-dimensions))
 (condition-case e (window-lines-pixel-dimensions 1) (error (list (car e) (cdr e)))))
(window-mode-line-height
 (window-mode-line-height)
 (condition-case e (window-mode-line-height 1) (error (list (car e) (cdr e)))))
(window-new-normal
 (window-new-normal)
 (condition-case e (window-new-normal 1) (error (list (car e) (cdr e)))))
