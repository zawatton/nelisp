(set-buffer-redisplay
 (set-buffer-redisplay 'probe 1 'set nil)
 (set-buffer-redisplay nil nil nil nil))
(tab-bar-height
 (tab-bar-height)
 (tab-bar-height nil t)
 (condition-case e (tab-bar-height "bad") (error (cons (car e) (cdr e)))))
(tool-bar-height
 (tool-bar-height)
 (tool-bar-height nil t)
 (tool-bar-height "bad" t))
(window-text-pixel-size
 (window-text-pixel-size)
 (condition-case e (window-text-pixel-size 1) (error (cons (car e) (cdr e))))
 (let ((w (split-window)))
   (unwind-protect
       (progn (with-current-buffer (window-buffer w)
                (erase-buffer) (insert "changed\nstate")
                (put-text-property 1 2 'face 'bold))
              (list (windowp w)
                    (window-text-pixel-size w 1 (with-current-buffer (window-buffer w) (point-max)))))
     (delete-window w)))
 (window-text-pixel-size nil '(1 . 8) 3 nil nil t)
 (condition-case e (window-text-pixel-size nil 1 2 nil nil nil t) (error (cons (car e) (cdr e)))))
