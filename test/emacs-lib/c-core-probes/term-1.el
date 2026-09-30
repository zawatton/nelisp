(controlling-tty-p
 (null (controlling-tty-p))
 (condition-case e (controlling-tty-p 7) (error (list (car e) (cdr e))))
 (null (controlling-tty-p (selected-frame))))
(tty-display-color-cells
 (= (tty-display-color-cells) 0)
 (condition-case e (tty-display-color-cells 7) (error (list (car e) (cdr e))))
 (let ((b (get-buffer-create " *term-probe*"))) (unwind-protect (= (tty-display-color-cells (selected-frame)) 0) (kill-buffer b))))
(tty-display-pixel-height
 (numberp (tty-display-pixel-height))
 (> (tty-display-pixel-height) 0))
(tty-display-pixel-width
 (numberp (tty-display-pixel-width))
 (> (tty-display-pixel-width) 0))
(tty-frame-at
 (let ((r (tty-frame-at 0 0))) (list (framep (car r)) (numberp (cadr r)) (numberp (caddr r))))
 (null (tty-frame-at 100000 100000))
 (condition-case e (tty-frame-at 'x 0) (error (list (car e) (cdr e))))
 (framep (car (tty-frame-at 1 1))))
(tty-frame-edges
 (null (tty-frame-edges))
 (condition-case e (tty-frame-edges 7) (error (list (car e) (cdr e))))
 (null (tty-frame-edges (selected-frame) 'outer-edges)))
(tty-frame-geometry
 (null (tty-frame-geometry))
 (condition-case e (tty-frame-geometry 7) (error (list (car e) (cdr e))))
 (null (tty-frame-geometry (selected-frame))))
(tty-frame-list-z-order
 (let ((r (tty-frame-list-z-order))) (list (= (length r) 1) (framep (car r))))
 (condition-case e (tty-frame-list-z-order 7) (error (list (car e) (cdr e))))
 (= (length (tty-frame-list-z-order (selected-frame))) 1))
(tty-frame-restack
 (condition-case e (tty-frame-restack (selected-frame) (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (tty-frame-restack 7 (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (tty-frame-restack (selected-frame) (selected-frame) t) (error (list (car e) (cdr e)))))
(tty-no-underline
 (null (tty-no-underline))
 (condition-case e (tty-no-underline 7) (error (list (car e) (cdr e))))
 (let ((b (get-buffer-create " *term-probe*"))) (unwind-protect (null (tty-no-underline (selected-frame))) (kill-buffer b))))
(tty--output-buffer-size
 (condition-case e (tty--output-buffer-size) (error (list (car e) (cdr e))))
 (condition-case e (tty--output-buffer-size 7) (error (list (car e) (cdr e))))
 (condition-case e (tty--output-buffer-size (selected-frame)) (error (list (car e) (cdr e)))))
(tty--set-output-buffer-size
 (condition-case e (tty--set-output-buffer-size 0) (error (list (car e) (cdr e))))
 (condition-case e (tty--set-output-buffer-size -1) (error (list (car e) (cdr e))))
 (condition-case e (tty--set-output-buffer-size 'x) (error (list (car e) (cdr e))))
 (condition-case e (tty--set-output-buffer-size 1 (selected-frame)) (error (list (car e) (cdr e)))))
