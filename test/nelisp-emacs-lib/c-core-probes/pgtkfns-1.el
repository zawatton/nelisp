(pgtk-backend-display-class
 (condition-case e (pgtk-backend-display-class) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-backend-display-class 1) (error (list (car e) (cdr e)))))
(pgtk-display-monitor-attributes-list
 (condition-case e (pgtk-display-monitor-attributes-list) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-display-monitor-attributes-list 1) (error (list (car e) (cdr e))))
 )
(pgtk-font-name (pgtk-font-name "fixed")
                (pgtk-font-name "fontset-probe")
                (condition-case e (pgtk-font-name nil) (error (list (car e) (cdr e)))))
(pgtk-frame-edges
 (condition-case e (pgtk-frame-edges) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (unwind-protect
      (condition-case e (pgtk-frame-edges (selected-frame) 'outer-edges)
        (error (list (car e) (cdr e)))) (delete-window w)))
 (condition-case e (pgtk-frame-edges 1) (error (list (car e) (cdr e))))
(pgtk-frame-geometry
 (condition-case e (pgtk-frame-geometry) (error (list (car e) (cdr e))))
 (let ((w (split-window))) (unwind-protect
      (condition-case e (pgtk-frame-geometry (selected-frame)) (error (list (car e) (cdr e))))
   (delete-window w)))
 (condition-case e (pgtk-frame-geometry 1) (error (list (car e) (cdr e))))
(pgtk-frame-restack
 (condition-case e (pgtk-frame-restack (selected-frame) (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-restack 1 (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-restack (selected-frame) (selected-frame) t) (error (list (car e) (cdr e))))
(pgtk-get-page-setup (pgtk-get-page-setup)
                     (let ((x (pgtk-get-page-setup))) (assq 'orientation x)))
(pgtk-mouse-absolute-pixel-position
 (condition-case e (pgtk-mouse-absolute-pixel-position) (error (list (car e) (cdr e))))
 (condition-case e (progn (split-window) (pgtk-mouse-absolute-pixel-position))
   (error (list (car e) (cdr e))))
(pgtk-page-setup-dialog
 (condition-case e (pgtk-page-setup-dialog) (error (list (car e) (cdr e))))
 (condition-case e (progn (insert "x") (pgtk-page-setup-dialog)) (error (list (car e) (cdr e))))
(pgtk-print-frames-dialog
 (condition-case e (pgtk-print-frames-dialog) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-print-frames-dialog (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-print-frames-dialog 1) (error (list (car e) (cdr e))))
(pgtk-set-monitor-scale-factor
 (pgtk-set-monitor-scale-factor "probe" 2)
 (pgtk-set-monitor-scale-factor "probe" nil)
 (condition-case e (pgtk-set-monitor-scale-factor nil 2) (error (list (car e) (cdr e)))))
(pgtk-set-mouse-absolute-pixel-position
 (condition-case e (pgtk-set-mouse-absolute-pixel-position 1 2) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-set-mouse-absolute-pixel-position nil 2) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-set-mouse-absolute-pixel-position 3 4) (error (list (car e) (cdr e)))))
