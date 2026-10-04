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
;; These frame metrics/reordering primitives require a real PGTK frame; a
;; batch terminal frame reaches unchecked GTK widget state. Probe frame type
;; validation instead. The GUI-specific success cases need a display server.
(pgtk-frame-edges
 (condition-case e (pgtk-frame-edges 1) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-edges 1 'outer-edges) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-edges 1) (error (list (car e) (cdr e)))))
(pgtk-frame-geometry
 (condition-case e (pgtk-frame-geometry 1) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-geometry 1) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-geometry 1) (error (list (car e) (cdr e)))))
(pgtk-frame-restack
 (condition-case e (pgtk-frame-restack 1 2) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-restack 1 (selected-frame)) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-frame-restack 1 (selected-frame) t) (error (list (car e) (cdr e)))))
(pgtk-get-page-setup (pgtk-get-page-setup)
                     (mapcar #'car (pgtk-get-page-setup))
                     (let ((x (pgtk-get-page-setup))) (assq 'orientation x)))
(pgtk-mouse-absolute-pixel-position
 (condition-case e (pgtk-mouse-absolute-pixel-position 1)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-mouse-absolute-pixel-position 1 2)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e)))))
;; Valid dialog calls open native GTK dialogs and are not batch-safe. Exercise
;; their native arity guards instead; GUI interaction remains untested.
(pgtk-page-setup-dialog
 (condition-case e (pgtk-page-setup-dialog 'extra)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-page-setup-dialog nil)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e)))))
(pgtk-print-frames-dialog
 (condition-case e (pgtk-print-frames-dialog nil t)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-print-frames-dialog (selected-frame) t)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-print-frames-dialog (list 1)) (error (list (car e) (cdr e)))))
(pgtk-set-monitor-scale-factor
 (pgtk-set-monitor-scale-factor "probe" 2)
 (pgtk-set-monitor-scale-factor "probe" nil)
 (condition-case e (pgtk-set-monitor-scale-factor nil 2) (error (list (car e) (cdr e)))))
(pgtk-set-mouse-absolute-pixel-position
 (condition-case e (pgtk-set-mouse-absolute-pixel-position 1)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-set-mouse-absolute-pixel-position 1 2 3)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e))))
 (condition-case e (pgtk-set-mouse-absolute-pixel-position 3 4 5)
   (wrong-number-of-arguments 'wrong-number-of-arguments) (error (list (car e) (cdr e)))))
