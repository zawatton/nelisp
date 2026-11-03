(pgtk-set-resource
 (condition-case e (pgtk-set-resource "NELISP-TEST" "value") (error (cons (car e) (cdr e))))
 (with-temp-buffer (insert "changed")
   (condition-case e (pgtk-set-resource "NELISP-TEST" nil) (error (cons (car e) (cdr e))))))
(x-close-connection
 (condition-case e (x-close-connection nil) (error (cons (car e) (cdr e))))
 (condition-case e (x-close-connection t) (error (cons (car e) (cdr e))))
 (let ((b (get-buffer-create " *pgtkfns-close*")))
   (with-current-buffer b (insert "changed"))
   (condition-case e (x-close-connection nil) (error (cons (car e) (cdr e))))))
(x-create-frame
 (condition-case e (x-create-frame nil) (error (cons (car e) (cdr e))))
 (condition-case e (x-create-frame 42) (error (cons (car e) (cdr e))))
 (let ((b (get-buffer-create " *pgtkfns-frame*")))
   (with-current-buffer b (insert "changed"))
   (condition-case e (x-create-frame '((name . "non-default"))) (error (cons (car e) (cdr e))))))
(x-display-backing-store
 (condition-case e (x-display-backing-store) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-backing-store t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-backing-store nil) (error (cons (car e) (cdr e)))))
(x-display-color-cells
 (condition-case e (x-display-color-cells) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-color-cells t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-color-cells nil) (error (cons (car e) (cdr e)))))
(x-display-grayscale-p
 (x-display-grayscale-p)
 (x-display-grayscale-p t)
 (list (x-display-grayscale-p nil) (windowp (selected-window))))
(x-display-list
 (x-display-list)
 (let ((b (get-buffer-create " *pgtkfns-displays*"))) (with-current-buffer b (insert "changed")) (x-display-list)))
(x-display-mm-height
 (condition-case e (x-display-mm-height) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-mm-height t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-mm-height nil) (error (cons (car e) (cdr e)))))
(x-display-mm-width
 (condition-case e (x-display-mm-width) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-mm-width t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-mm-width nil) (error (cons (car e) (cdr e)))))
(x-display-pixel-height
 (condition-case e (x-display-pixel-height) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-pixel-height t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-pixel-height nil) (error (cons (car e) (cdr e)))))
(x-display-pixel-width
 (condition-case e (x-display-pixel-width) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-pixel-width t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-pixel-width nil) (error (cons (car e) (cdr e)))))
(x-display-planes
 (condition-case e (x-display-planes) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-planes t) (error (cons (car e) (cdr e))))
 (condition-case e (x-display-planes nil) (error (cons (car e) (cdr e)))))
