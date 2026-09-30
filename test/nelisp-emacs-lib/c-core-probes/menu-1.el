(menu-bar-menu-at-x-y
 (menu-bar-menu-at-x-y 0 -1)
 (condition-case e (menu-bar-menu-at-x-y -1 -1 t) (error (cons (car e) (cdr e))))
 (let ((b (get-buffer-create " *menu-probe*")))
   (unwind-protect
       (progn
         (with-current-buffer b (erase-buffer) (insert "changed")
           (put-text-property 1 2 'face 'bold))
         (list (bufferp b) (windowp (selected-window))
               (menu-bar-menu-at-x-y 0 -1 (selected-frame))))
     (kill-buffer b))))
(x-popup-menu
 (x-popup-menu nil (make-sparse-keymap))
 (condition-case e (x-popup-menu 'nonsense (make-sparse-keymap))
   (error (cons (car e) (cdr e))))
 (let ((b (get-buffer-create " *menu-popup-probe*"))
       (map (make-sparse-keymap)))
   (unwind-protect
       (progn
         (with-current-buffer b (erase-buffer) (insert "changed")
           (put-text-property 1 2 'face 'bold))
         (define-key map [menu-bar probe] '("Probe" . ignore))
         (list (bufferp b) (windowp (selected-window))
               (x-popup-menu nil (list "Title" (list "Pane" (cons "item" 'value))))))
     (kill-buffer b))))
