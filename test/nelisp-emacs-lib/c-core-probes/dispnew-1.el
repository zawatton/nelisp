(display--update-for-mouse-movement
  (condition-case nil (display--update-for-mouse-movement nil 'x 2) (error 'error))
  (condition-case nil (display--update-for-mouse-movement nil 2 'y) (error 'error)))
(frame-or-buffer-changed-p
  (progn (defvar dispnew-probe-state nil)
         (list (frame-or-buffer-changed-p 'dispnew-probe-state)
               (frame-or-buffer-changed-p 'dispnew-probe-state)))
  (progn (let ((buffer (get-buffer-create "dispnew-probe")))
           (with-current-buffer buffer (insert "x"))
           (list (frame-or-buffer-changed-p 'dispnew-probe-state)
                 (frame-or-buffer-changed-p 'dispnew-probe-state)))))
(frame--z-order-lessp
  (frame--z-order-lessp (selected-frame) (selected-frame))
  (frame--z-order-lessp (selected-frame) (selected-frame)))
(internal-show-cursor
  (progn (internal-show-cursor nil nil) (internal-show-cursor-p))
  (progn (internal-show-cursor nil t) (internal-show-cursor-p)))
(internal-show-cursor-p
  (internal-show-cursor-p)
  (let ((window (split-window)))
    (prog1 (windowp window) (delete-window window))))
(redraw-frame
  (redraw-frame)
  (condition-case e (redraw-frame 1) (error e)))
