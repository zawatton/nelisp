(tty-type
 (tty-type)
 (tty-type nil)
 (condition-case e (tty-type 1) (error e))
 (let ((b (get-buffer-create " *term-2-probe*")))
   (unwind-protect
       (progn (with-current-buffer b (insert "changed"))
              (list (bufferp b) (tty-type)))
     (kill-buffer b)))
 (let ((w (split-window)))
   (unwind-protect
       (list (windowp w) (tty-type (selected-frame)))
     (delete-window w))))
