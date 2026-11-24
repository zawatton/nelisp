;;; -*- lexical-binding: t; -*-
(defun par-preload-row (name thunk)
  (prin1 (list name (condition-case err (funcall thunk)
                      (error err))))
  (terpri))

(par-preload-row 'window
 (lambda ()
   (let ((window (selected-window)))
     (list (= (window-total-width window) (window-total-size window t))
           (and (memq window (window-list)) t)))))

(par-preload-row 'register
 (lambda ()
   (with-temp-buffer
     (let (register-alist)
       (set-register ?r "register text")
       (insert-register ?r)
       (list (get-register ?r) (buffer-string))))))
