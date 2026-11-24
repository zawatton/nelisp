;;; -*- lexical-binding: t; -*-
(defun par-preload-row (name thunk)
  (prin1 (list name (condition-case err (funcall thunk)
                      (error err))))
  (terpri))

(par-preload-row 'page
 (lambda ()
   (with-temp-buffer
     (insert "first\n\f\nsecond\n")
     (goto-char (point-min))
     (forward-page)
     (list (point) (char-after)))))

(par-preload-row 'float-sup
 (lambda ()
   (list (round (* 100 float-pi)) (round (* 100 float-e)))))
