(frame-right-divider-width
 (frame-right-divider-width)
 (condition-case e (frame-right-divider-width "bad") (error (cons (car e) (cdr e)))))
(frame-root-frame
 (not (null (framep (frame-root-frame))))
 (condition-case e (frame-root-frame "bad") (error (cons (car e) (cdr e))))
 (eq (frame-root-frame (selected-frame)) (selected-frame)))
(frame-scale-factor
 (frame-scale-factor)
 (condition-case e (frame-scale-factor "bad") (error (cons (car e) (cdr e)))))
(frame-scroll-bar-height
 (frame-scroll-bar-height)
 (condition-case e (frame-scroll-bar-height "bad") (error (cons (car e) (cdr e)))))
(frame-scroll-bar-width
 (frame-scroll-bar-width)
 (condition-case e (frame-scroll-bar-width "bad") (error (cons (car e) (cdr e)))))
(frame--set-was-invisible
 (frame--set-was-invisible (selected-frame) nil)
 (frame--set-was-invisible (selected-frame) t)
 (condition-case e (frame--set-was-invisible "bad" nil) (error (cons (car e) (cdr e)))))
(frame-text-cols
 (frame-text-cols)
 (condition-case e (frame-text-cols "bad") (error (cons (car e) (cdr e)))))
(frame-text-height
 (frame-text-height)
 (condition-case e (frame-text-height "bad") (error (cons (car e) (cdr e)))))
(frame-text-lines
 (frame-text-lines)
 (condition-case e (frame-text-lines) (error (cons (car e) (cdr e))))
 (condition-case e (frame-text-lines "bad") (error (cons (car e) (cdr e)))))
(frame-text-width
 (frame-text-width)
 (condition-case e (frame-text-width "bad") (error (cons (car e) (cdr e)))))
(frame-total-cols
 (frame-total-cols)
 (condition-case e (frame-total-cols "bad") (error (cons (car e) (cdr e)))))
(frame-total-lines
 (frame-total-lines)
 (condition-case e (frame-total-lines "bad") (error (cons (car e) (cdr e)))))
