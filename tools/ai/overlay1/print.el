;;; -*- lexical-binding: nil -*-
;; Printed form of overlays (GNU prints "#<overlay from A to B in NAME>" and "#<overlay in no buffer>").
(defun pr-show (label v) (princ (format "%s %S\n" label v)))
(pr-show "scratch" (prin1-to-string (make-overlay 1 1)))
(with-temp-buffer
  (insert "hello world")
  (let ((ov (make-overlay 1 6)))
    (overlay-put ov 'face 'bold)
    (overlay-put ov 'button ov)
    (pr-show "self-ref" (prin1-to-string ov))
    (pr-show "format" (format "%S|%s" ov ov))
    (pr-show "list" (prin1-to-string (list ov (make-overlay 2 3))))
    (pr-show "equal-self" (equal ov ov))
    (delete-overlay ov)
    (pr-show "detached" (prin1-to-string ov))
    (pr-show "detached-format" (format "%S" (list ov)))))
(pr-show "type" (list (overlayp (make-overlay 1 1)) (overlayp 'x) (overlayp [1 2])))
nil
