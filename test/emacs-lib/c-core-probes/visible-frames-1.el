(visible-frame-list
 (listp (visible-frame-list))
 (not (null (memq (selected-frame) (visible-frame-list))))
 (mapcar #'frame-visible-p (visible-frame-list))
 (let ((first (visible-frame-list)) (second (visible-frame-list)))
   (list (equal first second) (eq first second)))
 (condition-case err (visible-frame-list nil) (error err)))

(frame-list
 (length (frame-list))
 (let ((frames (frame-list)))
   (list (not (null (memq (selected-frame) frames)))
         (mapcar #'frame-live-p frames))))

(frame-list
 (frame-height)
 (frame-height (selected-frame)))

(frame-list
 (frame-width)
 (frame-width (selected-frame)))

(frame-char-width (frame-char-width (selected-frame)))
(frame-char-height (frame-char-height (selected-frame)))
(frame-list (frame-pixel-width (selected-frame)))
(frame-list (frame-pixel-height (selected-frame)))
(make-frame-invisible
 (let ((frame (selected-frame)))
   (unwind-protect
       (list (make-frame-invisible frame t) (frame-visible-p frame)
             (length (visible-frame-list)))
     (make-frame-visible frame))))
(iconify-frame
 (let ((frame (selected-frame)))
   (unwind-protect
       (list (iconify-frame frame) (frame-visible-p frame)
             (length (visible-frame-list)))
     (make-frame-visible frame))))
