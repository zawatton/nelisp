(x-focus-frame
 (x-focus-frame (selected-frame))
 (x-focus-frame nil)
 (x-focus-frame (selected-frame) nil)
 (x-focus-frame (selected-frame) t)
 (x-focus-frame (selected-frame) 7)
 (x-focus-frame t)
 (x-focus-frame 'fake-frame)
 (x-focus-frame 7)
 (x-focus-frame "frame")
 (let* ((frame (selected-frame))
        (before (list window-system (length (frame-list))
                      (eq frame (selected-frame)) (frame-live-p frame)
                      (frame-visible-p frame) (frame-focus-state frame)))
        (result (condition-case err
                    (x-focus-frame frame)
                  (error (list 'ERR (car err) (cdr err)))))
        (after (list window-system (length (frame-list))
                     (eq frame (selected-frame)) (frame-live-p frame)
                     (frame-visible-p frame) (frame-focus-state frame))))
   (list result (equal before after)))
 (condition-case err
     (apply #'x-focus-frame nil)
   (wrong-number-of-arguments
    (list 'ERR (car err) '(x-focus-frame 0))))
 (condition-case err
     (apply #'x-focus-frame '(nil nil nil))
   (wrong-number-of-arguments
    (list 'ERR (car err) '(x-focus-frame 3))))
)
