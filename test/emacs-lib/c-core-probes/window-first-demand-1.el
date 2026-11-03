;;; window-first-demand-1.el --- First-use minibuffer window semantics -*- lexical-binding: t; -*-

(minibuffer-window
 (let* ((mini (minibuffer-window))
        (frame (selected-frame)))
   (list (window-live-p mini)
         (window-minibuffer-p mini)
         (eq (window-frame mini) frame)
         (eq mini (minibuffer-window frame))
         (null (window-parent mini))))
 (condition-case err
     (minibuffer-window 42)
   (error err)))
