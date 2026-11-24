;;; par-preload-window-kill.el --- Splitting after a displayed buffer is killed -*- lexical-binding: t; -*-

(let* ((window (selected-window))
       (previous (window-buffer window))
       temporary)
  (with-temp-buffer
    (setq temporary (current-buffer))
    (set-window-buffer window temporary))
  (prin1 (list (not (buffer-live-p temporary))
               (buffer-live-p (window-buffer window))
               (eq previous (window-buffer window))
               (eq previous (current-buffer))))
  (terpri)
  ;; The split-window/window-list case from nemacs-replacement-probe.el.
  (prin1 (condition-case err
             (progn (delete-other-windows) (split-window) (prog1 (= (length (window-list)) 2) (delete-other-windows)))
           (error err)))
  (terpri))
