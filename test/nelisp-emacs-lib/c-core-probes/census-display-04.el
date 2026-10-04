;;; census-display-04.el --- canonical probes  -*- lexical-binding: t; -*-

(terminal-live-p
 (terminal-live-p nil)
 (terminal-live-p 'bad)
 (condition-case e (terminal-live-p) (error (car e))))

(tty-display-color-p
 (tty-display-color-p)
 (tty-display-color-p (selected-frame))
 (condition-case e (tty-display-color-p 'bad) (error e)))

(window-body-height
 (> (window-body-height) 0)
 (>= (window-body-height nil t) (window-body-height nil nil))
 (condition-case e (window-body-height 'bad) (error e)))

(window-body-width
 (> (window-body-width) 0)
 (>= (window-body-width nil t) (window-body-width nil nil))
 (condition-case e (window-body-width 'bad) (error e)))

(window-buffer
 (bufferp (window-buffer))
 (bufferp (window-buffer (minibuffer-window)))
 (condition-case e (window-buffer 'bad) (error e)))

(window-combination-limit
 (save-window-excursion
   (split-window-right)
   (window-combination-limit (window-parent (selected-window))))
 (save-window-excursion
   (split-window-right)
   (let ((parent (window-parent (selected-window))))
     (set-window-combination-limit parent t)
     (window-combination-limit parent)))
 (condition-case e (window-combination-limit 'bad) (error e)))

(window-configuration-equal-p
 (let ((config (current-window-configuration)))
   (window-configuration-equal-p config config))
 (save-window-excursion
   (let ((config (current-window-configuration)))
     (split-window-right)
     (window-configuration-equal-p config (current-window-configuration))))
 (condition-case e (window-configuration-equal-p 'bad 'bad) (error e)))

(window-configuration-p
 (window-configuration-p (current-window-configuration))
 (window-configuration-p nil)
 (window-configuration-p [1 2]))

(window-dedicated-p
 (window-dedicated-p)
 (let* ((window (selected-window)) (old (window-dedicated-p window)))
   (unwind-protect
       (progn (set-window-dedicated-p window t) (window-dedicated-p window))
     (set-window-dedicated-p window old)))
 (condition-case e (window-dedicated-p 'bad) (error e)))

(window-display-table
 (null (window-display-table))
 (let* ((window (selected-window)) (old (window-display-table window))
        (table (make-display-table)))
   (unwind-protect
       (progn (aset table 65 [66]) (set-window-display-table window table)
              (aref (window-display-table window) 65))
     (set-window-display-table window old)))
 (condition-case e (window-display-table 'bad) (error e)))

(window-end
 (let ((window (selected-window)))
   (<= (window-end window) (with-current-buffer (window-buffer window) (point-max))))
 (let ((window (selected-window)))
   (<= (window-end window t) (with-current-buffer (window-buffer window) (point-max))))
 (condition-case e (window-end 'bad) (error e)))

(window-frame
 (framep (window-frame))
 (eq (window-frame (minibuffer-window)) (selected-frame))
 (condition-case e (window-frame 'bad) (error e)))

(window-hscroll
 (window-hscroll)
 (let* ((window (selected-window)) (old (window-hscroll window)))
   (unwind-protect
       (progn (set-window-hscroll window 3) (window-hscroll window))
     (set-window-hscroll window old)))
 (condition-case e (window-hscroll 'bad) (error e)))

(window-list
 (mapcar #'window-live-p (window-list))
 (mapcar #'window-minibuffer-p (window-list nil t))
 (condition-case e (mapcar #'windowp (window-list 'bad)) (error e)))

(window-list-1
 (mapcar #'window-live-p (window-list-1))
 (mapcar #'window-minibuffer-p (window-list-1 nil t t))
 (condition-case e (mapcar #'windowp (window-list-1 'bad)) (error e)))

(window-live-p
 (window-live-p (selected-window))
 (window-live-p nil)
 (window-live-p 'bad))

(window-margins
 (window-margins)
 (let* ((window (selected-window)) (old (window-margins window)))
   (unwind-protect
       (progn (set-window-margins window 2 3) (window-margins window))
     (set-window-margins window (car old) (cdr old))))
 (condition-case e (window-margins 'bad) (error e)))

(window-minibuffer-p
 (window-minibuffer-p)
 (window-minibuffer-p (minibuffer-window))
 (condition-case e (window-minibuffer-p 'bad) (error e)))

(window-next-buffers
 (length (window-next-buffers))
 (length (window-next-buffers (minibuffer-window)))
 (condition-case e (window-next-buffers 'bad) (error e)))

(window-parameter
 (window-parameter nil 'census-display-probe)
 (save-window-excursion
   (let ((window (split-window-right)))
     (set-window-parameter window 'census-display-probe '(7 stable))
     (window-parameter window 'census-display-probe)))
 (condition-case e (window-parameter 'bad 'census-display-probe) (error e)))

(window-point
 (let ((window (selected-window)))
   (= (window-point window) (with-current-buffer (window-buffer window) (point))))
 (window-point (minibuffer-window))
 (condition-case e (window-point 'bad) (error e)))

(window-prev-buffers
 (length (window-prev-buffers))
 (length (window-prev-buffers (minibuffer-window)))
 (condition-case e (window-prev-buffers 'bad) (error e)))

(window-start
 (let ((window (selected-window)))
   (= (window-start window) (with-current-buffer (window-buffer window) (point-min))))
 (window-start (minibuffer-window))
 (condition-case e (window-start 'bad) (error e)))

(window-system
 (window-system)
 (window-system (selected-frame))
 (condition-case e (window-system 'bad) (error e)))

(window-valid-p
 (window-valid-p (selected-window))
 (save-window-excursion
   (split-window-right)
   (window-valid-p (window-parent (selected-window))))
 (window-valid-p 'bad))

(windowp
 (windowp (selected-window))
 (windowp nil)
 (windowp [window]))

(x-popup-dialog
 (condition-case e (x-popup-dialog 'bad nil) (error e))
 (condition-case e (x-popup-dialog) (error (car e))))
