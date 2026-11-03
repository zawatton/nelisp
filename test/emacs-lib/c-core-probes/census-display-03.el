;;; census-display-03.el --- canonical probes  -*- lexical-binding: t; -*-
(scroll-right
 (save-window-excursion (set-window-hscroll nil 5) (scroll-right 2))
 (save-window-excursion (set-window-hscroll nil 0) (scroll-right 3 t)))
(scroll-up
 (save-window-excursion
   (with-temp-buffer
     (dotimes (_ 100) (insert "line\n"))
     (goto-char 1) (set-window-buffer nil (current-buffer))
     (set-window-start nil 1) (scroll-up 1) (window-start)))
 (save-window-excursion
   (with-temp-buffer
     (dotimes (_ 100) (insert "line\n"))
     (goto-char 1) (set-window-buffer nil (current-buffer))
     (set-window-start nil 1) (scroll-up 0) (window-start)))
 (save-window-excursion
   (with-temp-buffer
     (set-window-buffer nil (current-buffer))
     (condition-case e (scroll-up 1) (error e)))))
(select-frame
 (eq (select-frame (selected-frame) t) (selected-frame))
 (eq (select-frame (selected-frame) t) (window-frame))
 (condition-case e (select-frame 'bogus) (error e)))
(select-window
 (save-current-buffer (eq (select-window (selected-window) t) (selected-window)))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (save-current-buffer (eq (select-window w t) w)))))
 (condition-case e (select-window 'bogus) (error e)))
(selected-frame
 (framep (selected-frame))
 (eq (selected-frame) (window-frame (selected-window)))
 (condition-case e (selected-frame 1) (error (car e))))
(selected-window
 (window-live-p (selected-window))
 (eq (window-frame (selected-window)) (selected-frame))
 (condition-case e (selected-window 1) (error (car e))))
(send-string-to-terminal
 (send-string-to-terminal "")
 (send-string-to-terminal "" (selected-frame))
 (condition-case e (send-string-to-terminal 1) (error e)))
(set-frame-height
 (condition-case e (set-frame-height 'bogus 20) (error e))
 (condition-case e (set-frame-height) (error (car e))))
(set-frame-position
 (condition-case e (set-frame-position 'bogus 0 0) (error e))
 (condition-case e (set-frame-position) (error (car e))))
(set-frame-selected-window
 (save-current-buffer (eq (set-frame-selected-window nil (selected-window) t) (selected-window)))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (save-current-buffer (eq (set-frame-selected-window nil w t) w)))))
 (condition-case e (set-frame-selected-window 'bogus (selected-window) t) (error e)))
(set-frame-size
 (condition-case e (set-frame-size 'bogus 80 24) (error e))
 (condition-case e (set-frame-size) (error (car e))))
(set-frame-width
 (condition-case e (set-frame-width 'bogus 80) (error e))
 (condition-case e (set-frame-width) (error (car e))))
(set-fringe-bitmap-face
 (condition-case e (set-fringe-bitmap-face 'lane-no-such-bitmap nil) (error e))
 (condition-case e (set-fringe-bitmap-face 1 nil) (error e)))
(set-mouse-position
 (set-mouse-position (selected-frame) 0 0)
 (set-mouse-position (selected-frame) 1 1)
 (condition-case e (set-mouse-position 'bogus 0 0) (error e)))
(set-terminal-parameter
 (condition-case e (set-terminal-parameter 'bogus 'lane 1) (error e))
 (condition-case e (set-terminal-parameter) (error (car e))))
(set-window-buffer
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (insert "abc") (set-window-buffer w (current-buffer)) (eq (window-buffer w) (current-buffer))))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-buffer w (current-buffer) t) (eq (window-buffer w) (current-buffer))))))
 (condition-case e (set-window-buffer 'bogus (current-buffer)) (error e)))
(set-window-configuration
 (set-window-configuration (current-window-configuration))
 (set-window-configuration (current-window-configuration) t t)
 (condition-case e (set-window-configuration 1) (error e)))
(set-window-dedicated-p
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-dedicated-p w t) (window-dedicated-p w)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-dedicated-p w nil) (window-dedicated-p w))))))
(set-window-display-table
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-display-table w (make-display-table)) (char-table-p (window-display-table w))))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-display-table w nil) (window-display-table w))))))
(set-window-fringes
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-fringes w 0 0) (window-fringes w)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-fringes w 2 3 t) (window-fringes w))))))
(set-window-hscroll
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-hscroll w 4) (window-hscroll w)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-hscroll w -2) (window-hscroll w))))))
(set-window-margins
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-margins w 2 3) (window-margins w)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-margins w 0 0) (window-margins w))))))
(set-window-next-buffers
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-next-buffers w (list (current-buffer))) (eq (car (window-next-buffers w)) (current-buffer))))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-next-buffers w nil) (window-next-buffers w))))))
(set-window-parameter
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-parameter w 'lane 42) (window-parameter w 'lane)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-parameter w 'lane nil) (window-parameter w 'lane))))))
(set-window-point
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (insert "abc") (set-window-point w 2) (window-point w)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (insert "abc") (set-window-point w 99) (window-point w))))))
(set-window-prev-buffers
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-prev-buffers w (list (list (current-buffer) 1 1))) (let ((row (car (window-prev-buffers w)))) (list (eq (car row) (current-buffer)) (cdr row)))))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (set-window-prev-buffers w nil) (window-prev-buffers w))))))
(set-window-start
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (insert "a\nb\nc\n") (set-window-start w 3 t) (window-start w)))))
 (save-window-excursion
   (let ((w (split-window)))
     (with-temp-buffer
       (set-window-buffer w (current-buffer))
       (progn (insert "abc") (set-window-start w 1) (window-start w))))))
(sleep-for
 (sleep-for 0)
 (sleep-for 0 1)
 (condition-case e (sleep-for 'bogus) (error e)))
(suspend-tty
 (condition-case e (suspend-tty 'bogus) (error e))
 (condition-case e (suspend-tty 999999) (error e)))
