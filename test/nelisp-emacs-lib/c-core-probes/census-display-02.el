;;; census-display-02.el --- canonical probes  -*- lexical-binding: t; -*-
(internal-lisp-face-empty-p
 (internal-lisp-face-empty-p 'default)
 (internal-lisp-face-empty-p 'bold t)
 (condition-case e (internal-lisp-face-empty-p 'ccore-missing-face) (error e)))
(internal-lisp-face-equal-p
 (internal-lisp-face-equal-p 'bold 'bold)
 (internal-lisp-face-equal-p 'bold 'italic t)
 (condition-case e (internal-lisp-face-equal-p 'bold 'ccore-missing-face) (error e)))
(internal-lisp-face-p
 (not (null (internal-lisp-face-p 'default)))
 (not (null (internal-lisp-face-p 'ccore-missing-face)))
 (not (null (internal-lisp-face-p "bold" (selected-frame)))))
(internal-merge-in-global-face
 (let ((saved (face-all-attributes 'bold)))
   (unwind-protect
       (internal-merge-in-global-face 'bold (selected-frame))
     (dolist (attr saved) (set-face-attribute 'bold nil (car attr) (cdr attr)))))
 (let ((saved (face-all-attributes 'italic)))
   (unwind-protect
       (internal-merge-in-global-face 'italic (selected-frame))
     (dolist (attr saved) (set-face-attribute 'italic nil (car attr) (cdr attr)))))
 (condition-case e (internal-merge-in-global-face 'ccore-missing-face (selected-frame)) (error e)))
(internal-set-lisp-face-attribute
 (let ((saved (face-attribute 'bold :weight)))
   (unwind-protect
       (progn (internal-set-lisp-face-attribute 'bold :weight 'normal)
              (face-attribute 'bold :weight))
     (set-face-attribute 'bold nil :weight saved)))
 (let ((saved (face-attribute 'bold :weight)))
   (unwind-protect
       (progn (internal-set-lisp-face-attribute 'bold :weight 'bold)
              (face-attribute 'bold :weight))
     (set-face-attribute 'bold nil :weight saved)))
 (condition-case e (internal-set-lisp-face-attribute 'bold :ccore-invalid t) (error e)))
(invisible-p
 (with-temp-buffer
   (insert "abc") (put-text-property 1 2 'invisible 'ccore-hidden)
   (let ((buffer-invisibility-spec '(ccore-hidden))) (invisible-p 1)))
 (let ((buffer-invisibility-spec '((ccore-hidden . t)))) (invisible-p 'ccore-hidden))
 (let ((buffer-invisibility-spec nil)) (invisible-p 'ccore-hidden)))
(lower-frame
 (condition-case e (lower-frame 'ccore-invalid-frame) (error e))
 (condition-case e (lower-frame nil nil) (error (car e))))
(make-frame-visible
 (condition-case e (make-frame-visible 'ccore-invalid-frame) (error e))
 (condition-case e (make-frame-visible nil nil) (error (car e))))
(merge-face-attribute
 (merge-face-attribute :height 1.5 100)
 (merge-face-attribute :weight 'unspecified 'bold)
 (condition-case e (merge-face-attribute :height) (error (car e))))
(message
 (let ((inhibit-message t) (message-log-max nil)) (message "%s:%d" "probe" 7))
 (let ((inhibit-message t) (message-log-max nil)) (message ""))
 (condition-case e (message) (error (car e))))
(minibuffer-window
 (window-minibuffer-p (minibuffer-window))
 (eq (minibuffer-window nil) (minibuffer-window (selected-frame)))
 (condition-case e (minibuffer-window 'ccore-invalid-frame) (error e)))
(modify-frame-parameters
 (modify-frame-parameters nil nil)
 (modify-frame-parameters (selected-frame) nil)
 (condition-case e (modify-frame-parameters 'ccore-invalid-frame nil) (error e)))
(mouse-position
 (let ((mouse-position-function nil))
   (let ((p (mouse-position))) (list (framep (car p)) (cdr p))))
 (let ((mouse-position-function (lambda (p) 'ccore-mouse))) (mouse-position))
 (condition-case e (mouse-position nil) (error (car e))))
(next-frame
 (eq (next-frame) (selected-frame))
 (eq (next-frame nil t) (selected-frame))
 (condition-case e (next-frame 'ccore-invalid-frame) (error e)))
(next-window
 (eq (next-window nil 'never) (selected-window))
 (eq (next-window nil t) (minibuffer-window))
 (condition-case e (next-window 'ccore-invalid-window) (error e)))
(open-termscript
 (condition-case e (open-termscript nil) (error e))
 (condition-case e (open-termscript) (error (car e))))
(play-sound-internal
 (condition-case e (play-sound-internal nil) (error e))
 (condition-case e (play-sound-internal) (error (car e))))
(pos-visible-in-window-p
 (save-window-excursion
   (with-temp-buffer
     (insert "abc") (set-window-buffer (selected-window) (current-buffer))
     (pos-visible-in-window-p 1)))
 (save-window-excursion
   (with-temp-buffer
     (insert "abc") (set-window-buffer (selected-window) (current-buffer))
     (not (null (pos-visible-in-window-p 1 nil t)))))
 (condition-case e (pos-visible-in-window-p 1 'ccore-invalid-window) (error e)))
(posn-at-point
 (save-window-excursion
   (with-temp-buffer
     (insert "abc") (set-window-buffer (selected-window) (current-buffer))
     (null (posn-at-point 1))))
 (save-window-excursion
   (with-temp-buffer
     (insert "abc") (set-window-buffer (selected-window) (current-buffer))
     (null (posn-at-point 4))))
 (condition-case e (posn-at-point 1 'ccore-invalid-window) (error e)))
(previous-window
 (eq (previous-window nil 'never) (selected-window))
 (eq (previous-window nil t) (minibuffer-window))
 (condition-case e (previous-window 'ccore-invalid-window) (error e)))
(raise-frame
 (condition-case e (raise-frame 'ccore-invalid-frame) (error e))
 (condition-case e (raise-frame nil nil) (error (car e))))
(recenter
 (save-window-excursion
   (with-temp-buffer
     (insert "one\ntwo\nthree\n") (goto-char 1)
     (set-window-buffer (selected-window) (current-buffer)) (recenter 0)))
 (save-window-excursion
   (with-temp-buffer
     (insert "one\ntwo\nthree\n") (goto-char 1)
     (set-window-buffer (selected-window) (current-buffer)) (recenter -1)))
 (condition-case e (recenter nil nil nil) (error (car e))))
(redirect-frame-focus
 (condition-case e (redirect-frame-focus 'ccore-invalid-frame) (error e))
 (condition-case e (redirect-frame-focus) (error (car e))))
(redisplay
 (redisplay)
 (redisplay t)
 (condition-case e (redisplay nil nil) (error (car e))))
(redraw-display
 (condition-case e (redraw-display nil) (error (car e)))
 (condition-case e (redraw-display nil nil) (error (car e))))
(resume-tty
 (condition-case e (resume-tty 'ccore-invalid-terminal) (error e))
 (condition-case e (resume-tty nil nil) (error (car e))))
(run-window-configuration-change-hook
 (let ((window-configuration-change-hook nil)) (run-window-configuration-change-hook))
 (let ((window-configuration-change-hook nil)) (run-window-configuration-change-hook (selected-frame)))
 (condition-case e (run-window-configuration-change-hook 'ccore-invalid-frame) (error e)))
(scroll-down
 (save-window-excursion
   (with-temp-buffer
     (insert "one\ntwo\nthree\n") (goto-char 1)
     (set-window-buffer (selected-window) (current-buffer)) (scroll-down 0)))
 (save-window-excursion
   (with-temp-buffer
     (insert "one\ntwo\nthree\n") (goto-char 1)
     (set-window-buffer (selected-window) (current-buffer))
     (condition-case e (scroll-down 1) (error e)))))
(scroll-left
 (save-window-excursion
   (with-temp-buffer
     (insert "abc") (set-window-buffer (selected-window) (current-buffer))
     (set-window-hscroll (selected-window) 0) (scroll-left 3)))
 (save-window-excursion
   (with-temp-buffer
     (insert "abc") (set-window-buffer (selected-window) (current-buffer))
     (set-window-hscroll (selected-window) 0) (scroll-left 0)))
 (condition-case e (scroll-left nil nil nil) (error (car e))))
