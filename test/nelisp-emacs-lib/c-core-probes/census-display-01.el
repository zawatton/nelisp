;;; census-display-01.el --- canonical probes  -*- lexical-binding: t; -*-

(backtrace--frames-from-thread
 (consp (backtrace--frames-from-thread (current-thread)))
 (condition-case e (backtrace--frames-from-thread nil) (error e)))

(bitmap-spec-p
 (bitmap-spec-p '(8 1 "x"))
 (bitmap-spec-p '(9 1 "x"))
 (bitmap-spec-p nil))

(current-message
 (current-message)
 (condition-case e (current-message nil) (error (car e))))

(current-window-configuration
 (window-configuration-p (current-window-configuration))
 (window-configuration-p (current-window-configuration (selected-frame)))
 (condition-case e (window-configuration-p (current-window-configuration 'bogus))
   (error e)))

(define-fringe-bitmap
 (let ((bitmap (make-symbol "lane-fringe")))
   (unwind-protect
       (eq (define-fringe-bitmap bitmap [0 255] 2 8) bitmap)
     (destroy-fringe-bitmap bitmap)))
 (let ((bitmap (make-symbol "lane-fringe")))
   (unwind-protect
       (eq (define-fringe-bitmap bitmap "\0" nil 1 'top) bitmap)
     (destroy-fringe-bitmap bitmap)))
 (let ((bitmap (make-symbol "lane-invalid")))
   (unwind-protect
       (condition-case e (define-fringe-bitmap bitmap [0] 1 0) (error e))
     (destroy-fringe-bitmap bitmap))))

(delete-frame
 (condition-case e (delete-frame 'bogus) (error e))
 (condition-case e (delete-frame nil nil nil) (error (car e))))

(delete-terminal
 (condition-case e (delete-terminal nil nil nil) (error (car e)))
 (condition-case e (delete-terminal nil nil nil nil) (error (car e))))

(destroy-fringe-bitmap
 (let ((bitmap (make-symbol "lane-fringe")))
   (unwind-protect
       (progn (define-fringe-bitmap bitmap [0] 1 8)
              (destroy-fringe-bitmap bitmap))
     (destroy-fringe-bitmap bitmap)))
 (destroy-fringe-bitmap (make-symbol "lane-absent-fringe"))
 (condition-case e (destroy-fringe-bitmap 7) (error e)))

(ding
 (condition-case e (ding nil nil) (error (car e)))
 (condition-case e (ding nil nil nil) (error (car e))))

(display-supports-face-attributes-p
 (display-supports-face-attributes-p nil)
 (display-supports-face-attributes-p '(:underline t) (selected-frame))
 (condition-case e (display-supports-face-attributes-p nil nil nil) (error (car e))))

(face-attribute-relative-p
 (face-attribute-relative-p :height 1.5)
 (face-attribute-relative-p :height 120)
 (face-attribute-relative-p :weight 'unspecified))

(face-font
 (let ((font (face-font 'default t))) (or (null font) (stringp font)))
 (let ((font (face-font 'bold t))) (or (null font) (listp font)))
 (condition-case e (face-font 'lane-missing-face t) (error e)))

(fontset-list
 (listp (fontset-list))
 (condition-case e (fontset-list nil) (error (car e))))

(force-window-update
 (force-window-update (selected-window))
 (with-temp-buffer (force-window-update (current-buffer)))
 (condition-case e (force-window-update nil nil) (error (car e))))

(frame-first-window
 (window-live-p (frame-first-window))
 (eq (frame-first-window (selected-window)) (frame-first-window (selected-frame)))
 (condition-case e (frame-first-window 'bogus) (error e)))

(frame-focus
 (null (frame-focus))
 (eq (frame-focus nil) (frame-focus (selected-frame)))
 (condition-case e (frame-focus 'bogus) (error e)))

(frame-live-p
 (not (null (frame-live-p (selected-frame))))
 (frame-live-p nil)
 (frame-live-p 'bogus))

(frame-parameter
 (frame-parameter nil 'lane-missing-parameter)
 (frame-parameter (selected-frame) 'lane-missing-parameter)
 (condition-case e (frame-parameter 'bogus 'name) (error e)))

(frame-parameters
 (listp (frame-parameters))
 (equal (mapcar #'car (frame-parameters nil))
        (mapcar #'car (frame-parameters (selected-frame))))
 (condition-case e (frame-parameters 'bogus) (error e)))

(frame-selected-window
 (eq (frame-selected-window) (selected-window))
 (eq (frame-selected-window (selected-window)) (selected-window))
 (condition-case e (window-live-p (frame-selected-window 'bogus)) (error e)))

(frame-terminal
 (terminal-live-p (frame-terminal))
 (eq (frame-terminal nil) (frame-terminal (selected-frame)))
 (condition-case e (frame-terminal 'bogus) (error e)))

(frame-visible-p
 (frame-visible-p (selected-frame))
 (condition-case e (frame-visible-p nil) (error e)))

(framep
 (not (null (framep (selected-frame))))
 (framep nil)
 (framep 'bogus))

(get-buffer-window
 (with-temp-buffer (get-buffer-window (current-buffer)))
 (with-temp-buffer (get-buffer-window (current-buffer) t))
 (condition-case e (get-buffer-window 7) (error e)))

(internal--track-mouse
 (internal--track-mouse (lambda () 42))
 (let ((track-mouse nil))
   (list (internal--track-mouse (lambda () track-mouse)) track-mouse))
 (condition-case e (internal--track-mouse 7) (error e)))

(internal-copy-lisp-face
 (internal-copy-lisp-face 'default 'default t nil)
 (internal-copy-lisp-face 'bold 'bold t nil)
 (condition-case e (internal-copy-lisp-face 'lane-missing-face 'default t nil)
   (error e)))

(internal-face-x-get-resource
 (condition-case e (internal-face-x-get-resource 7 "Class") (error e))
 (condition-case e (internal-face-x-get-resource "name" 7) (error e)))

(internal-get-lisp-face-attribute
 (internal-get-lisp-face-attribute 'default :height t)
 (internal-get-lisp-face-attribute 'bold :weight t)
 (condition-case e (internal-get-lisp-face-attribute 'default :lane-bogus t)
   (error e)))

(internal-lisp-face-attribute-values
 (internal-lisp-face-attribute-values :weight)
 (internal-lisp-face-attribute-values :height)
 (internal-lisp-face-attribute-values :lane-bogus))
