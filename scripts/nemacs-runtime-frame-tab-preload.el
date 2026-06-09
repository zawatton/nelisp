;;; nemacs-runtime-frame-tab-preload.el --- frame/tab runtime image cells -*- lexical-binding: t; -*-

;;; Code:

(fset 'assq
      '(lambda (key alist)
         (let ((tail alist)
               (found nil))
           (while tail
             (if (eq (car (car tail)) key)
                 (progn
                   (setq found (car tail))
                   (setq tail nil))
               (setq tail (cdr tail))))
           found)))

(fset 'nth
      '(lambda (index list)
         (let ((tail list)
               (i index))
           (while (> i 0)
             (setq tail (cdr tail))
             (setq i (- i 1)))
           (car tail))))

(fset 'append
      '(lambda (left right)
         (if left
             (cons (car left) (append (cdr left) right))
           right)))

(setq emacs-frame--runtime-id-counter 0)
(setq emacs-frame--runtime-registry nil)
(setq emacs-frame--runtime-selected-frame nil)

(fset 'emacs-frame--runtime-cell
      '(lambda (frame key)
         (assq key frame)))

(fset 'emacs-frame--runtime-ref
      '(lambda (frame key)
         (cdr (emacs-frame--runtime-cell frame key))))

(fset 'emacs-frame--runtime-set
      '(lambda (frame key value)
         (let ((cell (emacs-frame--runtime-cell frame key)))
           (if cell
               (setcdr cell value)
             (setq frame (cons (cons key value) frame)))
           value)))

(fset 'emacs-frame--runtime-frame-object-p
      '(lambda (object)
         (if (consp object)
             (if (assq 'nemacs-frame object) t nil)
           nil)))

(fset 'emacs-frame--runtime-live-p
      '(lambda (object)
         (if (emacs-frame--runtime-frame-object-p object)
             (if (emacs-frame--runtime-ref object 'dead) nil t)
           nil)))

(fset 'emacs-frame--runtime-put-parameter
      '(lambda (frame key value)
         (let* ((pcell (emacs-frame--runtime-cell frame 'parameters))
                (plist (cdr pcell))
                (existing (assq key plist)))
           (if existing
               (setcdr existing value)
             (setcdr pcell (cons (cons key value) plist)))
           value)))

(fset 'emacs-frame--runtime-apply-parameters
      '(lambda (frame parameters)
         (dolist (pair parameters)
           (let ((key (car pair))
                 (value (cdr pair)))
             (cond
              ((eq key 'width)
               (emacs-frame--runtime-set frame 'width value)
               (emacs-frame--runtime-set frame 'pixel-width (* value 8)))
              ((eq key 'height)
               (emacs-frame--runtime-set frame 'height value)
               (emacs-frame--runtime-set frame 'pixel-height (* value 16)))
              ((eq key 'left)
               (emacs-frame--runtime-set frame 'left value))
              ((eq key 'top)
               (emacs-frame--runtime-set frame 'top value))
              ((eq key 'name)
               (emacs-frame--runtime-set frame 'name value))
              ((eq key 'visibility)
               (emacs-frame--runtime-set frame 'visible value)))
             (emacs-frame--runtime-put-parameter frame key value)))
         frame))

(fset 'emacs-frame--runtime-make-frame-object
      '(lambda (&optional parameters)
         (setq emacs-frame--runtime-id-counter
               (+ emacs-frame--runtime-id-counter 1))
         (let* ((id emacs-frame--runtime-id-counter)
                (frame (list (cons 'nemacs-frame t)
                             (cons 'id id)
                             (cons 'backend 'stub)
                             (cons 'name (concat "F" (number-to-string id)))
                             (cons 'width 80)
                             (cons 'height 24)
                             (cons 'pixel-width 640)
                             (cons 'pixel-height 384)
                             (cons 'left 0)
                             (cons 'top 0)
                             (cons 'visible t)
                             (cons 'parameters nil)
                             (cons 'dead nil))))
           (if parameters
               (emacs-frame--runtime-apply-parameters frame parameters)
             nil)
           frame)))

(fset 'emacs-frame--runtime-ensure-initial
      '(lambda ()
         (if (emacs-frame--runtime-live-p emacs-frame--runtime-selected-frame)
             nil
           (let ((frame (emacs-frame--runtime-make-frame-object nil)))
             (setq emacs-frame--runtime-registry (list frame))
             (setq emacs-frame--runtime-selected-frame frame)))
         emacs-frame--runtime-selected-frame))

(fset 'emacs-frame--runtime-get
      '(lambda (&optional frame)
         (let ((target (if frame
                           frame
                         (emacs-frame--runtime-ensure-initial))))
           (if (emacs-frame--runtime-live-p target)
               target
             (emacs-frame--runtime-ensure-initial)))))

(fset 'emacs-frame-reset
      '(lambda ()
         (setq emacs-frame--runtime-id-counter 0)
         (setq emacs-frame--runtime-registry nil)
         (setq emacs-frame--runtime-selected-frame nil)
         nil))

(fset 'framep
      '(lambda (object)
         (if (emacs-frame--runtime-frame-object-p object)
             (emacs-frame--runtime-ref object 'backend)
           nil)))

(fset 'frame-live-p
      '(lambda (object)
         (if (emacs-frame--runtime-live-p object)
             (emacs-frame--runtime-ref object 'backend)
           nil)))

(fset 'selected-frame
      '(lambda ()
         (emacs-frame--runtime-ensure-initial)))

(fset 'frame-list
      '(lambda ()
         (emacs-frame--runtime-ensure-initial)
         (let ((live nil))
           (dolist (frame emacs-frame--runtime-registry)
             (if (emacs-frame--runtime-live-p frame)
                 (setq live (append live (list frame)))
               nil))
           live)))

(fset 'make-frame
      '(lambda (&optional parameters)
         (emacs-frame--runtime-ensure-initial)
         (let ((frame (emacs-frame--runtime-make-frame-object parameters)))
           (setq emacs-frame--runtime-registry
                 (append emacs-frame--runtime-registry (list frame)))
           frame)))

(fset 'delete-frame
      '(lambda (&optional frame force)
         (let ((target (emacs-frame--runtime-get frame)))
           (if (<= (length (frame-list)) 1)
               nil
             (emacs-frame--runtime-set target 'dead t)
             (if (eq target emacs-frame--runtime-selected-frame)
                 (setq emacs-frame--runtime-selected-frame (car (frame-list)))
               nil)
             nil))))

(fset 'delete-other-frames
      '(lambda (&optional frame)
         (let ((keep (emacs-frame--runtime-get frame)))
           (dolist (candidate (frame-list))
             (if (eq candidate keep)
                 nil
               (delete-frame candidate)))
           nil)))

(fset 'window-frame
      '(lambda (&optional window)
         (if (framep window)
             window
           (selected-frame))))

(fset 'frame-width
      '(lambda (&optional frame)
         (emacs-frame--runtime-ref (emacs-frame--runtime-get frame) 'width)))

(fset 'frame-height
      '(lambda (&optional frame)
         (emacs-frame--runtime-ref (emacs-frame--runtime-get frame) 'height)))

(fset 'frame-char-width '(lambda (&optional frame) 8))
(fset 'frame-char-height '(lambda (&optional frame) 16))

(fset 'frame-pixel-width
      '(lambda (&optional frame)
         (emacs-frame--runtime-ref
          (emacs-frame--runtime-get frame) 'pixel-width)))

(fset 'frame-pixel-height
      '(lambda (&optional frame)
         (emacs-frame--runtime-ref
          (emacs-frame--runtime-get frame) 'pixel-height)))

(fset 'set-frame-size
      '(lambda (frame cols lines &optional pixelwise)
         (let ((target (emacs-frame--runtime-get frame)))
           (emacs-frame--runtime-set target 'width cols)
           (emacs-frame--runtime-set target 'height lines)
           (emacs-frame--runtime-set target 'pixel-width (* cols 8))
           (emacs-frame--runtime-set target 'pixel-height (* lines 16))
           nil)))

(fset 'set-frame-position
      '(lambda (frame x y)
         (let ((target (emacs-frame--runtime-get frame)))
           (emacs-frame--runtime-set target 'left x)
           (emacs-frame--runtime-set target 'top y)
           nil)))

(fset 'frame-parameters
      '(lambda (&optional frame)
         (let ((target (emacs-frame--runtime-get frame)))
           (append
            (list (cons 'width (emacs-frame--runtime-ref target 'width))
                  (cons 'height (emacs-frame--runtime-ref target 'height))
                  (cons 'pixel-width
                        (emacs-frame--runtime-ref target 'pixel-width))
                  (cons 'pixel-height
                        (emacs-frame--runtime-ref target 'pixel-height))
                  (cons 'left (emacs-frame--runtime-ref target 'left))
                  (cons 'top (emacs-frame--runtime-ref target 'top))
                  (cons 'name (emacs-frame--runtime-ref target 'name))
                  (cons 'visibility
                        (emacs-frame--runtime-ref target 'visible)))
            (emacs-frame--runtime-ref target 'parameters)))))

(fset 'frame-parameter
      '(lambda (frame parameter)
         (cdr (assq parameter (frame-parameters frame)))))

(fset 'set-frame-parameter
      '(lambda (frame parameter value)
         (emacs-frame--runtime-apply-parameters
          (emacs-frame--runtime-get frame)
          (list (cons parameter value)))
         value))

(fset 'modify-frame-parameters
      '(lambda (frame alist)
         (emacs-frame--runtime-apply-parameters
          (emacs-frame--runtime-get frame) alist)
         nil))

(fset 'frame-visible-p
      '(lambda (&optional frame)
         (emacs-frame--runtime-ref (emacs-frame--runtime-get frame) 'visible)))

(fset 'make-frame-visible
      '(lambda (&optional frame)
         (let ((target (emacs-frame--runtime-get frame)))
           (emacs-frame--runtime-set target 'visible t)
           target)))

(fset 'make-frame-invisible
      '(lambda (&optional frame force)
         (let ((target (emacs-frame--runtime-get frame)))
           (emacs-frame--runtime-set target 'visible nil)
           target)))

(fset 'raise-frame '(lambda (&optional frame) (emacs-frame--runtime-get frame)))
(fset 'lower-frame '(lambda (&optional frame) (emacs-frame--runtime-get frame)))

(fset 'select-frame
      '(lambda (frame &optional norecord)
           (if (emacs-frame--runtime-live-p frame)
               (setq emacs-frame--runtime-selected-frame frame)
             nil)
         emacs-frame--runtime-selected-frame))

(fset 'frame-focus
      '(lambda (&optional frame)
         (if frame
             (if (eq frame emacs-frame--runtime-selected-frame) frame nil)
           emacs-frame--runtime-selected-frame)))

(fset 'frame-windows '(lambda (&optional frame) nil))
(fset 'display-pixel-width '(lambda (&optional display) 1024))
(fset 'display-pixel-height '(lambda (&optional display) 768))

(setq tab-bar-mode nil)
(setq tab-bar--tabs nil)
(setq tab-bar--selected-index 0)
(setq tab-line-mode nil)
(setq global-tab-line-mode nil)
(setq tab-line-format nil)

(fset 'tab-bar--ensure-tabs
      '(lambda ()
         (if tab-bar--tabs
             nil
           (progn
             (setq tab-bar--tabs
                   (list (list (cons 'name "1")
                               (cons 'explicit-name nil))))
             (setq tab-bar--selected-index 0)))
         tab-bar--tabs))

(fset 'tab-bar-tabs '(lambda (&optional frame) (tab-bar--ensure-tabs)))
(fset 'tab-bar-current-tab
      '(lambda (&optional frame)
         (nth tab-bar--selected-index (tab-bar--ensure-tabs))))
(fset 'tab-bar-current-tab-index
      '(lambda (&optional frame)
         (tab-bar--ensure-tabs)
         tab-bar--selected-index))
(fset 'tab-bar-new-tab
      '(lambda (&optional arg)
         (tab-bar--ensure-tabs)
         (let ((tab (list (cons 'name
                                (number-to-string
                                 (+ (length tab-bar--tabs) 1)))
                          (cons 'explicit-name nil))))
           (setq tab-bar--tabs (append tab-bar--tabs (list tab)))
           (setq tab-bar--selected-index (- (length tab-bar--tabs) 1))
           tab)))
(fset 'tab-bar-select-tab
      '(lambda (tab-number)
         (tab-bar--ensure-tabs)
         (let ((index (- tab-number 1)))
           (if (>= index 0)
               (if (< index (length tab-bar--tabs))
                   (setq tab-bar--selected-index index)
                 nil)
             nil)
           (tab-bar-current-tab))))
(fset 'tab-bar-switch-to-next-tab
      '(lambda (&optional arg)
         (tab-bar--ensure-tabs)
         (let ((count (length tab-bar--tabs))
               (index (+ tab-bar--selected-index (if arg arg 1))))
           (while (< index 0) (setq index (+ index count)))
           (while (>= index count) (setq index (- index count)))
           (setq tab-bar--selected-index index)
           (tab-bar-current-tab))))
(fset 'tab-bar-switch-to-prev-tab
      '(lambda (&optional arg)
         (tab-bar-switch-to-next-tab (- 0 (if arg arg 1)))))
(fset 'tab-bar-close-tab
      '(lambda (&optional tab-number)
         (tab-bar--ensure-tabs)
         (if (<= (length tab-bar--tabs) 1)
             (tab-bar-current-tab)
           (let ((index (if tab-number (- tab-number 1) tab-bar--selected-index))
                 (i 0)
                 (new-tabs nil))
             (dolist (tab tab-bar--tabs)
               (if (= i index)
                   nil
                 (setq new-tabs (append new-tabs (list tab))))
               (setq i (+ i 1)))
             (setq tab-bar--tabs new-tabs)
             (if (>= tab-bar--selected-index (length tab-bar--tabs))
                 (setq tab-bar--selected-index (- (length tab-bar--tabs) 1))
               nil)
             (tab-bar-current-tab)))))
(fset 'tab-bar-rename-tab
      '(lambda (name &optional tab-number)
         (tab-bar--ensure-tabs)
         (let* ((index (if tab-number (- tab-number 1) tab-bar--selected-index))
                (tab (nth index tab-bar--tabs))
                (name-cell (assq 'name tab))
                (explicit-cell (assq 'explicit-name tab)))
           (if name-cell (setcdr name-cell name) nil)
           (if explicit-cell (setcdr explicit-cell t) nil)
           tab)))
(fset 'tab-bar-mode
      '(lambda (&optional arg)
         (setq tab-bar-mode (if arg (> arg 0) (if tab-bar-mode nil t)))
         (tab-bar--ensure-tabs)
         tab-bar-mode))
(fset 'tab-bar-height '(lambda (&optional frame) (if tab-bar-mode 1 0)))
(fset 'tab-new '(lambda (&optional arg) (tab-bar-new-tab arg)))
(fset 'tab-close '(lambda (&optional tab-number)
                    (tab-bar-close-tab tab-number)))
(fset 'tab-next '(lambda (&optional arg) (tab-bar-switch-to-next-tab arg)))
(fset 'tab-previous '(lambda (&optional arg)
                       (tab-bar-switch-to-prev-tab arg)))
(fset 'tab-select '(lambda (tab-number) (tab-bar-select-tab tab-number)))
(fset 'tab-rename '(lambda (name &optional tab-number)
                     (tab-bar-rename-tab name tab-number)))
(fset 'tab-line-mode
      '(lambda (&optional arg)
         (setq tab-line-mode (if arg (> arg 0) (if tab-line-mode nil t)))
         (setq tab-line-format (if tab-line-mode '(:eval (buffer-name)) nil))
         tab-line-mode))
(fset 'global-tab-line-mode
      '(lambda (&optional arg)
         (setq global-tab-line-mode
               (if arg (> arg 0) (if global-tab-line-mode nil t)))
         global-tab-line-mode))
(fset 'window-tab-line-height
      '(lambda (&optional window)
         (if tab-line-mode
             1
           (if global-tab-line-mode 1 0))))
(fset 'tab-line-tabs-buffer-list
      '(lambda () (if (fboundp 'buffer-list) (buffer-list) nil)))
(fset 'tab-line-tabs-window-buffers
      '(lambda () (tab-line-tabs-buffer-list)))
(fset 'tab-line-tabs-fixed-window-buffers
      '(lambda () (tab-line-tabs-buffer-list)))
(fset 'tab-line-tab-name-buffer
      '(lambda (buffer &optional buffers)
         (if (fboundp 'buffer-name) (buffer-name buffer) "")))

(provide 'frame)
(provide 'emacs-frame-builtins)
(provide 'tab-bar)
(provide 'tab-line)

;;; nemacs-runtime-frame-tab-preload.el ends here
