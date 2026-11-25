;;; nemacs-runtime-frame-tab-preload.el --- frame/tab runtime image cells -*- lexical-binding: t; -*-

;;; Code:

(dolist (symbol '(defface suppress-keymap make-mode-line-mouse-map
                          define-derived-mode easy-menu-binding
                          substitute-command-keys))
  (put symbol 'emacs-stub-bulk t))

(unless (fboundp 'defface)
  (defmacro defface (name spec doc &rest opts)
    "Early runtime-image fallback for top-level face declarations."
    `(progn
       (if (fboundp 'custom-declare-face)
           (custom-declare-face ',name ,spec ,doc ,@opts)
         (put ',name 'face-defface-spec ,spec))
       ',name)))

(unless (fboundp 'suppress-keymap)
  (fset 'suppress-keymap
        '(lambda (keymap &optional _nodigits)
           keymap)))

(unless (fboundp 'make-mode-line-mouse-map)
  (fset 'make-mode-line-mouse-map
        '(lambda (mouse function)
           (let ((map (if (fboundp 'make-sparse-keymap)
                          (make-sparse-keymap)
                        (list 'keymap))))
             (if (fboundp 'define-key)
                 (define-key map (vector 'mode-line mouse) function)
               (setcdr map (cons (cons (vector 'mode-line mouse) function)
                                 (cdr map))))
             map))))

(unless (fboundp 'define-derived-mode)
  (defmacro define-derived-mode (child parent name &optional doc &rest body)
    "Early runtime-image fallback for calendar/text-mode top-level modes."
    (if (fboundp 'emacs-mode-define-derived-mode)
        (append (list 'emacs-mode-define-derived-mode child parent name doc)
                body)
      `(progn
         (defun ,child ()
           ,doc
           (interactive)
           ,@body)
         (put ',child 'derived-mode-parent ',parent)
         ',child))))

(unless (fboundp 'easy-menu-binding)
  (defmacro easy-menu-binding (&rest _maps)
    "Early runtime-image fallback for menu probes."
    nil))

(unless (fboundp 'substitute-command-keys)
  (fset 'substitute-command-keys
        '(lambda (string &optional _no-face)
           string)))

(unless (fboundp 'assq)  ; native in current NeLisp; never shadow it
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
             found))))

(unless (fboundp 'nth)  ; native in current NeLisp; never shadow it
  (fset 'nth
        '(lambda (index list)
           (let ((tail list)
                 (i index))
             (while (> i 0)
               (setq tail (cdr tail))
               (setq i (- i 1)))
             (car tail)))))

;; Variadic `append': earlier args are copied, the final arg is the tail
;; (used as-is).  The previous 2-arg form silently dropped every argument
;; past the second, which broke backquote splicing `(a ,x ,@b)' -> the macro
;; expander emits `(append (list 'a) (list x) b)' (3 args), so `,@b' vanished;
;; that in turn broke `with-current-buffer' (its `(progn (set-buffer ..) ,@body)'
;; lost its body) and any cl-macro built on backquote.
(unless (fboundp 'append)  ; native in current NeLisp; never shadow it
  (fset 'append
        '(lambda (&rest lists)
           (cond
            ((null lists) nil)
            ((null (cdr lists)) (car lists))
            (t
             (let* ((rev (reverse lists))
                    (result (car rev))    ; last list = tail, used as-is
                    (earlier (cdr rev)))  ; remaining lists, last-to-first
               (while earlier
                 ;; Collect this arg's elements (reversed) into ACC.  Non-final
                 ;; args may be a list, vector, or string -- match the core
                 ;; `append', which converts any sequence (this is why
                 ;; `(append VECTOR nil)' / `vconcat' work).
                 (let ((acc nil) (cur (car earlier)))
                   (cond
                    ((consp cur)
                     (while (consp cur)
                       (setq acc (cons (car cur) acc))
                       (setq cur (cdr cur))))
                    ((or (vectorp cur) (stringp cur))
                     (let ((i 0) (n (length cur)))
                       (while (< i n)
                         (setq acc (cons (aref cur i) acc))
                         (setq i (1+ i))))))
                   (while acc
                     (setq result (cons (car acc) result))
                     (setq acc (cdr acc))))
                 (setq earlier (cdr earlier)))
               result))))))

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
         ;; A frame object is an alist; other cons objects (e.g. the
         ;; `(emacs-window ...)' window list `framep' sees via `window-frame')
         ;; have non-cons elements, on which this runtime's `assq' signals
         ;; (wrong-type-argument listp SYMBOL) instead of skipping them.
         (if (consp object)
             (if (consp (car object))
                 (if (assq 'nemacs-frame object) t nil)
               nil)
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

(fset 'window-system
      '(lambda (&optional frame)
         ;; The synthetic batch frame is a terminal for Lisp callers.
         (let* ((target (or frame (selected-frame)))
                (type (framep target)))
           (unless type
             (signal 'wrong-type-argument (list 'framep target)))
           (unless (memq type '(t stub)) type))))

;; Core's `nelisp--check-live-frame' (nelisp-stdlib-prelude.el) is the single
;; chokepoint behind `internal-lisp-face-p', `internal-make-lisp-face',
;; `internal-copy-lisp-face', `internal-get/set-lisp-face-attribute',
;; `internal-lisp-face-empty-p', `internal-merge-in-global-face',
;; and `frame-parameter'/`set-frame-parameter': it
;; unconditionally signals `wrong-type-argument' for ANY non-nil FRAME, on
;; the documented assumption that "this runtime never creates a frame" (so
;; `frame-list' is always `(nil)' and no FRAME argument can ever be live).
;; That assumption no longer holds once this file installs a real frame
;; registry above (`frame-list'/`frame-live-p'/`make-frame' etc. now return
;; genuine, non-nil frame objects), so every vendor `make-face' call's
;; `(dolist (frame (frame-list)) (internal-make-lisp-face face frame))'
;; aborts on its first iteration -- observed via the magit/transient bundle's
;; `defface' forms (S5.4).  All of the functions above use a single
;; global-only store regardless of which particular frame is passed (their
;; own comments say so: "F is always NULL in this runtime's terms"), so
;; accepting a genuinely live frame here is safe: it degrades to exactly the
;; same global-store behavior core already gives a nil FRAME.  Override
;; unconditionally (core's own `nelisp--check-live-frame' is not
;; `unless fboundp'-guarded), consulting the real `frame-live-p' just
;; defined above instead of unconditionally rejecting every FRAME.
(fset 'nelisp--check-live-frame
      '(lambda (frame)
         (unless (frame-live-p frame)
           (signal 'wrong-type-argument (list 'frame-live-p frame)))))

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

(provide 'frame)
(provide 'emacs-frame-builtins)

;;; nemacs-runtime-frame-tab-preload.el ends here
