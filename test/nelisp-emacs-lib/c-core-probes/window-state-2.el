;;; window-state-2.el --- Additional window-state semantics -*- lexical-binding: t; -*-

(set-window-dedicated-p
 (set-window-dedicated-p nil 'weak)
 (let ((window (selected-window)))
   (unwind-protect
       (progn (set-window-dedicated-p window 'weak)
              (window-dedicated-p window))
     (set-window-dedicated-p window nil))))

(set-window-buffer
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-hard-target*")))
     (unwind-protect
         (progn
           (set-window-dedicated-p window t)
           (condition-case err
               (progn (set-window-buffer window other) 'allowed)
             (error (list (car err) (cadr err)))))
       (set-window-dedicated-p window nil))))
 (save-window-excursion
   (let* ((window (selected-window))
          (buffer (window-buffer window)))
     (unwind-protect
         (progn
           (set-window-dedicated-p window t)
           (set-window-buffer window buffer)
           (list (eq (window-buffer window) buffer)
                 (window-dedicated-p window)))
       (set-window-dedicated-p window nil))))
 (save-window-excursion
 (let* ((window (selected-window))
          (other (get-buffer-create "*window-weak-target*")))
     (unwind-protect
         (progn
           (set-window-dedicated-p window 'weak)
           (set-window-buffer window other)
           (list (eq (window-buffer window) other)
                 (window-dedicated-p window)))
       (set-window-dedicated-p window nil))))
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-foo-target*")))
     (unwind-protect
         (progn
           (set-window-dedicated-p window 'foo)
           (set-window-buffer window other)
           (list (eq (window-buffer window) other)
                 (window-dedicated-p window)))
       (set-window-dedicated-p window nil))))
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-quote-target*"))
          (saved-style text-quoting-style))
     (unwind-protect
         (progn
           (setq text-quoting-style nil)
           (set-window-dedicated-p window t)
           (condition-case err (set-window-buffer window other) (error err)))
       (set-window-dedicated-p window nil)
       (setq text-quoting-style saved-style))))
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-quote-target*"))
          (saved-style text-quoting-style))
     (unwind-protect
         (progn
           (setq text-quoting-style 'grave)
           (set-window-dedicated-p window t)
           (condition-case err (set-window-buffer window other) (error err)))
       (set-window-dedicated-p window nil)
       (setq text-quoting-style saved-style))))
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-quote-target*"))
          (saved-style text-quoting-style))
     (unwind-protect
         (progn
           (setq text-quoting-style 'straight)
           (set-window-dedicated-p window t)
           (condition-case err (set-window-buffer window other) (error err)))
       (set-window-dedicated-p window nil)
       (setq text-quoting-style saved-style))))
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-quote-target*"))
          (saved-style text-quoting-style))
     (unwind-protect
         (progn
           (setq text-quoting-style 'curve)
           (set-window-dedicated-p window t)
           (condition-case err (set-window-buffer window other) (error err)))
       (set-window-dedicated-p window nil)
       (setq text-quoting-style saved-style))))
 (save-window-excursion
   (let* ((window (selected-window))
          (other (get-buffer-create "*window-quote-target*"))
          (saved-style text-quoting-style))
     (unwind-protect
         (progn
           (setq text-quoting-style 'invalid)
           (set-window-dedicated-p window t)
           (condition-case err (set-window-buffer window other) (error err)))
       (set-window-dedicated-p window nil)
       (setq text-quoting-style saved-style)))))

(window-minibuffer-p
 (let* ((window (selected-window))
        (count-before (if (fboundp 'emacs-window--all-leaves)
                          (length (emacs-window--all-leaves))
                        (length (window-list))))
        (result (window-minibuffer-p window))
        (count-after (if (fboundp 'emacs-window--all-leaves)
                         (length (emacs-window--all-leaves))
                       (length (window-list)))))
   (list result (= count-before count-after)))
 (window-minibuffer-p (selected-window)))

(minibuffer-window
 (let* ((selected (selected-window))
        (before-geometry (list (window-width selected)
                               (window-height selected)))
        (count-before (if (fboundp 'emacs-window--all-leaves)
                          (length (emacs-window--all-leaves))
                        (length (window-list))))
        (mini (minibuffer-window))
        (count-after (if (fboundp 'emacs-window--all-leaves)
                         (length (emacs-window--all-leaves))
                       (length (window-list))))
        (after-geometry (list (window-width selected)
                              (window-height selected))))
   (list count-before count-after
         (equal before-geometry after-geometry)
         (eq mini (minibuffer-window))
         (eq (selected-frame) (window-frame mini))
         (not (memq mini (if (fboundp 'emacs-window--all-leaves)
                             (emacs-window--all-leaves)
                           (window-list))))
         (length (window-list nil t))))
 (eq (minibuffer-window) (minibuffer-window (selected-frame)))
 (null (window-parent (minibuffer-window))))

(set-window-display-table
 (let ((table (make-display-table)))
   (eq (set-window-display-table nil table) table))
 (set-window-display-table nil nil))

(scroll-left
 (progn (set-window-hscroll nil 0) (scroll-left))
 (progn (set-window-hscroll nil 0) (scroll-left 0)))

(scroll-right
 (progn (set-window-hscroll nil 200) (scroll-right))
 (progn (set-window-hscroll nil 200) (scroll-right nil)))

(window-combination-limit
 (condition-case e (window-combination-limit nil) (error e))
 (condition-case e (window-combination-limit (selected-window)) (error e))
 (save-window-excursion
   (split-window-right)
   (window-combination-limit (window-parent (selected-window))))
 (save-window-excursion
   (split-window-right)
   (let ((parent (window-parent (selected-window))))
     (set-window-combination-limit parent 'weak)
     (window-combination-limit parent))))

(set-window-combination-limit
 (condition-case e (set-window-combination-limit nil 'weak) (error e))
 (condition-case e (set-window-combination-limit (selected-window) t) (error e))
 (save-window-excursion
   (split-window-right)
   (set-window-combination-limit (window-parent (selected-window)) 'weak))
 (save-window-excursion
   (split-window-right)
   (let ((parent (window-parent (selected-window))))
     (set-window-combination-limit parent 'weak)
     (window-combination-limit parent))))
