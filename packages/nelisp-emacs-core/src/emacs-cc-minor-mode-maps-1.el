;;; emacs-cc-minor-mode-maps-1.el --- Active minor maps -*- lexical-binding: t; -*-

(defun emacs-cc-minor-mode-maps-1--resolve (map)
  "Resolve a keymap symbol's function cell, leaving direct MAPs intact."
  (let (seen)
    (while (and (symbolp map) map (fboundp map)
                (not (memq map seen)))
      (push map seen)
      (setq map (symbol-function map)))
    (and (not (symbolp map)) map)))

(defun emacs-cc-minor-mode-maps-1--active (entry)
  "Return ENTRY's map when its minor-mode variable is active."
  (when (and (consp entry) (symbolp (car entry))
             (boundp (car entry)) (symbol-value (car entry)))
    (emacs-cc-minor-mode-maps-1--resolve (cdr entry))))

(unless (fboundp 'current-minor-mode-maps)
  (defun current-minor-mode-maps (&rest arguments)
    "Return active minor-mode maps in precedence order."
    (when arguments
      (signal 'wrong-number-of-arguments
              (list 'current-minor-mode-maps (length arguments))))
    (let (maps)
      (dolist (entry minor-mode-overriding-map-alist)
        (let ((map (emacs-cc-minor-mode-maps-1--active entry)))
          (when map (push map maps))))
      (dolist (entry minor-mode-map-alist)
        (when (and (consp entry)
                   (not (assq (car entry)
                              minor-mode-overriding-map-alist)))
          (let ((map (emacs-cc-minor-mode-maps-1--active entry)))
            (when map (push map maps)))))
      (nreverse maps))))

(defun emacs-cc-minor-mode-maps-1--entries ()
  "Return active (MODE . MAP) entries in minor-mode precedence order."
  (let (entries)
    (dolist (entry minor-mode-overriding-map-alist)
      (let ((map (emacs-cc-minor-mode-maps-1--active entry)))
        (when map (push (cons (car entry) map) entries))))
    (dolist (entry minor-mode-map-alist)
      (when (and (consp entry)
                 (not (assq (car entry)
                            minor-mode-overriding-map-alist)))
        (let ((map (emacs-cc-minor-mode-maps-1--active entry)))
          (when map (push (cons (car entry) map) entries)))))
    (nreverse entries)))

(unless (fboundp 'minor-mode-key-binding)
  (defun minor-mode-key-binding (&rest arguments)
    "Return visible minor-mode bindings for KEY."
    (let ((count (length arguments)))
      (unless (<= 1 count 2)
        (signal 'wrong-number-of-arguments
                (list 'minor-mode-key-binding count))))
    (let ((key (car arguments))
          (accept-default (cadr arguments))
          (entries (emacs-cc-minor-mode-maps-1--entries))
          (prefix-only nil)
          (done nil)
          (bindings nil))
      (while (and entries (not done))
        (let* ((entry (car entries))
               (binding (lookup-key (cdr entry) key accept-default)))
          (when (and binding (not (integerp binding))
                     (or (not prefix-only) (keymapp binding)))
            (push (cons (car entry) binding) bindings)
            (if (keymapp binding)
                (setq prefix-only t)
              (setq done t))))
        (setq entries (cdr entries)))
      (nreverse bindings))))

(provide 'emacs-cc-minor-mode-maps-1)
;;; emacs-cc-minor-mode-maps-1.el ends here
