;;; emacs-cc-keymap-internal-1.el --- GNU keymap C primitive fallbacks -*- lexical-binding: t; -*-

(unless (fboundp 'keymap--get-keyelt)
  (defun keymap--get-keyelt (&rest arguments)
    "Return the effective definition in keymap slot OBJECT.
AUTOLOAD is accepted for the GNU primitive's calling convention."
    (unless (= (length arguments) 2)
      (signal 'wrong-number-of-arguments
              (list 'keymap--get-keyelt (length arguments))))
    (let ((object (car arguments)))
      (while (and (consp object) (stringp (car object)))
        (setq object (cdr object)))
      (if (and (consp object) (eq (car object) 'menu-item))
          (nth 2 object)
        object))))

(defun emacs-keymap--accessible-map (object)
  "Resolve OBJECT as a prefix without loading a keymap autoload."
  (let ((definition (or (indirect-function object) object)))
    (cond ((emacs-keymap-keymapp definition) definition)
          ((and (symbolp object) (eq (car-safe definition) 'autoload)
                (eq (nth 4 definition) 'keymap))
           object))))

(defun emacs-keymap--accessible-keymaps (keymap &optional prefix)
  "Return prefix keymaps reachable from KEYMAP in breadth-first order."
  (let* ((start (if prefix
                    (emacs-keymap--accessible-map (lookup-key keymap prefix t))
                  (or (emacs-keymap--accessible-map keymap)
                      (signal 'wrong-type-argument (list 'keymapp keymap)))))
         (keys (cond ((null prefix) [])
                     ((vectorp prefix) prefix)
                     (t (vconcat (emacs-keymap-builtins--description-events prefix)))))
         (depth (length keys))
         (cursor (and start (list (list keys start (list start)))))
         (end cursor)
         result)
    (while cursor
      (let* ((node (car cursor))
             (path (car node))
             (map (cadr node))
             (ancestors (nth 2 node))
             (size (length path))
             (metize (and (> size depth)
                          (eq (aref path (1- size)) meta-prefix-char))))
        (push (cons path map) result)
        (when (consp map)
          (map-keymap
           (lambda (event raw)
             (let* ((binding (if (consp raw)
                                 (emacs-keymap-builtins--key-definition raw)
                               raw))
                    (child (emacs-keymap--accessible-map binding)))
               (when (and child (not (memq child ancestors)))
                 (let* ((meta (and metize (integerp event)))
                        (keys (if meta
                                  (emacs-keymap-builtins--where-is-append path event)
                                (vconcat path (vector event))))
                        ;; A metized edge replaces the last event, so the
                        ;; unmodified ESC path is no longer an ancestor.
                        (node (list keys child
                                    (cons child (if meta (cdr ancestors) ancestors)))))
                   (if (and meta (cdr cursor))
                       (setcdr cursor (cons node (cdr cursor)))
                     (setcdr end (list node))
                     (setq end (cdr end)))))))
           map)))
      (setq cursor (cdr cursor)))
    (nreverse result)))

(when (emacs-keymap-builtins--install-function-p 'accessible-keymaps)
  (defalias 'accessible-keymaps #'emacs-keymap--accessible-keymaps))

(provide 'emacs-cc-keymap-internal-1)
;;; emacs-cc-keymap-internal-1.el ends here
