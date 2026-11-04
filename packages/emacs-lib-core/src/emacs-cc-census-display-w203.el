;;; emacs-cc-census-display-w203.el --- Window configuration and dialog primitives  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-display-w203--same-tree-p (left right)
  "Compare the layout fields represented by window nodes LEFT and RIGHT."
  (if (or (null left) (null right))
      (eq left right)
    (and (eq (emacs-window-id left) (emacs-window-id right))
         (eq (emacs-window-leaf-p left) (emacs-window-leaf-p right))
         (eq (emacs-window-buffer left) (emacs-window-buffer right))
         (= (emacs-window-total-cols left) (emacs-window-total-cols right))
         (= (emacs-window-total-lines left) (emacs-window-total-lines right))
         (eq (emacs-window-direction left) (emacs-window-direction right))
         (eq (cdr (assq 'display-table (emacs-window-parameters left)))
             (cdr (assq 'display-table (emacs-window-parameters right))))
         (let ((left-children (emacs-window-children left))
               (right-children (emacs-window-children right))
               (same t))
           (while (and same left-children right-children)
             (setq same (emacs-cc-census-display-w203--same-tree-p
                         (car left-children) (car right-children))
                   left-children (cdr left-children)
                   right-children (cdr right-children)))
           (and same (null left-children) (null right-children))))))

(unless (fboundp 'window-configuration-equal-p)
  (defun window-configuration-equal-p (x y)
    "Return t if configurations X and Y have the same window layout.
Point and scrolling positions do not affect the comparison."
    (unless (window-configuration-p x)
      (signal 'wrong-type-argument (list 'window-configuration-p x)))
    (unless (window-configuration-p y)
      (signal 'wrong-type-argument (list 'window-configuration-p y)))
    ;; Snapshots in the window model contain a copied tree and selected ID.
    ;; Current-buffer and frame metadata are not represented by that model.
    (and (eq (emacs-window-configuration-selected x)
             (emacs-window-configuration-selected y))
         (emacs-cc-census-display-w203--same-tree-p
          (emacs-window-configuration-root x)
          (emacs-window-configuration-root y)))))

(defun emacs-cc-census-display-w203--dialog-frame (position)
  "Validate dialog POSITION and return its frame."
  (let ((target
         (cond ((eq position t) (selected-frame))
               ((or (framep position) (windowp position)) position)
               ((consp position)
                (if (consp (car position))
                    (car (cdr position))
                  (car (car (cdr position))))))))
    (if (framep target)
        target
      (unless (windowp target)
        (signal 'wrong-type-argument (list 'windowp target)))
      (window-frame target))))

(unless (fboundp 'x-popup-dialog)
  (defun x-popup-dialog (position contents &optional header)
    "Validate and display a dialog on the frame specified by POSITION.
CONTENTS is a title followed by dialog items.  HEADER selects the title
style.  On a terminal without a dialog backend, return nil."
    (emacs-cc-census-display-w203--dialog-frame position)
    (let ((title (car contents))
          (items (cdr contents)))
      (unless (stringp title)
        (signal 'wrong-type-argument (list 'stringp title)))
      (unless (consp items)
        (signal 'wrong-type-argument (list 'consp items)))
      (while (consp items)
        (let ((item (car items)))
          (when (and (consp item) (not (stringp (car item))))
            (signal 'wrong-type-argument (list 'stringp (car item)))))
        (setq items (cdr items)))
      ;; The standalone window model has no graphical dialog backend.
      (ignore header)
      nil)))

(provide 'emacs-cc-census-display-w203)
;;; emacs-cc-census-display-w203.el ends here
