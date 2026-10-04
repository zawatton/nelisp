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

(unless (fboundp 'map-keymap-internal)
  (defun map-keymap-internal (function keymap)
    "Call FUNCTION for each direct binding in KEYMAP and return its parent."
    (unless (emacs-keymap-keymapp keymap)
      (signal 'wrong-type-argument (list 'keymapp keymap)))
    (dolist (entry (cdr keymap))
      (cond
       ((emacs-char-table-p entry)
        (let ((vec (emacs-char-table-ascii-vector entry)))
          (dotimes (index (length vec))
            (let ((binding (aref vec index)))
              (when binding (funcall function index binding))))))
       ((and (consp entry) (eq (car entry) t) (vectorp (cdr entry)))
        (let ((vec (cdr entry)))
          (dotimes (index (length vec))
            (let ((binding (aref vec index)))
              (when binding (funcall function index binding))))))
       ((and (consp entry) (eq (car entry) :emacs-keymap-parent)) nil)
       ((and (consp entry) (not (stringp entry)))
        (funcall function (car entry) (cdr entry)))))
    (emacs-keymap-keymap-parent keymap)))

(unless (fboundp 'accessible-keymaps)
  (defun accessible-keymaps (&rest arguments)
    "Return prefix keymaps reachable from KEYMAP in breadth-first order."
    (let ((count (length arguments)))
      (unless (<= 1 count 2)
        (signal 'wrong-number-of-arguments (list 'accessible-keymaps count))))
    (let* ((keymap (car arguments))
           (prefix (cadr arguments))
           (prefix-vector nil)
           (queue (list (list [] keymap (list keymap))))
           (result nil))
      (unless (emacs-keymap-keymapp keymap)
        (signal 'wrong-type-argument (list 'keymapp keymap)))
      (when (and prefix (not (or (vectorp prefix) (stringp prefix))))
        (signal 'wrong-type-argument
                (list (if (listp prefix) 'arrayp 'sequencep) prefix)))
      (setq prefix-vector (and prefix (vconcat prefix)))
      (while queue
        (let* ((item (car queue))
               (keys (car item))
               (map (cadr item))
               (ancestors (caddr item))
               (seen nil))
          (setq queue (cdr queue))
          (when (or (null prefix-vector)
                    (and (>= (length keys) (length prefix-vector))
                         (equal (substring keys 0 (length prefix-vector))
                                prefix-vector)))
            (push (cons keys map) result))
          (map-keymap
           (lambda (event binding)
             (unless (member event seen)
               (push event seen)
               (when (and (keymapp binding)
                          (not (memq binding ancestors)))
                 (setq queue
                       (append queue
                               (list (list (vconcat keys (vector event))
                                           binding
                                           (cons binding ancestors))))))))
           map)))
      (nreverse result))))

(provide 'emacs-cc-keymap-internal-1)
;;; emacs-cc-keymap-internal-1.el ends here
