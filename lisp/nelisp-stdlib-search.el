;;; nelisp-stdlib-search.el --- Sweep 9 G3 list search  -*- lexical-binding: t; -*-

(defun memq (elt list)
  (let ((found nil))
    (while (and list (not found))
      (if (eq elt (car list))
          (setq found list)
        (setq list (cdr list))))
    found))

(defun member (elt list)
  (let ((found nil))
    (while (and list (not found))
      (if (equal elt (car list))
          (setq found list)
        (setq list (cdr list))))
    found))

(defun assq (key alist)
  (let ((found nil))
    (while (and alist (not found))
      (let ((pair (car alist)))
        (if (and (consp pair) (eq (car pair) key))
            (setq found pair)
          (setq alist (cdr alist)))))
    found))

(defun assoc (key alist)
  (let ((found nil))
    (while (and alist (not found))
      (let ((pair (car alist)))
        (if (and (consp pair) (equal (car pair) key))
            (setq found pair)
          (setq alist (cdr alist)))))
    found))

;; nelisp-stdlib-search.el ends here
