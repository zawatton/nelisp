;;; nelisp-stdlib-plist-str.el --- Sweep 9 G4 plist + simple string  -*- lexical-binding: t; -*-

(defun plist-member (plist key)
  (let ((cur plist) (found nil))
    (while (and cur (not found))
      (if (eq (car cur) key)
          (setq found cur)
        (setq cur (cdr (cdr cur)))))
    found))

(defun plist-get (plist key)
  (let ((tail (plist-member plist key)))
    (if tail (car (cdr tail)) nil)))

(defun plist-put (plist key value)
  (let ((tail (plist-member plist key)))
    (if tail
        (progn (setcar (cdr tail) value) plist)
      ;; absent — append "key value" by walking to the end
      (if (null plist)
          (cons key (cons value nil))
        (let ((cur plist))
          (while (cdr (cdr cur))
            (setq cur (cdr (cdr cur))))
          (setcdr (cdr cur) (cons key (cons value nil)))
          plist)))))

(defun string-empty-p (s)
  (= (length s) 0))

;; nelisp-stdlib-plist-str.el ends here
