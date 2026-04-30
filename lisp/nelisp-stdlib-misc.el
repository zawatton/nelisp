;;; nelisp-stdlib-misc.el --- Sweep 10 misc builtins  -*- lexical-binding: t; -*-

(defun list (&rest args) args)

(defun alist-get (key alist &optional default _remove testfn)
  (let ((cur alist) (found nil) (result default))
    (while (and cur (not found))
      (let ((pair (car cur)))
        (cond
         ((not (consp pair)) (setq cur (cdr cur)))
         ((cond
           ((null testfn) (equal (car pair) key))
           ((eq testfn 'eq) (eq (car pair) key))
           ((eq testfn 'equal) (equal (car pair) key))
           ((or (eq testfn 'string=) (eq testfn 'string-equal))
            (and (stringp (car pair)) (stringp key) (equal (car pair) key)))
           (t (funcall testfn (car pair) key)))
          (let ((tail (cdr pair)))
            (setq result (if (consp tail) (car tail) tail)))
          (setq found t))
         (t (setq cur (cdr cur))))))
    result))

(defun string-prefix-p (prefix string &optional _ignore-case)
  (let ((plen (length prefix)) (slen (length string)))
    (if (> plen slen)
        nil
      (string= prefix (substring string 0 plen)))))

(defun number-to-string (n)
  (if (integerp n) (format "%d" n) (format "%g" n)))

;; nelisp-stdlib-misc.el ends here
