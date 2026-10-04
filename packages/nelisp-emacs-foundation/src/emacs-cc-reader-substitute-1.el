;;; emacs-cc-reader-substitute-1.el --- Resolve circular reader placeholders -*- lexical-binding: t; -*-

(unless (fboundp 'lread--substitute-object-in-subtree)
  (defun lread--substitute-object-in-subtree (object placeholder completed)
    "Replace PLACEHOLDER references inside OBJECT with OBJECT."
    (unless (or (eq completed t) (hash-table-p completed))
      (signal 'wrong-type-argument (list 'hash-table-p completed)))
    (let ((todo (list object)) (seen (make-hash-table :test 'eq)))
      (while todo
        (let ((node (car todo)))
          (setq todo (cdr todo))
          (unless (gethash node seen)
            (puthash node t seen)
            (cond
             ((and (consp node) (not (hash-table-p node)))
              (if (eq (car node) placeholder) (setcar node object)
                (setq todo (cons (car node) todo)))
              (if (eq (cdr node) placeholder) (setcdr node object)
                (setq todo (cons (cdr node) todo))))
             ((vectorp node)
              (let ((i 0))
                (while (< i (length node))
                  (if (eq (aref node i) placeholder) (aset node i object)
                    (setq todo (cons (aref node i) todo)))
                  (setq i (1+ i)))))))))
      nil)))

(provide 'emacs-cc-reader-substitute-1)
;;; emacs-cc-reader-substitute-1.el ends here
