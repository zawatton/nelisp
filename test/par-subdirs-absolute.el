;;; par-subdirs-absolute.el --- ordered local startup search paths -*- lexical-binding: t; -*-

(let* ((root (make-temp-file "par-subdirs-" t))
       (default-directory (file-name-as-directory root))
       (advised-calls 0)
       (original (symbol-function 'normal-top-level-add-to-load-path)))
  (unwind-protect
      (progn
        (dolist (name '("a/b" "z" ".hidden" "CVS" "skip/child"))
          (make-directory name t))
        (write-region "" nil "skip/.nosearch" nil 'silent)
        (dolist (mode '(default replaced))
          (let ((load-path (list root "/keep"))
                (normal-top-level-add-subdirs-inode-list nil))
            (when (eq mode 'replaced)
              (fset 'normal-top-level-add-to-load-path
                    (lambda (dirs)
                      (setq advised-calls (1+ advised-calls))
                      (funcall original dirs))))
            (normal-top-level-add-subdirs-to-load-path)
            (prin1 (list mode
                         (mapcar (lambda (dir)
                                   (if (string-prefix-p root dir)
                                       (substring dir (length root)) dir))
                                 load-path)
                         advised-calls))
            (terpri))))
    (fset 'normal-top-level-add-to-load-path original)
    (delete-directory root t)))
