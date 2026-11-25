;;; -*- lexical-binding: t; -*-

(defun par-init-file-case-handler (operation filename)
  (let ((default-directory (file-name-directory filename)))
    (list operation (file-name-nondirectory filename))))

(let* ((root (make-temp-file (expand-file-name "par-case-" default-directory) t))
       (default-directory (file-name-as-directory root))
       (upper (expand-file-name "Mixed"))
       (lower (expand-file-name "mIXED")))
  (unwind-protect
      (progn
        (write-region "test" nil upper nil 'silent)
        ;; Two names for one inode do not imply a caseless filesystem.
        (add-name-to-file upper lower)
        (prin1 (list 'local (file-name-case-insensitive-p "Mixed")
                     (file-name-case-insensitive-p "missing")
                     (file-name-nondirectory (file-truename "Mixed"))))
        (terpri)
        (let ((file-name-handler-alist
               '(("/virtual-case\\'" . par-init-file-case-handler))))
          (put 'par-init-file-case-handler 'operations
               '(file-name-case-insensitive-p))
          (prin1 (list 'handler (file-name-case-insensitive-p "virtual-case")))
          (terpri)
          (let ((inhibit-file-name-operation 'file-name-case-insensitive-p)
                (inhibit-file-name-handlers '(par-init-file-case-handler)))
            (prin1 (list 'inhibited
                         (file-name-case-insensitive-p "virtual-case")))
            (terpri))))
    (delete-directory root t)))
