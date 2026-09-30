(access-file
 (let ((f (make-temp-file "fileio-1"))) (unwind-protect (progn (access-file f "probe") t) (delete-file f)))
 (condition-case e (access-file nil "probe") (error e)))
(clear-buffer-auto-save-failure
 (clear-buffer-auto-save-failure)
 (with-temp-buffer (setq-local auto-save-failure t) (clear-buffer-auto-save-failure) auto-save-failure))
(delete-directory-internal
 (let ((d (make-temp-file "fileio-1" t))) (delete-directory-internal d) (file-exists-p d))
 (condition-case e (delete-directory-internal nil) (error e)))
(delete-file-internal
 (let ((f (make-temp-file "fileio-1"))) (delete-file-internal f) (file-exists-p f))
 (condition-case e (delete-file-internal nil) (error e)))
(directory-name-p
 (list (directory-name-p "folder/") (directory-name-p "folder"))
 (condition-case e (directory-name-p nil) (error e)))
(do-auto-save
 (do-auto-save t t)
 (with-temp-buffer (insert "changed") (do-auto-save t t)))
(file-acl
 (list (null (file-acl "/definitely/missing-fileio-1")) (listp (file-acl "/tmp")))
 (condition-case e (file-acl nil) (error e)))
(file-selinux-context
 (file-selinux-context "/definitely/missing-fileio-1")
 (condition-case e (file-selinux-context nil) (error e)))
(file-system-info
 (file-system-info "/definitely/missing-fileio-1")
 (condition-case e (file-system-info nil) (error e)))
(make-directory-internal
 (let ((d (make-temp-name (expand-file-name "fileio-1" temporary-file-directory)))) (unwind-protect (progn (make-directory-internal d) (file-directory-p d)) (when (file-directory-p d) (delete-directory d))))
 (condition-case e (make-directory-internal nil) (error e)))
(make-temp-file-internal
 (let ((f (make-temp-file-internal "fileio-1" nil ".txt" "abc"))) (unwind-protect (list (file-exists-p f) (= (nth 7 (file-attributes f)) 3)) (delete-file f)))
 (let ((d (make-temp-file-internal "fileio-1" t "" nil))) (unwind-protect (file-directory-p d) (delete-directory d))))
(next-read-file-uses-dialog-p
 (next-read-file-uses-dialog-p)
 (list (windowp (selected-window)) (next-read-file-uses-dialog-p)))
