(file-attributes-lessp
 (file-attributes-lessp (file-attributes "/tmp") (file-attributes "/"))
 (condition-case e (file-attributes-lessp nil nil) (error e)))
(file-name-all-completions
 (let ((dir (make-temp-file "dired1-list" t)))
   (unwind-protect
       (progn (write-region "" nil (expand-file-name "alpha" dir))
              (make-directory (expand-file-name "alpine" dir))
              (file-name-all-completions "al" dir))
     (delete-directory dir t)))
 (let ((dir (make-temp-file "dired1-all" t)))
   (unwind-protect
       (progn (write-region "" nil (expand-file-name "alpha" dir))
              (write-region "" nil (expand-file-name "beta" dir))
              (file-name-all-completions "al" dir))
     (delete-directory dir t)))
 (condition-case e (file-name-all-completions nil "/tmp") (error e)))
(file-name-completion
 (file-name-completion "" "/tmp")
 (let ((dir (make-temp-file "dired1-one" t)))
   (unwind-protect
       (progn (write-region "" nil (expand-file-name "alpha" dir))
              (file-name-completion "al" dir))
     (delete-directory dir t)))
 (let ((dir (make-temp-file "dired1-pred" t)))
   (unwind-protect
       (progn (write-region "" nil (expand-file-name "apple" dir))
              (write-region "" nil (expand-file-name "apricot" dir))
              (file-name-completion "ap" dir (lambda (name) (string-suffix-p "e" name))))
     (delete-directory dir t)))
 (condition-case e (file-name-completion nil "/tmp") (error e)))
(system-groups
 (list (listp (system-groups)) (and (member "root" (system-groups)) t)))
(system-users
 (list (listp (system-users)) (and (member (user-real-login-name) (system-users)) t)))
