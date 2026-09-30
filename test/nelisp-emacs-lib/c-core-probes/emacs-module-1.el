(module-load
 (condition-case e
     (module-load (concat temporary-file-directory "nelisp-module-probe-missing.so"))
   (error e))
 (condition-case e (module-load nil) (error e))
 (let ((file (expand-file-name "nelisp-module-probe-invalid.so"
                               temporary-file-directory)))
   (unwind-protect
       (progn
         (with-temp-file file (insert (make-string 128 ?x)))
         (condition-case e (module-load file) (error e)))
     (delete-file file)))
 (let ((file (expand-file-name "nelisp-module-probe-state.so"
                               temporary-file-directory)))
   (unwind-protect
       (progn
         (with-temp-file file (insert (make-string 128 ?x)))
         (list (file-exists-p file)
               (condition-case e (module-load file) (error (car e)))))
     (delete-file file))))
