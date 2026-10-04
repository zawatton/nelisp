;;; delete-region-properties-1.el --- Property ownership after deletion -*- lexical-binding: t; -*-

(delete-region
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 2 6 'sample t)
   (delete-region 2 6)
   (list (buffer-string) (get-text-property 2 'sample)))
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 2 6 'sample t)
   (delete-region 3 5)
   (list (buffer-string) (get-text-property 2 'sample)
         (get-text-property 3 'sample) (get-text-property 4 'sample)))
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 2 6 'sample t)
   (delete-region 5 3)
   (buffer-string))
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 2 6 'sample t)
   (let ((start (copy-marker 3)) (end (copy-marker 5)))
     (delete-region start end)
     (list (buffer-string) (marker-position start) (marker-position end))))
 (with-temp-buffer
   (insert "abcdef")
   (put-text-property 3 5 'read-only t)
   (list (condition-case err (delete-region 2 6) (error (car err)))
         (substring-no-properties (buffer-string))
         (get-text-property 3 'read-only)))
 (with-temp-buffer
   (condition-case err (delete-region nil 1) (error err)))
 (with-temp-buffer
   (condition-case err (delete-region 1.5 1) (error err)))
 (with-temp-buffer
   (condition-case err (delete-region (make-marker) 1) (error err))))
