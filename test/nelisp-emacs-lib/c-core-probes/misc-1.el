(treesit-parser-tracking-line-column-p
 (condition-case e (treesit-parser-tracking-line-column-p nil) (error e))
 (condition-case e (treesit-parser-tracking-line-column-p t) (error e))
 (condition-case e (with-temp-buffer
                     (insert "line one\nline two")
                     (treesit-parser-tracking-line-column-p nil))
   (error e)))

(treesit-tracking-line-column-p
 (treesit-tracking-line-column-p)
 (condition-case e (treesit-tracking-line-column-p t) (error e))
 (with-temp-buffer
   (insert "changed\ntext")
   (put-text-property (point-min) (point-max) 'face 'bold)
   (list (treesit-tracking-line-column-p (current-buffer))
         (with-temp-buffer (treesit-tracking-line-column-p)))))
