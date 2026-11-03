;;; eval-region-bounds-1.el --- Region endpoints and visible read movement -*- lexical-binding: t; -*-

(eval-region
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (list (eval-region 0 8) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (list (eval-region -4 8) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (list (eval-region 1 nil) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (list (eval-region 8 1) (eval-region 200 1) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 1)
   (list (eval-region nil nil) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (list (condition-case err (eval-region nil 8) (error (car err))) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (list (condition-case err (eval-region 1 0) (error (cdr err)))
         (condition-case err (eval-region 1 200) (error (cdr err)))
         (point)))
 (with-temp-buffer
   (insert "xx (+ 1 2) yy")
   (narrow-to-region 4 11)
   (goto-char 6)
   (list (eval-region 0 11) (point)))
 (with-temp-buffer
   (insert "(+ 1")
   (goto-char 1)
   (list (condition-case err (eval-region nil nil) (error (car err))) (point)))
 (with-temp-buffer
   (insert "(+ 1 2)")
   (goto-char 3)
   (let ((start (copy-marker 1)) (end (copy-marker 8)))
     (unwind-protect (list (eval-region start end) (point))
       (set-marker start nil) (set-marker end nil)))))
