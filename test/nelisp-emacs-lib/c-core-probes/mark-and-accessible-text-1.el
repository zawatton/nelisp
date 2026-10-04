;;; -*- lexical-binding: t; -*-
(mark-marker
 (with-temp-buffer (null (marker-position (mark-marker))))
 (with-temp-buffer
   (insert "hello")
   (set-mark 4)
   (list (mark t) (marker-position (mark-marker))
         (eq (marker-buffer (mark-marker)) (current-buffer))))
 (with-temp-buffer
   (insert "hello")
   (set-mark 4)
   (set-mark nil)
   (list (mark t) (marker-buffer (mark-marker)))))
(buffer-string
 (with-temp-buffer
   (insert "hello world")
   (narrow-to-region 2 6)
   (list (buffer-string) (buffer-size) (point-min) (point-max)))
 (with-temp-buffer
   (insert "hello world")
   (put-text-property 2 6 'sample t)
   (narrow-to-region 2 6)
   (let ((text (buffer-string)))
     (list (substring-no-properties text) (get-text-property 0 'sample text)))))
(substring-no-properties
 (let* ((source (propertize "hello" 'sample t))
        (copy (substring-no-properties source)))
   (list copy (get-text-property 0 'sample copy)
         (get-text-property 0 'sample source) (eq copy source)))
 (let ((source (propertize "hello" 'sample t)))
   (substring-no-properties source -4 -1)))
