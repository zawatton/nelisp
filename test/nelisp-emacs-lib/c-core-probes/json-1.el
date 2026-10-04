(json-insert
 (with-temp-buffer
   (list (json-insert nil) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert nil :null-object nil) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert nil :false-object nil) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert nil :null-object nil :false-object nil)
         (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert [1 2]) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert '(:a 1)) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert '((a . 1))) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert ["line\n\"\\" "雪☃"]) (buffer-string) (point)))
 (with-temp-buffer
   (list (json-insert :custom :null-object :custom) (buffer-string)))
 (with-temp-buffer
   (list (json-insert :custom :false-object :custom) (buffer-string)))
 (with-temp-buffer
   (insert "keep")
   (goto-char 3)
   (let ((result (condition-case error-data
                     (json-insert [1] :unknown-option t)
                   (error (list (car error-data) (cadr error-data))))))
     (list result (buffer-string) (point))))
 (with-temp-buffer
   (insert "keep")
   (goto-char 3)
   (let ((result (condition-case error-data
                     (json-insert [1] :null-object)
                   (error (list (car error-data) (cadr error-data))))))
     (list result (buffer-string) (point))))
)
