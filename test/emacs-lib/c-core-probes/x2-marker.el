;;; GNU comparison: arithmetic and text positions accept positioned markers.
(+
 (with-temp-buffer
   (insert "abcdef")
   (let ((m (copy-marker 2)))
     (mapcar (lambda (op)
               (list op (funcall op m 2) (funcall op 2 m)))
             '(+ - * / < > = <= >= max min))))
 (with-temp-buffer
   (insert "abcdef")
   (let ((m (copy-marker 2)))
     (list (1+ m) (1- m) (+ m) (- m) (* m) (/ m)
           (max m) (min m) (+ m 0.5) (* 1.5 m))))
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op (make-marker) 2)
                      (error (list (car err) (cadr err))))))
         '(+ - * / < > = <= >= max min))
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op "bad" 2)
                      (error (list (car err) (cadr err) (caddr err))))))
         '(+ - * / < > = <= >= max min)))
(buffer-substring
 (with-temp-buffer
   (insert "abcdef")
   (let ((a (copy-marker 2)) (b (copy-marker 5)))
     (list (buffer-substring a b) (buffer-substring-no-properties b a)
           (char-after a) (char-before b)
           (progn (goto-char a) (point))
           (progn (narrow-to-region a b) (list (point-min) (point-max)))
           (progn (widen) (delete-region a b) (buffer-string))))))
(char-after
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op (make-marker))
                      (error (list (car err) (cadr err))))))
         '(char-after char-before goto-char 1+ 1-)))

(+
 (with-temp-buffer
   (insert "abcdef")
   (let* ((m (copy-marker 2)) (arguments (list m 3 m)))
     (list (apply #'+ arguments) (mapcar #'markerp arguments)
           (marker-position m) (apply #'* arguments))))
 (with-temp-buffer
   (insert "abcdef")
   (let ((m (copy-marker 2)))
     (mapcar (lambda (op)
               (list op (funcall op m 2 3) (funcall op 3 2 m)))
             '(+ - * / < > = <= >= max min)))))
(buffer-substring-no-properties
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op (make-marker) 2)
                      (error (list (car err) (cadr err))))))
         '(buffer-substring buffer-substring-no-properties delete-region narrow-to-region)))

(+
 (let ((m (make-marker)))
   (mapcar (lambda (op)
             (list op
                   (condition-case err (funcall op "bad" m)
                     (error (list (car err) (cadr err))))
                   (condition-case err (funcall op m "bad")
                     (error (list (car err) (cadr err))))))
           '(+ - * / < > = <= >= max min))))
(goto-char
 (with-temp-buffer
   (insert "abcd")
   (let ((m (copy-marker 2)))
     (list (eq (goto-char m) m) (point)
           (goto-char 999) (point)))))

(char-after
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op "bad")
                      (error (list (car err) (cadr err))))))
         '(char-after char-before goto-char))
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op 1.5)
                      (error (list (car err) (cadr err))))))
         '(char-after char-before goto-char)))
(buffer-substring
 (mapcar (lambda (op)
           (list op (condition-case err (funcall op "bad" 2)
                      (error (list (car err) (cadr err))))))
         '(buffer-substring buffer-substring-no-properties delete-region narrow-to-region)))
(<
 (let ((m (make-marker)))
   (list (< m) (> m) (= m) (<= m) (>= m)
         (< 2 1 m) (> 1 2 m) (= 1 2 m) (<= 2 1 m) (>= 1 2 m))))
