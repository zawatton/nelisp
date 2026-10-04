(funcall
 (funcall #'+ 1 2)
 (funcall (lambda (value) (* value value)) 4)
 (funcall 'list 'a 'b))
(apply
 (apply #'+ '(1 2 3))
 (apply 'list 'a '(b c)))
(mapcar
 (mapcar #'1+ '(1 2 3))
 (mapcar #'symbol-name '(a b)))
(assq
 (assq 'b '((a . 1) (b . 2)))
 (assq 'c '((a . 1) (b . 2)))
 (let* ((pair (cons 'a 1)) (list (list pair)))
   (eq (assq 'a list) pair)))
(plist-get
 (plist-get '(:a 1 :b 2) :b)
 (plist-get '(:a 1 :b 2) :missing))
(concat
 (concat "a" "b")
 (concat '("a" "b"))
 (concat [97 98]))
(format
 (format "%s-%d" 'item 2)
 (format "%S" '(a b)))
(string
 (string ?a ?b)
 (string))
