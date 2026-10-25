;;; nelisp-parity7-corpus.el --- Sequence mapping edge cases -*- lexical-binding: t; -*-

(defconst p7-cases
  '((mapcar-circular
     (let* ((xs (list 1 2 3)) (calls 0))
       (setcdr (cdr (cdr xs)) xs)
       (list (condition-case e
                 (mapcar (lambda (_) (setq calls (1+ calls)) (error "callback")) xs)
               ;; GNU reports the detection tail, not the input head.
               (circular-list (list (car e) (eq (cadr e) (cdr (cdr xs)))))
               (error e)) calls))
     ((circular-list t) 0))
    (mapcar-dotted
     (let ((calls 0))
       (list (condition-case e
                 (mapcar (lambda (_) (setq calls (1+ calls)) (error "callback")) '(1 2 . 9))
               (error e)) calls))
     ((wrong-type-argument listp 9) 0))
    (mapcar-invalid-function-dotted
     (condition-case e (mapcar 'p7-void '(1 . 9)) (error e))
     (wrong-type-argument listp 9))
    (mapcar-bool (mapcar 'identity (bool-vector t nil t nil nil t)) (t nil t nil nil t))
    (mapcar-bool-long
     (let ((v (make-bool-vector 65 nil)))
       (aset v 0 t) (aset v 63 t) (aset v 64 t)
       (list (length (mapcar 'identity v))
             (mapcar 'identity (bool-vector (aref v 0) (aref v 1) (aref v 63) (aref v 64)))))
     (65 (t nil t t)))
    (mapcar-bool-empty (mapcar 'p7-void (make-bool-vector 0 nil)) nil)
    (mapcar-string (mapcar 'identity "a日本λ") (97 26085 26412 955))
    (mapcar-unibyte (mapcar 'identity (unibyte-string 0 127 128 255)) (0 127 128 255))
    (mapcar-vector (mapcar 'identity [1 nil three]) (1 nil three))
    (mapcar-list (mapcar '1+ '(1 2 3)) (2 3 4))
    (mapcar-nil (mapcar 'p7-void nil) nil)
    (mapcar-empty-vector (mapcar 'p7-void []) nil)
    (mapcar-empty-string (mapcar 'p7-void "") nil)
    (mapcar-bad-sequence (condition-case e (mapcar 'p7-void 9) (error e))
     (wrong-type-argument sequencep 9))
    (mapcar-bool-signal
     (let ((calls 0))
       (list (condition-case e
                 (mapcar (lambda (x) (setq calls (1+ calls))
                           (if x x (signal 'error '(p7-stop)))) (bool-vector t nil t))
               (error e)) calls))
     ((error p7-stop) 2))
    (mapcar-list-signal
     (let ((calls 0))
       (list (condition-case e
                 (mapcar (lambda (x) (setq calls (1+ calls))
                           (if (= x 2) (signal 'error '(p7-stop)) x)) '(1 2 3))
               (error e)) calls))
     ((error p7-stop) 2))
    (mapcar-shortened
     (let ((xs (list 1 2 3)))
       (mapcar (lambda (x) (setcdr xs nil) x) xs)) (1))
    (mapcar-extended
     (let ((xs (list 1 2)))
       (mapcar (lambda (x) (when (= x 1) (setcdr (cdr xs) (list 3))) x) xs)) (1 2))
    (mapcan-bool (mapcan 'list (bool-vector t nil t)) (t nil t))
    (mapcan-vector (mapcan 'list [1 nil three]) (1 nil three))
    (mapcan-string (mapcan 'list "aλ") (97 955))
    (mapcan-nil (mapcan 'p7-void nil) nil)
    (mapcan-dotted
     (condition-case e (mapcan 'p7-void '(1 . 9)) (error e))
     (wrong-type-argument listp 9))
    (mapconcat-bool
     (mapconcat (lambda (x) (if x "t" "nil")) (bool-vector t nil t) ",") "t,nil,t")
    (mapconcat-vector (mapconcat 'symbol-name [a b c] "/") "a/b/c")
    (mapconcat-string (mapconcat 'char-to-string "aλ" "/") "a/λ")
    (mapconcat-unibyte
     (mapconcat 'number-to-string (unibyte-string 0 128 255) "/") "0/128/255")
    (mapconcat-nil (mapconcat 'p7-void nil "/") "")
    (mapconcat-dotted
     (condition-case e (mapconcat 'p7-void '(1 . 9) "/") (error e))
     (wrong-type-argument listp 9))
    (mapconcat-signal
     (condition-case e (mapconcat (lambda (_) (signal 'error '(p7-stop))) [1] "/")
       (error e)) (error p7-stop))
    (mapcan-circular
     (let ((xs (list 1)) (calls 0))
       (setcdr xs xs)
       (list (condition-case e
                 (mapcan (lambda (_) (setq calls (1+ calls)) (error "callback")) xs)
               (circular-list (list (car e) (eq (cadr e) xs))) (error e)) calls))
     ((circular-list t) 0))
    (mapconcat-circular
     (let ((xs (list 1)) (calls 0))
       (setcdr xs xs)
       (list (condition-case e
                 (mapconcat (lambda (_) (setq calls (1+ calls)) (error "callback")) xs "/")
               (circular-list (list (car e) (eq (cadr e) xs))) (error e)) calls))
     ((circular-list t) 0))))

(provide 'nelisp-parity7-corpus)
