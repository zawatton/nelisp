;;; -*- lexical-binding: t; -*-

(string-to-multibyte
 (mapcar (lambda (bytes)
           (let* ((source (apply #'unibyte-string bytes))
                  (result (string-to-multibyte source)))
             (list (append result nil) (string-bytes result)
                   (multibyte-string-p result) (eq source result))))
         '(() (65) (128 255) (194 160) (192 128) (224 160 128)))
 (let* ((source (char-to-string #x3fffff)) (result (string-to-multibyte source)))
   (list (eq source result) (append result nil)))
 (let ((source (string-to-multibyte (unibyte-string 65))))
   (mapcar (lambda (result)
             (list (append result nil) (string-bytes result)
                   (multibyte-string-p result) (eq source result)))
           (list (concat source) (substring source 0 1)
                 (substring source 0 0) (copy-sequence source)
                 (format "%s" source) (format source)
                 (string-to-unibyte source) (string-as-unibyte source)
                 (string-to-multibyte source) (string-as-multibyte source))))
 (condition-case err (string-to-multibyte nil) (error err)))

(string-as-multibyte
 (mapcar (lambda (bytes)
           (let* ((source (apply #'unibyte-string bytes))
                  (result (string-as-multibyte source)))
             (list (append result nil) (string-bytes result)
                   (multibyte-string-p result) (eq source result))))
         '(() (65) (128 255) (194 160) (192 128) (193 191)
           (224 128 128) (224 160 128) (237 160 128)
           (240 128 128 128) (247 191 191 191)
           (248 136 128 128 128) (248 143 191 189 191)
           (248 143 191 190 128) (195) (195 65)))
 (let* ((source (char-to-string #x3fffff)) (result (string-as-multibyte source)))
   (list (eq source result) (append result nil)))
 (condition-case err (string-as-multibyte nil) (error err)))

(string-as-unibyte
 (mapcar (lambda (source)
           (let ((result (string-as-unibyte source)))
             (list (append result nil) (string-bytes result)
                   (multibyte-string-p result) (eq source result))))
         (list (unibyte-string 128 255) "abc" "あ"
               (char-to-string 255) (char-to-string #x200000)
               (char-to-string #x3fff7f) (char-to-string #x3fff80)
               (char-to-string #x3fffff)))
 (condition-case err (string-as-unibyte nil) (error err)))

(string-to-unibyte
 (mapcar (lambda (source)
           (condition-case err
               (let ((result (string-to-unibyte source)))
                 (list (append result nil) (string-bytes result)
                       (multibyte-string-p result) (eq source result)))
             (error err)))
         (list (unibyte-string 128 255) "abc" "あ"
               (char-to-string 255) (char-to-string #x3fff80)
               (char-to-string #x3fffff)
               (concat "a" (char-to-string #x3fff80) "あ")))
 (condition-case err (string-to-unibyte nil) (error err))
 (condition-case err (string-to-unibyte) (error err))
 (condition-case err (string-to-unibyte "a" "b") (error err)))

(string-make-unibyte
 (let ((source (string-to-multibyte (unibyte-string 128 255))))
   (list (append (string-make-unibyte source) nil)
         (multibyte-string-p (string-make-unibyte source))))
 (condition-case err
     (string-make-unibyte (concat "a" (char-to-string #x3fff80) "あ"))
   (error err))
 (condition-case err (string-make-unibyte) (error err))
 (condition-case err (string-make-unibyte "a" "b") (error err))
 (condition-case err (string-make-unibyte nil) (error err))
 (mapcar (lambda (source)
           (let ((result (string-make-unibyte source)))
             (list (append result nil) (eq result source)
                   (multibyte-string-p result))))
         (list "abc" (string-to-multibyte "abc") ""
               (string-to-multibyte "") "あ"
               (char-to-string #x3fff80) (char-to-string #x200000))))

(concat
 (let ((result (concat (unibyte-string 128 255) "あ")))
   (list (append result nil) (string-bytes result) (multibyte-string-p result)))
 (let ((result (concat "あ" (unibyte-string 128 255))))
   (list (append result nil) (string-bytes result) (multibyte-string-p result)))
 (let ((result (concat '(#x200000 #x3fff7f #x3fff80 #x3fffff))))
   (list (append result nil) (string-bytes result) (multibyte-string-p result)))
 (let ((result (concat [#x200000 #x3fff7f #x3fff80 #x3fffff])))
   (list (append result nil) (string-bytes result) (multibyte-string-p result)))
 (let* ((source (concat "あ" "a")) (result (substring source 1)))
   (list (append result nil) (string-bytes result) (multibyte-string-p result))))

(format
 (let ((result (format "あ%s" (unibyte-string 128 255))))
   (list (append result nil) (string-bytes result) (multibyte-string-p result)))
 (let ((result (format (unibyte-string 255 37 115) "あ")))
   (list (append result nil) (string-bytes result) (multibyte-string-p result)))
 (let ((result (format "%s" (unibyte-string 128 255))))
   (list (append result nil) (string-bytes result) (multibyte-string-p result))))

(append
 (append (unibyte-string 128 255) "あ" nil))

(vconcat
 (vconcat (unibyte-string 128 255) "あ"))
