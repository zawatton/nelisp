;; -*- lexical-binding: t; -*-
;; json.el layer: json-read-from-string / json-encode against GNU json.el (null and error symbols
;; are left out: the standalone reader keeps :json-null and `json-error').
(require 'json)
(defvar algo1-n 0)
(defmacro algo1-case (form)
  `(progn (setq algo1-n (1+ algo1-n))
          (princ (format "%d: %S\n" algo1-n (condition-case e ,form (error 'ERR))))))
(dolist (d '("[]" "{}" "[1,2,3]" "{\"a\":1,\"b\":[1,2,{\"c\":\"x\"}],\"d\":true,\"e\":false}" "[[],[[]],{}]" " [ 1 , 2 ] "
             "{\"a\":{\"b\":{\"c\":[1.5,-2,3e2]}}}" "\"a\\nb\\u00e9\\\"\"" "[\"é\",\"日本\"]" "{\"é\":1}" "[1,2" "{\"a\":}" "x" ""))
  (algo1-case (json-read-from-string d))
  (algo1-case (let ((json-array-type 'list)) (json-read-from-string d)))
  (algo1-case (let ((json-array-type 'vector)) (json-read-from-string d)))
  (algo1-case (let ((json-object-type 'plist)) (json-read-from-string d)))
  (algo1-case (let ((json-object-type 'plist) (json-array-type 'list)) (json-read-from-string d)))
  (algo1-case (let ((json-object-type 'hash-table)) (let ((v (json-read-from-string d))) (if (hash-table-p v) (let (l) (maphash (lambda (k x) (push (cons k x) l)) v) (nreverse l)) v))))
  (algo1-case (let ((json-key-type 'symbol)) (json-read-from-string d)))
  (algo1-case (let ((json-false 'F)) (json-read-from-string d))))
(dolist (o (list '((a . 1) (b . 2)) '((a . [1 2]) (b . "x\ny\"z")) [1 2 3] "é\n\t\"\\" 5 1.5 t :json-false '(1 2 3) '((a . ((b . 1))))
                 (let ((s (make-string 300 ?a))) s) (concat (make-string 100 ?a) "\"" (make-string 100 ?b))
                 '(:a 1 :b (2 3)) (list 1 "two" 3.0 [4])))
  (algo1-case (json-encode o)))
