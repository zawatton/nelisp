;; -*- lexical-binding: t; -*-
;; Parity corpus: json-parse-string / json-serialize / json-parse-buffer, every option set.
(defvar algo1-n 0)
(defun algo1-show (v)
  (cond ((hash-table-p v)
         (let (l) (maphash (lambda (k x) (push (cons k (algo1-show x)) l)) v) (list 'HASH (nreverse l))))
        ((consp v) (cons (algo1-show (car v)) (algo1-show (cdr v))))
        ((vectorp v) (apply #'vector (mapcar #'algo1-show v)))
        ((stringp v) (list 'STR v (multibyte-string-p v)))
        (t v)))
(defmacro algo1-case (form)
  `(progn (setq algo1-n (1+ algo1-n))
          (princ (format "%d: %S\n" algo1-n
                         (condition-case e (algo1-show ,form) (error (list 'ERR (car e) (cdr e))))))))
(defun algo1-json-docs ()
  (list "[]" "{}" "[1,2,3]" "[ 1 , 2 , 3 ]" "{\"a\":1}" "{\"a\":1,\"b\":[1,2,{\"c\":null}],\"d\":true,\"e\":false}"
        "{\"a\":1,\"b\":2,\"a\":3}" "[true,false,null]" "[[],[[]],{}]" "{\"t\":1,\"nil\":2,\"nil\":3}"
        "[1.5,-0.0,1e2,1E2,1.5e-3,-12,0,-0,12345678901234567890,1.0e+2]" "[01]" "[1.]" "[.5]" "[-]" "[1e]" "[1e+]"
        "[1,]" "[,1]" "{\"a\":}" "{\"a\"}" "{a:1}" "{\"a\":1,}" "[1 2]" "[1" "{\"a\":1" "" "   " "1" "1.5" "\"s\"" "true" "null" "false"
        "1 2" "[1] x" "  [1]  " "[1]\n\n" "\n[1,\n2]\n" "[\t1,\r\n2]"
        "\"abc\"" "[\"a\",\"b\\\"c\",\"d\\\\e\",\"\\/\",\"\\n\\t\\r\\b\\f\"]" "\"\\u00e9\\u65e5\\u0041\"" "\"\\ud83d\\ude00\"" "\"\\ud83d\"" "\"\\ude00\""
        "\"\\u0000\"" "\"\\x\"" "\"\\u12\"" "\"\\u12g4\"" "\"a\nb\"" "\"a\tb\"" "\"abc" "\"a\\" "[\"é\",\"日本\"]" "{\"é\":1,\"日本\":[\"x\"]}"
        "{\"a\\nb\":1,\"a\\u0062\":2,\"a\\\"\":3}" "{\"k\":\"v\",\"k\":\"w\"}"
        "[1e400]" "[-1e400]" "[1e-400]" "[truefalse]" "[nul]" "[tru]" "[nulll]" "[true1]"
        "{\"a\":{\"b\":{\"c\":[{\"d\":[1,2,[3,[4]]]}]}}}"
        "[1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20]"))
(let ((docs (algo1-json-docs)))
  (dolist (d docs)
    (algo1-case (json-parse-string d))
    (algo1-case (json-parse-string d :object-type 'alist :array-type 'list))
    (algo1-case (json-parse-string d :object-type 'plist :array-type 'list))
    (algo1-case (json-parse-string d :object-type 'plist :null-object nil :false-object 'F))
    (algo1-case (json-parse-string d :object-type 'alist :null-object 'N :false-object nil))))
;; multibyte / unibyte sources
(algo1-case (json-parse-string (string-to-multibyte "[\"abc\",\"d\"]")))
(algo1-case (json-parse-string (concat "[\"abc\",\"é\"]")))
(algo1-case (json-parse-string (string-to-multibyte "{\"abc\":\"d\"}")))
(algo1-case (json-parse-string (encode-coding-string "[\"é\"]" 'utf-8)))
(algo1-case (json-parse-string (encode-coding-string "{\"é\":\"日本\"}" 'utf-8)))
(algo1-case (json-parse-string (string-to-multibyte (encode-coding-string "[\"é\"]" 'utf-8))))
(algo1-case (json-parse-string "[\"\\xff\"]"))
(algo1-case (json-parse-string 5))
(algo1-case (json-parse-string "[1]" :object-type 'foo))
(algo1-case (json-parse-string "[1]" :array-type 'foo))
(algo1-case (json-parse-string "[1]" :bogus 1))
(algo1-case (json-parse-string "[1]" :object-type))
(algo1-case (json-parse-string "[1]" :null-object 'A :null-object 'B))
(algo1-case (json-parse-string "[null,false]" :null-object 'A :null-object 'B :false-object 'C :false-object 'D))
;; json-parse-buffer
(algo1-case (with-temp-buffer (insert "{\"a\":[1,2]} rest") (goto-char 1) (list (json-parse-buffer :object-type 'alist) (point))))
(algo1-case (with-temp-buffer (insert "xx [1,2,\"é\"] rest") (goto-char 4) (list (json-parse-buffer) (point))))
(algo1-case (with-temp-buffer (insert "[1,") (goto-char 1) (list (json-parse-buffer) (point))))
(algo1-case (with-temp-buffer (insert "[1] [2]") (goto-char 1) (list (json-parse-buffer) (json-parse-buffer) (point))))
;; serialize
(defun algo1-h (&rest kv) (let ((h (make-hash-table :test 'equal))) (while kv (puthash (car kv) (cadr kv) h) (setq kv (cddr kv))) h))
(let ((objs (list nil t :null :false 0 -5 1.5 -0.0 1e100 100.0 0.1 1e-5 (expt 2 70) "" "abc" "a\"b" "a\\b" "a\nb\tc\rd" (string 1 2 31) "é日本😀" "x\x7fy"
                  [] [1 2 3] [nil t :null :false] [[1] [2 [3]]] [1.5 "a" nil]
                  '((a . 1) (b . 2)) '((a . 1) (b . 2) (a . 3)) '(:a 1 :b 2) '(:a 1 :b 2 :a 3) '(:a nil :b t) '((a . nil) (b . t))
                  '((:a . 1)) '((a . [1 2]) (b . ((c . d)))) '((a . (1 2))) '(:a (:b (:c 1)))
                  '((nil . 1)) '(nil 1) '((a . 1) b) '(:a 1 :b) '(a 1) '("a" . 1) '(1 2) '(1 . 2) '((1 . 2)) '((a . 1) . 2) 'foo
                  (algo1-h "a" 1 "b" [1 2]) (algo1-h) (algo1-h "é" "ü" "a\nb" 2) (algo1-h 'a 1) (algo1-h "k" '((x . 1)))
                  (list (algo1-h "a" 1) (algo1-h "b" 2))
                  (vector (algo1-h "a" (list :x 1)))
                  (let ((l (list 'a 1))) (setcdr (cdr l) l) l)
                  (let ((v (vector 1 2))) (aset v 0 v) v)
                  (encode-coding-string "é" 'utf-8) (string-to-multibyte "abc") (1+ most-positive-fixnum)
                  0.0e+NaN 1.0e+INF -1.0e+INF)))
  (dolist (o objs)
    (algo1-case (json-serialize o))
    (algo1-case (json-serialize o :null-object nil))
    (algo1-case (json-serialize o :false-object nil :null-object 'N))
    (algo1-case (json-serialize o :null-object :false :false-object :null))))
(algo1-case (json-serialize [1] :object-type 'alist))
(algo1-case (json-serialize [1] :bogus 1))
(algo1-case (json-serialize [1] :null-object))
(let ((deep nil)) (dotimes (_ 45) (setq deep (list deep))) (algo1-case (json-serialize (vector deep))))
(let ((deep [])) (dotimes (_ 45) (setq deep (vector deep))) (algo1-case (json-serialize deep)))
(let ((deep [])) (dotimes (_ 60) (setq deep (vector deep))) (algo1-case (json-serialize deep)))
(let ((deep [])) (dotimes (_ 30) (setq deep (vector deep))) (algo1-case (json-serialize deep)))
(algo1-case (json-serialize (let (l) (dotimes (i 40) (push (cons (intern (format "k%d" i)) i) l)) (append l l))))
(algo1-case (json-serialize (let (l) (dotimes (i 40) (push (intern (format "k%d" (% i 30))) l) (push i l)) l)))
(algo1-case (json-serialize (let (l) (dotimes (i 3) (push (cons (intern (format "a%d" i)) (make-vector 5 i)) l)) l)))
;; round trip of a large document
(let* ((big (let (l) (dotimes (i 300) (push (list :id i :name (format "n\"%d\n" i) :tags [1.5 "é" t] :o (list :x i :y :null)) l)) (vconcat (nreverse l))))
       (txt (json-serialize big :null-object :null)))
  (algo1-case (length txt))
  (algo1-case (secure-hash 'md5 txt))
  (algo1-case (secure-hash 'md5 (json-serialize (json-parse-string txt :object-type 'plist :array-type 'array))))
  (algo1-case (equal (json-parse-string txt :object-type 'plist) (json-parse-string (json-serialize (json-parse-string txt :object-type 'plist)) :object-type 'plist))))
