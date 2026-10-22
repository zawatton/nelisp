;;; GNU 31.1 string oracle. -*- lexical-binding: t; -*-
(setq print-escape-nonascii t print-escape-multibyte t)
(defun strings-normalize (value)
  (cond ((stringp value) (list 'string (multibyte-string-p value)
                              (string-to-list value) (length value) (string-bytes value)))
        ((consp value) (mapcar #'strings-normalize value))
        ((vectorp value) (cons 'vector (mapcar #'strings-normalize (append value nil))))
        (t value)))
(defun strings-case (name thunk)
  (let ((value (condition-case e (funcall thunk) (error (list 'error (car e) (cdr e))))))
    (princ (format "STRINGS-CASE %s %S\n" name (strings-normalize value))))
  nil)
(strings-case 'kinds (lambda () (mapcar (lambda (s) (list (length s) (string-bytes s) (multibyte-string-p s))) (list "abc" "あい" (unibyte-string 0 128 255) (make-string 8 120) (string-to-multibyte "abc")))))
(strings-case 'unibyte-aset (lambda () (let ((s (unibyte-string 0 128 255))) (aset s 1 200) (list s (aref s 1) (multibyte-string-p s)))))
(strings-case 'multibyte-aset (lambda () (let ((s (copy-sequence "あいう"))) (aset s 1 ?漢) (list s (length s) (string-bytes s)))))
(strings-case 'multibyte-ascii-aset (lambda () (let ((s (copy-sequence "aあ"))) (aset s 0 ?Z) (list s (length s) (string-bytes s)))))
(strings-case 'growing-aset (lambda () (let ((s (string-to-multibyte "abc"))) (aset s 1 ?漢) (list s (length s) (string-bytes s)))))
(strings-case 'substring (lambda () (list (substring "あいうえ" 1 -1) (substring (unibyte-string 0 128 255) 1))))
(strings-case 'concat (lambda () (list (concat "ab" "cd") (concat "あ" "い") (concat (unibyte-string 255) "あ"))))
(strings-case 'properties (lambda () (let ((s (copy-sequence "あいう"))) (put-text-property 1 2 'face 'bold s) (list (get-text-property 1 'face s) (text-properties-at 0 (substring s 1 3)) (text-properties-at 2 (concat "x" s))))))
(strings-case 'constructors (lambda () (list (make-string 3 ?漢) (format "%s:%d" "あ" 12) (number-to-string -12345))))
(strings-case 'conversions (lambda () (let ((s (unibyte-string 128 255))) (list (string-to-multibyte s) (string-to-unibyte (string-to-multibyte s)) (string-as-unibyte "漢")))))
(strings-case 'unibyte-error (lambda () (string-to-unibyte "漢")))
(strings-case 'clear-alias (lambda () (let* ((s (copy-sequence "aあ")) (v (vector s s))) (clear-string s) (list (eq (aref v 0) (aref v 1)) (multibyte-string-p s) s))))
(strings-case 'identity-gc (lambda () (let* ((s (copy-sequence "abcdefgh")) (v (vector s s))) (garbage-collect) (aset (aref v 0) 0 ?Z) (list (eq (aref v 0) (aref v 1)) (aref v 1)))))
nil

;; GNU styled_format oracle: retain representation and every property position.
(defun strings-format-result (fmt args)
  (let* ((value (apply #'format fmt args)) (i 0) (props nil))
    (while (< i (length value))
      (push (text-properties-at i value) props)
      (setq i (1+ i)))
    (list value (nreverse props))))
(dolist (row '((fmt-s "%s|%s|%s|%s|%s|%s" ("abc" 42 1.25 hello nil t))
               (fmt-S "%S|%S|%S|%S|%S" ("a\"b" hello nil t (1 "x")))
               (fmt-integers "%d %i %o %x %X %c %%" (-42 42 63 255 255 65))
               (fmt-floats "%f %e %g" (1.25 -0.125 12345.0))
               (fmt-width "[%8s][%-8s][%8d][%-8d]" ("abc" "abc" -12 -12))
               (fmt-precision "[%.2s][%.3S][%.3f][%.2e][%.3g]" ("abc" "abc" 1.25 1.25 1.25))
               (fmt-flags "%+08d|% 8d|%#08x|%#o|%#X|%#.3g|%-+8.2f" (42 42 42 9 255 1.0 1.25))
               (fmt-negative "%d %o %x %X" (-1 -1 -1 -1))
               (fmt-bignums "%s|%S|%d|%o|%x|%X" (18446744073709551617 18446744073709551617 18446744073709551617 18446744073709551617 18446744073709551617 18446744073709551617))
               (fmt-float-integer "%d|%o|%x|%X" (12.75 -12.75 12.75 12.75))
               (fmt-wide "%s:%c" ("あ漢" 128512))
               (fmt-extra "%s" ("one" unused))
               (fmt-wrong-format 123 (1))
               (fmt-wrong-d "%d" ("x"))
               (fmt-wrong-f "%f" (nil))
               (fmt-wrong-c "%c" (1.25))
               (fmt-too-few "%s %s" ("one"))
               (fmt-invalid "%q" (1))
               (fmt-dangling "%" ())))
  (let ((entry row))
    (strings-case (car entry)
                  (lambda () (strings-format-result (nth 1 entry) (nth 2 entry))))))
(strings-case 'fmt-byte-args
              (lambda () (list (strings-format-result "%s:%s" (list (unibyte-string 128 255) "漢"))
                               (strings-format-result "%S" (list (unibyte-string 128 255)))
                               (strings-format-result "%s:%s" (list (string-to-multibyte "abc") "x")))))
(strings-case 'fmt-byte-formats
              (lambda () (list (strings-format-result (concat (unibyte-string 255) "%s") '("漢"))
                               (strings-format-result (string-to-multibyte "%s:%d") '("abc" 3))
                               (strings-format-result (string-as-unibyte "%s:%d") '("abc" 3)))))
(strings-case 'fmt-properties
              (lambda () (let ((fmt (copy-sequence "a%sZ")) (arg (copy-sequence "bcd")))
                           (put-text-property 0 4 'face 'bold fmt)
                           (put-text-property 1 2 'face 'italic arg)
                           (list (strings-format-result fmt (list arg))
                                 (strings-format-result "x%6.2sy" (list arg))
                                 (strings-format-result "%s" (list arg))
                                 (strings-format-result (propertize "%s" 'help-echo "fmt") (list arg))))))
nil
(strings-case 'fmt-integer-precision
              (lambda () (strings-format-result "%.4d|%08.4d|%.0d|%#.0o|%.4x" '(12 -12 0 0 12))))
(strings-case 'fmt-property-fields
              (lambda () (let ((a (propertize "ab" 'face 'italic)))
                           (list (strings-format-result "x%-5sy" (list a))
                                 (strings-format-result (propertize "x%5sy" 'help-echo "f") (list a))
                                 (strings-format-result (propertize "x%%y" 'face 'bold) nil)
                                 (strings-format-result "%S" (list a))))))
nil
(strings-case 'fmt-extra-representation
              (lambda () (list (strings-format-result "%d" (list 1 "漢"))
                               (strings-format-result "%%:%s" (list 'hello "漢")))))
(strings-case 'fmt-symbols
              (lambda () (strings-format-result "%s|%s|%s|%s" (list 'hello '日本語 (make-symbol "a b") nil))))
(strings-case 'fmt-float-princ
              (lambda () (strings-format-result "%s|%s|%s|%s" (list -0.0 1e20 1e-10 (/ 1.0 3.0)))))
(strings-case 'fmt-fixnum-extrema
              (lambda () (strings-format-result "%d|%x|%o" (list most-negative-fixnum most-positive-fixnum most-negative-fixnum))))
nil
(strings-case 'fmt-symbol-name-kinds
              (lambda () (list (strings-format-result "[%s]" (list (make-symbol (string-to-multibyte "abc"))))
                               (strings-format-result "[%s]" (list (make-symbol (unibyte-string 128 255))))
                               (strings-format-result "%d" (list 1 (make-symbol "日本語"))))))
(strings-case 'fmt-same-source-properties
              (lambda () (let* ((arg (propertize "ab" 'face 'italic))
                                (value (strings-format-result (propertize "[%s]" 'face 'bold) (list arg))))
                           (list value (text-properties-at 0 arg)))))
nil
(strings-case 'fmt-display-width
              (lambda () (strings-format-result "[%5s][%.1s][%.2s][%-5.3s][%4.1s]" '("漢" "漢x" "漢x" "漢xy" "éx"))))
(strings-case 'fmt-alternate-floats
              (lambda () (strings-format-result "%#.0f|%#.0e|%#.0g|%08.2f" '(1.0 1.0 1.0 1.25))))
nil
