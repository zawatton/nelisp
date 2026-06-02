;;; subr-x-test.el --- ERT for lightweight subr-x facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'subr-x)

(ert-deftest subr-x-test/require-loads-standard-feature ()
  (should (featurep 'subr-x))
  (dolist (sym '(thread-first thread-last hash-table-empty-p hash-table-keys
                              hash-table-values string-remove-prefix
                              string-remove-suffix string-replace
                              string-truncate-left string-limit string-pad
                              string-chop-newline named-let proper-list-p
                              mapcan))
    (should (fboundp sym))))

(ert-deftest subr-x-test/threading-macros ()
  (should (= (thread-first 5 (+ 20) (/ 25)) 1))
  (should (equal (thread-last '(1 2 3) (mapcar #'1+) reverse)
                 '(4 3 2))))

(ert-deftest subr-x-test/hash-table-values-and-empty ()
  (let ((table (make-hash-table :test 'equal)))
    (should (hash-table-empty-p table))
    (puthash "a" 1 table)
    (puthash "b" 2 table)
    (should-not (hash-table-empty-p table))
    (should (equal (sort (hash-table-keys table) #'string<)
                   '("a" "b")))
    (should (equal (sort (hash-table-values table) #'<)
                   '(1 2)))))

(ert-deftest subr-x-test/string-helpers ()
  (should (string= (string-remove-prefix "foo" "foobar") "bar"))
  (should (string= (string-remove-prefix "no" "foobar") "foobar"))
  (should (string= (string-remove-suffix "bar" "foobar") "foo"))
  (should (string= (string-replace "aa" "b" "aaaaa") "bba"))
  (should (string= (string-truncate-left "abcdef" 5) "...ef"))
  (should (string= (string-limit "abcdef" 3) "abc"))
  (should (string= (string-limit "abcdef" 3 t) "def"))
  (should (string= (string-pad "ab" 4 ?.) "ab.."))
  (should (string= (string-pad "ab" 4 ?. t) "..ab"))
  (should (string= (string-chop-newline "line\n") "line")))

(ert-deftest subr-x-test/proper-list-p-and-mapcan ()
  (should (= (proper-list-p '(1 2 3)) 3))
  (should (= (proper-list-p nil) 0))
  (should-not (proper-list-p '(1 . 2)))
  (should (equal (mapcan (lambda (x) (list x (- x))) '(1 2 3))
                 '(1 -1 2 -2 3 -3))))

(ert-deftest subr-x-test/named-let-recurses ()
  (should (= (named-let loop ((n 5) (acc 1))
              (if (= n 0)
                  acc
                (loop (1- n) (* acc n))))
             120)))

(provide 'subr-x-test)

;;; subr-x-test.el ends here
