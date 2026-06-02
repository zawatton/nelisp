;;; map-test.el --- ERT for lightweight map facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/map.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest map-test/require-loads-standard-feature ()
  (should (featurep 'map))
  (dolist (sym '(mapp map-elt map-put map-put! map-delete map-nested-elt
                      map-keys map-values map-pairs map-length map-copy
                      map-apply map-do map-keys-apply map-values-apply
                      map-filter map-remove map-empty-p map-contains-key
                      map-some map-every-p map-into map-insert map-merge
                      map-merge-with))
    (should (fboundp sym))))

(ert-deftest map-test/alist-and-plist-lookup ()
  (should (mapp '((a . 1))))
  (should (= (map-elt '((a . 1) (b . 2)) 'b) 2))
  (should (eq (map-elt '((a . nil)) 'a 'fallback) nil))
  (should (eq (map-elt '((a . 1)) 'missing 'fallback) 'fallback))
  (should (= (map-elt '(:a 1 :b 2) :b) 2))
  (should (map-contains-key '((a . nil)) 'a))
  (should (map-contains-key '(:a nil) :a))
  (should-not (map-contains-key '(:a nil) :missing)))

(ert-deftest map-test/hash-table-and-array-lookup ()
  (let ((table (make-hash-table :test 'equal)))
    (puthash "a" 1 table)
    (puthash "b" nil table)
    (should (= (map-elt table "a") 1))
    (should (eq (map-elt table "b" 'fallback) nil))
    (should (map-contains-key table "b"))
    (should (= (map-length table) 2)))
  (should (= (map-elt [10 20] 1) 20))
  (should (map-contains-key [10 20] 0))
  (should-not (map-contains-key [10 20] 2))
  (should (equal (map-values [a b]) '(a b))))

(ert-deftest map-test/iteration-and-filtering ()
  (should (equal (map-keys '((a . 1) (b . 2))) '(a b)))
  (should (equal (map-values '(:a 1 :b 2)) '(1 2)))
  (should (equal (map-pairs '(:a 1 :b 2)) '((:a . 1) (:b . 2))))
  (should (equal (map-apply (lambda (key value) (list key value))
                            '((a . 1) (b . 2)))
                 '((a 1) (b 2))))
  (should (equal (map-filter (lambda (_key value) (> value 1))
                             '((a . 1) (b . 2) (c . 3)))
                 '((b . 2) (c . 3))))
  (should (equal (map-remove (lambda (_key value) (> value 1))
                             '((a . 1) (b . 2) (c . 3)))
                 '((a . 1)))))

(ert-deftest map-test/mutation-copy-and-insert ()
  (let ((alist '((a . 1) (b . 2))))
    (should (= (map-put! alist 'b 20) 20))
    (should (equal alist '((a . 1) (b . 20))))
    (should-error (map-put! alist 'c 30) :type 'map-not-inplace)
    (should (equal (map-insert alist 'c 30) '((c . 30) (a . 1) (b . 20)))))
  (let ((plist (list :a 1 :b 2)))
    (should (= (map-put! plist :a 10) 10))
    (should (equal plist '(:a 10 :b 2)))
    (should (equal (map-delete plist :a) '(:b 2))))
  (let ((vec [a b]))
    (should (eq (map-put! vec 1 'z) 'z))
    (should (equal vec [a z]))))

(ert-deftest map-test/conversion-and-merge ()
  (let ((table (map-into '((a . 1) (b . 2)) 'hash-table)))
    (should (hash-table-p table))
    (should (= (gethash 'a table) 1)))
  (should (equal (map-into '((a . 1) (b . 2)) 'plist) '(a 1 b 2)))
  (should (equal (map-merge 'list '((a . 1) (b . 2)) '((b . 20) (c . 3)))
                 '((a . 1) (b . 20) (c . 3))))
  (should (equal (map-merge-with 'list #'+
                                 '((a . 1) (b . 2))
                                 '((b . 20) (c . 3)))
                 '((a . 1) (b . 22) (c . 3)))))

(ert-deftest map-test/predicates-and-nested-values ()
  (should (map-empty-p nil))
  (should-not (map-empty-p '((a . 1))))
  (should (eq (map-some (lambda (key value)
                          (and (= value 2) key))
                        '((a . 1) (b . 2)))
              'b))
  (should (map-every-p (lambda (_key value) (numberp value))
                       '((a . 1) (b . 2))))
  (should-not (map-every-p (lambda (_key value) (numberp value))
                           '((a . 1) (b . x))))
  (should (= (map-nested-elt '((a . ((b . 42)))) '(a b)) 42))
  (should (eq (map-nested-elt '((a . nil)) '(a b) 'fallback) 'fallback)))

(provide 'map-test)

;;; map-test.el ends here
