;;; seq-test.el --- ERT for lightweight seq facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(load (expand-file-name
       "../src/seq.el"
       (file-name-directory (or load-file-name buffer-file-name)))
      nil t)

(ert-deftest seq-test/require-loads-standard-feature ()
  (should (featurep 'seq))
  (dolist (sym '(seqp seq-length seq-elt seq-first seq-rest seq-copy
                      seq-into seq-do seq-doseq seq-do-indexed seq-map
                      seq-map-indexed seq-mapn seq-subseq seq-take seq-drop
                      seq-take-while seq-drop-while seq-filter seq-remove
                      seq-find seq-some seq-every-p seq-empty-p
                      seq-contains-p seq-position seq-reduce seq-uniq
                      seq-concatenate seq-sort seq-sort-by seq-max seq-min
                      seq-random-elt seq-group-by))
    (should (fboundp sym))))

(ert-deftest seq-test/basic-accessors ()
  (should (seqp '(a b)))
  (should (seqp "ab"))
  (should (seqp [a b]))
  (should (= (seq-length [a b c]) 3))
  (should (eq (seq-elt '(a b c) 1) 'b))
  (should (eq (seq-first [x y]) 'x))
  (should (equal (seq-rest '(x y z)) '(y z))))

(ert-deftest seq-test/conversion-and-subseq ()
  (should (equal (seq-into "abc" 'list) '(?a ?b ?c)))
  (should (equal (seq-into '(?a ?b) 'string) "ab"))
  (should (equal (seq-into '(a b) 'vector) [a b]))
  (should (equal (seq-subseq '(a b c d) 1 3) '(b c)))
  (should (equal (seq-subseq [a b c d] 1 3) [b c]))
  (should (equal (seq-subseq "abcd" 1 3) "bc")))

(ert-deftest seq-test/take-drop-filter-find ()
  (should (equal (seq-take '(1 2 3 4) 2) '(1 2)))
  (should (equal (seq-drop '(1 2 3 4) 2) '(3 4)))
  (should (equal (seq-take-while (lambda (x) (< x 3)) '(1 2 3 1))
                 '(1 2)))
  (should (equal (seq-drop-while (lambda (x) (< x 3)) '(1 2 3 1))
                 '(3 1)))
  (should (equal (seq-filter (lambda (x) (= (% x 2) 1)) '(1 2 3 4))
                 '(1 3)))
  (should (equal (seq-remove (lambda (x) (= (% x 2) 1)) '(1 2 3 4))
                 '(2 4)))
  (should (eq (seq-find (lambda (x) (> x 2)) '(1 2 3 4)) 3))
  (should (eq (seq-find (lambda (x) (> x 9)) '(1 2) 'none) 'none)))

(ert-deftest seq-test/predicates-and-reductions ()
  (should (seq-some (lambda (x) (and (> x 2) x)) '(1 2 3)))
  (should (seq-every-p #'numberp '(1 2 3)))
  (should-not (seq-every-p #'numberp '(1 a 3)))
  (should (seq-empty-p []))
  (should (seq-contains-p '(a b c) 'b))
  (should (= (seq-position '(a b c) 'c) 2))
  (should (= (seq-reduce #'+ '(1 2 3) 10) 16))
  (should (equal (seq-uniq '(a b a c b)) '(a b c))))

(ert-deftest seq-test/combine-sort-group ()
  (should (equal (seq-map #'1+ [1 2 3]) '(2 3 4)))
  (should (equal (seq-map-indexed (lambda (x i) (+ x i)) '(10 20 30))
                 '(10 21 32)))
  (should (equal (seq-mapn #'+ '(1 2 3) [10 20]) '(11 22)))
  (should (equal (seq-concatenate 'string '(?a) [?b] "c") "abc"))
  (should (equal (seq-sort #'< '(3 1 2)) '(1 2 3)))
  (should (equal (seq-sort-by #'length #'< '("aaa" "b" "cc"))
                 '("b" "cc" "aaa")))
  (should (= (seq-max '(3 1 2)) 3))
  (should (= (seq-min '(3 1 2)) 1))
  (should (equal (seq-group-by #'car '((a . 1) (b . 2) (a . 3)))
                 '((a (a . 1) (a . 3))
                   (b (b . 2))))))

(ert-deftest seq-test/doc16-round2-set-and-partition ()
  "Doc 16 breadth round 2: seq-partition / seq-mapcat / seq-keep /
seq-difference / seq-intersection / seq-union (+ seq-reverse), which the
NeLisp seq facade was missing."
  (should (equal '((1 2) (3 4) (5)) (seq-partition '(1 2 3 4 5) 2)))
  (should (equal '(1 1 2 2) (seq-mapcat (lambda (x) (list x x)) '(1 2))))
  (should (equal '(10 30)
                 (seq-keep (lambda (x) (and (= 1 (% x 2)) (* x 10))) '(1 2 3))))
  (should (equal '(1 3) (seq-difference '(1 2 3 4) '(2 4))))
  (should (equal '(2 4) (seq-intersection '(1 2 3 4) '(2 4 6))))
  (should (equal '(1 2 3 4 5) (seq-union '(1 2 3) '(3 4 5))))
  (should (equal '(3 2 1) (seq-reverse '(1 2 3)))))

(provide 'seq-test)

;;; seq-test.el ends here
