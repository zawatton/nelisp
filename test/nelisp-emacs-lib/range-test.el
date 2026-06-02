;;; range-test.el --- ERT for lightweight range facade  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)

(require 'range)

(ert-deftest range-test/require-loads-standard-feature ()
  (should (featurep 'range))
  (dolist (sym '(range-normalize range-denormalize range-difference
                 range-intersection range-compress-list range-uncompress
                 range-add-list range-remove range-member-p
                 range-list-intersection range-list-difference
                 range-length range-concat range-map))
    (should (fboundp sym))))

(ert-deftest range-test/compress-and-uncompress ()
  (should (equal (range-compress-list '(1 2 2 3 7 9 10))
                 '((1 . 3) 7 (9 . 10))))
  (should (equal (range-uncompress '((1 . 3) 7 (9 . 10)))
                 '(1 2 3 7 9 10)))
  (should (equal (range-uncompress '(4 . 6))
                 '(4 5 6))))

(ert-deftest range-test/difference-and-remove ()
  (should (equal (range-difference '((1 . 6) 10) '((3 . 4) 10))
                 '((1 . 2) (5 . 6))))
  (should (equal (range-remove '((1 . 6) 10) '(2 5))
                 '(1 (3 . 4) 6 10))))

(ert-deftest range-test/intersection-denormalizes-single-span ()
  (should (equal (range-intersection '((1 . 5) 8) '((3 . 7)))
                 '(3 . 5)))
  (should (equal (range-intersection '(1 4 7) '(4 5 6))
                 '(4))))

(ert-deftest range-test/add-list-and-concat ()
  (should (equal (range-add-list '((1 . 3) 9) '(4 5 10))
                 '((1 . 5) (9 . 10))))
  (should (equal (range-concat '((1 . 2) 8) '((3 . 5) 7))
                 '((1 . 5) (7 . 8)))))

(ert-deftest range-test/member-and-list-filters ()
  (should (range-member-p 4 '((2 . 5) 9)))
  (should-not (range-member-p 6 '((2 . 5) 9)))
  (should (equal (range-list-intersection '(1 2 3 4 5) '((2 . 4)))
                 '(2 3 4)))
  (should (equal (range-list-difference '(1 2 3 4 5) '((2 . 4)))
                 '(1 5))))

(ert-deftest range-test/length-and-map ()
  (let (seen)
    (range-map (lambda (number) (push number seen)) '((1 . 3) 5))
    (should (equal (nreverse seen) '(1 2 3 5))))
  (should (= (range-length '((1 . 3) 5 (7 . 9))) 7)))

(provide 'range-test)

;;; range-test.el ends here
