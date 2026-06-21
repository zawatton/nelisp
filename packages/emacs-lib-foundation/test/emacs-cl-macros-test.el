;;; emacs-cl-macros-test.el --- Tests for emacs-cl-macros  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests for the CL compatibility layer split out of `emacs-stub.el'.
;; The batch host already provides the real CL library, so these tests
;; focus on the contract the module preserves under host Emacs and on
;; the internal parsing helpers that are defined in this file.

;;; Code:

(require 'ert)
(require 'emacs-cl-macros)

;;;; Load / feature contract

(ert-deftest emacs-cl-macros-test/require-loads-cleanly ()
  (should (featurep 'emacs-cl-macros))
  (should (featurep 'cl-macs))
  (should (featurep 'cl-seq))
  (should (featurep 'cl-extra))
  (should (featurep 'cl-generic))
  (should (fboundp 'cl-deftype)))

;;;; Arglist helpers

(ert-deftest emacs-cl-macros-test/split-arglist-positional-only ()
  (should (equal (emacs-cl-macros--split-arglist '(a b c))
                 '((a b c) nil nil nil))))

(ert-deftest emacs-cl-macros-test/split-arglist-optional-rest-and-key ()
  (should (equal (emacs-cl-macros--split-arglist
                  '(a &optional b &rest rest &key (k1 1) k2))
                 '((a) (b) rest ((:k1 k1 1) (:k2 k2 nil))))))

(ert-deftest emacs-cl-macros-test/key-bindings-shape-is-correct ()
  (should (equal (emacs-cl-macros--key-bindings '((:k1 k1 1) (:k2 k2 nil)) 'rest)
                 '((k1 (or (car (cdr (memq ':k1 rest))) 1))
                   (k2 (or (car (cdr (memq ':k2 rest))) nil))))))

;;;; Sequence predicates

(ert-deftest emacs-cl-macros-test/cl-some-every-find-position-contract ()
  (should (equal (cl-some (lambda (x) (and (= x 5) 'hit)) '(1 3 5 7)) 'hit))
  (should (cl-every (lambda (x) (< x 10)) '(1 2 3)))
  (should-not (cl-every (lambda (x) (< x 3)) '(1 2 3)))
  (should (equal (cl-find 3 '(1 2 3 4)) 3))
  (should (equal (cl-position 3 '(1 2 3 4)) 2))
  (should (equal (cl-position-if (lambda (x) (> x 3)) '(1 2 4 5)) 2)))

(ert-deftest emacs-cl-macros-test/cl-numeric-predicates ()
  "Doc 15 B4: cl-evenp / cl-oddp / cl-plusp / cl-minusp (were void)."
  (should (cl-evenp 4))
  (should-not (cl-evenp 3))
  (should (cl-oddp 3))
  (should-not (cl-oddp 4))
  (should (cl-plusp 1))
  (should-not (cl-plusp 0))
  (should (cl-minusp -1))
  (should-not (cl-minusp 0))
  ;; usable as a predicate argument
  (should (equal '(1 3) (cl-remove-if #'cl-evenp '(1 2 3 4)))))

(ert-deftest emacs-cl-macros-test/cl-remove-if-and-cl-remove-if-not-filter-correctly ()
  (should (equal (cl-remove-if (lambda (x) (= 1 (% x 2))) '(1 2 3 4 5)) '(2 4)))
  (should (equal (cl-remove-if-not (lambda (x) (= 1 (% x 2))) '(1 2 3 4 5))
                 '(1 3 5))))

;;;; Loop / mutation macros

(ert-deftest emacs-cl-macros-test/cl-loop-collect-roundtrip ()
  (should (equal (cl-loop for x in '(1 2 3) collect (* 2 x))
                 '(2 4 6))))

(ert-deftest emacs-cl-macros-test/cl-loop-sum-and-count-roundtrip ()
  (should (= 6 (cl-loop for x in '(1 2 3) sum x)))
  (should (= 1 (cl-loop for x in '(1 0 2) count (> x 1)))))

(ert-deftest emacs-cl-macros-test/cl-incf-cl-decf-and-cl-pushnew-contract ()
  (let ((n 1)
        (xs '(b a)))
    (should (= 4 (cl-incf n 3)))
    (should (= 1 (cl-decf n 3)))
    (should (equal (progn (cl-pushnew 'a xs) xs) '(b a)))
    (should (equal (progn (cl-pushnew 'c xs) xs) '(c b a)))))

(ert-deftest emacs-cl-macros-test/letrec-and-cl-progv-load-time-contract ()
  (letrec ((countdown (lambda (n)
                        (if (= n 0)
                            42
                          (funcall countdown (1- n))))))
    (should (= 42 (funcall countdown 3))))
  (should (equal (cl-progv nil nil 'ok) 'ok)))

(ert-deftest emacs-cl-macros-test/doc16-round5-cl-numeric-list ()
  "Doc 16 round 5: cl-caddr / cl-signum / cl-gcd / cl-lcm / cl-isqrt /
cl-list* / cl-revappend / cl-ldiff were void in the standalone runtime."
  (should (equal 3 (cl-caddr '(1 2 3 4))))
  (should (equal 1 (cl-signum 5)))
  (should (equal -1 (cl-signum -3)))
  (should (equal 0 (cl-signum 0)))
  (should (equal 1 (cl-signum 2.0)))
  (should (equal 0 (cl-signum 0.0)))
  (should (equal -1 (cl-signum -2.5)))
  (should (equal 6 (cl-gcd 12 18)))
  (should (equal 12 (cl-gcd 24 36 60)))
  (should (equal 7 (cl-gcd 7)))
  (should (equal 12 (cl-lcm 4 6)))
  (should (equal 12 (cl-lcm 2 3 4)))
  (should (equal 0 (cl-lcm 3 0)))
  (should (equal 3 (cl-isqrt 10)))
  (should (equal 4 (cl-isqrt 16)))
  (should (equal 0 (cl-isqrt 0)))
  (should (equal 9 (cl-isqrt 99)))
  (should (equal '(1 2 . 3) (cl-list* 1 2 3)))
  (should (equal '(1 2 3 4) (cl-list* 1 2 '(3 4))))
  (should (equal '(3 2 1 4 5) (cl-revappend '(1 2 3) '(4 5))))
  (should (equal '(1 2)
                 (let* ((tl '(3 4)) (l (cons 1 (cons 2 tl)))) (cl-ldiff l tl)))))

(ert-deftest emacs-cl-macros-test/doc16-round6-cl-division-family ()
  "Doc 16 round 6: cl-floor / cl-ceiling / cl-round / cl-truncate / cl-mod /
cl-rem each return (Q R); built around the runtime's broken 2-arg floor/mod."
  ;; cl-truncate (toward zero)
  (should (equal '(3 1) (cl-truncate 7 2)))
  (should (equal '(-3 -1) (cl-truncate -7 2)))
  (should (equal '(3 1.5) (cl-truncate 7.5 2)))
  ;; cl-floor (toward -inf)
  (should (equal '(3 1) (cl-floor 7 2)))
  (should (equal '(-4 1) (cl-floor -7 2)))
  (should (equal '(-4 -1) (cl-floor 7 -2)))
  (should (equal '(-4 0.5) (cl-floor -7.5 2)))
  ;; cl-ceiling (toward +inf)
  (should (equal '(4 -1) (cl-ceiling 7 2)))
  (should (equal '(-3 -1) (cl-ceiling -7 2)))
  ;; cl-round (ties to even)
  (should (equal '(4 -1) (cl-round 7 2)))
  (should (equal '(2 1) (cl-round 5 2)))
  (should (equal '(6 -1) (cl-round 11 2)))
  (should (equal '(-2 -1) (cl-round -5 2)))
  ;; cl-mod (sign of Y) / cl-rem (sign of X)
  (should (equal 2 (cl-mod -7 3)))
  (should (equal -2 (cl-mod 7 -3)))
  (should (equal -1 (cl-rem -7 3)))
  (should (equal 1 (cl-rem 7 3))))

(ert-deftest emacs-cl-macros-test/doc16-round9-keyword-cl-sequence ()
  "Doc 16 round 9: cl-remove-duplicates / cl-count(-if) / cl-reduce /
cl-adjoin / cl-set-exclusive-or / cl-substitute with :test/:key keywords."
  ;; cl-remove-duplicates (keep last by default, first with :from-end)
  (should (equal '(1 3 2) (cl-remove-duplicates '(1 2 1 3 2))))
  (should (equal '(1 2 3) (cl-remove-duplicates '(1 2 1 3 2) :from-end t)))
  (should (equal '(3 4) (cl-remove-duplicates '(1 2 3 4) :key (lambda (x) (% x 2)))))
  ;; cl-count / cl-count-if
  (should (equal 3 (cl-count 2 '(1 2 2 3 2))))
  (should (equal 2 (cl-count-if (lambda (x) (= 0 (% x 2))) '(1 2 3 4))))
  ;; cl-reduce (left fold, :initial-value, right fold, :key, empty)
  (should (equal -8 (cl-reduce #'- '(1 2 3 4))))
  (should (equal 16 (cl-reduce #'+ '(1 2 3) :initial-value 10)))
  (should (equal -2 (cl-reduce #'- '(1 2 3 4) :from-end t)))
  (should (equal 6 (cl-reduce #'+ '((1) (2) (3)) :key #'car)))
  (should (equal 5 (cl-reduce #'+ '() :initial-value 5)))
  ;; cl-adjoin
  (should (equal '(1 2 3) (cl-adjoin 2 '(1 2 3))))
  (should (equal '(9 1 2 3) (cl-adjoin 9 '(1 2 3))))
  ;; cl-set-exclusive-or / cl-substitute
  (should (equal '(1 4) (cl-set-exclusive-or '(1 2 3) '(2 3 4))))
  (should (equal '(1 9 3 9) (cl-substitute 9 2 '(1 2 3 2)))))

(provide 'emacs-cl-macros-test)

;;; emacs-cl-macros-test.el ends here
