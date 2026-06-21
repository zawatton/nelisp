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

(ert-deftest emacs-cl-macros-test/doc16-round10-remaining-cl ()
  "Doc 16 round 10: cl-member / cl-assoc(-if) / cl-rassoc(-if) / cl-notany /
cl-notevery / cl-mapcan / cl-maplist / cl-mapcon / cl-subst /
cl-position-if-not / cl-subsetp / cl-tailp / cl-delete / cl-nsubstitute."
  (should (equal '(2 3) (cl-member 2 '(1 2 3))))
  (should-not (cl-member 9 '(1 2 3)))
  (should (equal '(b . 2) (cl-assoc 'b '((a . 1) (b . 2)))))
  (should (equal '(b . 2) (cl-rassoc 2 '((a . 1) (b . 2)))))
  (should (equal '(3 . y) (cl-assoc-if (lambda (k) (= 1 (% k 2))) '((2 . x) (3 . y)))))
  (should (equal '(b . 2) (cl-rassoc-if (lambda (v) (> v 1)) '((a . 1) (b . 2)))))
  (should (cl-notany (lambda (x) (= 1 (% x 2))) '(2 4 6)))
  (should-not (cl-notany (lambda (x) (= 1 (% x 2))) '(2 3)))
  (should (cl-notevery (lambda (x) (= 0 (% x 2))) '(2 4 5)))
  (should-not (cl-notevery (lambda (x) (= 0 (% x 2))) '(2 4)))
  (should (equal '(1 1 2 2) (cl-mapcan (lambda (x) (list x x)) '(1 2))))
  (should (equal '((1 2 3) (2 3) (3)) (cl-maplist #'identity '(1 2 3))))
  (should (equal '(3 2 1) (cl-mapcon (lambda (l) (list (length l))) '(1 2 3))))
  (should (equal '(1 (9 3) 9) (cl-subst 9 2 '(1 (2 3) 2))))
  (should (equal 2 (cl-position-if-not (lambda (x) (= 1 (% x 2))) '(1 3 4 5))))
  (should (cl-subsetp '(1 2) '(1 2 3)))
  (should-not (cl-subsetp '(1 4) '(1 2 3)))
  (should (let ((l '(1 2 3))) (cl-tailp (cddr l) l)))
  (should-not (cl-tailp '(9) '(1 2 3)))
  (should (equal '(1 3) (cl-delete 2 '(1 2 3 2))))
  (should (equal '(1 9 3 9) (cl-nsubstitute 9 2 '(1 2 3 2)))))

(ert-deftest emacs-cl-macros-test/doc16-round17-cl-typep ()
  "Doc 16 round 17: cl-typep over atomic and compound type specifiers.
On the batch host the real `cl-typep' runs, pinning the contract the
NeLisp runtime polyfill must reproduce."
  ;; atomic
  (should (cl-typep 3 'integer))
  (should-not (cl-typep 3.0 'integer))
  (should (cl-typep 3.0 'float))
  (should (cl-typep "x" 'string))
  (should (cl-typep ?a 'character))
  (should (cl-typep :k 'keyword))
  (should (cl-typep 'sym 'symbol))
  (should (cl-typep '(1) 'cons))
  (should (cl-typep '(1) 'list))
  (should (cl-typep nil 'null))
  (should (cl-typep [1] 'vector))
  (should (cl-typep [1] 'array))
  (should (cl-typep "x" 'array))
  (should (cl-typep '(1) 'sequence))
  (should (cl-typep [1] 'sequence))
  (should (cl-typep "x" 'sequence))
  (should (cl-typep (make-hash-table) 'hash-table))
  (should-not (cl-typep 3 'string))
  (should (cl-typep t t))
  (should-not (cl-typep t nil))
  ;; `TYPEp' predicate-convention fallback
  (should (cl-typep (current-buffer) 'buffer))
  ;; compound specifiers
  (should (cl-typep 5 '(integer 1 10)))
  (should-not (cl-typep 50 '(integer 1 10)))
  (should (cl-typep 99 '(integer 1 *)))
  (should (cl-typep 'b '(member a b c)))
  (should-not (cl-typep 'z '(member a b c)))
  (should (cl-typep "s" '(or integer string)))
  (should (cl-typep 4 '(and integer (satisfies cl-evenp))))
  (should (cl-typep "s" '(not integer)))
  (should (cl-typep 4 '(satisfies cl-evenp))))

(ert-deftest emacs-cl-macros-test/doc16-round17-type-dispatch-macros ()
  "Doc 16 round 17: cl-the / cl-locally / cl-check-type / cl-typecase /
cl-etypecase / cl-ecase."
  (should (= 7 (cl-the integer (+ 3 4))))
  (should (= 9 (cl-locally (ignore) (+ 4 5))))
  (should (null (cl-check-type 5 integer)))
  (should-error (cl-check-type "x" integer) :type 'wrong-type-argument)
  (should (eq 'is-int (cl-typecase 5 (string 'is-str) (integer 'is-int) (t 'other))))
  (should (eq 'is-str (cl-typecase "x" (string 'is-str) (integer 'is-int) (t 'other))))
  (should (eq 'other (cl-typecase 'sym (string 'is-str) (integer 'is-int) (t 'other))))
  (should (eq 'ow (cl-typecase 'sym (string 'is-str) (otherwise 'ow))))
  (should (null (cl-typecase 'sym (string 'is-str) (integer 'is-int))))
  (should (eq 'i (cl-etypecase 5 (string 's) (integer 'i))))
  (should-error (cl-etypecase 'sym (string 's) (integer 'i)) :type 'error)
  (should (eq 'b (cl-ecase 2 (1 'a) (2 'b) (3 'c))))
  (should-error (cl-ecase 9 (1 'a) (2 'b)) :type 'error))

(ert-deftest emacs-cl-macros-test/doc16-round17-values-and-gentemp ()
  "Doc 16 round 17: cl-values / cl-values-list / cl-gentemp."
  (should (equal '(1 2 3) (cl-values 1 2 3)))
  (should (equal '(a b) (cl-values-list '(a b))))
  (let ((s (cl-gentemp "R17")))
    (should (symbolp s))
    (should (string-prefix-p "R17" (symbol-name s)))))

(ert-deftest emacs-cl-macros-test/doc16-round18-destructuring-bind ()
  "Doc 16 round 18: cl-destructuring-bind over flat lambda-lists.
The batch host runs the real macro, pinning the contract the NeLisp
runtime shim must reproduce (nested patterns are out of scope)."
  (should (equal '(1 2 3) (cl-destructuring-bind (a b c) '(1 2 3) (list a b c))))
  (should (equal '(1 (2 3 4)) (cl-destructuring-bind (a &rest r) '(1 2 3 4) (list a r))))
  (should (equal '(1 (2 3)) (cl-destructuring-bind (a &body r) '(1 2 3) (list a r))))
  (should (equal '(1 2 9) (cl-destructuring-bind (a b &optional c) '(1 2 9) (list a b c))))
  (should (equal '(1 2 nil) (cl-destructuring-bind (a b &optional c) '(1 2) (list a b c))))
  (should (equal '(1 2 7) (cl-destructuring-bind (a b &optional (c 7)) '(1 2) (list a b c))))
  (should (equal '(1 2 5) (cl-destructuring-bind (a b &optional (c 7)) '(1 2 5) (list a b c))))
  (should (equal '(1 10 20) (cl-destructuring-bind (a &key x y) '(1 :x 10 :y 20) (list a x y))))
  (should (equal '(1 99 nil) (cl-destructuring-bind (a &key (x 99) y) '(1) (list a x y))))
  (should (equal '(1 nil 5) (cl-destructuring-bind (a &key x y) '(1 :y 5) (list a x y))))
  (should (equal '(1 2 (3 4)) (cl-destructuring-bind (a &optional b &rest r) '(1 2 3 4) (list a b r))))
  (should (= 6 (cl-destructuring-bind (a b) '(2 4) (ignore a) (+ a b)))))

(ert-deftest emacs-cl-macros-test/doc16-round18-multiple-value ()
  "Doc 16 round 18: cl-multiple-value-bind / cl-multiple-value-setq."
  (should (equal '(1 2) (cl-multiple-value-bind (a b) '(1 2 3) (list a b))))
  (should (equal '(1 2) (cl-multiple-value-bind (a b) '(1 2 3 4 5) (list a b))))
  (should (equal '(1 nil) (cl-multiple-value-bind (a b) '(1) (list a b))))
  (should (equal '(10 20) (let (p q) (cl-multiple-value-setq (p q) '(10 20)) (list p q))))
  (should (= 10 (let (p q) (cl-multiple-value-setq (p q) '(10 20))))))

(ert-deftest emacs-cl-macros-test/doc16-round19-place-macros ()
  "Doc 16 round 19: cl-psetq / cl-psetf / cl-rotatef / cl-shiftf /
cl-callf / cl-callf2.  The batch host autoloads the real cl-lib macros,
pinning the contract the NeLisp runtime shims must reproduce."
  ;; parallel set (swap semantics prove values are read before writes)
  (should (equal '(2 1) (let ((a 1) (b 2)) (cl-psetq a b b a) (list a b))))
  (should (equal '(2 1) (let ((a 1) (b 2)) (cl-psetf a b b a) (list a b))))
  (should (null (let ((a 1) (b 2)) (cl-psetq a b b a))))
  ;; rotate
  (should (equal '(2 3 1) (let ((a 1) (b 2) (c 3)) (cl-rotatef a b c) (list a b c))))
  (should (equal '(2 1) (let ((a 1) (b 2)) (cl-rotatef a b) (list a b))))
  (should (null (let ((a 1) (b 2)) (cl-rotatef a b))))
  ;; shift (returns original first value)
  (should (equal '(1 2 3 9) (let ((a 1) (b 2) (c 3))
                              (let ((old (cl-shiftf a b c 9))) (list old a b c)))))
  ;; callf / callf2 (FUNC spliced literally: unquoted name; non-symbol place)
  (should (= 4 (let ((x 3)) (cl-callf 1+ x) x)))
  (should (= 4 (let ((x 3)) (cl-callf 1+ x))))
  (should (equal "abc" (let ((s "ab")) (cl-callf concat s "c") s)))
  (should (equal '(1 2 3) (let ((l '(2 3))) (cl-callf2 cons 1 l) l)))
  (should (= 2 (let ((h (make-hash-table)))
                 (puthash 'k 1 h) (cl-callf 1+ (gethash 'k h)) (gethash 'k h)))))

(provide 'emacs-cl-macros-test)

;;; emacs-cl-macros-test.el ends here
