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

(ert-deftest seq-test/doc16-round13-seq-let-and-vectors ()
  "Doc 16 round 13: seq-let destructuring, and seq ops over vectors after
the seq-do/seq-map list-conversion fix."
  ;; seq-let
  (should (equal '(10 20 30) (seq-let (a b c) '(10 20 30) (list a b c))))
  (should (equal '(1 (2 3 4)) (seq-let (a &rest r) '(1 2 3 4) (list a r))))
  (should (equal '(1 2 nil) (seq-let (a b c) '(1 2) (list a b c))))
  (should (equal '(10 20) (seq-let (a b) [10 20] (list a b))))
  ;; seq ops over vectors (these delegate through seq-do / seq-map)
  (should (equal '(2 3 4) (seq-map #'1+ [1 2 3])))
  (should (equal '(2 4) (seq-filter (lambda (x) (= 0 (% x 2))) [1 2 3 4])))
  (should (equal 6 (seq-reduce #'+ [1 2 3] 0)))
  (should (equal '(1 4 9)
                 (let (acc)
                   (seq-doseq (x [1 2 3]) (push (* x x) acc))
                   (nreverse acc)))))

(ert-deftest seq-test/doc16-round14-subseq-over-vectors ()
  "Doc 16 round 14: seq-subseq / seq-take / seq-drop / seq-rest over vectors
after routing the vector path through a list copy (the runtime's `substring'
mishandles vectors)."
  (should (equal '(2 3) (append (seq-subseq [1 2 3 4] 1 3) nil)))
  (should (equal '(3 4) (append (seq-subseq [1 2 3 4] -2) nil)))
  (should (equal "bc" (seq-subseq "abcde" 1 3)))
  (should (equal '(1 2) (append (seq-take [1 2 3 4] 2) nil)))
  (should (equal '(3 4) (append (seq-drop [1 2 3 4] 2) nil)))
  (should (equal '(1 2 3) (append (seq-take [1 2 3] 9) nil)))
  (should (equal '(2 3 4) (append (seq-rest [1 2 3 4]) nil)))
  ;; lists still behave
  (should (equal '(2 3) (seq-subseq '(1 2 3 4) 1 3)))
  (should (equal '(1 2) (seq-take '(1 2 3 4) 2))))

(ert-deftest seq-test/s2-batch-loads ()
  "S2 coverage batch: remaining plain GNU `seq.el' names are bound."
  (dolist (sym '(seq-contains seq-set-equal-p seq-positions
                 seq-remove-at-position seq--count-successive seq--elt-safe
                 seq--into-list seq--into-vector seq--into-string seq-split
                 seq-setq))
    (should (fboundp sym))))

(ert-deftest seq-test/s2-batch-values ()
  "Exact values, pinned against real GNU Emacs 31.1's own `seq.el'."
  (should (equal (seq--count-successive #'cl-evenp '(2 4 6 7 8)) 3))
  (should (equal (seq--elt-safe '(1 2 3) 5) nil))
  (should (equal (seq--elt-safe '(1 2 3) 1) 2))
  (should (equal (seq--into-list [1 2 3]) '(1 2 3)))
  (should (equal (seq--into-list '(1 2 3)) '(1 2 3)))
  (should (equal (seq--into-vector '(1 2 3)) [1 2 3]))
  (should (equal (seq--into-vector [1 2 3]) [1 2 3]))
  (should (equal (seq--into-string '(?a ?b ?c)) "abc"))
  (should (equal (seq-split '(1 2 3 4 5) 2) '((1 2) (3 4) (5))))
  (should (equal (seq-contains '(1 2 3) 2) 2))
  (should (equal (seq-contains '(1 2 3) 9) nil))
  (should (equal (seq-set-equal-p '(1 2 3) '(3 2 1)) t))
  (should (equal (seq-set-equal-p '(1 2 3) '(1 2)) nil))
  (should (equal (seq-positions '(1 2 3 2 1) 2) '(1 3)))
  (should (equal (seq-remove-at-position '(1 2 3 4) 1) '(1 3 4)))
  (should (equal (seq-remove-at-position [1 2 3 4] 1) [1 3 4]))
  (let ((a nil) (b nil))
    (seq-setq (a b) '(10 20))
    (should (equal (list a b) '(10 20))))
  (let ((a nil) (r nil))
    (seq-setq (a &rest r) '(1 2 3))
    (should (equal (list a r) '(1 (2 3))))))

(ert-deftest seq-test/s2-batch-errors ()
  "Error case pinned against real GNU Emacs 31.1: `seq-split' rejects
a non-positive LENGTH with a plain `error' signal."
  (should (eq (car (should-error (seq-split '(1 2 3) 0))) 'error)))

;; S2 coverage batch (2026-09-28): the real `seq' pcase pattern, ported
;; verbatim from GNU Emacs 31.1's `seq.el'.  This is new, additive
;; surface -- `seq-let'/`seq-setq' above keep their existing direct
;; `seq-elt'/`seq-drop' shim rather than being rewired onto it.  Values
;; pinned against real GNU Emacs 31.1.

(ert-deftest seq-test/s2-batch-pcase-fboundp ()
  (dolist (sym '(seq--make-pcase-bindings seq--make-pcase-patterns
                 seq--pcase-macroexpander))
    (should (fboundp sym))))

(ert-deftest seq-test/pcase-seq-pattern ()
  (should (equal (pcase [1 2 3] ((seq x y z) (list x y z))) '(1 2 3)))
  ;; Fewer patterns than elements: extras are ignored, match still succeeds.
  (should (equal (pcase '(1 2 3) ((seq x y) (list x y))) '(1 2)))
  ;; Fewer elements than patterns: missing ones bind to nil.
  (should (equal (pcase '(1) ((seq x y) (list x y))) '(1 nil)))
  ;; No match (not a seq) falls through to the next clause.
  (should (equal (pcase 5 ((seq x) x) (_ 'no-match)) 'no-match)))

(ert-deftest seq-test/s2-batch-pcase-helper-values ()
  "Exact shapes of the pcase-building helpers, pinned against real GNU
Emacs 31.1."
  (should (equal (seq--make-pcase-bindings '(a b))
                 '((app (seq--elt-safe _ 1) b) (app (seq--elt-safe _ 0) a))))
  (should (equal (seq--make-pcase-patterns '(a b)) '(seq a b))))

(provide 'seq-test)

;;; seq-test.el ends here
