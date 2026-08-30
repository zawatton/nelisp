;;; nl-clj-lazy-test.el --- Tests for nl-clj-lazy.el -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.7's own DoD-shaped verification for the lazy phase: no
;; host-Emacs control (plain Emacs has no lazy seq at all), checked
;; against Clojure's own documented `lazy-seq'/`take'/`range'/...
;; contract by hand.  Against-the-bug shape, per that section: (1)
;; realize-once -- a thunk with a side-effecting counter fires exactly
;; once across multiple forces; (2) the reentrancy case -- a thunk
;; that forces its own still-unrealized outer value signals
;; `nl-borrow-error', not silent double evaluation or infinite
;; recursion (named there as "the single most important negative test
;; in this whole subsection, and the one most likely to be skipped");
;; (3) an unbounded lazy walk does not exhaust the interpreter's own
;; call stack.  Plus this package's own headline demo: `(take 5
;; (range))' terminates, and a lazy `map'/`filter' pipeline over an
;; unbounded range never calls its own functions on an element nothing
;; downstream ever forces.

;;; Code:

(require 'ert)
(require 'nl-safe)
(require 'nl-clj-core)
(require 'nl-clj-vector)
(require 'nl-clj-hash)
(require 'nl-clj-seq)
(require 'nl-clj-lazy)

;;;; realize-once + reentrancy (Doc 195 §4.7's own named-critical cases) ----

(ert-deftest nl-clj-lazy-test-realized-once ()
  "A thunk's side effect fires exactly once, no matter how many times,
or from which entry point (`first'/`rest'/`seq'), the result is forced."
  (let ((calls 0))
    (let ((lz (nl-clj-lazy-seq (setq calls (1+ calls)) (cons 1 nil))))
      (should (= (nl-clj-first lz) 1))
      (should (= (nl-clj-first lz) 1))
      (should (null (nl-clj-rest lz)))
      (should (equal (nl-clj-seq lz) '(1)))
      (should (= calls 1)))))

(ert-deftest nl-clj-lazy-test-realized-once-across-nested-forces ()
  "Forcing the SECOND cell of a two-cell lazy chain does not re-run the
first cell's own thunk a second time."
  (let ((c1 0) (c2 0))
    (let ((lz (nl-clj-lazy-seq
                (setq c1 (1+ c1))
                (cons 'a (nl-clj-lazy-seq (setq c2 (1+ c2)) (cons 'b nil))))))
      (should (equal (nl-clj-lazy-doall lz) '(a b)))
      (should (equal (nl-clj-lazy-doall lz) '(a b)))
      (should (= c1 1))
      (should (= c2 1)))))

(ert-deftest nl-clj-lazy-test-reentrant-force-signals ()
  "A thunk that forces its own still-unrealized outer lazy value
signals `nl-borrow-error' -- loud, not silent double evaluation and
not infinite recursion (Doc 195 §4.7's own named critical case)."
  (let (lz)
    (setq lz (nl-clj-lazy-seq (cons (nl-clj-first lz) nil)))
    (should-error (nl-clj-first lz) :type 'nl-borrow-error)))

(ert-deftest nl-clj-lazy-test-lazy-p ()
  (should (nl-clj-lazy-p (nl-clj-lazy-seq nil)))
  (should-not (nl-clj-lazy-p (nl-clj-vector 1 2)))
  (should-not (nl-clj-lazy-p '(1 2)))
  (should-not (nl-clj-lazy-p nil)))

;;;; generic seq dispatch: first/rest/next/seq transparently on lazy -------

(ert-deftest nl-clj-lazy-test-seq-first-rest-next ()
  (let ((lz (nl-clj-lazy-seq (cons 1 (cons 2 (cons 3 nil))))))
    (should (= (nl-clj-first lz) 1))
    (should (equal (nl-clj-rest lz) '(2 3)))
    (should (equal (nl-clj-next lz) '(2 3)))))

(ert-deftest nl-clj-lazy-test-seq-of-empty-lazy-is-nil ()
  (should (null (nl-clj-seq (nl-clj-lazy-seq nil))))
  (should (null (nl-clj-first (nl-clj-lazy-seq nil))))
  (should (null (nl-clj-rest (nl-clj-lazy-seq nil)))))

(ert-deftest nl-clj-lazy-test-seq-forces-only-one-step ()
  "`nl-clj-seq' on a lazy value forces exactly one cell -- its own
`rest' stays an unrealized lazy value, not walked further."
  (let ((calls 0))
    (let* ((lz (nl-clj-lazy-map (lambda (x) (setq calls (1+ calls)) x)
                                 (nl-clj-lazy-range)))
           (s (nl-clj-seq lz)))
      (should (= (car s) 0))
      (should (nl-clj-lazy-p (cdr s)))
      (should (= calls 1)))))

(ert-deftest nl-clj-lazy-test-count-finite-lazy ()
  (should (= (nl-clj-count (nl-clj-lazy-take 7 (nl-clj-lazy-range))) 7)))

(ert-deftest nl-clj-lazy-test-nth-over-lazy ()
  (should (= (nl-clj-nth (nl-clj-lazy-range) 4) 4)))

(ert-deftest nl-clj-lazy-test-into-from-bounded-lazy ()
  (let ((v (nl-clj-into (nl-clj-vector) (nl-clj-lazy-take 5 (nl-clj-lazy-range)))))
    (should (nl-clj-vector-p v))
    (should (equal (nl-clj-seq v) '(0 1 2 3 4)))))

(ert-deftest nl-clj-lazy-test-conj-prepends-onto-lazy ()
  "`conj' onto a lazy seq is itself an eager cons (Clojure's own `conj'
on a seq is always O(1) eager) whose REST may still be lazy -- forced
here with `nl-clj-lazy-doall' before comparing, matching `nl-clj-seq''s
own \"forces exactly one step\" contract this file's other tests check
directly (`nl-clj-lazy-test-seq-forces-only-one-step')."
  (should (equal (nl-clj-lazy-doall (nl-clj-conj (nl-clj-lazy-take 3 (nl-clj-lazy-range)) -1))
                 '(-1 0 1 2))))

;;;; the killer test: infinite range + take ---------------------------------

(ert-deftest nl-clj-lazy-test-infinite-range-take ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 5 (nl-clj-lazy-range))) '(0 1 2 3 4))))

(ert-deftest nl-clj-lazy-test-range-bounded ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-range 5)) '(0 1 2 3 4)))
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-range 2 5)) '(2 3 4)))
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-range 0 10 2)) '(0 2 4 6 8)))
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-range 5 0 -1)) '(5 4 3 2 1))))

(ert-deftest nl-clj-lazy-test-range-empty-bounds ()
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-range 0))))
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-range 5 5)))))

(ert-deftest nl-clj-lazy-test-large-take-does-not-overflow-stack ()
  "Against-the-bug (Doc 195 §4.7): a naive self-recursive walk driving
this many forces would blow the interpreter's own call stack; this
package's `while'-loop drivers do not.  Mirrors nl-clj-seq-test.el's
own pre-existing eager `nl-clj-reduce' analog at a comparable size."
  (should (= (length (nl-clj-lazy-doall (nl-clj-lazy-take 20000 (nl-clj-lazy-range)))) 20000))
  (should (= (nl-clj-reduce #'+ 0 (nl-clj-lazy-take 20000 (nl-clj-lazy-range)))
             (/ (* 19999 20000) 2))))

;;;; laziness proof: map/filter do not run on un-taken elements ------------

(ert-deftest nl-clj-lazy-test-map-is-lazy ()
  (let ((calls 0))
    (let ((mapped (nl-clj-map (lambda (x) (setq calls (1+ calls)) (* x 2))
                               (nl-clj-lazy-range))))
      (should (nl-clj-lazy-p mapped))
      (should (= calls 0))
      (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 3 mapped)) '(0 2 4)))
      (should (= calls 3)))))

(ert-deftest nl-clj-lazy-test-filter-is-lazy ()
  (let ((calls 0))
    (let ((filtered (nl-clj-filter (lambda (x) (setq calls (1+ calls)) (= 0 (mod x 2)))
                                    (nl-clj-lazy-range))))
      (should (nl-clj-lazy-p filtered))
      (should (= calls 0))
      (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 3 filtered)) '(0 2 4)))
      ;; 0,1,2,3,4 checked (in order) to find 3 evens (0, 2, 4)
      (should (= calls 5)))))

(ert-deftest nl-clj-lazy-test-map-over-eager-still-eager ()
  "`nl-clj-map'/`nl-clj-filter' over an EAGER coll are unchanged: a
plain Elisp list, not a lazy value -- the lazy phase must not regress
this package's own pre-existing eager contract."
  (should (equal (nl-clj-map #'1+ (nl-clj-vector 1 2 3)) '(2 3 4)))
  (should-not (nl-clj-lazy-p (nl-clj-map #'1+ (nl-clj-vector 1 2 3))))
  (should (equal (nl-clj-filter (lambda (x) (= 0 (mod x 2))) (nl-clj-vector 1 2 3 4)) '(2 4))))

(ert-deftest nl-clj-lazy-test-reduce-forces ()
  "`reduce' over a lazy seq forces it -- unlike `map'/`filter', it does
not stay lazy (Doc 195's own lazy-phase brief: \"reduce forces\")."
  (let ((calls 0))
    (let* ((r (nl-clj-lazy-take 5 (nl-clj-lazy-range)))
           (mapped (nl-clj-map (lambda (x) (setq calls (1+ calls)) x) r)))
      (should (= calls 0))
      (should (= (nl-clj-reduce #'+ 0 mapped) 10))
      (should (= calls 5)))))

(ert-deftest nl-clj-lazy-test-pipeline-stays-lazy-end-to-end ()
  "->> range (map square) (filter even?) (take 3): a full pipeline
that only ever forces as many upstream elements as the final `take'
needs, proving `map'/`filter' compose without either one forcing more
than the next stage actually asks for."
  (let ((map-calls 0) (filter-calls 0))
    (let* ((r (nl-clj-lazy-range))
           (mapped (nl-clj-map (lambda (x) (setq map-calls (1+ map-calls)) (* x x)) r))
           (filtered (nl-clj-filter (lambda (x) (setq filter-calls (1+ filter-calls)) (= 0 (mod x 2)))
                                     mapped))
           (taken (nl-clj-lazy-take 3 filtered)))
      (should (nl-clj-lazy-p taken))
      (should (= map-calls 0))
      (should (= filter-calls 0))
      (should (equal (nl-clj-lazy-doall taken) '(0 4 16)))
      ;; squares of 0..4 are 0 1 4 9 16 -- 5 candidates examined to find
      ;; the first 3 even squares (0, 4, 16).
      (should (= map-calls 5))
      (should (= filter-calls 5)))))

;;;; take / drop / take-while / drop-while boundaries -----------------------

(ert-deftest nl-clj-lazy-test-take-fewer-than-available ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 5 (nl-clj-vector 1 2))) '(1 2))))

(ert-deftest nl-clj-lazy-test-take-zero-is-empty ()
  (should (null (nl-clj-lazy-take 0 (nl-clj-lazy-range))))
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-take 0 (nl-clj-lazy-range))))))

(ert-deftest nl-clj-lazy-test-take-negative-is-empty ()
  (should (null (nl-clj-lazy-take -3 (nl-clj-lazy-range)))))

(ert-deftest nl-clj-lazy-test-drop-more-than-available ()
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-drop 10 (nl-clj-vector 1 2 3))))))

(ert-deftest nl-clj-lazy-test-drop-zero-is-identity ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-drop 0 (nl-clj-vector 1 2 3))) '(1 2 3))))

(ert-deftest nl-clj-lazy-test-take-while ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take-while (lambda (x) (< x 5)) (nl-clj-lazy-range)))
                 '(0 1 2 3 4))))

(ert-deftest nl-clj-lazy-test-take-while-none-match-is-empty ()
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-take-while (lambda (x) (< x 0)) (nl-clj-lazy-range))))))

(ert-deftest nl-clj-lazy-test-take-while-all-match ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take-while (lambda (_) t) (nl-clj-vector 1 2 3)))
                 '(1 2 3))))

(ert-deftest nl-clj-lazy-test-drop-while ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 3 (nl-clj-lazy-drop-while (lambda (x) (< x 5))
                                                                                 (nl-clj-lazy-range))))
                 '(5 6 7))))

(ert-deftest nl-clj-lazy-test-drop-while-all-match-is-empty ()
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-drop-while (lambda (_) t) (nl-clj-vector 1 2 3))))))

(ert-deftest nl-clj-lazy-test-drop-while-none-match-is-identity ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-drop-while (lambda (x) (< x 0)) (nl-clj-vector 1 2 3)))
                 '(1 2 3))))

;;;; lazy-cons ----------------------------------------------------------------

(ert-deftest nl-clj-lazy-test-lazy-cons-basic ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-cons 1 (nl-clj-vector 2 3))) '(1 2 3))))

(ert-deftest nl-clj-lazy-test-lazy-cons-defers-both-forms ()
  (let ((head-evaluated nil) (tail-evaluated nil))
    (let ((lz (nl-clj-lazy-cons (progn (setq head-evaluated t) 1)
                                 (progn (setq tail-evaluated t) (nl-clj-vector 2 3)))))
      (should-not head-evaluated)
      (should-not tail-evaluated)
      (should (equal (nl-clj-lazy-doall lz) '(1 2 3)))
      (should head-evaluated)
      (should tail-evaluated))))

;;;; iterate / repeat / cycle --------------------------------------------

(ert-deftest nl-clj-lazy-test-iterate ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 5 (nl-clj-lazy-iterate #'1+ 0))) '(0 1 2 3 4)))
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 4 (nl-clj-lazy-iterate (lambda (x) (* x 2)) 1)))
                 '(1 2 4 8))))

(ert-deftest nl-clj-lazy-test-repeat-infinite ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 4 (nl-clj-lazy-repeat :x))) '(:x :x :x :x))))

(ert-deftest nl-clj-lazy-test-repeat-n ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-repeat 3 :x)) '(:x :x :x)))
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-repeat 0 :x)))))

(ert-deftest nl-clj-lazy-test-cycle ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 7 (nl-clj-lazy-cycle (nl-clj-vector 1 2 3))))
                 '(1 2 3 1 2 3 1))))

(ert-deftest nl-clj-lazy-test-cycle-of-empty-is-empty ()
  "`nl-clj-lazy-cycle' always returns a lazy-tagged value (its own
outer `nl-clj-lazy-seq' wrapper), even for an empty COLL -- only
FORCING it yields nil; see `nl-clj-lazy-test-take-zero-is-empty' for
the contrasting case (`nl-clj-lazy-take' can decide to return nil
UNwrapped, with no forcing needed, since it never has to inspect COLL
to know N<=0 means empty)."
  (should (nl-clj-lazy-p (nl-clj-lazy-cycle (nl-clj-vector))))
  (should (null (nl-clj-seq (nl-clj-lazy-cycle (nl-clj-vector)))))
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-cycle nil)))))

;;;; doall / dorun --------------------------------------------------------

(ert-deftest nl-clj-lazy-test-doall-realizes-and-keeps-values ()
  (should (equal (nl-clj-lazy-doall (nl-clj-lazy-take 4 (nl-clj-lazy-range))) '(0 1 2 3))))

(ert-deftest nl-clj-lazy-test-dorun-forces-for-side-effects-returns-nil ()
  (let ((calls 0))
    (let ((mapped (nl-clj-map (lambda (x) (setq calls (1+ calls)) x)
                               (nl-clj-lazy-take 5 (nl-clj-lazy-range)))))
      (should (null (nl-clj-lazy-dorun mapped)))
      (should (= calls 5)))))

(ert-deftest nl-clj-lazy-test-doall-of-empty-is-nil ()
  (should (null (nl-clj-lazy-doall (nl-clj-lazy-seq nil)))))

;;;; bignum-adjacent sanity on host (the standalone smoke is the real proof
;;;; that NeLisp's own bignum promotion, not host Emacs's, is exercised) ---

(ert-deftest nl-clj-lazy-test-range-past-fixnum-boundary-mechanics ()
  "Host Emacs has always had native bignums, so this does not exercise
NeLisp's own Doc 190 Phase B promotion (the standalone smoke does,
against the real `target/nelisp' binary) -- it proves this package's
own `range'/`take' MECHANICS are correct at and past that boundary,
independent of which substrate is doing the arithmetic."
  (let* ((start (1- most-positive-fixnum))
         (taken (nl-clj-lazy-doall (nl-clj-lazy-take 5 (nl-clj-lazy-range start nil)))))
    (should (= (length taken) 5))
    (should (= (car taken) start))
    (should (> (nth 4 taken) most-positive-fixnum))))

;;; nl-clj-lazy-test.el ends here
