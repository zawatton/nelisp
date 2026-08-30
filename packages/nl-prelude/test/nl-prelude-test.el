;;; nl-prelude-test.el --- ERT tests for nl-prelude -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Coverage for `src/nl-prelude.el' (Doc 169 Phase 1): Result / Option
;; construction, predicates, extraction, transforms, `nl-collect'
;; short-circuiting, and the `nl-?' early-return operator including its
;; expansion-time constraints (outside `nl-defun', crossing `lambda').
;;
;; This file deliberately avoids cl-lib and ert-x helpers so the same
;; test bodies can run on `target/nelisp' standalone through the mini
;; harness in `test/nl-prelude-standalone-smoke.el'.

;;; Code:

(require 'ert)
(require 'nl-prelude)

;;; Construction + predicates ----------------------------------------

(ert-deftest nl-prelude-ok-p-basics ()
  (should (nl-ok-p (nl-ok 1)))
  (should-not (nl-ok-p (nl-err 1)))
  (should-not (nl-ok-p 1))
  (should-not (nl-ok-p nil))
  (should-not (nl-ok-p "ok")))

(ert-deftest nl-prelude-err-p-basics ()
  (should (nl-err-p (nl-err 'boom)))
  (should-not (nl-err-p (nl-ok 'boom)))
  (should-not (nl-err-p 'boom))
  (should-not (nl-err-p nil)))

(ert-deftest nl-prelude-result-p ()
  (should (nl-result-p (nl-ok 1)))
  (should (nl-result-p (nl-err 1)))
  (should-not (nl-result-p (nl-some 1)))
  (should-not (nl-result-p nl-none))
  (should-not (nl-result-p '(1 . 2))))

(ert-deftest nl-prelude-result-equal ()
  (should (equal (nl-ok 1) (nl-ok 1)))
  (should (equal (nl-err "x") (nl-err "x")))
  (should-not (equal (nl-ok 1) (nl-err 1))))

(ert-deftest nl-prelude-ok-nil-value ()
  "ok wrapping nil stays distinguishable from err/none."
  (should (nl-ok-p (nl-ok nil)))
  (should (eq (nl-unwrap (nl-ok nil)) nil)))

(ert-deftest nl-prelude-some-p-basics ()
  (should (nl-some-p (nl-some 1)))
  (should-not (nl-some-p nl-none))
  (should-not (nl-some-p (nl-ok 1)))
  (should-not (nl-some-p 1)))

(ert-deftest nl-prelude-none-p-basics ()
  (should (nl-none-p nl-none))
  (should-not (nl-none-p nil))
  (should-not (nl-none-p (nl-some nil))))

(ert-deftest nl-prelude-option-p ()
  (should (nl-option-p (nl-some 1)))
  (should (nl-option-p nl-none))
  (should-not (nl-option-p (nl-ok 1)))
  (should-not (nl-option-p nil)))

(ert-deftest nl-prelude-some-nil-value ()
  (should (nl-some-p (nl-some nil)))
  (should (eq (nl-unwrap (nl-some nil)) nil)))

(ert-deftest nl-prelude-none-is-constant ()
  (should (eq nl-none 'nl--none))
  (should (nl-none-p 'nl--none)))

;;; Extraction --------------------------------------------------------

(ert-deftest nl-prelude-unwrap-ok ()
  (should (equal (nl-unwrap (nl-ok "value")) "value")))

(ert-deftest nl-prelude-unwrap-err-signals ()
  (let ((err (should-error (nl-unwrap (nl-err 'payload))
                           :type 'nl-unwrap-error)))
    (should (equal (cdr err) '(payload)))))

(ert-deftest nl-prelude-unwrap-some ()
  (should (equal (nl-unwrap (nl-some 42)) 42)))

(ert-deftest nl-prelude-unwrap-none-signals ()
  (should-error (nl-unwrap nl-none) :type 'nl-unwrap-error))

(ert-deftest nl-prelude-unwrap-type-error ()
  (should-error (nl-unwrap 5) :type 'nl-type-error)
  (should-error (nl-unwrap '(a b)) :type 'nl-type-error))

(ert-deftest nl-prelude-unwrap-or ()
  (should (equal (nl-unwrap-or (nl-ok 1) 9) 1))
  (should (equal (nl-unwrap-or (nl-err 'e) 9) 9))
  (should (equal (nl-unwrap-or (nl-some 1) 9) 1))
  (should (equal (nl-unwrap-or nl-none 9) 9)))

(ert-deftest nl-prelude-unwrap-or-else-err ()
  (should (equal (nl-unwrap-or-else (nl-err 3) (lambda (e) (* e 10)))
                 30)))

(ert-deftest nl-prelude-unwrap-or-else-ok-skips-fn ()
  (let ((calls 0))
    (should (equal (nl-unwrap-or-else (nl-ok 7)
                                      (lambda (_e) (setq calls (1+ calls)) 0))
                   7))
    (should (= calls 0))))

(ert-deftest nl-prelude-unwrap-or-else-none ()
  (should (equal (nl-unwrap-or-else nl-none (lambda () 'fallback))
                 'fallback)))

;;; Transform ---------------------------------------------------------

(ert-deftest nl-prelude-map-ok ()
  (should (equal (nl-map (nl-ok 2) #'1+) (nl-ok 3))))

(ert-deftest nl-prelude-map-err-passthrough ()
  (let ((e (nl-err 'boom)))
    (should (eq (nl-map e #'1+) e))))

(ert-deftest nl-prelude-map-option ()
  (should (equal (nl-map (nl-some 2) #'1+) (nl-some 3)))
  (should (eq (nl-map nl-none #'1+) nl-none)))

(ert-deftest nl-prelude-map-type-error ()
  (should-error (nl-map 2 #'1+) :type 'nl-type-error))

(ert-deftest nl-prelude-map-err-transforms ()
  (should (equal (nl-map-err (nl-err 3) #'1+) (nl-err 4))))

(ert-deftest nl-prelude-map-err-ok-passthrough ()
  (let ((r (nl-ok 3)))
    (should (eq (nl-map-err r #'1+) r))))

(ert-deftest nl-prelude-map-err-rejects-option ()
  (should-error (nl-map-err (nl-some 1) #'1+) :type 'nl-type-error))

(ert-deftest nl-prelude-and-then-ok-chain ()
  (should (equal (nl-and-then (nl-ok 2) (lambda (v) (nl-ok (* v v))))
                 (nl-ok 4)))
  (should (equal (nl-and-then (nl-ok 2) (lambda (_v) (nl-err 'no)))
                 (nl-err 'no))))

(ert-deftest nl-prelude-and-then-err-short-circuits ()
  (let ((calls 0)
        (e (nl-err 'stop)))
    (should (eq (nl-and-then e (lambda (_v) (setq calls (1+ calls)) (nl-ok 1)))
                e))
    (should (= calls 0))))

(ert-deftest nl-prelude-and-then-option ()
  (should (equal (nl-and-then (nl-some 2) (lambda (v) (nl-some (1+ v))))
                 (nl-some 3)))
  (should (eq (nl-and-then nl-none (lambda (v) (nl-some v))) nl-none)))

(ert-deftest nl-prelude-ok-to-option ()
  (should (equal (nl-ok->option (nl-ok 1)) (nl-some 1)))
  (should (eq (nl-ok->option (nl-err 'e)) nl-none)))

(ert-deftest nl-prelude-option-to-ok ()
  (should (equal (nl-option->ok (nl-some 1) 'missing) (nl-ok 1)))
  (should (equal (nl-option->ok nl-none 'missing) (nl-err 'missing))))

(ert-deftest nl-prelude-conversion-type-errors ()
  (should-error (nl-ok->option (nl-some 1)) :type 'nl-type-error)
  (should-error (nl-option->ok (nl-ok 1) 'e) :type 'nl-type-error))

;;; Aggregation -------------------------------------------------------

(ert-deftest nl-prelude-collect-all-ok ()
  (should (equal (nl-collect (list (nl-ok 1) (nl-ok 2) (nl-ok 3)))
                 (nl-ok '(1 2 3)))))

(ert-deftest nl-prelude-collect-empty ()
  (should (equal (nl-collect nil) (nl-ok nil))))

(ert-deftest nl-prelude-collect-first-err ()
  (let ((e1 (nl-err 'first))
        (e2 (nl-err 'second)))
    (should (eq (nl-collect (list (nl-ok 1) e1 (nl-ok 2) e2)) e1))))

(ert-deftest nl-prelude-collect-short-circuits ()
  "Elements after the first err are not inspected at all.
Junk after the err would signal `nl-type-error' if it were reached."
  (let ((e (nl-err 'stop)))
    (should (eq (nl-collect (list (nl-ok 1) e :junk-not-a-result)) e))))

(ert-deftest nl-prelude-collect-type-error ()
  (should-error (nl-collect (list (nl-ok 1) 42)) :type 'nl-type-error))

;;; nl-? early return -------------------------------------------------

(nl-defun nl-prelude-test--half (n)
  "Return (nl-ok N/2) for even N, else an err Result."
  (if (= (% n 2) 0)
      (nl-ok (/ n 2))
    (nl-err (list 'odd n))))

(nl-defun nl-prelude-test--quarter (n)
  (nl-let* ((h (nl-? (nl-prelude-test--half n)))
            (q (nl-? (nl-prelude-test--half h))))
    (nl-ok q)))

(ert-deftest nl-prelude-?-unwraps-ok-inline ()
  (should (equal (nl-prelude-test--quarter 8) (nl-ok 2))))

(ert-deftest nl-prelude-?-early-returns-err ()
  (should (equal (nl-prelude-test--quarter 6) (nl-err '(odd 3))))
  (should (equal (nl-prelude-test--quarter 5) (nl-err '(odd 5)))))

(nl-defun nl-prelude-test--identity-? (r)
  (nl-ok (nl-? r)))

(ert-deftest nl-prelude-?-propagates-same-err-object ()
  (let ((e (nl-err 'same)))
    (should (eq (nl-prelude-test--identity-? e) e))))

(ert-deftest nl-prelude-?-runtime-type-error ()
  (nl-defun nl-prelude-test--bad ()
    (nl-ok (nl-? 5)))
  (should-error (nl-prelude-test--bad) :type 'nl-type-error))

(ert-deftest nl-prelude-?-multiple-in-one-form ()
  (nl-defun nl-prelude-test--sum (a b)
    (nl-ok (+ (nl-? a) (nl-? b))))
  (should (equal (nl-prelude-test--sum (nl-ok 1) (nl-ok 2)) (nl-ok 3)))
  (should (equal (nl-prelude-test--sum (nl-err 'l) (nl-ok 2)) (nl-err 'l)))
  (should (equal (nl-prelude-test--sum (nl-ok 1) (nl-err 'r)) (nl-err 'r))))

(defvar nl-prelude-test--cleanup-log nil
  "Side-effect log observed by the `unwind-protect' cooperation test.")

(nl-defun nl-prelude-test--cleanup (r)
  (unwind-protect
      (nl-ok (nl-? r))
    (setq nl-prelude-test--cleanup-log
          (cons 'cleanup nl-prelude-test--cleanup-log))))

(ert-deftest nl-prelude-?-cooperates-with-unwind-protect ()
  "Early return through `unwind-protect' still runs cleanup forms."
  (setq nl-prelude-test--cleanup-log nil)
  (should (equal (nl-prelude-test--cleanup (nl-err 'e)) (nl-err 'e)))
  (should (equal nl-prelude-test--cleanup-log '(cleanup)))
  (should (equal (nl-prelude-test--cleanup (nl-ok 1)) (nl-ok 1)))
  (should (equal nl-prelude-test--cleanup-log '(cleanup cleanup))))

(ert-deftest nl-prelude-?-outside-nl-defun-is-expansion-error ()
  (should-error (macroexpand '(nl-? (nl-ok 1)))))

(ert-deftest nl-prelude-?-across-lambda-is-expansion-error ()
  (should-error (macroexpand '(nl-defun nl-prelude-test--x (xs)
                                (mapcar (lambda (x) (nl-? x)) xs))))
  (should-error (macroexpand '(nl-defun nl-prelude-test--y (x)
                                (funcall #'(lambda () (nl-? x)))))))

(ert-deftest nl-prelude-?-wrong-arity-is-expansion-error ()
  (should-error (macroexpand '(nl-defun nl-prelude-test--z ()
                                (nl-? (nl-ok 1) (nl-ok 2))))))

(ert-deftest nl-prelude-?-inside-nl-lambda ()
  (let ((f (nl-lambda (r) (nl-ok (1+ (nl-? r))))))
    (should (equal (funcall f (nl-ok 1)) (nl-ok 2)))
    (should (equal (funcall f (nl-err 'e)) (nl-err 'e)))))

(ert-deftest nl-prelude-?-nested-nl-lambda-has-own-block ()
  "An err inside a nested `nl-lambda' returns from the inner
function only; the outer `nl-defun' keeps running."
  (nl-defun nl-prelude-test--outer (rs)
    (nl-ok (mapcar (nl-lambda (r) (nl-unwrap-or (nl-? r) nil)) rs)))
  (let ((e (nl-err 'inner)))
    (should (equal (nl-prelude-test--outer (list (nl-ok (nl-ok 1)) e))
                   (nl-ok (list 1 e))))))

(ert-deftest nl-prelude-?-quoted-forms-untouched ()
  (nl-defun nl-prelude-test--quoted ()
    (nl-ok '(nl-? marker)))
  (should (equal (nl-prelude-test--quoted) (nl-ok '(nl-? marker)))))

(ert-deftest nl-prelude-defun-keeps-docstring ()
  "The docstring stays outside the `cl-block' wrapper."
  (should (equal (car (nl--wrap-?-body '("Probe docstring." (nl-ok 1))))
                 "Probe docstring."))
  ;; `documentation' does not exist on target/nelisp standalone.
  (when (fboundp 'documentation)
    (should (stringp (documentation 'nl-prelude-test--half)))))

(ert-deftest nl-prelude-defun-plain-body-still-works ()
  "`nl-defun' without any `nl-?' behaves like `defun'."
  (nl-defun nl-prelude-test--plain (a b) (+ a b))
  (should (= (nl-prelude-test--plain 1 2) 3)))

(ert-deftest nl-prelude-let*-is-let* ()
  (should (= (nl-let* ((a 1) (b (1+ a))) (+ a b)) 3)))

;;; nl-defdata / nl-match (Phase 2a) ----------------------------------

(nl-defdata nlt-shape
  (nlt-circle radius)
  (nlt-rect width height)
  (nlt-dot))

(ert-deftest nl-prelude-defdata-constructor-and-type ()
  (let ((c (nlt-circle 3.0)))
    (should (nlt-shape-p c))
    (should (nlt-circle-p c))
    (should-not (nlt-rect-p c))
    (should (equal (nlt-circle-radius c) 3.0))))

(ert-deftest nl-prelude-defdata-multi-field-accessors ()
  (let ((r (nlt-rect 2 5)))
    (should (equal (nlt-rect-width r) 2))
    (should (equal (nlt-rect-height r) 5))))

(ert-deftest nl-prelude-defdata-zero-field-variant ()
  (should (nlt-dot-p (nlt-dot)))
  (should (nlt-shape-p (nlt-dot))))

(ert-deftest nl-prelude-defdata-accessor-type-error ()
  (should-error (nlt-circle-radius (nlt-rect 1 2)) :type 'nl-type-error)
  (should-error (nlt-circle-radius 42) :type 'nl-type-error))

(ert-deftest nl-prelude-defdata-predicates-reject-other-values ()
  (should-not (nlt-shape-p (vector 'nl--data 'nlt-other 'nlt-circle 1)))
  (should-not (nlt-shape-p [1 2 3]))
  (should-not (nlt-shape-p nil))
  (should-not (nlt-circle-p (nl-ok 1))))

(ert-deftest nl-prelude-match-dispatches-all-variants ()
  (let ((area (lambda (s)
                (nl-match s
                  ((nlt-circle r) (* 3 r r))
                  ((nlt-rect w h) (* w h))
                  ((nlt-dot) 0)))))
    (should (equal (funcall area (nlt-circle 2)) 12))
    (should (equal (funcall area (nlt-rect 3 4)) 12))
    (should (equal (funcall area (nlt-dot)) 0))))

(ert-deftest nl-prelude-match-missing-variant-is-expansion-error ()
  (should-error (macroexpand '(nl-match s
                                ((nlt-circle r) r)
                                ((nlt-dot) 0)))))

(ert-deftest nl-prelude-match-wildcard-skips-exhaustiveness ()
  (let ((f (lambda (s)
             (nl-match s
               ((nlt-circle r) r)
               (_ 'other)))))
    (should (equal (funcall f (nlt-circle 9)) 9))
    (should (eq (funcall f (nlt-rect 1 2)) 'other))
    (should (eq (funcall f (nlt-dot)) 'other))))

(ert-deftest nl-prelude-match-arity-mismatch-is-expansion-error ()
  (should-error (macroexpand '(nl-match s
                                ((nlt-circle r extra) r)
                                ((nlt-rect w h) w)
                                ((nlt-dot) 0))))
  (should-error (macroexpand '(nl-match s
                                ((nlt-circle) 1)
                                ((nlt-rect w h) w)
                                ((nlt-dot) 0)))))

(nl-defdata nlt-color (nlt-red) (nlt-blue))

(ert-deftest nl-prelude-match-mixed-types-is-expansion-error ()
  (should-error (macroexpand '(nl-match s
                                ((nlt-circle r) r)
                                ((nlt-red) 0)))))

(ert-deftest nl-prelude-match-runtime-error-on-foreign-value ()
  "Exhaustive over the declared type, but handed a different value."
  (should-error (nl-match (nlt-red)
                  ((nlt-circle r) r)
                  ((nlt-rect w h) w)
                  ((nlt-dot) 0))
                :type 'nl-match-error))

(ert-deftest nl-prelude-match-unregistered-falls-back-unchecked ()
  "Unknown variant heads skip exhaustiveness (non-strict) but still
dispatch on the variant tag at runtime."
  (let ((ghost (vector 'nl--data 'nlt-ghost 'nlt-ghost-a 7)))
    (should (equal (nl-match ghost
                     ((nlt-ghost-a x) x)
                     (_ 'other))
                   7))
    (should-error (nl-match (nlt-dot)
                    ((nlt-ghost-a x) x))
                  :type 'nl-match-error)))

(ert-deftest nl-prelude-match-unregistered-under-strict-errors ()
  (let ((nl--strict t))
    (should-error (macroexpand '(nl-match s ((nlt-ghost-b x) x))))))

(ert-deftest nl-prelude-match-underscore-fields-not-bound ()
  (let ((r (nlt-rect 2 5)))
    (should (equal (nl-match r
                     ((nlt-circle _r) 'circle)
                     ((nlt-rect _w h) h)
                     ((nlt-dot) 'dot))
                   5))))

(ert-deftest nl-prelude-defdata-reregistration-replaces-variants ()
  (eval '(nl-defdata nlt-temp (nlt-temp-a x) (nlt-temp-b)))
  (eval '(nl-defdata nlt-temp (nlt-temp-a x)))
  ;; nlt-temp-b is no longer part of the type: matching only nlt-temp-a
  ;; must now be exhaustive.
  (should (equal (eval '(nl-match (nlt-temp-a 1) ((nlt-temp-a x) x)))
                 1)))

;;; nl-defmacro auto-gensym (Phase 2a) --------------------------------

;; NOTE: the host Emacs reader needs the `#' escaped (`v\#'); the
;; NeLisp standalone reader accepts both `v#' and `v\#'.
(nl-defmacro nlt-twice (form)
  `(let ((v\# ,form))
     (list v\# v\#)))

(defun nl-prelude-test--tree-member (needle tree)
  "Return non-nil when symbol NEEDLE occurs anywhere in TREE."
  (cond ((eq tree needle) t)
        ((consp tree) (or (nl-prelude-test--tree-member needle (car tree))
                          (nl-prelude-test--tree-member needle (cdr tree))))
        (t nil)))

(ert-deftest nl-prelude-auto-gensym-no-capture ()
  "User binding named like the template variable is not captured."
  (let ((v 1))
    (should (equal (nlt-twice (+ v 1)) '(2 2)))))

(ert-deftest nl-prelude-auto-gensym-renames-in-expansion ()
  (should-not (nl-prelude-test--tree-member 'v\# (macroexpand '(nlt-twice x)))))

(ert-deftest nl-prelude-auto-gensym-consistent-within-expansion ()
  (let* ((e (macroexpand '(nlt-twice x)))
         (bound (car (car (nth 1 e))))       ; (let ((SYM x)) (list SYM SYM))
         (body (nth 2 e)))
    (should (eq bound (nth 1 body)))
    (should (eq bound (nth 2 body)))))

(ert-deftest nl-prelude-auto-gensym-unique-across-expansions ()
  (let ((s1 (car (car (nth 1 (macroexpand '(nlt-twice x))))))
        (s2 (car (car (nth 1 (macroexpand '(nlt-twice x)))))))
    (should-not (eq s1 s2))))

(nl-defmacro nlt-swap-order (a b)
  `(let ((x\# ,a) (y\# ,b))
     (list y\# x\#)))

(ert-deftest nl-prelude-auto-gensym-distinct-names-distinct-gensyms ()
  (should (equal (nlt-swap-order 1 2) '(2 1)))
  (let* ((e (macroexpand '(nlt-swap-order 1 2)))
         (sx (car (car (nth 1 e))))
         (sy (car (nth 1 (nth 1 e)))))
    (should-not (eq sx sy))))

;;; nl-loop / nl-recur (Phase 2a) -------------------------------------

(ert-deftest nl-prelude-loop-doc-example ()
  (should (equal (nl-loop ((acc 0) (n 100))
                   (if (zerop n)
                       acc
                     (nl-recur (+ acc n) (1- n))))
                 5050)))

(ert-deftest nl-prelude-loop-constant-stack ()
  "100k iterations must not grow the stack (while-based expansion)."
  (should (equal (nl-loop ((n 100000) (acc 0))
                   (if (zerop n) acc (nl-recur (1- n) (1+ acc))))
                 100000)))

(defun nl-prelude-test--naive-self-recursion (n acc)
  "Count N recursive calls into ACC without `nl-recur'."
  (if (zerop n)
      acc
    (nl-prelude-test--naive-self-recursion (1- n) (1+ acc))))

(ert-deftest nl-prelude-naive-self-recursion-overflows-at-1e6 ()
  "The against-the-bug control must exhaust the real evaluator stack."
  (should-error (nl-prelude-test--naive-self-recursion 1000000 0)))

(ert-deftest nl-prelude-loop-1e6-does-not-overflow ()
  "The same count through `nl-recur' must complete in constant stack."
  (should (equal (nl-loop ((n 1000000) (acc 0))
                   (if (zerop n) acc (nl-recur (1- n) (1+ acc))))
                 1000000)))

(ert-deftest nl-prelude-loop-simultaneous-rebinding ()
  "nl-recur rebinds all variables from the pre-recur values."
  (should (equal (nl-loop ((a 0) (b 1) (i 0))
                   (if (= i 5) a (nl-recur b (+ a b) (1+ i))))
                 5)))

(ert-deftest nl-prelude-loop-no-recur-returns-body-value ()
  (should (equal (nl-loop ((x 7)) (* x 2)) 14)))

(ert-deftest nl-prelude-loop-tail-positions ()
  (should (equal (nl-loop ((n 3) (out nil))
                   (cond ((zerop n) out)
                         (t (nl-recur (1- n) (cons n out)))))
                 '(1 2 3)))
  (should (equal (nl-loop ((n 4))
                   (let ((done (< n 2)))
                     (if done 'done (nl-recur (- n 2)))))
                 'done))
  (should (equal (nl-loop ((n 2))
                   (progn 'ignored
                          (if (zerop n) 'end (nl-recur (1- n)))))
                 'end)))

(ert-deftest nl-prelude-loop-nested-inner-recur ()
  "nl-recur inside a nested nl-loop targets the inner loop."
  (should (equal (nl-loop ((i 2) (total 0))
                   (if (zerop i)
                       total
                     (nl-recur (1- i)
                               (+ total
                                  (nl-loop ((j 3) (s 0))
                                    (if (zerop j) s (nl-recur (1- j) (+ s j))))))))
                 12)))

(ert-deftest nl-prelude-recur-outside-loop-is-expansion-error ()
  (should-error (macroexpand '(nl-recur 1))))

(ert-deftest nl-prelude-recur-non-tail-is-expansion-error ()
  (should-error (macroexpand '(nl-loop ((n 1)) (1+ (nl-recur 0)))))
  (should-error (macroexpand '(nl-loop ((n 1))
                                (if (nl-recur 0) 'a 'b)))))

(ert-deftest nl-prelude-recur-arity-is-expansion-error ()
  (should-error (macroexpand '(nl-loop ((a 1) (b 2))
                                (nl-recur 1)))))

(ert-deftest nl-prelude-loop-bad-binding-is-expansion-error ()
  (should-error (macroexpand '(nl-loop (a) a)))
  (should-error (macroexpand '(nl-loop ((a 1 2)) a))))

;;; nl-trampoline / nl-bounce (Doc 198 Phase 2) ------------------------

(defun nl-prelude-test--naive-even-p (n)
  "Return whether N is even using unprotected mutual recursion."
  (if (zerop n) t (nl-prelude-test--naive-odd-p (1- n))))

(defun nl-prelude-test--naive-odd-p (n)
  "Return whether N is odd using unprotected mutual recursion."
  (if (zerop n) nil (nl-prelude-test--naive-even-p (1- n))))

(defun nl-prelude-test--bouncing-even-p (n)
  "Return whether N is even, bouncing to the odd predicate."
  (if (zerop n)
      t
    (nl-bounce #'nl-prelude-test--bouncing-odd-p (1- n))))

(defun nl-prelude-test--bouncing-odd-p (n)
  "Return whether N is odd, bouncing to the even predicate."
  (if (zerop n)
      nil
    (nl-bounce #'nl-prelude-test--bouncing-even-p (1- n))))

(ert-deftest nl-prelude-naive-mutual-recursion-overflows-at-1e6 ()
  "The mutual-recursion control must exhaust the evaluator stack."
  (should-error (nl-prelude-test--naive-even-p 1000000)))

(ert-deftest nl-prelude-trampoline-mutual-recursion-1e6 ()
  "One million explicit bounces must complete in constant stack."
  (should (eq (nl-trampoline #'nl-prelude-test--bouncing-even-p 1000000)
              t))
  (should (eq (nl-trampoline #'nl-prelude-test--bouncing-even-p 999999)
              nil)))

(ert-deftest nl-prelude-trampoline-returns-function-values-unchanged ()
  "A genuine function result is not mistaken for a bounce sentinel."
  (let* ((value (lambda () 'business-value))
         (result (nl-trampoline (lambda () value))))
    (should (eq result value))
    (should (eq (funcall result) 'business-value))))

(ert-deftest nl-prelude-strict-flag-roundtrip ()
  "The nl-strict macro sets and clears `nl--strict'."
  (let ((old nl--strict))
    (unwind-protect
        ;; nl-strict is a top-level declaration; `eval' models that
        ;; (inline in a function body, `eval-and-compile' fires during
        ;; macroexpansion of the whole body, before any `should' runs).
        (progn (eval '(nl-strict t))
               (should nl--strict)
               (eval '(nl-strict nil))
               (should-not nl--strict))
      (setq nl--strict old))))

(ert-deftest nl-prelude-strict-p-accessor-roundtrip ()
  "`nl-strict-p' reflects the flag set by eval'd (nl-strict ...) forms."
  (let ((old nl--strict))
    (unwind-protect
        ;; nl-strict is a top-level declaration; `eval' models that
        ;; (see nl-prelude-strict-flag-roundtrip above).
        (progn (eval '(nl-strict t))
               (should (nl-strict-p))
               (eval '(nl-strict nil))
               (should-not (nl-strict-p)))
      (setq nl--strict old))))

;;; Review-fix regressions (2026-08-15 code review) --------------------

(ert-deftest nl-prelude-loop-shadowed-variable-still-advances ()
  "An inner `let' shadowing a loop variable must not break nl-recur.
Pre-fix this looped forever: the emitted setq mutated the shadow."
  (should (equal (nl-loop ((x 0))
                   (let ((x (1+ x)))
                     (if (> x 5) x (nl-recur x))))
                 6)))

(ert-deftest nl-prelude-loop-empty-and-or-tail ()
  "Zero-argument (and)/(or) in tail position stay valid Elisp.
Pre-fix they were rewritten to the bogus (and and) / (or or)."
  (should (eq (nl-loop ((x 0)) (and)) t))
  (should (eq (nl-loop ((x 0)) (or)) nil)))

(ert-deftest nl-prelude-defdata-cross-type-variant-collision-errors ()
  "A variant name claimed by another type is a loud error, not a
silent registry/defun clobber."
  (should-error (eval '(nl-defdata nlt-other-shape (nlt-circle r)))))

(ert-deftest nl-prelude-defdata-variant-naming-type-errors ()
  "A variant sharing the type name would clobber the TYPE-p predicate."
  (should-error (macroexpand '(nl-defdata nlt-selfname (nlt-selfname a)))))

(nl-defmacro nlt-quoted-tag (x)
  `(list 'tag\# ,x))

(ert-deftest nl-prelude-auto-gensym-preserves-quoted-literals ()
  "Trailing-# symbols under `quote' are data, not template variables."
  (should (equal (nlt-quoted-tag 5) (list 'tag\# 5)))
  (let ((e (macroexpand '(nlt-quoted-tag 5))))
    (should (nl-prelude-test--tree-member 'tag\# e))))

(ert-deftest nl-prelude-match-warning-fails-compile-gate ()
  "Duplicate-clause warnings must trip byte-compile-error-on-warn.
Skipped (trivially true) on standalone, which has no byte compiler."
  (if (not (fboundp 'byte-compile))
      (should t)
    (let ((byte-compile-error-on-warn t))
      ;; byte-compile logs the error and returns nil (it does not
      ;; signal); batch-byte-compile turns that into a non-zero exit.
      (should-not
       (functionp
        (byte-compile '(lambda (s)
                        (nl-match s
                          ((nlt-circle r) r)
                          ((nlt-circle r2) r2)
                          ((nlt-rect w h) w)
                          ((nlt-dot) 0))))))
      ;; Control: the clean form still compiles.
      (should (functionp (byte-compile '(lambda (s)
                               (nl-match s
                                 ((nlt-circle r) r)
                                 ((nlt-rect w h) (+ w h))
                                 ((nlt-dot) 0)))))))))

(provide 'nl-prelude-test)

;;; nl-prelude-test.el ends here
