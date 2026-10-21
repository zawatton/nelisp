;;; nelisp-aot-differential-cases.el --- programs for the AOT/interpreter diff  -*- lexical-binding: t; -*-

;;; Commentary:

;; Builds the population `test/nelisp-aot-differential-driver.el' runs.
;;
;; Every fixture in `nelisp-native-load-fixtures' states its own expected
;; answer, so it can only catch a shape someone thought of first.  That
;; found real defects one at a time and missed the rest: `if' arms that
;; disagree on a slot's representation, a `while' body that never runs, a
;; parameter used in arithmetic.  Each was found by hand, and each was
;; the same underlying defect wearing a different construct.
;;
;; So enumerate instead.  A frame slot holds one machine word and the
;; compiler tracks, per slot, whether that word is a raw integer or a
;; Sexp pointer; the defects live where two paths reach the same slot
;; with different answers.  This crosses the value producers that differ
;; in representation against the control shapes that have a path which
;; does not run, and the driver compares the native answer against the
;; interpreter's rather than against a number written here.

;;; Code:

(require 'nelisp-artifact)
(require 'nelisp-aot-compiler)

(defconst nelisp-aot-differential-producers
  '(("zero"  "0")
    ("aref"  "(aref v 0)")
    ("arith" "(+ i 1)")
    ("len"   "(length v)")
    ("deep"  "(aref (aref w 1) 0)"))
  "(TAG . FORM) for values that differ in how they are represented.
`zero' and `arith' produce raw machine integers, `aref' and `deep' a
pointer to a Sexp the dispatcher returned, and `len' a dispatcher result
that is a small integer -- the case where the two are hardest to tell
apart by looking at the answer.")

(defconst nelisp-aot-differential-bounds
  '(("b0" "0") ("b1" "1") ("b3" "3") ("bn" "n"))
  "Loop bounds.  `b0' is the zero-iteration path a `while' body always has.")

(defconst nelisp-aot-differential-tag-values
  '(("nil" "nil") ("t" "t") ("fix" "(length v)") ("elt" "(aref v 0)")
    ("cons" "(list 1 2)") ("str" "\"ab\"") ("sym" "'foo")
    ("vec" "v") ("deep" "(aref w 1)") ("flt" "1.5"))
  "(TAG . FORM) for values of every type the tag predicates tell apart.
Doc 224 CG5 fuses a predicate's (= (sexp-tag X) K) in a test into a
pointer test and a tag-byte compare; each of these values goes through
every fused predicate and the interpreter answers for comparison.  nil, t
and the cons came back once Doc 229 made them the Lisp objects and the
cons constant loadable.")

;; Doc 229: Lisp values in native artifacts follow GNU's Lisp_Object
;; rules.  Each entry is (STAGE KIND SOURCE ARGS).  KIND `control' marks a
;; program the 9d5f4a515 compiler gets wrong (a wrong answer, a crash or
;; a refused artifact), `guard' one it already gets right and must keep.
;; STAGE says which step of Doc 229 makes the controls pass: 1 nil, t,
;; booleans, truth tests and variables, `loader' quoted list constants, 2
;; numbers (arithmetic and comparison on boxed numbers, float literals,
;; floor, mod), `apply' funcall and apply, `const' quoted constants,
;; `unit' same-unit calls.  SOURCE may define, after the
;; defun the case calls, a helper it calls.  The whole list runs through
;; tools/ai/codegen-lispvalue.py against both compilers.
(defconst nelisp-aot-differential-lisp-value-cases
  '((1 control "(defun lv-if-param (a) (if a 1 2))" (nil t 0 "s"))
    (1 control "(defun lv-when-param (a) (when a 5))" (nil 1))
    (1 control "(defun lv-unless-param (a) (unless a 5))" (nil 1))
    (1 control "(defun lv-cond-null (a) (cond ((null a) 1) (a 2) (t 3)))" (nil 1))
    (1 control "(defun lv-cond-value (a) (cond ((consp a) (car a)) (t a)))" ((nil) (1) nil 2))
    (1 control "(defun lv-and-value (a) (and a (< 0 1)))" (nil 1))
    (1 control "(defun lv-or-value (a) (or a 7))" (nil 3 t))
    (1 control "(defun lv-or-test (a) (if (or (null a) (eq a 3)) 1 2))" (nil 3 4))
    (1 control "(defun lv-cmp-return (a) (< a 3))" (0 5))
    (1 control "(defun lv-cmp-let-symbolp (a) (let ((x (< a 2))) (if (symbolp x) 1 2)))" (1 5))
    (1 control "(defun lv-cmp-in-list (a) (list (< a 2) (> a 2)))" (1 5))
    (1 control "(defun lv-eq-nil (a) (eq a nil))" (nil 0 t))
    (1 control "(defun lv-eq-t-var (a) (let ((x t)) (if (eq a x) 1 2)))" (t nil))
    (1 control "(defun lv-car-if (a) (if (car a) 1 2))" ((nil) (1)))
    (1 control "(defun lv-aref-nil-if (a) (if (aref (vector nil 1) a) 1 2))" (0 1))
    (1 control "(defun lv-aref-arg-if (a) (if (aref a 0) 1 2))" ([nil] [t] [0]))
    (1 control "(defun lv-null-aref-nil (a) (let ((x (aref (vector nil 1) a))) (if (null x) 1 2)))" (0 1))
    (1 control "(defun lv-length-nil (a) (let ((x nil)) (length (list x x a))))" (0))
    (1 control "(defun lv-null-zero (a) (let ((x 0)) (if (null x) 1 2)))" (0))
    (1 control "(defun lv-symbolp-t-elt (a) (let ((x (aref (vector t 1) a))) (if (symbolp x) 1 2)))" (0 1))
    (1 control "(defun lv-symbolp-t-test (a) (if (symbolp a) 1 2))" (nil t foo 0))
    (1 control "(defun lv-vector-bools (a) (vector nil t (< a 1)))" (0 5))
    (1 control "(defun lv-while-value (a) (let ((i 0)) (while (< i a) (setq i (+ i 1)))))" (0 3))
    (1 control "(defun lv-setq-nil (a) (let ((x 5)) (when (< a 1) (setq x nil)) x))" (0 3))
    (1 guard "(defun lv-big-return (a) (+ (length a) 5000))" ("ab"))
    (1 guard "(defun lv-big-raw-return (a) (* a 1000))" (5))
    (1 control "(defun lv-mixed-if-value (a) (if (< a 1) 5000 a))" (0 3))
    (1 control "(defun lv-let-alias (a) (let ((x (car a)) (y (cdr a))) (list x y)))" ((1 . 2)))
    (1 control "(defun lv-let-alias2 (a) (let ((x (car a))) (length a) x))" ((5 6 7)))
    (1 control "(defun lv-let-star-alias (a) (let* ((x (car a)) (y (cdr a))) (list x y)))" ((1 . 2)))
    (1 control "(defun lv-loop-join (a) (let ((x (aref a 0)) (i 0)) (while (< i (aref a 1)) (setq x 5) (setq i (+ i 1))) x))" ([7 0] [7 2]))
    (1 control "(defun lv-if-join (a) (let ((x (aref a 0))) (if (< (aref a 1) 1) (setq x 5)) x))" ([7 0] [7 2]))
    (1 control "(defun lv-loop-bool (a) (let ((x 0) (i 0)) (while (< i a) (setq x (< i 1)) (setq i (+ i 1))) x))" (0 1 3))
    (1 control "(defun lv-helper-caller (a) (lv-helper a)) (defun lv-helper (a) (if (null a) t nil))" (nil 1))
    (1 guard "(defun lv-let-literals (a) (let ((x 'foo) (y \"b\")) (list x y a)))" (0))
    (1 guard "(defun lv-nil-return (a) nil)" (0))
    (1 guard "(defun lv-t-return (a) t)" (0))
    (1 guard "(defun lv-not-param (a) (not a))" (nil t 0))
    (1 guard "(defun lv-null-param-if (a) (if (null a) 1 2))" (nil t 0))
    (1 guard "(defun lv-symbolp-param (a) (symbolp a))" (nil t foo 0))
    (1 guard "(defun lv-consp-test (a) (if (consp a) (car a) 0))" ((5) 3 nil))
    (loader control "(defun lv-quoted-list-memq (a) (if (memq a '(1 2)) 1 2))" (1 3))
    (loader control "(defun lv-list-const (a) (list a '(1 \"two\" (3 . 4)) \"five\"))" (0 1))
    (loader control "(defun lv-list-12 (a) (let ((x (list 1 2))) (if (consp x) (car x) 0)))" (0))
    (2 control "(defun lv-plus-float (a) (+ a 1))" (1 1.5))
    (2 control "(defun lv-plus-aref-float (a) (+ (aref a 0) 1))" ([1] [1.5]))
    (2 control "(defun lv-lss-float (a) (< a 2))" (1 1.5))
    (2 control "(defun lv-loop-float (a) (let ((s 0) (i 0)) (while (< i 3) (setq s (+ s a)) (setq i (+ i 1))) s))" (1 0.5))
    (2 control "(defun lv-mul-float (a) (* a 2))" (1.25 3))
    (2 control "(defun lv-sub-float (a) (- 10 a))" (2.5 3))
    (2 control "(defun lv-div-float (a) (/ a 2))" (-7 7 7.0))
    (2 control "(defun lv-abs-float (a) (abs a))" (-1.5 -2 3))
    (2 control "(defun lv-min-mixed (a) (min a 2))" (1.5 3 2))
    (2 control "(defun lv-floor-float (a) (floor a))" (1.5 -1.5 2))
    (2 control "(defun lv-mod-neg (a) (mod a 3))" (-7 7))
    (2 control "(defun lv-mod-raw (a) (let ((x -7)) (mod x a)))" (3 -3))
    (2 guard "(defun lv-rem-neg (a) (% a 3))" (-7 7))
    (2 control "(defun lv-float-lit-add (a) (+ a 0.5))" (1 2.25))
    (2 control "(defun lv-float-lit-list (a) (list 1.5 -0.25 a))" (0))
    (2 control "(defun lv-float-lit-cmp (a) (if (< a 2.5) 1 2))" (2 3))
    ;; funcall and apply (GNU Ffuncall, Fapply): refused on 9d5f4a515, the
    ;; nelisp_aot_funcall*/apply* entries being unbridged.
    (apply control "(defun lv-funcall-sym (a) (funcall 'car a))" ((5 6)))
    (apply control "(defun lv-funcall-fn (a) (funcall #'car a))" ((5 6)))
    (apply control "(defun lv-funcall-arg (a) (funcall a 41))" (1+ 1-))
    (apply control "(defun lv-funcall-2 (a) (funcall '+ a 1))" (41 1.5))
    (apply control "(defun lv-funcall-3 (a) (funcall 'list a 1 2))" (0))
    (apply control "(defun lv-funcall-n (a) (funcall 'list a 1 2 3 4))" (0))
    (apply control "(defun lv-apply-list (a) (apply '+ a))" ((1 2 3) (1.5 2)))
    ;; Not -7.5: GNU's optimizer makes this (mod a 3), and the AOT
    ;; dispatcher's float mod is a known runtime defect (gc-port S5).
    (apply control "(defun lv-apply-spread (a) (apply 'mod (list a 3)))" (-7 7))
    (apply control "(defun lv-apply-n (a) (apply 'list 1 a (list 2 3)))" (9))
    ;; Quoted constants (GNU's constant vector): one object for every
    ;; evaluation, so eq between two of them is t.  On 9d5f4a515 a string or
    ;; a vector is rebuilt per evaluation and a quoted list is refused.
    (const control "(defun lv-const-str (a) (let ((r nil) (i 0)) (while (< i 2) (setq r (cons \"abc\" r)) (setq i (+ i 1))) (eq (car r) (car (cdr r)))))" (0))
    (const control "(defun lv-const-vec (a) (let ((r nil) (i 0)) (while (< i 2) (setq r (cons [1 2] r)) (setq i (+ i 1))) (eq (car r) (car (cdr r)))))" (0))
    (const control "(defun lv-const-list (a) (let ((r nil) (i 0)) (while (< i 2) (setq r (cons '(1 2) r)) (setq i (+ i 1))) (eq (car r) (car (cdr r)))))" (0))
    (const control "(defun lv-const-loop (a) (let ((x nil) (same t) (i 0)) (while (< i 3) (let ((y '(p q))) (if (and x (not (eq x y))) (setq same nil)) (setq x y)) (setq i (+ i 1))) same))" (0))
    ;; A constructor stays fresh: two (list 1 2) are two objects (refused
    ;; on 9d5f4a515, the folded list being a cons literal).
    (const control "(defun lv-const-fresh (a) (let ((x (list 1 2)) (y (list 1 2))) (list (eq x y) (eq (car x) (car y)))))" (0))
    (const control "(defun lv-lone-string (a) \"abc\")" (0))
    ;; Same-unit calls: the callee reads the dispatcher boundary from hidden
    ;; slots only the loader's trampoline filled, so a helper that calls the
    ;; runtime or reads a constant crashed (on 9d5f4a515 too).
    ;; GNU bytecode.c Bplus/Bdiff/Bnegate/Bmult/Bquo: overflowing fixnum
    ;; results delegate to the runtime, including arithmetic folded in ENV.
    ;; All six controls fail on the measured pre-S4.4 compiler; small inputs
    ;; in the same groups guard the existing nonoverflow path and effects.
    (overflow control "(defun lv-overflow-add-effects (a) (let ((trace 0)) (list (+ (progn (setq trace (+ trace 1)) a) (progn (setq trace (+ (* trace 10) 2)) 1)) trace)))" (0 2305843009213693951))
    (overflow control "(defun lv-overflow-sub (a) (- a 1))" (0 -2305843009213693951 -2305843009213693952 2305843009213693951))
    (overflow control "(defun lv-overflow-neg (a) (- a))" (0 -2305843009213693952 2305843009213693951))
    (overflow control "(defun lv-overflow-mul (a) (* a a))" (0 1 -1 1518500249 1518500250 4294967296 2305843009213693951 -2305843009213693952))
    (overflow control "(defun lv-overflow-known (a) (let ((x 4294967296)) (* x x)))" (0))
    (overflow control "(defun lv-overflow-div (a) (/ a -1))" (0 -2305843009213693952 2305843009213693951))
    (unit control "(defun lv-unit-call (a) (lv-unit-car a)) (defun lv-unit-car (a) (car (list a 2)))" (7))
    (unit control "(defun lv-unit-const (a) (length (lv-unit-k a))) (defun lv-unit-k (a) '(1 2 3))" (0))
    (unit control "(defun lv-unit-chain (x) (+ (lv-unit-b x) 1)) (defun lv-unit-b (x) (length (lv-unit-c x))) (defun lv-unit-c (x) (list x x x))" (1))
    (unit control "(defun lv-unit-len (l) (if l (+ 1 (lv-unit-len (cdr l))) 0))" ((1 2 3) nil))
    (unit control "(defun lv-unit-opt (a) (lv-unit-o a)) (defun lv-unit-o (a &optional b) (list a b))" (1))
    ;; A &rest callee: its list was built by `nelisp_aot_listn', which the
    ;; reader cannot bridge, so the artifact was refused (on 9d5f4a515 too).
    (unit control "(defun lv-unit-rest (a) (lv-unit-r a 2 3)) (defun lv-unit-r (a &rest xs) (cons a xs))" (1))
    (unit control "(defun lv-unit-rest0 (a) (lv-unit-r0 a)) (defun lv-unit-r0 (a &rest xs) (list a xs))" (1))
    (unit control "(defun lv-unit-rl (a) (+ (lv-unit-rn a a a) 1)) (defun lv-unit-rn (&rest xs) (length xs))" (1))
    (unit control "(defun lv-unit-fresh (a) (eq (lv-unit-fr a 2) (lv-unit-fr a 2))) (defun lv-unit-fr (a &rest xs) xs)" (1))
    ;; Arguments each in a slot of their own, left to right: a value one
    ;; argument left in OUT survives the next argument and the callee.
    (unit control "(defun lv-unit-two (x) (lv-unit-t2 (list x) (list (+ x 1)))) (defun lv-unit-t2 (a b) (list a b))" (1))
    (unit control "(defun lv-unit-cc (x) (lv-unit-c2 (cons x nil) (cons (+ x 1) nil))) (defun lv-unit-c2 (a b) (cons b a))" (1))
    (unit control "(defun lv-unit-mix (x) (lv-unit-m2 (lv-unit-mq x) (lv-unit-mq (+ x 1)))) (defun lv-unit-m2 (a b) (list a b)) (defun lv-unit-mq (x) (list x))" (1))
    (unit control "(defun lv-unit-ord (x) (lv-unit-o2 x (setq x 5))) (defun lv-unit-o2 (a b) (list a b))" (1))
    ;; A callee with no internal entry is called through the dispatcher.
    (unit control "(defun lv-unit-seven (a) (lv-unit-s7 a 1 2 3 4 5 6)) (defun lv-unit-s7 (a b c d e f g) (list a g))" (1))
    ;; The word ABI preserves immediates and uses the active mirror environment.
    (word control "(defun lv-word-equal (a) (equal a (list 1 2)))" ((1 2) (1 3)))
    (word control "(defun lv-word-eql (a) (eql a 7))" (7 8))
    (word control "(defun lv-word-float-mod (a) (mod a 2.0))" (3.5))
    (word guard "(defun lv-word-id (a) a)" (nil t 0 -7 2305843009213693951 -2305843009213693952))
    (word guard "(defun lv-word-return (a) (+ a 1))" (0 7))
    ;; GNU comp.c push_handler / eval.c signal descriptors, Doc 229 S4.5.
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-unit (x) (+ 1 (lv-signal-leaf x)))
(defun lv-signal-leaf (x) (funcall 'car x))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-handler-error (x) (condition-case outer (condition-case inner (funcall 'car x) (t (funcall 'car 9))) (t outer)))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-empty (x) (condition-case err (funcall 'car x) (t)))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-all (x) (condition-case err (funcall 'car x) (t err)))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-catch (x) (condition-case err (funcall 'car x) (error err)))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-stop (x) (let ((value 1)) (condition-case err (progn (funcall 'car x) (setq value 9) value) (t value))))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-nested (x) (condition-case outer (condition-case inner (funcall 'car x) (arith-error 9)) (error outer)))" (3 (4 5)))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-direct (x) (signal 'arith-error (list x 'payload)))" (3 nil))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-inherited (x) (condition-case err (signal 'file-missing (list x 'payload)) (file-error err)))" (3 nil))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-immediates (x) (if x (condition-case err (funcall 'car t) (error err)) (funcall 'car nil)))" (nil t))
    (signal control ";;; -*- lexical-binding: t; -*-
(defun lv-signal-uncaught (x) (condition-case err (funcall 'car x) (arith-error 9)))" (3 (4 5)))
)
  "Doc 229 controls and guards: (STAGE KIND SOURCE ARGS).")

(defvar nelisp-aot-differential-lisp-value-stages '(1 loader 2 apply const unit word overflow signal)
  "Doc 229 stages whose cases join the regular differential corpus.
A stage is added once its implementation lands, so the regular run keeps
measuring only what is supposed to pass.")

(defun nelisp-aot-differential-lisp-value-entries (&optional stages)
  "Return (NAME SOURCE ARGS) for the Doc 229 cases of STAGES (all if t)."
  (let ((out nil))
    (dolist (c nelisp-aot-differential-lisp-value-cases)
      (when (or (eq stages t) (member (nth 0 c) stages))
        (let ((source (nth 2 c)))
          (push (list (symbol-name (nth 1 (car (read-from-string source))))
                      source (nth 3 c))
                out))))
    (nreverse out)))

(defun nelisp-aot-differential--wrap (name body)
  "Return the source of defun NAME whose frame holds BODY."
  (format (concat "(defun %s (n)\n"
                  "  (let ((v (vector 7 8 9))\n"
                  "        (w (vector 1 (vector 7 8 9) 3))\n"
                  "        (i 0)\n"
                  "        (acc 0))\n"
                  "    %s\n"
                  "    acc))\n")
          name body))

(defun nelisp-aot-differential-cases ()
  "Return (NAME SOURCE ARGS) for every generated program.

The test is `(< n 1)' rather than a constant so both paths are reachable
from the argument: called with 0 the first arm runs, with 3 the second.
That is the whole point -- a slot whose representation was recorded by
the arm that did NOT run is only wrong on one of the two calls."
  (let ((cases nil))
    ;; Two-armed shapes: the arms can disagree, so cross the producers.
    (dolist (a nelisp-aot-differential-producers)
      (dolist (b nelisp-aot-differential-producers)
        (dolist (shape
                 (list
                  (cons "if2" (format "(if (< n 1) (setq acc %s) (setq acc %s))"
                                      (cadr a) (cadr b)))
                  (cons "cond2" (format "(cond ((< n 1) (setq acc %s)) (t (setq acc %s)))"
                                        (cadr a) (cadr b)))))
          (let ((name (format "d-%s-%s-%s" (car shape) (car a) (car b))))
            (push (list name
                        (nelisp-aot-differential--wrap name (cdr shape))
                        '(0 3))
                  cases)))))
    ;; One-armed shapes: the path that does not run leaves the slot as it
    ;; was bound, which is the same disagreement with fewer moving parts.
    (dolist (a nelisp-aot-differential-producers)
      (dolist (shape
               (list
                (cons "if1" (format "(if (< n 1) (setq acc %s) nil)" (cadr a)))
                (cons "cond1" (format "(cond ((< n 1) (setq acc %s)))" (cadr a)))
                (cons "and" (format "(and (< n 1) (setq acc %s))" (cadr a)))
                (cons "or" (format "(or (< n 1) (setq acc %s))" (cadr a)))))
        (let ((name (format "d-%s-%s" (car shape) (car a))))
          (push (list name
                      (nelisp-aot-differential--wrap name (cdr shape))
                      '(0 3))
                cases))))
    ;; Type predicates in tests (Doc 224 CG5), one program per value:
    ;; bit k of acc says whether predicate k held, so a wrong tag test on
    ;; any of them shows up as a different answer.
    (dolist (x nelisp-aot-differential-tag-values)
      (let ((name (format "d-tag-%s" (car x)))
            (body "(setq acc 0)")
            (bit 1))
        (dolist (pred '(null consp listp atom symbolp stringp vectorp))
          (setq body (format "%s (if (%s %s) (setq acc (+ acc %d)) nil)"
                             body pred (cadr x) bit)
                bit (* bit 2)))
        (push (list name (nelisp-aot-differential--wrap name (concat "(progn " body ")"))
                    '(0 3))
              cases)))
    ;; Loops, including the bound that makes the body run zero times.
    (dolist (a nelisp-aot-differential-producers)
      (dolist (bound nelisp-aot-differential-bounds)
        (let ((name (format "d-while-%s-%s" (car bound) (car a))))
          (push (list name
                      (nelisp-aot-differential--wrap
                       name
                       (format "(while (< i %s) (setq acc %s) (setq i (+ i 1)))"
                               (cadr bound) (cadr a)))
                      '(0 3))
                cases))))
    (dolist (entry (nelisp-aot-differential-lisp-value-entries
                    nelisp-aot-differential-lisp-value-stages))
      (push entry cases))
    (nreverse cases)))

(defun nelisp-aot-differential-build (dir)
  "Compile every case into DIR and write the manifest the driver reads.

A case the compiler refuses is not a failure: the artifact simply has no
native defun and the caller falls back.  Record that so the driver can
report it apart from a wrong answer, which is the only real failure."
  (make-directory dir t)
  (let ((cases (nelisp-aot-differential-cases))
        (manifest nil)
        (refused 0))
    (dolist (case cases)
      (let* ((name (nth 0 case))
             (source (nth 1 case))
             (el (expand-file-name (concat name ".el") dir))
             (neln (expand-file-name (concat name ".neln") dir))
             (nelisp-aot-compiler--dynamic-user-calls t)
             (native t))
        (with-temp-file el
          (insert source (format "(provide '%s)\n" name)))
        (condition-case err
            (nelisp-artifact-compile-file el neln nil nil nil nil nil 'neln)
          (error (setq native nil)
                 (message "[diff] %s did not compile: %S" name err)))
        (when native
          (let ((nat (plist-get (nelisp-artifact--read-payload neln) :native)))
            (unless (and nat (plist-get nat :defuns))
              (setq native nil))))
        (unless native (setq refused (1+ refused)))
        (push (list name source (nth 2 case) native) manifest)))
    (setq manifest (nreverse manifest))
    (with-temp-file (expand-file-name "cases.el" dir)
      (insert ";; generated by nelisp-aot-differential-build\n")
      (insert "(defvar nelisp-aot-differential-manifest\n  '")
      (prin1 manifest (current-buffer))
      (insert ")\n"))
    ;; The runner needs the count to know how many chunks to expect.  A
    ;; chunk that dies prints nothing, and without knowing how many there
    ;; should be, a run that checked half the population looks identical
    ;; to one that checked all of it -- which is what happened first:
    ;; chunks 5 to 8 were killed and 40 of 90 cases went unreported.
    (with-temp-file (expand-file-name "count.txt" dir)
      (insert (format "%d\n" (length cases))))
    (message "[diff] %d cases, %d without a native defun" (length cases) refused)))

(defun nelisp-aot-differential-main ()
  "Entry point for the make target."
  (nelisp-aot-differential-build
   (or (getenv "NELISP_DIFF_DIR")
       (expand-file-name "target/aot-differential" default-directory))))

(provide 'nelisp-aot-differential-cases)
;;; nelisp-aot-differential-cases.el ends here
