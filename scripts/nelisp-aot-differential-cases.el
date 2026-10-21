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
;; floor, mod).  SOURCE may define, after the defun the case calls, a
;; helper it calls.  The whole list runs through
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
    (2 control "(defun lv-float-lit-cmp (a) (if (< a 2.5) 1 2))" (2 3)))
  "Doc 229 controls and guards: (STAGE KIND SOURCE ARGS).")

(defvar nelisp-aot-differential-lisp-value-stages '(1 loader 2)
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
