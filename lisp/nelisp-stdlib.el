;;; nelisp-stdlib.el --- Sweep 9 S0 Elisp stdlib (Rust→Elisp migration)  -*- lexical-binding: t; -*-

(defun identity (x) x)
(defun null (x) (eq x nil))
(defun not (x) (eq x nil))
(defun 1+ (x) (+ x 1))
(defun 1- (x) (- x 1))

;; Rust-min batch 6q (2026-05-06): `atom' / `arrayp' / `sequencep'
;; migrated from Rust to elisp.  Each was a 1-line `bi_predicate'
;; dispatch entry composing already-existing primitives — the elisp
;; versions are direct transliterations.  CharTable / BoolVector
;; legacy variants are folded out of the array/sequence union: the
;; CharTable variant has no live constructors (Rust-min batch 5b),
;; and BoolVector instances surface as plain `Sexp::Vector' through
;; the elisp `bool-vector' constructor (also batch 5b), so
;; `(vectorp v)' covers both.

;; Rust-min batch 6u (2026-05-06): predicate bundle migrated from
;; Rust to elisp on top of a new `type-of' primitive (see
;; build-tool/src/eval/builtins.rs `bi_type_of').  Each previous
;; `bi_predicate' dispatch arm collapses to a 1-line `(eq (type-of
;; x) 'TAG)' form below.  `eq' / `equal' / `functionp' kept in Rust:
;; eq + equal need cycle-safe Sexp internals, functionp is on the
;; HOF dispatch hot path.  `atom' / `arrayp' / `sequencep' (= batch
;; 6q) compose these primitives further.

(defun consp (x)    (eq (type-of x) 'cons))
(defun symbolp (x)  (eq (type-of x) 'symbol))
(defun stringp (x)  (eq (type-of x) 'string))
(defun integerp (x) (eq (type-of x) 'integer))
(defun floatp (x)   (eq (type-of x) 'float))
(defun vectorp (x)  (eq (type-of x) 'vector))

(defun listp (x)
  "Return t if X is a list (= nil or a cons cell)."
  (let ((tag (type-of x)))
    (or (eq tag 'cons) (eq x nil))))

(defun numberp (x)
  "Return t if X is a number (= integer or float)."
  (let ((tag (type-of x)))
    (or (eq tag 'integer) (eq tag 'float))))

(defun atom (x)
  "Return t if X is not a cons cell."
  (not (consp x)))

(defun arrayp (x)
  "Return t if X is an array (= string or vector)."
  (or (stringp x) (vectorp x)))

(defun sequencep (x)
  "Return t if X is a sequence (= nil, cons, string, or vector)."
  (or (null x) (consp x) (stringp x) (vectorp x)))

;; Rust-min batch 6l (2026-05-06): `mod' migrated from Rust to
;; elisp.  Reproduces the previous `bi_mod' contract exactly:
;;   r = euclidean_mod(a, |b|)   (always >= 0)
;;   result = sign(b) * r
;; Built from `/' (NeLisp int-div = trunc toward zero) plus a
;; sign-adjust step.  This matches NeLisp's prior Rust semantics,
;; not host Emacs's pure floor-mod — the two differ only when
;; sign(a) != sign(b), and a codebase grep confirmed no extant
;; caller passes a negative divisor.
(defun mod (a b)
  (when (= b 0) (error "Arithmetic error"))
  (let* ((n (if (< b 0) (- b) b))
         (r (- a (* n (/ a n)))))
    (when (< r 0) (setq r (+ r n)))
    (if (< b 0) (- r) r)))

;; Rust-min batch 6j (2026-05-06): variadic bitwise fold via 2-arg
;; primitives `nelisp--logior2' / -logand2 / -logxor2.  Elisp
;; folds over INTS with the identity element of each operation
;; (= 0 for OR/XOR, -1 for AND).  54 callers in the substrate are
;; all exactly 2-arg, so the fold path is fast in practice.
(defun logior (&rest ints)
  (let ((acc 0) (cur ints))
    (while cur
      (setq acc (nelisp--logior2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

(defun logand (&rest ints)
  (let ((acc -1) (cur ints))
    (while cur
      (setq acc (nelisp--logand2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

(defun logxor (&rest ints)
  (let ((acc 0) (cur ints))
    (while cur
      (setq acc (nelisp--logxor2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

;; nelisp-stdlib.el ends here
