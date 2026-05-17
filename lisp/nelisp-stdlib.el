;;; nelisp-stdlib.el --- Sweep 9 S0 Elisp stdlib (Rust→Elisp migration)  -*- lexical-binding: t; -*-

(defun identity (x) x)
(defun null (x) (eq x nil))
(defun not (x) (eq x nil))

;; Rust-min batch 6v (2026-05-06): variadic `+' / `-' / `*' migrated
;; from Rust to elisp, folding over new 2-arg primitives
;; `nelisp--add2' / `nelisp--sub2' / `nelisp--mul2'.  Same pattern
;; as batch 6j (= bitwise variadic fold over `nelisp--logior2'
;; etc.).  Doc 86 §86.1.b (2026-05-10): `/' migrated to elisp on
;; top of the new `nl_jit_float_div' trampoline (jit/float.rs).
;; The upfront-promote-then-trunc-on-all-int semantics (= which
;; previously kept `bi_div' in Rust to avoid step-wise precision
;; loss) are now elisp-native via a 2-pass fold: pass 1 detects
;; any-float, pass 2 does the f64 division through the trampoline.
;;
;; Identity elements: (+) = 0, (-) = 0, (*) = 1.  Unary cases:
;; (- x) = (nelisp--sub2 0 x) = -x; (+ x) and (* x) return x.

(defun + (&rest args)
  (let ((acc 0) (cur args))
    (while cur
      (setq acc (nelisp--add2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

(defun - (&rest args)
  (cond
   ((null args) 0)
   ((null (cdr args)) (nelisp--sub2 0 (car args)))
   (t
    (let ((acc (car args)) (cur (cdr args)))
      (while cur
        (setq acc (nelisp--sub2 acc (car cur)))
        (setq cur (cdr cur)))
      acc))))

(defun * (&rest args)
  (let ((acc 1) (cur args))
    (while cur
      (setq acc (nelisp--mul2 acc (car cur)))
      (setq cur (cdr cur)))
    acc))

;; Doc 86 §86.1.b — `/' (variadic).  Pass 1: detect any Float arg
;; (= drives the int-trunc gate at the end).  Pass 2: f64 division
;; through `nl_jit_float_div' trampoline; division-by-zero check
;; uses `nelisp--num-eq2' (= the same numeric `=' the deleted
;; `bi_div' tested via `vs[0] == 0.0').  1-arg case is reciprocal
;; `(/ x)' = `(1.0 / x)' with int-trunc gating just like n-arg.
;; All-int-trunc uses `(nl-jit-call-i64-i64 "nelisp_jit_add2" 0 X)'
;; (= the as_int coerce idiom from `ash') so `truncate' (which
;; lives in Rust as a separate primitive yet to migrate in §86.1.d)
;; is not on the dependency path here.
(defun / (&rest args)
  (cond
   ((null args) (signal 'wrong-number-of-arguments (list '/ 0)))
   ((null (cdr args))
    ;; 1-arg: reciprocal.
    (let ((x (car args)))
      (when (nelisp--num-eq2 x 0)
        (signal 'arith-error (cons "/" args)))
      (let ((res (nl-jit-call-float-float "nl_jit_float_div" 1.0 x)))
        (if (eq (type-of x) 'float)
            res
          (nl-jit-call-i64-i64 "nelisp_jit_add2" 0 res)))))
   (t
    ;; n-arg: pass 1 detect any-float.
    (let ((all-int t) (cur args))
      (while cur
        (when (eq (type-of (car cur)) 'float)
          (setq all-int nil))
        (setq cur (cdr cur)))
      ;; Pass 2: float-fold (upfront promote = matches deleted bi_div).
      (let ((acc (car args)) (rest (cdr args)))
        (while rest
          (let ((b (car rest)))
            (when (nelisp--num-eq2 b 0)
              (signal 'arith-error (cons "/" args)))
            (setq acc (nl-jit-call-float-float "nl_jit_float_div" acc b))
            (setq rest (cdr rest))))
        (if all-int
            (nl-jit-call-i64-i64 "nelisp_jit_add2" 0 acc)
          acc))))))

(defun 1+ (x) (nelisp--add2 x 1))
(defun 1- (x) (nelisp--sub2 x 1))

;; Rust-min batch 6w (2026-05-06): chained-pairwise variadic
;; comparisons `<' / `>' / `<=' / `>=' / `=' / `/=' migrated from
;; Rust to elisp folds over new 2-arg primitives `nelisp--num-lt2'
;; / `-num-gt2' / `-num-le2' / `-num-ge2' / `-num-eq2'.  Float
;; tolerance (= 1e-15) is in the `=' primitive.  `/=' is just
;; `(not (= a b))' for strict 2-arg.
;;
;; Chained-pairwise semantics: (< a b c) = (and (< a b) (< b c)).
;; With 1 arg returns t (= trivially-true single-element chain).

(defun nelisp--cmp-chain (args cmp2-fn)
  (cond
   ((null args) (signal 'wrong-number-of-arguments (list cmp2-fn 0)))
   ((null (cdr args)) t)
   (t
    (let ((ok t) (a (car args)) (rest (cdr args)))
      (while (and ok rest)
        (let ((b (car rest)))
          (unless (funcall cmp2-fn a b) (setq ok nil))
          (setq a b)
          (setq rest (cdr rest))))
      ok))))

(defun < (&rest args)  (nelisp--cmp-chain args (function nelisp--num-lt2)))
(defun > (&rest args)  (nelisp--cmp-chain args (function nelisp--num-gt2)))
(defun <= (&rest args) (nelisp--cmp-chain args (function nelisp--num-le2)))
(defun >= (&rest args) (nelisp--cmp-chain args (function nelisp--num-ge2)))
(defun = (&rest args)  (nelisp--cmp-chain args (function nelisp--num-eq2)))
(defun /= (a b)        (not (nelisp--num-eq2 a b)))

;; Rust-min batch 7g (2026-05-07): `min' / `max' / `abs' migrated from
;; Rust to elisp.  `min' / `max' fold over the 2-arg `nelisp--num-lt2'
;; / `nelisp--num-gt2' (= same primitives that drive `<' / `>' since
;; batch 6w), tracking the winning argument's value AND type — so
;; `(min 1 2.5)' = 1 (integer, host-Emacs contract) instead of the
;; prior Rust impl's 1.0 (which coerced ALL args to float upfront when
;; any was float).  Tree-internal callers were all-int so the type
;; behaviour change is invisible there.  Empty args case signals
;; `wrong-number-of-arguments' as before.

(defun min (&rest args)
  (cond
   ((null args) (signal 'wrong-number-of-arguments (list 'min 0)))
   (t (let ((acc (car args)) (cur (cdr args)))
        (while cur
          (when (nelisp--num-lt2 (car cur) acc) (setq acc (car cur)))
          (setq cur (cdr cur)))
        acc))))

(defun max (&rest args)
  (cond
   ((null args) (signal 'wrong-number-of-arguments (list 'max 0)))
   (t (let ((acc (car args)) (cur (cdr args)))
        (while cur
          (when (nelisp--num-gt2 (car cur) acc) (setq acc (car cur)))
          (setq cur (cdr cur)))
        acc))))

(defun abs (x)
  (if (nelisp--num-lt2 x 0) (nelisp--sub2 0 x) x))

;; Rust-min batch 7h (2026-05-07): `floor' / `ceiling' / `round'
;; migrated from Rust to elisp.  The shared f64-division-and-truncate
;; kernel is the new `nelisp--f64-trunc MODE X DIV' primitive (Rust);
;; everything above it — arity (1..=2), DIV defaulting to 1, and the
;; integer 1-arg fast-path that bypasses f64 entirely so large
;; magnitudes keep their precision — lives here.  Mirrors the host
;; Emacs contract `(floor 5)' = 5 (integer passthrough), `(floor 5.7)'
;; = 5, `(floor 5 2)' = 2, `(floor -5 2)' = -3.

(defun floor (x &optional div)
  (cond
   ((and (null div) (integerp x)) x)
   (t (nelisp--f64-trunc 'floor x (or div 1)))))

(defun ceiling (x &optional div)
  (cond
   ((and (null div) (integerp x)) x)
   (t (nelisp--f64-trunc 'ceiling x (or div 1)))))

(defun round (x &optional div)
  (cond
   ((and (null div) (integerp x)) x)
   (t (nelisp--f64-trunc 'round x (or div 1)))))

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

;; Doc 86 §86.1.a (Tier 1 predicate / type-of) — `functionp' migrated
;; from Rust to elisp.  The previous `bi_predicate' dispatch arm
;; matched `Sexp::Cons(b)' whose `b.car' was the symbol `lambda',
;; `closure', or `builtin'.  Direct transliteration on top of `consp'
;; + `car' + `memq' = same semantics, no new primitive needed (= Tier 1
;; "elisp で直接実装可能" per Doc 86 §2.2.1).
(defun functionp (x)
  "Return t if X is callable (= a `lambda' / `closure' / `builtin' form)."
  (and (consp x) (memq (car x) '(lambda closure builtin)) t))

;; Doc 111 §111.B — `recordp' now routes through the internal
;; `nelisp--recordp-cc' bridge.  Linux x86_64 uses the new
;; Phase 47-compiled tag-check object; other targets keep the legacy
;; direct Rust fallback behind the same builtin name.
(defun recordp (x)
  "Return t if X is a record."
  (nelisp--recordp-cc x))

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

;; Rust-min batch 7k (2026-05-07, Doc 65 closing batch).
;; (lognot X) = bitwise NOT of fixnum X.  In two's complement
;; ~x = x XOR -1, so the elisp form needs no extra primitive.
(defun lognot (x) (nelisp--logxor2 x -1))

;; Doc 86 §86.1.b — `sxhash' migrated from Rust to elisp on top of
;; the `nl_jit_sxhash' trampoline (jit/predicate.rs).  Recursive
;; Sexp → hash fold stays in Rust for `DefaultHasher' bit-exactness
;; (Doc 87 §3.2 risk note).  `sxhash-{equal,eq,eql}' defaliases
;; live in `nelisp-stdlib-misc.el' and pick up this function-cell.
(defun sxhash (object)
  (nl-jit-call-out-1 "nelisp_jit_sxhash" object))

;; nelisp-stdlib.el ends here
