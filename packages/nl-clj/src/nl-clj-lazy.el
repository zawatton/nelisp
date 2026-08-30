;;; nl-clj-lazy.el --- Lazy sequences for nl-clj -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 195 §4.7's deferred half: `lazy-seq'/`lazy-cons', `take'/`drop'/
;; `take-while'/`drop-while', a lazy-aware `map'/`filter' (wired into
;; nl-clj-seq.el's own generic dispatch, see below), `range' (bounded
;; AND unbounded -- an unbounded range is the headline demo: `(nl-clj-
;; lazy-take 5 (nl-clj-lazy-range))' must terminate), `iterate',
;; `repeat', `cycle', and `doall'/`dorun' to force one back to eager.
;;
;; Representation (Doc 195 §3.2, nl-clj-core.el):
;;
;;   [nl-clj--lazy CELL]   CELL an `nl-cell' (packages/nl-safe) wrapping
;;                         (THUNK . REALIZED-VALUE-OR-NIL)
;;
;; THUNK is nil once realized (so "already realized" is `(null (car
;; pair))', not a separate flag); REALIZED-VALUE-OR-NIL is only
;; meaningful once THUNK is nil.  A THUNK, when called, must return a
;; "seq-shaped" value: nil (empty), a cons (HEAD . REST), or another
;; not-yet-realized `[nl-clj--lazy ...]' value (a thunk that tail-calls
;; into another `nl-clj-lazy-seq' without forcing it -- this is exactly
;; how `nl-clj-lazy-map'/`nl-clj-lazy-take'/etc. below build their own
;; "rest" without becoming eager).
;;
;; Realize-once + reentrancy, both for free from ONE reused mechanism
;; (Doc 195's own point, made twice in that doc for two different
;; purposes -- transients, §4.4; lazy-seq, §4.7 -- and now landed here
;; for the second one): `nl-clj-lazy--force-one-step' wraps its whole
;; body, INCLUDING the `(funcall thunk)' call, in `nl-with-borrow-mut'
;; on the cell.  That gives two properties at once, neither hand-rolled:
;;
;;   - Realize-once: the thunk only runs when `(car pair)' is still
;;     non-nil; the very next line inside the SAME borrow replaces it
;;     with `(nil . RESULT)' via `nl-cell-set' (the write-back API
;;     `nl-with-borrow-mut''s own docstring names for exactly this
;;     pattern), so a second force of the same cell sees `(car pair)'
;;     already nil and returns the cached RESULT without re-running
;;     anything.
;;   - Reentrancy signals loudly: if THUNK, directly or through a chain
;;     of calls, forces the SAME still-unrealized cell again before
;;     returning (a real, documented Clojure footgun, not a
;;     hypothetical -- Doc 195 §4.7), that nested force tries
;;     `nl-with-borrow-mut' on a cell already exclusively held (state
;;     -1, because the OUTER force's borrow has not been released yet
;;     -- it is still inside `(funcall thunk)') and `nl-safe' signals
;;     `nl-borrow-error' immediately, per its own borrow-violation path.
;;     This is strictly MORE informative than real Clojure's own
;;     behavior on the identical bug (a `StackOverflowError' or silent
;;     re-evaluation, depending on shape) -- see
;;     `nl-clj-lazy-test-reentrant-force-signals' in this package's
;;     test file for the against-the-bug proof.
;;
;; Unwrapping a THUNK-returns-another-lazy-value chain (the "seq-shaped"
;; contract above) is done by `nl-clj-lazy--force''s `while' loop, NOT
;; by having `nl-clj-lazy--force-one-step' call itself -- this mirrors
;; real Clojure's own `LazySeq.seq()' (also a loop, not recursion, for
;; the identical reason: an arbitrarily long chain of `(lazy-seq
;; (lazy-seq (lazy-seq ...)))' before the first real cons must not grow
;; the host call stack).
;;
;; A refinement of Doc 195 §4.7's own text, named rather than silently
;; done differently (the same discipline nl-clj-seq.el's Commentary
;; already applies to its own doc-vs-implementation gaps): that section
;; frames `nl-loop'/`nl-recur' (packages/nl-prelude) as LOAD-BEARING for
;; the lazy walk, "or a long `nl-clj-reduce' over a million-element lazy
;; sequence would blow the interpreter's own call stack."  Neither this
;; file nor nl-clj-seq.el's own `nl-clj-reduce'/`nl-clj-lazy-doall'/
;; `nl-clj-lazy-dorun' below use them, and none needs to: every DRIVER
;; here (`reduce', `doall', `dorun', `nl-clj-lazy--force''s own
;; unwrap loop) is a plain `while' loop -- Elisp's `while' does not grow
;; the call stack per iteration, so it is already exactly as stack-safe
;; as `nl-loop'/`nl-recur' would make a self-recursive walker, for the
;; same reason nl-clj-seq.el's PRE-lazy-phase eager walk already needed
;; neither (its own Commentary makes this same point).  And every
;; PRODUCER here (`nl-clj-lazy-map'/`filter'/`take'/`drop'/`take-while'/
;; `drop-while'/`range'/`iterate'/`repeat'/`cycle') calls itself once
;; per element ONLY in the sense of building a brand-new, unforced
;; `[nl-clj--lazy ...]' value and returning immediately -- it never
;; forces that value itself, so the "recursive-looking" call is O(1)
;; and returns before any deeper call happens, exactly the same
;; non-strict construction real Clojure's own `clojure.core/map'/
;; `filter' use (also apparently self-recursive, also not actually
;; recursive at runtime for the identical reason).  `nl-clj-lazy-test-
;; large-take-does-not-overflow-stack' is this file's own against-the-
;; bug proof, mirroring nl-clj-seq-test.el's pre-existing eager one.
;;
;; Dependencies, and the direction that matters (see nl-clj-core.el's
;; Commentary for the full require-cycle explanation): this file
;; requires `nl-safe' (the borrow cell) and `nl-clj-seq' (the generic
;; `nl-clj-seq'/`nl-clj-first'/`nl-clj-rest', needed to pull from an
;; arbitrary source -- lazy or eager -- when building a lazy producer
;; over it).  `nl-clj-seq.el' does NOT require this file back; it reads
;; three forward-reference vars this file sets once, at the bottom.

;;; Code:

(require 'nl-clj-core)
(require 'nl-safe)
(require 'nl-clj-seq)

;;;; Construction: lazy-seq / lazy-cons / the realize-once+reentrant force -

(defun nl-clj-lazy-p (object)
  "Return non-nil when OBJECT is an nl-clj lazy seq (realized or not)."
  (nl-clj--tagged-p object nl-clj--lazy-tag))

(defun nl-clj-lazy--make (thunk)
  "Return a new, unrealized lazy seq wrapping THUNK (a 0-arg function)."
  (vector nl-clj--lazy-tag (nl-cell (cons thunk nil))))

(defmacro nl-clj-lazy-seq (&rest body)
  "Return a new, unrealized lazy seq: BODY (implicit `progn') runs at
most once, the first time the result is forced, and its value is
cached from then on (`nl-clj-lazy--force').  BODY's own return value
must itself be seq-shaped -- nil, a cons (HEAD . REST), or another
`nl-clj-lazy-seq' value -- exactly Clojure's own `lazy-seq' contract;
see this file's Commentary for the realize-once/reentrancy guarantee
this gets from `nl-with-borrow-mut', and for why BODY re-forcing the
very value `nl-clj-lazy-seq' is building is a loud `nl-borrow-error',
not silent double evaluation or infinite recursion."
  (declare (indent 0))
  `(nl-clj-lazy--make (lambda () ,@body)))

(defmacro nl-clj-lazy-cons (x-form coll-form)
  "Return a lazy seq whose first element is X-FORM and whose rest is
COLL-FORM's own seq.  Unlike plain `cons', NEITHER X-FORM NOR
COLL-FORM is evaluated until the result is forced -- the old pre-1.0
Clojure `lazy-cons' this name follows; modern Clojure spells the
identical idiom `(cons x (lazy-seq coll))'."
  (declare (indent 0))
  `(nl-clj-lazy-seq (cons ,x-form (nl-clj-seq ,coll-form))))

(defun nl-clj-lazy--force-one-step (lz)
  "Force exactly one layer of lazy seq LZ; return the (possibly still
lazy) result -- realize-once + reentrancy-signals-loudly, both via
`nl-with-borrow-mut' on LZ's own cell.  See this file's Commentary."
  (let ((cell (aref lz 1)))
    (nl-with-borrow-mut (pair cell)
      (if (car pair)
          (let* ((thunk (car pair))
                 (result (funcall thunk)))
            (nl-cell-set cell (cons nil result))
            result)
        (cdr pair)))))

(defun nl-clj-lazy--force (lz)
  "Force lazy seq LZ down to a non-lazy seq view: nil, or a cons (HEAD
. REST) whose REST may itself still be an unrealized lazy value (only
THIS layer is forced -- exactly one step, matching `nl-clj-seq''s own
contract for a lazy COLL).  Loops (does not recurse) through any chain
of nested not-yet-realized lazy values a thunk chain returns before
finally producing a real cons or nil -- see this file's Commentary for
why this cannot blow the interpreter's own call stack no matter how
long that chain is."
  (while (nl-clj-lazy-p lz)
    (setq lz (nl-clj-lazy--force-one-step lz)))
  lz)

;; Install the forward-reference nl-clj-seq.el reads (nl-clj-core.el's
;; Commentary explains why this indirection exists instead of a direct
;; require).  Installed here, right after the function it names, not
;; deferred to the bottom of the file, so a reader scanning top-to-
;; bottom sees each wire-up immediately next to its own definition.
(setq nl-clj--lazy-force-fn #'nl-clj-lazy--force)

;;;; map / filter (lazy producers; wired into nl-clj-seq.el's own dispatch) -

(defun nl-clj-lazy-map (f coll)
  "Return a lazy seq of (F x) for each x in COLL's seq (COLL may itself
be eager or lazy -- pulled one element at a time, on demand, via the
generic `nl-clj-seq', so a lazy COLL's own laziness is preserved
rather than defeated by an eager pull)."
  (nl-clj-lazy-seq
    (let ((s (nl-clj-seq coll)))
      (when s (cons (funcall f (car s)) (nl-clj-lazy-map f (cdr s)))))))

(defun nl-clj-lazy-filter (pred coll)
  "Return a lazy seq of COLL's elements for which PRED is non-nil,
pulling from COLL only as far as needed to find each match."
  (nl-clj-lazy-seq
    (let ((s (nl-clj-seq coll)))
      (while (and s (not (funcall pred (car s))))
        (setq s (nl-clj-seq (cdr s))))
      (when s (cons (car s) (nl-clj-lazy-filter pred (cdr s)))))))

(setq nl-clj--lazy-map-fn #'nl-clj-lazy-map)
(setq nl-clj--lazy-filter-fn #'nl-clj-lazy-filter)

;;;; take / drop / take-while / drop-while -----------------------------------

(defun nl-clj-lazy-take (n coll)
  "Return a lazy seq of the first N elements of COLL (fewer if COLL's
own seq is shorter than N).  The headline lazy demo: `(nl-clj-lazy-take
5 (nl-clj-lazy-range))' terminates even though COLL is unbounded,
because forcing this NEVER calls `nl-clj-seq' on COLL a 6th time -- N
reaching 0 ends the chain with a plain nil, not another pull."
  (if (<= n 0)
      nil
    (nl-clj-lazy-seq
      (let ((s (nl-clj-seq coll)))
        (when s (cons (car s) (nl-clj-lazy-take (1- n) (cdr s))))))))

(defun nl-clj-lazy-drop (n coll)
  "Return COLL's seq with its first N elements removed.  Lazy in
construction (skipping N elements happens on first force, not when
this is called) but, once forced, necessarily pulls N+1 elements from
COLL up front to find where the remainder starts."
  (nl-clj-lazy-seq
    (let ((s (nl-clj-seq coll)) (i n))
      (while (and s (> i 0))
        (setq s (nl-clj-seq (cdr s)))
        (setq i (1- i)))
      s)))

(defun nl-clj-lazy-take-while (pred coll)
  "Return a lazy seq of COLL's leading elements for which PRED holds,
stopping at (and not including) the first element PRED rejects."
  (nl-clj-lazy-seq
    (let ((s (nl-clj-seq coll)))
      (when (and s (funcall pred (car s)))
        (cons (car s) (nl-clj-lazy-take-while pred (cdr s)))))))

(defun nl-clj-lazy-drop-while (pred coll)
  "Return COLL's seq with its leading PRED-matching elements removed."
  (nl-clj-lazy-seq
    (let ((s (nl-clj-seq coll)))
      (while (and s (funcall pred (car s)))
        (setq s (nl-clj-seq (cdr s))))
      s)))

;;;; range / iterate / repeat / cycle -----------------------------------------

(defun nl-clj-lazy--range-from (i end step)
  "Helper: the lazy seq I, I+STEP, I+2*STEP, ... stopping before END
(exclusive) in STEP's own direction, or never stopping when END is nil."
  (if (and end (if (> step 0) (>= i end) (<= i end)))
      nil
    (nl-clj-lazy-seq (cons i (nl-clj-lazy--range-from (+ i step) end step)))))

(defun nl-clj-lazy-range (&rest args)
  "Return a lazy seq of numbers -- Clojure's own `range' arity contract:

  (nl-clj-lazy-range)              unbounded: 0, 1, 2, ... (Doc 195's
                                    own headline lazy-range demo)
  (nl-clj-lazy-range END)          0 to END, exclusive, step 1
  (nl-clj-lazy-range START END)    START to END, exclusive, step 1;
                                    END may be nil for UNBOUNDED from
                                    START (not itself a Clojure form --
                                    real `range' has no unbounded-from-
                                    START arity -- but a natural, cheap
                                    extension given `nl-clj-lazy--range-
                                    from' already supports a nil END,
                                    and this package's own bignum-
                                    boundary tests want exactly this)
  (nl-clj-lazy-range START END STEP)  as above, stepping by STEP (may
                                    be negative when END < START)

Each step is plain `+' -- this branch's own bignum Phase B (Doc 190)
means advancing past `most-positive-fixnum' promotes to a Bignum
rather than signalling `overflow-error', which is what makes an
unbounded range actually usable this far rather than merely to
fixnum's own ceiling; see this package's standalone smoke for the
proof against the real `target/nelisp' binary, not host Emacs (which
has always had native bignums regardless of NeLisp's own progress)."
  (pcase (length args)
    (0 (nl-clj-lazy--range-from 0 nil 1))
    (1 (nl-clj-lazy--range-from 0 (car args) 1))
    (2 (nl-clj-lazy--range-from (car args) (cadr args) 1))
    (3 (nl-clj-lazy--range-from (car args) (cadr args) (caddr args)))
    (_ (signal 'nl-clj-error (list 'nl-clj-lazy-range "expects 0-3 args" args)))))

(defun nl-clj-lazy-iterate (f x)
  "Return the infinite lazy seq X, (F X), (F (F X)), ..."
  (nl-clj-lazy-seq (cons x (nl-clj-lazy-iterate f (funcall f x)))))

(defun nl-clj-lazy--repeat-forever (x)
  (nl-clj-lazy-seq (cons x (nl-clj-lazy--repeat-forever x))))

(defun nl-clj-lazy-repeat (&rest args)
  "(nl-clj-lazy-repeat X): the infinite lazy seq of X repeated forever.
(nl-clj-lazy-repeat N X): a lazy seq of X repeated N times."
  (cond
   ((= (length args) 1) (nl-clj-lazy--repeat-forever (car args)))
   ((= (length args) 2) (nl-clj-lazy-take (car args) (nl-clj-lazy--repeat-forever (cadr args))))
   (t (signal 'nl-clj-error (list 'nl-clj-lazy-repeat "expects (X) or (N X)" args)))))

(defun nl-clj-lazy--cycle-from (remaining items)
  "REMAINING is the not-yet-emitted suffix of ITEMS for the current lap;
wraps back to the head of ITEMS when REMAINING runs out."
  (nl-clj-lazy-seq
    (cons (car remaining) (nl-clj-lazy--cycle-from (or (cdr remaining) items) items))))

(defun nl-clj-lazy-cycle (coll)
  "Return an infinite lazy seq of COLL's elements repeated forever, or
nil if COLL's seq is empty (Clojure's own `cycle' contract on an empty
collection: there is nothing to repeat, so the result is the empty
seq -- not an error, and not an infinite seq of nothing)."
  (nl-clj-lazy-seq
    (let ((items (nl-clj-seq coll)))
      (when items (nl-clj-lazy--cycle-from items items)))))

;;;; doall / dorun (force back to eager) ---------------------------------

(defun nl-clj-lazy-doall (coll)
  "Force every element of COLL's seq, left to right, and return the
fully realized result as a proper Elisp list -- Clojure's own `doall'
contract: walk for side effects AND keep the values.  Hangs,
faithfully, on an unbounded lazy COLL (same as real Clojure's own
`(doall (range))'; nothing can make forcing infinity finite -- bound
it with `nl-clj-lazy-take' first)."
  (let (acc (s (nl-clj-seq coll)))
    (while s
      (push (car s) acc)
      (setq s (nl-clj-seq (cdr s))))
    (nreverse acc)))

(defun nl-clj-lazy-dorun (coll)
  "Force every element of COLL's seq for side effects only; always
returns nil (Clojure's own `dorun' contract -- unlike `nl-clj-lazy-
doall', does not retain the realized values, so a walk whose only
purpose is side effects does not also pay for an O(n) result list)."
  (let ((s (nl-clj-seq coll)))
    (while s (setq s (nl-clj-seq (cdr s)))))
  nil)

(provide 'nl-clj-lazy)

;;; nl-clj-lazy.el ends here
