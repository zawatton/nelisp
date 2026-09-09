;;; nelisp-cl-macros.el --- cl-loop / cl-block / cl-return elisp impl  -*- lexical-binding: t; -*-

;;; Commentary:

;; Rust-min: cl-loop family を elisp 実装として stdlib に集約。
;; (= 各 consumer (nelisp-emacs / nelisp-cc / etc.) が独自の stub を
;; defun し合うのを止めて NeLisp 上流で共通実装を持つ。)
;;
;; 提供:
;;   cl-block NAME BODY...        catch+throw 経由の名前付き block
;;   cl-return-from NAME &optional VAL   block NAME を VAL で抜ける
;;   cl-return &optional VAL      最近接の unnamed block を VAL で抜ける
;;   cl-loop CLAUSES...           Common Lisp loop の subset
;;
;; cl-loop の対応 clause:
;;   for VAR in LIST                      list iterator
;;   for VAR on LIST                      tail iterator
;;   for VAR across VECTOR                vector iterator
;;   for VAR [from N] to|below|downto|above M [by S]   numeric iterator
;;   for VAR = INIT [then UPDATE]         stepped value
;;   with VAR = VAL                       binding
;;   repeat N                             counted iteration
;;   do FORM …                            unconditional side-effect
;;   collect / append / sum / count FORM  accumulators
;;   always FORM                          t unless FORM is ever nil
;;   when COND CLAUSE / unless COND CLAUSE  guard the clause that follows
;;   return FORM                          exit with FORM
;;   while COND                           continue while COND non-nil
;;   until COND                           continue until COND non-nil
;;   bodyless (= no for/with/do keyword)  infinite loop with cl-return
;;
;; Several `for' clauses run IN PARALLEL and the loop ends with the first
;; iterator that runs out, which is what CL does.  That was written once as
;; `nelisp-cl-macros--loop-build-parallel' and never wired up: the parser
;; kept one iterator, so a second `for' made the whole shape unrecognised
;; and -- see below -- unrecognised means nil.
;;
;; cl-loop は最終的に `cl-block nil (... while ...)' に展開され、
;; `cl-return' で抜ける。

;;; Code:

;;;; --- block / return ----------------------------------------------------

(defun nelisp-cl-macros--block-tag (name)
  "Catch tag symbol used by `cl-block' NAME (default NAME = anon)."
  (intern (format "cl-block-%s" (or name "anon"))))

(defmacro cl-block (name &rest body)
  "Establish a named BLOCK, returning a value or via `cl-return-from'.
NAME is captured as a catch tag; `(cl-return-from NAME VAL)' inside
BODY immediately exits the block with VAL.  A bare `(cl-return VAL)'
targets the nearest *unnamed* block (= NAME = nil), matching CL."
  (declare (indent 1) (debug (symbolp body)))
  (let ((tag (nelisp-cl-macros--block-tag name)))
    (list 'catch (list 'quote tag) (cons 'progn body))))

(defmacro cl-return-from (name &optional val)
  "Throw VAL out of the cl-block named NAME."
  (declare (indent 1) (debug (symbolp &optional form)))
  (list 'throw (list 'quote (nelisp-cl-macros--block-tag name)) val))

(defmacro cl-return (&optional val)
  "Throw VAL out of the nearest unnamed cl-block."
  (declare (debug (&optional form)))
  (list 'cl-return-from nil val))

;;;; --- loop builder ------------------------------------------------------

(defun nelisp-cl-macros--loop-destructure-bindings (pattern source)
  "Return `let' bindings destructuring PATTERN from SOURCE."
  (let ((bindings nil)
        (cur pattern)
        (access source))
    (while (consp cur)
      (when (car cur)
        (setq bindings
              (cons (list (car cur) (list 'car access)) bindings)))
      (setq access (list 'cdr access))
      (setq cur (cdr cur)))
    (when cur
      (setq bindings (cons (list cur access) bindings)))
    (nreverse bindings)))

(defun nelisp-cl-macros--loop-wrap-body (pattern item forms)
  "Return one loop body form for PATTERN bound from ITEM and FORMS."
  (if (symbolp pattern)
      (cons 'progn forms)
    (cons 'let
          (cons (nelisp-cl-macros--loop-destructure-bindings pattern item)
                forms))))

(defun nelisp-cl-macros--loop-build-counted
    (step-init step-test step-incr collect-form sum-form count-form
     do-forms with-bindings)
  "Shared codegen for a counted `while'-driven cl-loop iteration.
STEP-INIT is the counter's `let' binding (VAR INIT-FORM), STEP-TEST the
loop continuation test, STEP-INCR the per-iteration counter update.
Used by both the numeric `for VAR from N {to,below} M' clause and the
`repeat N' clause -- the two counted (non `for...in...') iteration
shapes -- so a `collect'/`sum'/`count' clause is honoured the same way
the `for VAR in LIST' clauses already honour it, instead of only ever
wiring DO-FORMS."
  (let ((rev nil))
    (while do-forms (setq rev (cons (car do-forms) rev))
           (setq do-forms (cdr do-forms)))
    (cond
     (collect-form
      (let ((acc-sym (make-symbol "--loop-acc--")))
        (list 'let (cons (list acc-sym nil) (cons step-init with-bindings))
              (list 'while step-test
                    (list 'setq acc-sym (list 'cons collect-form acc-sym))
                    step-incr)
              (list 'nreverse acc-sym))))
     (sum-form
      (let ((acc-sym (make-symbol "--loop-sum--")))
        (list 'let (cons (list acc-sym 0) (cons step-init with-bindings))
              (list 'while step-test
                    (list 'setq acc-sym (list '+ acc-sym sum-form))
                    step-incr)
              acc-sym)))
     (count-form
      (let ((acc-sym (make-symbol "--loop-count--")))
        (list 'let (cons (list acc-sym 0) (cons step-init with-bindings))
              (list 'while step-test
                    (list 'when count-form (list 'setq acc-sym (list '+ acc-sym 1)))
                    step-incr)
              acc-sym)))
     (t
      (list 'let (cons step-init with-bindings)
            (list 'while step-test
                  (cons 'progn rev)
                  step-incr))))))

(defun nelisp-cl-macros--loop-iter-parse (cur)
  "Parse one `for' clause from CUR, or return nil when it is not one.
Returns (ITER . REST).  ITER is (:kind KIND :pat PAT ...) describing one
iterator; parallel `for' clauses each produce one and are stepped
together, which is what CL does."
  (when (eq (car cur) 'for)
    (let ((pat (car (cdr cur)))
          (kw (car (cdr (cdr cur))))
          (rest (cdr (cdr (cdr cur)))))
      (cond
       ((eq kw 'in)
        ;; `by STEPFN' walks the list with something other than `cdr', which
        ;; is how a plist is iterated two cells at a time.
        (let ((seq (car rest)) (by nil) (tail (cdr rest)))
          (when (eq (car tail) 'by)
            (setq by (car (cdr tail)) tail (cdr (cdr tail))))
          (cons (list :kind 'in :pat pat :seq seq :by by) tail)))
       ((eq kw 'on)
        (cons (list :kind 'on :pat pat :seq (car rest)) (cdr rest)))
       ((eq kw 'across)
        (cons (list :kind 'across :pat pat :seq (car rest)) (cdr rest)))
       ((memq kw '(from downfrom upfrom to below downto above by))
        ;; [from N] to|below|downto|above M [by S].  `from' is optional --
        ;; CL lets `for i below N' start at 0, and this tree writes seven
        ;; loops that way.
        (let ((from 0)
              (limit-kw nil) (limit nil) (by nil) (down nil)
              (tail nil))
          (if (memq kw '(from downfrom upfrom))
              (setq from (car rest) tail (cdr rest)
                    down (eq kw 'downfrom))
            (setq tail (cdr (cdr cur))))
          (when (memq (car tail) '(to below downto above))
            (setq limit-kw (car tail) limit (car (cdr tail)) tail (cdr (cdr tail))))
          (when (eq (car tail) 'by)
            (setq by (car (cdr tail)) tail (cdr (cdr tail))))
          (cons (list :kind 'num :pat pat :from from
                      :limit-kw limit-kw :limit limit :by by :down down)
                tail)))
       ((eq kw '=)
        (let ((init (car rest)) (then nil) (tail (cdr rest)))
          (when (eq (car tail) 'then)
            (setq then (car (cdr tail)) tail (cdr (cdr tail))))
          (cons (list :kind 'eq :pat pat :init init :then then) tail)))
       (t nil)))))

(defun nelisp-cl-macros--loop-iter-plan (iters first-sym)
  "Return (BINDS TESTS VARBINDS STEPS) for ITERS.
BINDS are outer state bindings, TESTS the exhaustion tests, VARBINDS the
bindings for one step, STEPS the end-of-iteration updates.  VARBINDS are
sequential: a `for VAR = FORM' clause may read a variable an earlier
clause bound this iteration, which is what CL promises and the only way
`for a in L for s = (* a 2)' can work.  FIRST-SYM names the flag that is
non-nil during the first iteration only, for `= INIT then UPDATE'."
  (let ((binds nil) (tests nil) (varbinds nil) (steps nil))
    (while iters
      (let* ((it (car iters))
             (kind (plist-get it :kind))
             (pat (plist-get it :pat)))
        (cond
         ((memq kind '(in on))
          (let ((cur (make-symbol "--loop-cur--")))
            (setq binds (append binds (list (list cur (plist-get it :seq)))))
            (setq tests (append tests (list cur)))
            (setq varbinds
                  (append varbinds
                          (let ((src (if (eq kind 'on) cur (list 'car cur))))
                            (if (symbolp pat)
                                (list (list pat src))
                              (nelisp-cl-macros--loop-destructure-bindings pat src)))))
            (setq steps
                  (append steps
                          (list (list 'setq cur
                                      (let ((by (plist-get it :by)))
                                        (if by
                                            (list 'funcall by cur)
                                          (list 'cdr cur)))))))))
         ((eq kind 'across)
          (let ((vec (make-symbol "--loop-vec--"))
                (idx (make-symbol "--loop-idx--")))
            (setq binds (append binds (list (list vec (plist-get it :seq))
                                            (list idx 0))))
            (setq tests (append tests (list (list '< idx (list 'length vec)))))
            (setq varbinds (append varbinds (list (list pat (list 'aref vec idx)))))
            (setq steps (append steps (list (list 'setq idx (list '1+ idx)))))))
         ((eq kind 'num)
          (let* ((n (make-symbol "--loop-n--"))
                 (by (or (plist-get it :by) 1))
                 (limit-kw (plist-get it :limit-kw))
                 ;; `downfrom' counts down whatever the limit keyword is, so
                 ;; `for i downfrom N to M' walks N, N-1, ... M inclusive.
                 (down (or (plist-get it :down) (memq limit-kw '(downto above))))
                 (limit (plist-get it :limit)))
            (setq binds (append binds (list (list n (plist-get it :from)))))
            (when limit-kw
              (setq tests
                    (append tests
                            (list (list (cond ((eq limit-kw 'to) (if down '>= '<=))
                                              ((eq limit-kw 'below) (if down '> '<))
                                              ((eq limit-kw 'downto) '>=)
                                              (t '>))
                                        n limit)))))
            (setq varbinds (append varbinds (list (list pat n))))
            (setq steps (append steps
                                (list (list 'setq n
                                            (list (if down '- '+) n by)))))))
         ((eq kind 'eq)
          ;; No outer state and no test: the value is computed each
          ;; iteration, in scope, so it can read the clauses before it.
          (setq varbinds
                (append varbinds
                        (list (list pat
                                    (if (plist-get it :then)
                                        (list 'if first-sym
                                              (plist-get it :init)
                                              (plist-get it :then))
                                      (plist-get it :init)))))))))
      (setq iters (cdr iters)))
    (list binds tests varbinds steps)))

(defun nelisp-cl-macros--loop-guard (cond negate form)
  "Wrap FORM in COND, negated when NEGATE, or return FORM when COND is nil."
  (cond ((null cond) form)
        (negate (list 'if cond nil form))
        (t (list 'if cond form nil))))

(defun nelisp-cl-macros--loop-unsupported (clauses)
  "Return the expansion for CLAUSES, a shape this subset does not model.

It signals when it RUNS rather than expanding to nil, and rather than
signalling at expansion time.  Expanding to nil is what made the gap
dangerous: a loop nobody could build did not fail, it silently did not
run, and 48 of this repository's 62 `cl-loop' forms were in that state at
once -- the AOT compiler's stack-argument push loop among them, which
therefore emitted nothing at all.  Every host test stayed green, because
the host has the real macro.

Deferring to run time is deliberate: vendor Elisp that merely CONTAINS an
exotic loop still loads, and only a loop that actually executes fails."
  (list 'signal (list 'quote 'error)
        (list 'list "cl-loop: unsupported clause shape" (list 'quote clauses))))

(defun nelisp-cl-macros--loop-unsupported-form-p (form)
  "Return non-nil when FORM is what `nelisp-cl-macros--loop-unsupported' builds."
  (and (consp form)
       (eq (car form) 'signal)
       (equal (car (cdr (cdr form)))
              (list 'list "cl-loop: unsupported clause shape"
                    (car (cdr (cdr (car (cdr (cdr form)))))))) ))

(defun nelisp-cl-macros--loop-build (clauses)
  "Build expansion for `cl-loop' CLAUSES.

Iterators (`for' in / on / across / from ... to|below|downto|above [by] /
= [then]) are stepped in parallel and the loop ends with the first one
that runs out.  `when' / `unless' guard the clause that follows.  A shape
this subset does not model expands to nil, as it always has."
  (let ((iters nil) (with-bindings nil) (do-forms nil)
        (acc-kind nil) (acc-form nil)
        (guard nil) (guard-negate nil)
        (extra-tests nil) (bodyless-forms nil)
        (repeat-count nil)
        (first-sym (make-symbol "--loop-first--"))
        (cur clauses) (recognised t))
    (when (and clauses
               (not (memq (car clauses)
                          '(for with do collect append sum count when unless
                                while until repeat finally return named
                                always))))
      (setq bodyless-forms clauses cur nil))
    (while (and cur recognised)
      (let ((kw (car cur))
            (parsed nil))
        (cond
         ((and (eq kw 'for)
               (setq parsed (nelisp-cl-macros--loop-iter-parse cur)))
          (setq iters (append iters (list (car parsed))) cur (cdr parsed)))
         ((eq kw 'for) (setq recognised nil))
         ((eq kw 'repeat)
          (setq repeat-count (car (cdr cur)) cur (cdr (cdr cur))))
         ((eq kw 'with)
          (if (eq (car (cdr (cdr cur))) '=)
              (progn
                (setq with-bindings
                      (append with-bindings
                              (list (list (car (cdr cur))
                                          (car (cdr (cdr (cdr cur))))))))
                (setq cur (cdr (cdr (cdr (cdr cur))))))
            (setq recognised nil)))
         ((memq kw '(when unless))
          (setq guard (car (cdr cur))
                guard-negate (eq kw 'unless)
                cur (cdr (cdr cur))))
         ((eq kw 'do)
          (setq cur (cdr cur))
          (let ((forms nil))
            (while (and cur
                        (not (memq (car cur)
                                   '(for with do collect append sum count
                                         when unless while until repeat
                                         finally return named always and))))
              (setq forms (append forms (list (car cur))) cur (cdr cur)))
            ;; `and do' / `and return' continue the same guarded clause, so
            ;; `when C do X and return Y' returns Y rather than dropping it.
            (while (and cur (eq (car cur) 'and)
                        (memq (car (cdr cur)) '(do return)))
              (if (eq (car (cdr cur)) 'return)
                  (progn
                    (setq forms (append forms
                                        (list (list 'cl-return (car (cdr (cdr cur)))))))
                    (setq cur (cdr (cdr (cdr cur)))))
                (setq cur (cdr (cdr cur)))
                (while (and cur
                            (not (memq (car cur)
                                       '(for with do collect append sum count
                                             when unless while until repeat
                                             finally return named always and))))
                  (setq forms (append forms (list (car cur))) cur (cdr cur)))))
            (setq do-forms
                  (append do-forms
                          (list (nelisp-cl-macros--loop-guard
                                 guard guard-negate (cons 'progn forms)))))
            (setq guard nil guard-negate nil)))
         ((memq kw '(collect append sum count))
          (if (and acc-kind (not (eq acc-kind kw)))
              (setq recognised nil)
            (setq acc-kind kw acc-form (car (cdr cur)) cur (cdr (cdr cur)))))
         ((eq kw 'always)
          (setq acc-kind 'always acc-form (car (cdr cur)) cur (cdr (cdr cur))))
         ((eq kw 'return)
          (setq do-forms
                (append do-forms
                        (list (nelisp-cl-macros--loop-guard
                               guard guard-negate
                               (list 'cl-return (car (cdr cur)))))))
          (setq guard nil guard-negate nil cur (cdr (cdr cur))))
         ((eq kw 'while)
          (setq extra-tests (append extra-tests (list (car (cdr cur)))))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'until)
          (setq extra-tests
                (append extra-tests (list (list 'not (car (cdr cur))))))
          (setq cur (cdr (cdr cur))))
         (t (setq recognised nil)))))
    (cond
     ((not recognised) (nelisp-cl-macros--loop-unsupported clauses))
     (bodyless-forms
      (list 'cl-block nil (cons 'while (cons t bodyless-forms))))
     ;; `cl-loop while COND do ...' has no iterator and is still a loop.  The
     ;; branch for it was lost when the iterators were generalised, and
     ;; nothing caught that: no form in this tree is written that way, so the
     ;; body simply stopped running -- the same silent shape this rewrite
     ;; existed to remove.
     ((and (null iters) (null repeat-count) (null extra-tests))
      (nelisp-cl-macros--loop-unsupported clauses))
     (t
      (when repeat-count
        (setq iters (append iters
                            (list (list :kind 'num
                                        :pat (make-symbol "--loop-r--")
                                        :from 1 :limit-kw 'to
                                        :limit repeat-count :by nil)))))
      (let* ((plan (nelisp-cl-macros--loop-iter-plan iters first-sym))
             (binds (nth 0 plan))
             (tests (nth 1 plan))
             (varbinds (nth 2 plan))
             (steps (nth 3 plan))
             (going (and extra-tests (make-symbol "--loop-going--")))
             (acc (and acc-kind (make-symbol "--loop-acc--")))
             (acc-init (cond ((memq acc-kind '(sum count)) 0)
                             ((eq acc-kind 'always) t)
                             (t nil)))
             (acc-body
              (cond
               ((eq acc-kind 'collect) (list 'setq acc (list 'cons acc-form acc)))
               ((eq acc-kind 'append)
                (list 'setq acc (list 'cons (list 'append acc-form nil) acc)))
               ((eq acc-kind 'sum) (list 'setq acc (list '+ acc acc-form)))
               ((eq acc-kind 'count)
                (list 'when acc-form (list 'setq acc (list '+ acc 1))))
               ((eq acc-kind 'always)
                (list 'unless acc-form (list 'cl-return nil)))))
             (body (append (if acc-body
                               (list (nelisp-cl-macros--loop-guard
                                      guard guard-negate acc-body))
                             nil)
                           do-forms))
             ;; `while' / `until' read the iteration variables, so they are
             ;; tested inside the per-iteration bindings, not in the `while'
             ;; head where those names do not exist yet.
             (guarded-body
              (if extra-tests
                  (list (list 'if (cons 'and extra-tests)
                              (cons 'progn (or body (list nil)))
                              (list 'setq going nil)))
                body))
             (result (cond ((eq acc-kind 'collect) (list 'nreverse acc))
                           ((eq acc-kind 'append)
                            (list 'apply (list 'quote 'append) (list 'nreverse acc)))
                           ((memq acc-kind '(sum count)) acc)
                           ((eq acc-kind 'always) t)
                           (t nil)))
             (all-binds (append (if acc (list (list acc acc-init)) nil)
                                (if going (list (list going t)) nil)
                                (list (list first-sym t))
                                binds with-bindings))
             (head (if going
                       (cons 'and (cons going tests))
                     (if tests (cons 'and tests) t)))
             (loop-form
              (cons 'while
                    (cons head
                          (append
                           (list (cons 'let* (cons varbinds guarded-body)))
                           (if going
                               (list (cons 'when
                                           (cons going
                                                 (append steps
                                                         (list (list 'setq first-sym nil))))))
                             (append steps (list (list 'setq first-sym nil)))))))))
        (list 'cl-block nil
              (cons 'let (cons all-binds
                               (append (list loop-form)
                                       (if result (list result) nil))))))))))

(defun nelisp-cl-macros--loop-unbuildable-p (clauses)
  "Return non-nil when CLAUSES are a shape this subset does not model."
  (nelisp-cl-macros--loop-unsupported-form-p
   (nelisp-cl-macros--loop-build clauses)))


(defmacro cl-loop (&rest clauses)
  "Loop CLAUSES — minimal CL-style iteration macro.

See `nelisp-cl-macros--loop-build' commentary for supported shapes.
A shape this subset does not model expands to a form that SIGNALS when
it runs, rather than to nil.

Expanding to nil is what made this dangerous: a loop nobody could build
did not fail, it silently did not run, and 48 of the 62 `cl-loop' forms
in this repository were in that state at once -- including the AOT
compiler's stack-argument push loop, which therefore emitted nothing.
Every host test stayed green throughout, because the host has the real
macro.  The signal is at run time and not at expansion time on purpose:
vendor Elisp that merely CONTAINS an exotic loop still loads, and only a
loop that actually executes can fail."
  (declare (debug (&rest sexp)))
  (nelisp-cl-macros--loop-build clauses))

;;;; --- defstruct ---------------------------------------------------------
;;
;; Doc 50 stage 4e — `cl-defstruct' macro built on the Stage 4c
;; record primitives (`nelisp--make-record' / -ref / -set / -length /
;; -type / `recordp').  Minimal CL surface: positional + keyword
;; constructor, predicate, accessors.  Intentionally does NOT yet
;; implement: `:include' / `:type' / `:print-function' / `:copier'
;; auto-name / setf integration.  Those land alongside Stage 4d
;; (equality / setf gv) in a follow-up.
;;
;; Expansion shape for `(cl-defstruct point x y)':
;;   (progn
;;     (defun point-p (obj)
;;       (and (recordp obj) (eq (nelisp--record-type obj) 'point)))
;;     (defun make-point (&rest cl-defstruct--args)
;;       (apply 'nelisp--make-record 'point
;;              (list (nelisp-cl-macros--struct-arg :x cl-defstruct--args nil)
;;                    (nelisp-cl-macros--struct-arg :y cl-defstruct--args nil))))
;;     (defun point-x (cl-defstruct--rec) (nelisp--record-ref cl-defstruct--rec 0))
;;     (defun point-y (cl-defstruct--rec) (nelisp--record-ref cl-defstruct--rec 1))
;;     'point)
;;
;; The slot-spec helper `nelisp-cl-macros--struct-arg' is plain elisp
;; so it can be byte-compiled and reused; the macro itself just
;; assembles `defun' forms.

(defun nelisp-cl-macros--struct-arg (kw args default)
  "Look up KW (a keyword like :x) in plist ARGS, returning DEFAULT
when absent.  Used by the constructor expanded from `cl-defstruct'."
  (let ((cell (memq kw args)))
    (if cell (car (cdr cell)) default)))

(defun nelisp-cl-macros--struct-slot-name (slot-spec)
  "Return the slot symbol for SLOT-SPEC (a symbol or `(NAME DEFAULT)')."
  (if (consp slot-spec) (car slot-spec) slot-spec))

(defun nelisp-cl-macros--struct-slot-default (slot-spec)
  "Return the default-value form for SLOT-SPEC (nil if symbol-only)."
  (if (consp slot-spec) (car (cdr slot-spec)) nil))

(defun nelisp-cl-macros--defstruct-ctor-parts (arglist)
  "Return (FORMALS AUX-BINDINGS VALUE-SYMS) for constructor ARGLIST."
  (let ((cur arglist)
        (formals nil)
        (aux-bindings nil)
        (value-syms nil)
        (in-aux nil))
    (while cur
      (let ((item (car cur)))
        (cond
         ((eq item '&aux)
          (setq in-aux t))
         (in-aux
          (let ((var (if (consp item) (car item) item))
                (init (and (consp item) (consp (cdr item)) (cadr item))))
            (push (list var init) aux-bindings)
            (push var value-syms)))
         ((memq item '(&optional &rest))
          (push item formals))
         ((consp item)
          (let ((var (car item)))
            (push var formals)
            (push var value-syms)))
         (t
          (push item formals)
          (push item value-syms))))
      (setq cur (cdr cur)))
    (list (nreverse formals)
          (nreverse aux-bindings)
          (nreverse value-syms))))

(defun nelisp-cl-macros--struct-name-or-options (head)
  "Return the type symbol from HEAD (a symbol or `(NAME OPTION ...)').
OPTIONS are parsed by `nelisp-cl-macros--struct-options' separately."
  (if (consp head) (car head) head))

(defun nelisp-cl-macros--struct-options (head)
  "Return the option list from HEAD: nil for symbol, cdr for cons.
Each option is `(KEY VALUE)' (e.g. `(:copier my-copy)' or
`(:constructor nil)')."
  (if (consp head) (cdr head) nil))

(defvar nelisp-cl-macros--struct-absent
  (make-symbol "nelisp-cl-macros--struct-absent")
  "Sentinel returned by `--struct-opt' when an option key is absent.
Distinct from any user-supplied value — used to differentiate
`(:copier nil)' (= explicit disable) from no `:copier' clause at
all (= use default name `copy-NAME').")

(defun nelisp-cl-macros--struct-opt (key options)
  "Look up KEY in OPTIONS plist-of-cells.
Return the (cadr cell) when found, or
`nelisp-cl-macros--struct-absent' when no `(KEY ...)' cell exists."
  (let ((cell (assq key options)))
    (if cell (car (cdr cell)) nelisp-cl-macros--struct-absent)))

(defun nelisp-cl-macros--struct-resolve-name (name-form default-sym)
  "Resolve a `:copier'/`:constructor'-style NAME-FORM.
Returns:
  - `nelisp-cl-macros--struct-absent' → use DEFAULT-SYM (auto-generate)
  - nil (= explicit disable in option) → return nil (skip generation)
  - any other symbol → use that symbol verbatim."
  (cond
   ((eq name-form nelisp-cl-macros--struct-absent) default-sym)
   ((null name-form) nil)
   (t name-form)))

;;;; --- defstruct registry (Stage 4f-4 :include) -------------------------

(defvar nelisp-cl-macros--struct-info nil
  "Alist of (NAME . PLIST) describing every defined cl-defstruct.
PLIST has keys :slot-names (list of symbols, parent-first when
:included) and :parent (symbol or nil).  Populated both at
macro expansion time (so that `:include' can resolve parent
slots while expanding the child) and at runtime evaluation
of the macro's expansion (so that AOT-compiled callers and
predicates see the same data).  `assq' takes the most-recent
push, which keeps re-loading idempotent.")

(defvar nelisp-cl-macros--accessor-info nil
  "Alist of (ACCESSOR-SYM . INDEX) for every cl-defstruct slot accessor.
`setf' consults this at expansion time to rewrite
`(setf (ACCESSOR REC) VAL)' into `(nelisp--record-set REC INDEX VAL)'.")

(defun nelisp-cl-macros--struct-record (name parent slot-names)
  "Push (NAME . (:slot-names SLOT-NAMES :parent PARENT)) into the
runtime struct registry.  Re-pushes shadow earlier entries — the
front-of-list wins on lookup.  Also (re-)registers every accessor's
slot index in `nelisp-cl-macros--accessor-info' so `setf' can find it."
  (setq nelisp-cl-macros--struct-info
        (cons (cons name (list :slot-names slot-names :parent parent))
              nelisp-cl-macros--struct-info))
  (let ((i 0))
    (dolist (s slot-names)
      (let ((acc (intern (format "%s-%s" name s))))
        (setq nelisp-cl-macros--accessor-info
              (cons (cons acc i) nelisp-cl-macros--accessor-info)))
      (setq i (1+ i)))))

(defun nelisp-cl-macros--struct-isa (tag target)
  "Return non-nil iff TAG = TARGET or one of TAG's :include ancestors
is TARGET.  Walks `nelisp-cl-macros--struct-info' chain.  Used by
predicates of structs that have been `:included' as a parent so a
descendant record satisfies the parent predicate."
  (cond
   ((eq tag target) t)
   ((null tag) nil)
   (t
    (let ((info (cdr (assq tag nelisp-cl-macros--struct-info))))
      (let ((parent (and info (car (cdr (memq :parent info))))))
        (and parent (nelisp-cl-macros--struct-isa parent target)))))))

(defun nelisp-cl-macros--struct-lookup-slots (name)
  "Return the :slot-names list for struct NAME, or nil if unknown."
  (let ((info (cdr (assq name nelisp-cl-macros--struct-info))))
    (and info (car (cdr (memq :slot-names info))))))

(defmacro cl-defstruct (name-or-options &rest slots)
  "Define a record type and its predicate / constructor / accessors.

NAME-OR-OPTIONS is either NAME (symbol) or `(NAME OPTION ...)'.
Each SLOT is `SLOT-NAME' or `(SLOT-NAME DEFAULT)'.  Generated:
  - `NAME-p OBJECT'        predicate
  - `make-NAME &rest ARGS' constructor (keyword form: `:slot value')
  - `copy-NAME REC'        shallow copier (option `:copier')
  - `NAME-SLOT REC'        accessor (one per slot)

Supported options:
  - `(:constructor nil)'    → suppress make-NAME generation
  - `(:constructor NAME)'   → rename make-NAME
  - `(:copier nil)'         → suppress copy-NAME generation
  - `(:copier NAME)'        → rename copy-NAME
  - `(:include PARENT)'     → inherit PARENT's slots (parent-first)

Slot index assignment: positional, in declaration order.  The
record's `type_tag' is NAME (a symbol); accessors call
`nelisp--record-ref' which is 0-based and excludes the tag — the
type tag is reachable via `nelisp--record-type'.

`:include' semantics: child slots come AFTER parent slots, so the
parent's accessor indices remain valid for the child record.  The
parent's predicate continues to satisfy child records via the
runtime chain walk in `nelisp-cl-macros--struct-isa'.

Limitations: no `:type', no `setf' integration, no docstring slot
form.

Note: `(declare ...)' metadata is intentionally omitted because the
NeLisp Rust evaluator does not yet strip declare forms from macro
bodies (= Stage 4 follow-up).  Indent / edebug specs come back when
`defmacro' grows declare-handling parity with host Emacs."
  (let* ((name (nelisp-cl-macros--struct-name-or-options name-or-options))
         (options (nelisp-cl-macros--struct-options name-or-options))
         (parent-form (nelisp-cl-macros--struct-opt :include options))
         (parent (if (eq parent-form nelisp-cl-macros--struct-absent)
                     nil parent-form))
         (own-slot-names (mapcar #'nelisp-cl-macros--struct-slot-name slots))
         (own-slot-defaults
          (mapcar #'nelisp-cl-macros--struct-slot-default slots))
         (parent-slot-names
          (and parent (nelisp-cl-macros--struct-lookup-slots parent)))
         (slot-names (append parent-slot-names own-slot-names))
         (slot-defaults
          (let ((pads nil) (rem parent-slot-names))
            (while rem (setq pads (cons nil pads)) (setq rem (cdr rem)))
            (append pads own-slot-defaults)))
         (slot-default-alist
          (let ((names slot-names)
                (defaults slot-defaults)
                (out nil))
            (while names
              (push (cons (car names) (car defaults)) out)
              (setq names (cdr names)
                    defaults (cdr defaults)))
            (nreverse out)))
         (predicate (intern (format "%s-p" name)))
         (constructor-cell (assq :constructor options))
         (constructor-arglist
          (and constructor-cell (consp (cdr (cdr constructor-cell)))
               (car (cdr (cdr constructor-cell)))))
         (constructor
          (nelisp-cl-macros--struct-resolve-name
           (if constructor-cell
               (car (cdr constructor-cell))
             nelisp-cl-macros--struct-absent)
           (intern (format "make-%s" name))))
         (copier
          (nelisp-cl-macros--struct-resolve-name
           (nelisp-cl-macros--struct-opt :copier options)
           (intern (format "copy-%s" name)))))
    (when (and parent (null parent-slot-names))
      ;; Either parent doesn't exist or parent has zero slots.  The
      ;; latter is rare but legal — distinguish via registry presence.
      (unless (assq parent nelisp-cl-macros--struct-info)
        (error "cl-defstruct :include — parent struct `%s' not defined"
               parent)))
    ;; Expansion-time registry update so subsequent (cl-defstruct
    ;; (CHILD (:include NAME)) ...)  macros expanded in this same
    ;; pass can resolve our slot list.
    (nelisp-cl-macros--struct-record name parent slot-names)
    (let ((forms nil)
          (args-sym (make-symbol "cl-defstruct--args"))
          (rec-sym (make-symbol "cl-defstruct--rec"))
          (src-sym (make-symbol "cl-defstruct--src"))
          (slot-arg-forms nil)
          (copy-arg-forms nil)
          (i 0))
      ;; Build slot value-extraction forms for the constructor.
      (dolist (s slot-names)
        (let ((kw (intern (format ":%s" s)))
              (def (nth i slot-defaults)))
          (push (list 'nelisp-cl-macros--struct-arg
                      (list 'quote kw) args-sym def)
                slot-arg-forms))
        (setq i (1+ i)))
      (setq slot-arg-forms (nreverse slot-arg-forms))
      ;; Build per-slot ref forms for the copier.
      (setq i 0)
      (dolist (_s slot-names)
        (push (list 'nelisp--record-ref src-sym i) copy-arg-forms)
        (setq i (1+ i)))
      (setq copy-arg-forms (nreverse copy-arg-forms))
      ;; Runtime registry update — keeps the registry in sync with
      ;; the runtime form (matters for AOT-compiled code where the
      ;; expansion-time setq above no longer runs in fresh processes).
      (push (list 'nelisp-cl-macros--struct-record
                  (list 'quote name)
                  (list 'quote parent)
                  (list 'quote slot-names))
            forms)
      ;; Predicate form — uses --struct-isa for chain matching so
      ;; descendant records still satisfy the parent predicate when
      ;; this struct is later used as someone else's `:include'.
      (push (list 'defun predicate (list 'obj)
                  (list 'and
                        (list 'recordp 'obj)
                        (list 'nelisp-cl-macros--struct-isa
                              (list 'nelisp--record-type 'obj)
                              (list 'quote name))))
            forms)
      ;; Constructor form (keyword args by default; positional
      ;; `(:constructor NAME ARGLIST)' when requested).
      (when constructor
        (if constructor-arglist
            (let* ((ctor-parts
                    (nelisp-cl-macros--defstruct-ctor-parts
                     constructor-arglist))
                   (ctor-formals (car ctor-parts))
                   (ctor-aux-bindings (cadr ctor-parts))
                   (ctor-value-syms (caddr ctor-parts))
                   (body
                    (cons 'apply
                          (cons (list 'quote 'nelisp--make-record)
                                (cons (list 'quote name)
                                      (list
                                       (cons
                                        'list
                                        (mapcar
                                         (lambda (slot)
                                           (if (memq slot ctor-value-syms)
                                               slot
                                             (cdr (assq slot slot-default-alist))))
                                         slot-names))))))))
              (push (list 'defun constructor ctor-formals
                          (if ctor-aux-bindings
                              (list 'let ctor-aux-bindings body)
                            body))
                    forms))
          (push (list 'defun constructor (list '&rest args-sym)
                      (cons 'apply
                            (cons (list 'quote 'nelisp--make-record)
                                  (cons (list 'quote name)
                                        (list (cons 'list slot-arg-forms))))))
                forms)))
      ;; Copier form (shallow copy via record-ref / make-record).
      (when copier
        (push (list 'defun copier (list src-sym)
                    (cons 'apply
                          (cons (list 'quote 'nelisp--make-record)
                                (cons (list 'quote name)
                                      (list (cons 'list copy-arg-forms))))))
              forms))
      ;; Accessor forms — one per slot, indexed positionally.
      (setq i 0)
      (dolist (s slot-names)
        (let ((acc (intern (format "%s-%s" name s))))
          (push (list 'defun acc (list rec-sym)
                      (list 'nelisp--record-ref rec-sym i))
                forms))
        (setq i (1+ i)))
      ;; Result form: (progn DEFUN ... 'NAME).
      (cons 'progn
            (append (nreverse forms)
                    (list (list 'quote name)))))))


;;;; --- cl-generic subset (Doc 185) ----------------------------------------
;;
;; `cl-defgeneric'/`cl-defmethod' subset: type + eql specializers plus
;; (struct dispatch via the already-working `nelisp-cl-macros--struct-isa'
;; ancestry walker above, NOT `cl-typep', which knows nothing about
;; `cl-defstruct'-generated types -- docs/design/185-cl-generic-subset.org
;; §1.2/§2.1a), primary methods plus `cl-call-next-method'/
;; `cl-next-method-p' (§2.2), a lazily-built per-generic dispatch table
;; (§2.3/§3.4 -- this is the direct answer to Doc 157's 280-second eager
;; global-prefill wall, §1.4: nothing here does `eval'-based dispatcher
;; compilation, and no work happens for a generic until it is first
;; called).  §2.2 extension: the `:extra STRING' qualifier is also
;; supported -- several methods can share one specializer, distinguished
;; by STRING, coexisting and chaining via `cl-call-next-method' ordered
;; newest-defined-first (matching real Emacs, verified against host
;; Emacs 31.1 -- see the `cl-defmethod' docstring below); `:extra' may
;; also combine with exactly one of `:before'/`:after'/`:around', per
;; real Emacs's own grammar.  Bare `:before'/`:after'/`:around' methods
;; are supported with the same standard combination.  Every other unsupported
;; form -- an
;; unsupported qualifier shape, or a specializer kind other than
;; type/eql/head/subclass/unspecialized -- is a loud `error' at
;; `cl-defmethod' macroexpansion
;; time, never a silent no-dispatch (§3.5).  T59 addendum: `(head VALUE)'
;; specializers are now supported too (real Emacs generalizer priority
;; 80 -- between `eql' (100) and any type match (10); matches when the
;; argument is a cons whose `car' is `eql' to VALUE), needed by `eat.el'
;; transitively via EIEIO/cl-generic subclass dispatch.  T81 addendum:
;; `cl-defgeneric' now supports real Emacs's full `(NAME ARGS
;; [DOCSTRING] [OPTIONS-AND-METHODS...] &rest DEFAULT-BODY)' grammar --
;; a non-empty DEFAULT-BODY becomes an ordinary unspecialized primary
;; method (no new dispatch machinery: it is just another method-table
;; entry), `(:method ...)' forms expand to ordinary `cl-defmethod'
;; calls, `(declare ...)'/`(:documentation ...)'/
;; `(:argument-precedence-order ...)' are accepted (the first two
;; syntactically only, with no side effect; the third records the
;; precedence of multiple required dispatch arguments) -- needed by
;; `eieio.el' itself, whose very first `cl-defgeneric' call
;; (`eieio-object-name-string') uses exactly the default-body shape.
;;
;; No `declare' in the macro bodies below -- see the `cl-defstruct'
;; comment above: the standalone reader does not yet strip `declare'
;; forms from macro bodies, so one here would break under
;; `target/nelisp' even though it loads fine under host Emacs.

(defvar nelisp-cl-generic--next-methods nil
  "Dynamic: the remaining (less-specific) applicable method functions for
the `cl-call-next-method' chain of the call in progress.  Bound by
`nelisp-cl-generic--invoke' and rebound by `cl-call-next-method' itself
around each further step, so a chain of N applicable methods can walk
itself in most-specific -> least-specific order.  Always `defvar'd
explicitly: this file's mirror copy in `scripts/nelisp-stdlib-prelude.el'
is `lexical-binding: nil' but this file itself is `lexical-binding: t',
and a plain `let' only gets real dynamic (special) scoping across
function calls when the variable has been declared special first.")

(defvar nelisp-cl-generic--call-args nil
  "Dynamic: the full argument list of the generic-function call in
progress.  `cl-call-next-method' with no arguments of its own reuses
this (matches real Emacs `cl-generic').")

(defvar nelisp-cl-generic--current-name nil
  "Dynamic: the generic-function symbol of the call in progress, purely
for `cl-no-next-method''s signal data -- not used for dispatch.")

(defvar nelisp-cl-generic--conditions-registered nil
  "Non-nil once `nelisp-cl-generic--ensure-conditions' has run.")

(defun nelisp-cl-generic--ensure-conditions ()
  "Register `cl-no-applicable-method'/`cl-no-next-method' as real error
conditions, the same two `put's `define-error' itself makes with a
nil/`error' PARENT -- but done directly, and lazily (called from
`nelisp-cl-generic--ensure', i.e. the first time any generic is actually
defined, not at this file's own top level).  Two reasons neither
`define-error' nor a top-level `put' call works here: this file's mirror
copy in `scripts/nelisp-stdlib-prelude.el' loads sequentially, top to
bottom, and BOTH `define-error''s own `(unless (fboundp ...) ...)'
fallback AND plain `put' itself are defined LATER in that file than this
cl-generic block -- confirmed this session, calling either at this
point in the standalone reader is `void-function'.  Deferring to first
use, well after the whole prelude has finished loading, sidesteps the
ordering question entirely in both copies."
  (unless nelisp-cl-generic--conditions-registered
    (setq nelisp-cl-generic--conditions-registered t)
    (put 'cl-no-applicable-method 'error-conditions '(cl-no-applicable-method error))
    (put 'cl-no-applicable-method 'error-message "No applicable method")
    (put 'cl-no-next-method 'error-conditions '(cl-no-next-method error))
    (put 'cl-no-next-method 'error-message "No next method")
    ;; `cl-no-primary-method' (real Emacs's own condition, same name/
    ;; message): only reachable once `:before'/`:after'/`:around' exist
    ;; at all (the `:extra' combination extension below) -- a call whose
    ;; applicable methods are entirely before/after/around, no primary.
    (put 'cl-no-primary-method 'error-conditions '(cl-no-primary-method error))
    (put 'cl-no-primary-method 'error-message "No primary method")))

(defun nelisp-cl-generic--parse-specializer (arg-form)
  "Parse one specialized arglist entry of a `cl-defmethod' form.
Return a plist: `(:kind unspecialized)' for a bare symbol,
`(:kind type :type-name TYPE)' for `(VAR TYPE)',
`(:kind eql :value-form FORM)' for `(VAR (eql FORM))',
`(:kind head :head-value VALUE)' for `(VAR (head VALUE))', or
`(:kind subclass :subclass-name CLASS)' for `(VAR (subclass CLASS))'. (T59
addendum -- VALUE is taken literally, never evaluated, matching real
Emacs's own `(cadr specializer)').  Signals a loud `error' for any other
shape -- `&context', list-head grammar beyond plain `head' or `subclass',
or anything else Doc 185 §2.1 does not support."
  (cond
   ((symbolp arg-form) (list :kind 'unspecialized))
   ((and (consp arg-form) (symbolp (car arg-form))
         (consp (cdr arg-form)) (null (cddr arg-form)))
    (let ((spec (car (cdr arg-form))))
      (cond
       ((and (consp spec) (eq (car spec) 'eql)
             (consp (cdr spec)) (null (cddr spec)))
        (list :kind 'eql :value-form (car (cdr spec))))
       ((and (consp spec) (eq (car spec) 'head)
             (consp (cdr spec)) (null (cddr spec)))
        (list :kind 'head :head-value (car (cdr spec))))
       ((and (consp spec) (eq (car spec) 'subclass)
             (consp (cdr spec)) (null (cddr spec)))
        (list :kind 'subclass :subclass-name (car (cdr spec))))
       ((symbolp spec) (list :kind 'type :type-name spec))
       (t (error "cl-defmethod: unsupported specializer form %S (Doc 185 \
§2.1 -- type name, (eql VALUE), (head VALUE), or (subclass CLASS) only)"
                 arg-form)))))
   (t (error "cl-defmethod: unsupported specializer form %S (Doc 185 §2.1 \
-- type name, (eql VALUE), (head VALUE), or (subclass CLASS) only)"
             arg-form))))

(defun nelisp-cl-generic--parse-arglist (arglist name)
  "Split a `cl-defmethod' ARGLIST into (PLAIN-ARGLIST . SPEC-PLIST).
SPEC-PLIST contains `:specializers', an alist of required argument
positions and the parsed specializer at each position.  The old position-0
keys remain in the returned plist for compatibility with callers that only
need the first dispatch argument.  A non-bare-symbol specializer is allowed
on every required argument, matching real `cl-defmethod'; an unrecognised
`&FOO' lambda-list keyword (e.g. `&context') is still a loud `error' naming
NAME and the position."
  (let ((plain nil) (spec nil) (specializers nil) (i 0)
        (in-required t) (cur arglist))
    (while cur
      (let ((item (car cur)))
        (cond
         ((memq item '(&optional &rest &key &aux))
         (setq in-required nil)
          (push item plain))
         ((and (symbolp item) (> (length (symbol-name item)) 0)
               (eq (aref (symbol-name item) 0) ?&))
          (error "cl-defmethod %s: unsupported lambda-list keyword %S \
(Doc 185 subset: &optional/&rest/&key/&aux only)"
                 name item))
         ((not in-required)
          (push item plain))
         ((= i 0)
          (let ((parsed (nelisp-cl-generic--parse-specializer item)))
            (setq spec parsed)
            (when (consp item)
              (setq specializers
                    (cons (cons i parsed) specializers)))
            (push (if (consp item) (car item) item) plain))
          (setq i (1+ i)))
         (t
          (if (symbolp item)
              (push item plain)
            (let ((parsed (nelisp-cl-generic--parse-specializer item)))
              (setq specializers
                    (cons (cons i parsed) specializers))
              (push (car item) plain)))
          (setq i (1+ i)))))
      (setq cur (cdr cur)))
    (let ((out (or spec (list :kind 'unspecialized))))
      (setq out (plist-put out :specializers (nreverse specializers)))
      (cons (nreverse plain) out))))

(defun nelisp-cl-generic--builtin-type-p (type-name)
  "Non-nil iff TYPE-NAME is one of `cl-typep''s ten frozen builtins
(Doc 185 §2.1a -- `cl-typep' itself is never widened)."
  (memq type-name '(integer number float string symbol cons list vector null t)))

(defun nelisp-cl-generic--type-match (val type)
  "Doc 185 §3.2, verbatim: `cl-typep' for the ten builtins, struct
ancestry via `nelisp-cl-macros--struct-isa' for anything `recordp'."
  (cond
   ;; `funcall' with a QUOTED symbol, not a direct `(cl-typep val type)'
   ;; call: this file loads under host Emacs for host-ERT testing, and
   ;; `ert' (required by every test file) primes the FULL `cl-lib'
   ;; autoload table as a side effect -- so the bare symbol `cl-typep' in
   ;; CALL-HEAD position gets visited by `internal-macroexpand-for-load''s
   ;; macro-expansion walk of this DEFUN's body, which resolves whether it
   ;; is a macro via `autoload-do-load' -- fully loading real Emacs's
   ;; `cl-macs.el' as a side effect, which then unconditionally redefines
   ;; `cl-defstruct'/`cl-defgeneric'/`cl-defmethod' with ITS OWN versions,
   ;; silently clobbering this file's own overrides (confirmed by direct
   ;; repro this session: a bare `(defun f (v ty) (cl-typep v ty))', with
   ;; nothing else, flips `(featurep 'cl-macs)' from nil to t the moment
   ;; this file loads after `(require 'ert)').  A quoted symbol argument
   ;; to `funcall' is plain data, never visited by that walk.
   ((nelisp-cl-generic--builtin-type-p type) (funcall 'cl-typep val type))
   ((and (recordp val)
         (nelisp-cl-macros--struct-isa (nelisp--record-type val) type))
    t)
   (t nil)))

(defun nelisp-cl-generic--struct-parent (tag)
  "The immediate `:include' parent of struct type TAG, or nil."
  (let ((info (cdr (assq tag nelisp-cl-macros--struct-info))))
    (and info (car (cdr (memq :parent info))))))

(defun nelisp-cl-generic--struct-depth (tag target)
  "Number of `:include' hops from TAG up to TARGET.  Callers only call
this once `nelisp-cl-macros--struct-isa' has already confirmed TAG isa
TARGET, so the walk is expected to terminate at TARGET -- but the walk
is bounded defensively at the registry's own size rather than trusting
that invariant unconditionally: a `nelisp-cl-macros--struct-isa' that
lies (this session's own `s/(nelisp-cl-macros--struct-isa ...)/t)/'
`tools/gate-mutations.txt' row does exactly that) sends TAG walking up
through nil forever otherwise, hanging the whole dispatch instead of
signalling -- exactly the silent-vs-loud failure shape §3.5 exists to
avoid, just one level lower than a dispatch decision."
  (let ((n 0) (cur tag)
        (bound (1+ (length nelisp-cl-macros--struct-info))))
    (while (and (not (eq cur target)) (> bound 0))
      (setq cur (nelisp-cl-generic--struct-parent cur))
      (setq n (1+ n))
      (setq bound (1- bound)))
    (unless (eq cur target)
      (error "nelisp-cl-generic--struct-depth: %S never reaches %S via :include ancestry (nelisp-cl-macros--struct-isa said it would)"
             tag target))
    n))

(defun nelisp-cl-generic--same-specializer-p (a b)
  "Non-nil iff method entries A and B are the SAME method identity --
same specializer AND the same qualifiers (the `:extra' string, if any,
and the before/after/around `:combinator', if any).  A new entry
this-equal to an existing one REPLACES it (`cl-defmethod' redefinition,
not accumulation; Doc 185 §3.4).  Entries that share a specializer but
differ in `:extra' string are a DIFFERENT identity -- they coexist
instead of replacing each other (Doc 185 §2.2's `:extra' extension:
\"several `:extra' methods for the same specializers coexist\")."
  (and (eq (plist-get a :kind) (plist-get b :kind))
       (eq (plist-get a :type-name) (plist-get b :type-name))
       (eql (plist-get a :value) (plist-get b :value))
       (eql (plist-get a :head-value) (plist-get b :head-value))
       (eq (plist-get a :subclass-name) (plist-get b :subclass-name))
       (equal (plist-get a :specializers) (plist-get b :specializers))
       (equal (plist-get a :extra) (plist-get b :extra))
       (eq (plist-get a :combinator) (plist-get b :combinator))))

(defun nelisp-cl-generic--register-method (name entry)
  "Install ENTRY on generic NAME's method list.  An entry whose full
identity (`nelisp-cl-generic--same-specializer-p') already exists is
REPLACED IN PLACE, at its existing list position, never moved -- exactly
like real Emacs's own `cl-generic-define-method' (\"Keep the ordering;
important for methods with :extra qualifiers.\"): when several `:extra'
methods share one specializer, list position IS call order (newest-
defined-first, Doc 185 §2.2 extension), and redefining one of them must
not silently reorder its siblings.  A genuinely NEW identity is consed
onto the FRONT of the list (most-recently-defined-first) -- also matching
real Emacs.  Invalidates NAME's cached dispatch table (Doc 185 §3.4) so
the next call rebuilds it."
  (let* ((methods (get name 'nelisp-cl-generic--methods))
         (existing
          (let (hit)
            (dolist (e methods)
              (when (and (not hit) (nelisp-cl-generic--same-specializer-p e entry))
                (setq hit e)))
            hit)))
    (put name 'nelisp-cl-generic--methods
         (if existing
             (mapcar (lambda (e) (if (eq e existing) entry e)) methods)
           (cons entry methods)))
    (put name 'nelisp-cl-generic--dispatch-cache nil)))

(defun nelisp-cl-generic--subclass-name-equal-p (a b)
  "Return non-nil when class designators A and B denote the same class."
  (or (eq a b)
      (and (fboundp 'eieio-class-name)
           (ignore-errors
             (equal (eieio-class-name a) (eieio-class-name b))))))

(defun nelisp-cl-generic--subclass-parents (class)
  "Return CLASS and its known parents, without requiring EIEIO helpers."
  (cond
   ((fboundp 'cl--class-allparents)
    (ignore-errors (cl--class-allparents class)))
   ((fboundp 'cl--class-parents)
    (let ((todo (list class)) (seen nil) (out nil))
      (while todo
        (let ((cur (car todo)))
          (setq todo (cdr todo))
          (unless (memq cur seen)
            (push cur seen)
            (push cur out)
            (let ((obj (if (and (symbolp cur) (fboundp 'cl--find-class))
                           (ignore-errors (cl--find-class cur))
                         cur)))
              (setq todo
                    (append (ignore-errors (cl--class-parents obj)) todo))))))
      (nreverse out)))
   (t (list class))))

(defun nelisp-cl-generic--subclass-match-p (value target)
  "Return non-nil when VALUE denotes TARGET or one of its subclasses.
VALUE may be a class name/object, or the `(subclass CLASS)' data form used
by `cl-generic-generalizers'."
  (let* ((actual (if (and (consp value) (eq (car value) 'subclass))
                     (car (cdr value))
                   value))
         (target-object (if (and (symbolp target) (fboundp 'cl--find-class))
                            (or (ignore-errors (cl--find-class target)) target)
                          target))
         (actual-object (if (and (symbolp actual) (fboundp 'cl--find-class))
                            (or (ignore-errors (cl--find-class actual)) actual)
                          actual))
         (parents (nelisp-cl-generic--subclass-parents actual-object))
         (hit (nelisp-cl-generic--subclass-name-equal-p actual target)))
    (dolist (parent parents hit)
      (when (nelisp-cl-generic--subclass-name-equal-p parent target-object)
        (setq hit t)))))

(defun nelisp-cl-generic--subclass-matches (methods value)
  "Return subclass-specializer METHODS applicable to VALUE."
  (let (hits)
    (dolist (e methods)
      (when (nelisp-cl-generic--subclass-match-p
             value (plist-get e :subclass-name))
        (push e hits)))
    (nreverse hits)))

(defun nelisp-cl-generic--build-dispatch-table (name)
  "Partition NAME's registered methods into the four static specializer
tiers (Doc 185 §3.3) and cache the result on NAME's plist (§3.4).  Struct
vs. builtin `:type' matching stays a per-call decision
(`nelisp-cl-generic--type-match') since a struct named by a specializer
may not have been `cl-defstruct'-registered yet at the time this table is
first built.  `nelisp-cl-generic--methods' is stored most-recently-
defined-first (`nelisp-cl-generic--register-method'); the `push' below
reverses that per bucket while partitioning, so each bucket is
`nreverse'd back afterwards -- several `:extra' methods sharing one
specializer must stay ordered newest-first within their tier (Doc 185
§2.2 extension), matching real Emacs."
  (let (eql-methods head-methods subclass-methods type-methods
        unspecialized-methods)
    (dolist (e (get name 'nelisp-cl-generic--methods))
      (let ((k (plist-get e :kind)))
        (cond
         ((eq k 'eql) (push e eql-methods))
         ((eq k 'head) (push e head-methods))
         ((eq k 'subclass) (push e subclass-methods))
         ((eq k 'type) (push e type-methods))
         (t (push e unspecialized-methods)))))
    (let ((table (list :eql (nreverse eql-methods)
                        :head (nreverse head-methods)
                        :subclass (nreverse subclass-methods)
                        :type (nreverse type-methods)
                        :unspecialized (nreverse unspecialized-methods))))
      (put name 'nelisp-cl-generic--dispatch-cache table)
      table)))

(defun nelisp-cl-generic--dispatch-table (name)
  "NAME's cached dispatch table, building it lazily on first use
(Doc 185 §2.3/§3.4 -- this is the whole answer to Doc 157's eager,
global, `eval'-based ~280s prefill wall: nothing runs for a generic
until it is actually called, and the cost then is proportional only to
that one generic's own method count)."
  (or (get name 'nelisp-cl-generic--dispatch-cache)
      (nelisp-cl-generic--build-dispatch-table name)))

(defun nelisp-cl-generic--eql-matches (methods value)
  "All `:eql'-tier METHODS whose `:value' is `eql' to VALUE, in METHODS'
own order.  METHODS arrives newest-defined-first
(`nelisp-cl-generic--build-dispatch-table'); before the `:extra'
extension (Doc 185 §2.2), redefinition meant at most one method could
ever match a given VALUE here.  Several `:extra' variants of the same
`eql' specializer now coexist and chain via `cl-call-next-method',
newest first, exactly like a builtin-type or unspecialized tier."
  (let (hits)
    (dolist (e methods)
      (when (eql (plist-get e :value) value) (push e hits)))
    (nreverse hits)))

(defun nelisp-cl-generic--head-matches (methods value)
  "T59 addendum: `:head'-tier METHODS applicable to VALUE -- VALUE is a
cons whose `car' is `eql' to a method's `:head-value' (real Emacs's own
`(head VAL)' contract; generalizer priority 80, strictly between `eql'
\(100) and any type match (10), §2.1).  Never ambiguous: two different
`:head-value's can never both match one `car'.  METHODS arrives newest-
defined-first; filtering preserves that order, exactly like
`nelisp-cl-generic--eql-matches'."
  (let (hits)
    (dolist (e methods)
      (when (and (consp value) (eql (plist-get e :head-value) (car value)))
        (push e hits)))
    (nreverse hits)))

(defun nelisp-cl-generic--ordered-type-matches (methods value)
  "The `:type'-tier METHODS applicable to VALUE, most specific first
(Doc 185 §3.3).  Struct matches are ordered by
`nelisp-cl-generic--struct-depth' from VALUE's own runtime type -- always
a strict total order for one value, since `cl-defstruct''s `:include' is
single-parent only (§6.1), so every applicable struct specializer for a
given VALUE necessarily lies on that value's one ancestry chain.  Builtin
`cl-typep' matches carry no such order in this subset (§2.1a does not add
a type lattice on top of `cl-typep''s ten-symbol contract): two or more
DIFFERENT builtin type-names both matching the same VALUE is an ambiguous
dispatch, signalled loudly here rather than guessed at (§3.5).

Doc 185 §2.2's `:extra' extension: several methods can now share the
exact same specializer (struct type-name, or builtin type-name) -- that
is NOT ambiguous, just several applicable methods at that one
specificity tier, ordered newest-defined-first.  Grouping by
type-name/depth (an `assq'-keyed accumulator, each group's own methods
appended in METHODS' incoming, already newest-first order via `nconc' --
never `push', which would silently re-reverse an `:extra' group back to
oldest-first) keeps that distinct from a genuine cross-type-name tie,
which still errors exactly as before this extension."
  (let (struct-hits builtin-groups)
    (dolist (e methods)
      (let ((tn (plist-get e :type-name)))
        (when (nelisp-cl-generic--type-match value tn)
          (if (nelisp-cl-generic--builtin-type-p tn)
              (let ((cell (assq tn builtin-groups)))
                (if cell
                    (setcdr cell (nconc (cdr cell) (list e)))
                  (push (cons tn (list e)) builtin-groups)))
            (let* ((depth (nelisp-cl-generic--struct-depth
                           (nelisp--record-type value) tn))
                   (cell (assq depth struct-hits)))
              (if cell
                  (setcdr cell (nconc (cdr cell) (list e)))
                (push (cons depth (list e)) struct-hits)))))))
    (when (> (length builtin-groups) 1)
      (error "cl-generic: ambiguous dispatch -- builtin-type methods %S \
all match %S"
             (mapcar #'car builtin-groups)
             value))
    (append
     (apply #'append
            (mapcar #'cdr
                    (sort struct-hits (lambda (a b) (< (car a) (car b))))))
     (and builtin-groups (cdr (car builtin-groups))))))

(defun nelisp-cl-generic--method-specializers (method)
  "Return METHOD's dispatch specializers as a position alist.
The fallback keeps method entries created by older expansions readable."
  (or (plist-get method :specializers)
      (and (not (eq (plist-get method :kind) 'unspecialized))
           (list (cons 0 (list :kind (plist-get method :kind)
                               :type-name (plist-get method :type-name)
                               :value (plist-get method :value)
                               :head-value (plist-get method :head-value)
                               :subclass-name
                               (plist-get method :subclass-name)))))))

(defun nelisp-cl-generic--dispatch-specializer-match-p (specializer value)
  "Return non-nil when SPECIALIZER applies to VALUE."
  (pcase (plist-get specializer :kind)
    ('eql (eql (plist-get specializer :value) value))
    ('head (and (consp value)
                (eql (plist-get specializer :head-value) (car value))))
    ('subclass (nelisp-cl-generic--subclass-match-p
                value (plist-get specializer :subclass-name)))
    ('type (nelisp-cl-generic--type-match
            value (plist-get specializer :type-name)))
    (_ t)))

(defun nelisp-cl-generic--dispatch-specializer-rank (specializer)
  "Return the generalizer priority represented by SPECIALIZER."
  (pcase (plist-get specializer :kind)
    ('eql 100)
    ('head 80)
    ('subclass 60)
    ('type 10)
    (_ 0)))

(defun nelisp-cl-generic--multi-dispatch-p (name)
  "Return non-nil when NAME has a dispatch position other than zero."
  (let ((found nil))
    (dolist (method (get name 'nelisp-cl-generic--methods) found)
      (dolist (entry (nelisp-cl-generic--method-specializers method))
        (when (> (car entry) 0)
          (setq found t))))))

(defun nelisp-cl-generic--argument-precedence-order (name args)
  "Return NAME's dispatch positions in argument-precedence order."
  (or (get name 'nelisp-cl-generic--argument-precedence-order)
      (let ((positions nil) (i 0) (cur args))
        (while cur
          (push i positions)
          (setq i (1+ i) cur (cdr cur)))
        (nreverse positions))))

(defun nelisp-cl-generic--method-specializer-rank-at (method position)
  "Return METHOD's specificity rank at argument POSITION."
  (let ((entry (assq position
                     (nelisp-cl-generic--method-specializers method))))
    (if entry
        (nelisp-cl-generic--dispatch-specializer-rank (cdr entry))
      0)))

(defun nelisp-cl-generic--multi-applicable-methods (name args)
  "Return methods applicable to all their dispatch arguments.
Methods are ordered lexicographically by argument-precedence priority, with
total priority and method-table order breaking exact ties."
  (let ((indexed nil) (index 0)
        (precedence (nelisp-cl-generic--argument-precedence-order name args)))
    (dolist (method (get name 'nelisp-cl-generic--methods))
      (let ((score 0) (matches t))
        (dolist (entry (nelisp-cl-generic--method-specializers method))
          (let ((position (car entry))
                (specializer (cdr entry)))
            (if (and (< position (length args))
                     (nelisp-cl-generic--dispatch-specializer-match-p
                      specializer (nth position args)))
                (setq score (+ score
                               (nelisp-cl-generic--dispatch-specializer-rank
                                specializer)))
              (setq matches nil))))
        (when matches
          (let ((ranks nil) (order precedence))
            (while order
              (setq ranks
                    (cons (nelisp-cl-generic--method-specializer-rank-at
                           method (car order)) ranks)
                    order (cdr order)))
            (push (list score index method (nreverse ranks)) indexed))))
      (setq index (1+ index)))
    (mapcar #'caddr
            (sort indexed
                  (lambda (a b)
                    (let ((ar (nth 3 a)) (br (nth 3 b)) (different nil)
                          (greater nil))
                      (while (and ar br (not different))
                        (unless (= (car ar) (car br))
                          (setq different t
                                greater (> (car ar) (car br))))
                        (setq ar (cdr ar) br (cdr br)))
                      (or (and different greater)
                          (and (not different)
                               (or (> (car a) (car b))
                                   (and (= (car a) (car b))
                                        (< (cadr a) (cadr b))))))))))))

(defun nelisp-cl-generic--applicable-methods (name args)
  "NAME's methods applicable to ARGS, most specific first: an `eql'
match (if any) outranks a `head' match (T59 addendum), which outranks a
`subclass' match, which outranks every type match, which outranks the
unspecialized fallback (if any) -- Doc 185 §3.3, matching real Emacs's
generalizer priorities (eql 100 > head 80 > subclass 60 > type 10 > t 0).
Within one specificity tier, several `:extra' variants (Doc 185 §2.2
extension) are newest-defined-first, matching real Emacs."
  (let ((value (car args)))
    (if (nelisp-cl-generic--multi-dispatch-p name)
        (nelisp-cl-generic--multi-applicable-methods name args)
      (let ((table (nelisp-cl-generic--dispatch-table name)))
      (append
       (nelisp-cl-generic--eql-matches (plist-get table :eql) value)
       (nelisp-cl-generic--head-matches (plist-get table :head) value)
       (nelisp-cl-generic--subclass-matches
        (plist-get table :subclass) value)
       (nelisp-cl-generic--ordered-type-matches (plist-get table :type) value)
       (plist-get table :unspecialized))))))

(defun nelisp-cl-generic--invoke-combined (name args before after around primary)
  "Run one generic-function call once `nelisp-cl-generic--invoke' has
partitioned its applicable methods into BEFORE/AFTER/AROUND/PRIMARY
lists (each most-specific/most-recently-defined first; Doc 185 §2.2's
`:extra' extension -- reachable via bare `:before'/`:after'/`:around'
or `:extra STRING' plus one of them).  Mirrors real Emacs's own
`cl--generic-standard-method-combination' exactly: every BEFORE method
runs first, most-specific first, for effect only; then the PRIMARY chain
runs (walkable via `cl-call-next-method'/`cl-next-method-p' exactly as
when there is no combinator at all -- `:extra' primary variants
included, newest first); then every AFTER method runs, LEAST-specific
first (the reverse of BEFORE); the whole before/primary/after
combination's return value is the PRIMARY chain's own value, before/
after never affect it.  AROUND methods wrap that whole combination, most-
specific outermost, each able to reach the next (a less-specific
`:around', or the before/primary/after combination itself) via
`cl-call-next-method', exactly like a primary method.

The `combined' lambda and the around-chain below are only ever invoked
SYNCHRONOUSLY, within this function's own dynamic extent (directly, or
via `cl-call-next-method' called from a running `:around' method's body,
still inside this same call) -- unlike `nelisp-cl-generic--make-dispatcher'
\(which must build a lambda as literal DATA because that one is stored
via `defalias' and invoked arbitrarily later, long after its defining
call has returned), BEFORE/AFTER/PRIMARY/ARGS/NAME here are ordinary
function parameters of this call, still live on the stack, so both
lexical closure (`lisp/nelisp-cl-macros.el', `lexical-binding: t') and
dynamic scoping (this file's `scripts/nelisp-stdlib-prelude.el' mirror,
`lexical-binding: nil') resolve them correctly -- no data-lambda trick
needed here."
  (let* ((primary-fns (mapcar (lambda (e) (plist-get e :fn)) primary))
         (combined
          (lambda (&rest call-args)
            (let ((use-args (if call-args call-args args)))
              (dolist (bf before) (apply (plist-get bf :fn) use-args))
              (prog1
                  (let ((nelisp-cl-generic--next-methods (cdr primary-fns))
                        (nelisp-cl-generic--call-args use-args)
                        (nelisp-cl-generic--current-name name))
                    (apply (car primary-fns) use-args))
                (dolist (af (reverse after))
                  (apply (plist-get af :fn) use-args)))))))
    (if (null around)
        (apply combined args)
      (let ((around-fns (mapcar (lambda (e) (plist-get e :fn)) around)))
        (let ((nelisp-cl-generic--next-methods
               (append (cdr around-fns) (list combined)))
              (nelisp-cl-generic--call-args args)
              (nelisp-cl-generic--current-name name))
          (apply (car around-fns) args))))))

(defun nelisp-cl-generic--invoke (name args)
  "Dispatch a call to generic NAME with ARGS (Doc 185 §3-§3.5, extended
by §2.2's `:extra' combination): find the applicable methods for
`(car ARGS)', most specific first, partition them by combinator
(`:before'/`:after'/`:around'/primary -- bare or `:extra STRING' plus one
of the three), and run them via `nelisp-cl-generic--invoke-combined'.
Signals `cl-no-applicable-method' when nothing at all matches, or
`cl-no-primary-method' when something matches but none of it is a
primary (or `:extra'-primary) method."
  (let* ((applicable (nelisp-cl-generic--applicable-methods name args))
         before after around primary)
    (dolist (e applicable)
      (let ((c (plist-get e :combinator)))
        (cond
         ((eq c :before) (push e before))
         ((eq c :after) (push e after))
         ((eq c :around) (push e around))
         (t (push e primary)))))
    (setq before (nreverse before) after (nreverse after)
          around (nreverse around) primary (nreverse primary))
    (cond
     ((null applicable)
      ;; Data shape is `(cons name args)' -- `(GENERIC ARG1 ARG2 ...)',
      ;; flat -- measured against real Emacs 30.1 this session (not the
      ;; nested `(list generic-name args)' Doc 185 §3.5's table text
      ;; says; that reading does not match what real `cl-generic'
      ;; actually signals, confirmed empirically, so this follows the
      ;; measured behaviour over the doc's imprecise transcription).
      (signal 'cl-no-applicable-method (cons name args)))
     ((null primary)
      (signal 'cl-no-primary-method (cons name args)))
     (t
      (nelisp-cl-generic--invoke-combined name args before after around primary)))))

(defun cl-call-next-method (&rest new-args)
  "Call the next-most-specific applicable method in the `cl-defmethod'
dispatch chain currently running (Doc 185 §2.2).  With no NEW-ARGS,
reuses the current call's own arguments (matches real Emacs
`cl-generic').  Signals `cl-no-next-method' if there is no next method
(§3.5)."
  (if (null nelisp-cl-generic--next-methods)
      (signal 'cl-no-next-method (list nelisp-cl-generic--current-name))
    (let* ((fn (car nelisp-cl-generic--next-methods))
           (rest (cdr nelisp-cl-generic--next-methods))
           (use-args (if new-args new-args nelisp-cl-generic--call-args)))
      (let ((nelisp-cl-generic--next-methods rest)
            (nelisp-cl-generic--call-args use-args))
        (apply fn use-args)))))

(defun cl-next-method-p ()
  "Non-nil iff `cl-call-next-method' would find a next method right now."
  (and nelisp-cl-generic--next-methods t))

(defun nelisp-cl-generic--make-dispatcher (name)
  "Build NAME's dispatcher as a literal lambda FORM (data), with NAME
spliced in as a quoted constant rather than closed over.  Required
because `scripts/nelisp-stdlib-prelude.el' loads with
`lexical-binding: nil' -- a `(lambda (&rest args) (... name ...))' built
by a running function would look up `name' by DYNAMIC scoping at call
time there, long after this function's own local binding is gone, not
capture it.  The same technique `cl-defstruct''s own constructor/accessor
codegen above already uses, for the same reason."
  (list 'lambda '(&rest nelisp-cl-generic--dispatch-args)
        (list 'nelisp-cl-generic--invoke (list 'quote name)
              'nelisp-cl-generic--dispatch-args)))

(defun nelisp-cl-generic--ensure (name)
  "Make NAME callable as a Doc 185 generic function, once.  Idempotent
and callable from both `cl-defgeneric' and `cl-defmethod' -- real
`cl-generic' allows a bare `cl-defmethod' with no preceding
`cl-defgeneric', and this subset does too."
  (nelisp-cl-generic--ensure-conditions)
  (unless (get name 'nelisp-cl-generic--dispatcher-installed)
    (put name 'nelisp-cl-generic--dispatcher-installed t)
    (defalias name (nelisp-cl-generic--make-dispatcher name))))

(defun nelisp-cl-generic--parse-defgeneric-methods (name arglist body)
  "Parse the OPTIONS-AND-METHODS/DEFAULT-BODY grammar of a `cl-defgeneric'
call for NAME (Doc 185 §3.1, T81 addendum -- default-method bodies and
inline `:method' forms).  BODY is `cl-defgeneric''s own trailing macro
argument, with any leading docstring not yet stripped.  Real Emacs's own
`cl-defgeneric' (`lisp/emacs-lisp/cl-generic.el') walks
OPTIONS-AND-METHODS left to right consuming any run of forms whose CAR
is a keyword or the symbol `declare'; the first form whose CAR is
neither ends that run, and everything from there on (however many forms
follow) is \"assumed to be a default method body\" -- this reproduces
that exact walk, form for form, rather than a stricter re-derivation of
it.  Returns a list of method forms, each ready to splice as
`(cl-defmethod ,name ,@method)', in the exact order they should be
*defined* (source order): every `(:method QUALIFIERS ARGS BODY...)'
form's `(QUALIFIERS ARGS BODY...)' cdr verbatim -- QUALIFIERS/ARGS/BODY
go through `cl-defmethod''s own parsing and loud-failure matrix
unchanged, so standard bare qualifiers are accepted there exactly as they
would be at a top-level `cl-defmethod' call -- then,
if anything remains once that run ends, one final method
`(ARGLIST . REMAINING-BODY)': NAME's own ARGLIST, verbatim, with no
specializers at all, i.e. an ordinary unspecialized primary method (T81's
whole point: the default body needs no new dispatch machinery, it is
just another entry in the same method table `cl-defmethod' already
maintains, so it participates in dispatch, chains via
`cl-call-next-method', and gets replaced in place -- never duplicated --
by a later `cl-defgeneric' redefinition, exactly like any other
unspecialized primary method).  Nothing remaining (e.g. a `cl-defgeneric'
whose only content is a docstring and/or `declare') defines no default
method, exactly as an empty `cl-defgeneric' already did before this
addendum.

`(declare ...)' is accepted and its content is NOT applied: this subset
does not implement e.g. `(indent N)' or `(obsolete ...)' side effects --
editor/byte-compiler metadata, with no cl-generic dispatch semantics
either way (confirmed against host Emacs 31.1: `(declare (obsolete nil
\"26.1\"))' with no other body defines zero methods there too).  A
leading docstring and/or one `(:documentation STRING)' form are likewise
accepted and discarded, never installed as the generic's own
documentation (unchanged from before this addendum).  Giving a doc
string twice, or `declare' twice, is a loud `error' -- real Emacs
signals `Multiple doc strings for %S'/`Multiple \\=`declare\\=' for %S'
for the same two cases, measured this session.
`(:argument-precedence-order ...)' is accepted and controls the lexicographic
ordering of multiple dispatch positions, as in real `cl-generic'.  Any OTHER keyword-headed
form is a loud macroexpansion-time `error' naming it -- Doc 185 §3.5's
loud-failure discipline extended to this grammar, never a silently-
dropped option."
  (let ((methods nil) (have-doc nil) (have-declare nil) (looping t))
    (when (and body (stringp (car body)))
      (setq have-doc t)
      (setq body (cdr body)))
    (while (and looping body)
      (let* ((form (car body))
             (head (car-safe form)))
        (cond
         ((not (or (keywordp head) (eq head 'declare)))
          (setq looping nil))
         ((eq head :documentation)
          (when have-doc
            (error "cl-defgeneric %s: multiple doc strings (a leading \
docstring together with (:documentation ...), or (:documentation ...) \
given twice)" name))
          (setq have-doc t)
          (setq body (cdr body)))
         ((eq head 'declare)
          (when have-declare
            (error "cl-defgeneric %s: multiple `declare' forms" name))
          (setq have-declare t)
          (setq body (cdr body)))
         ((eq head :method)
          (push (cdr form) methods)
          (setq body (cdr body)))
         ((eq head :argument-precedence-order)
          (setq body (cdr body)))
         (t
          (error "cl-defgeneric %s: unsupported option %S (Doc 185 \
subset: (:documentation STRING), (declare ...), (:method ...), \
(:argument-precedence-order ...) only)"
                 name form)))))
    (when body
      (push (cons arglist body) methods))
    (nreverse methods)))

(defun nelisp-cl-generic--parse-argument-precedence-order (arglist body)
  "Return the required argument positions named by BODY's APO option.
An absent option returns nil, which means the normal left-to-right order."
  (let ((cur body) (names nil) (mandatory nil) (i 0) (found nil))
    (while (and cur (not found))
      (let ((form (car cur)))
        (when (and (consp form)
                   (eq (car form) :argument-precedence-order))
          (setq names (cdr form) found t)))
      (setq cur (cdr cur)))
    (when found
      (let ((in-required t))
        (dolist (arg arglist)
          (if (memq arg '(&optional &rest &key &aux))
              (setq in-required nil)
            (when in-required
              (push (cons (if (consp arg) (car arg) arg) i) mandatory)
              (setq i (1+ i))))))
      (let ((positions nil))
        (dolist (name names)
          (let ((cell (assq name mandatory)))
            (when cell (push (cdr cell) positions))))
        (nreverse positions)))))

(defmacro cl-defgeneric (name arglist &rest body)
  "Declare NAME as a generic function over ARGLIST (Doc 185 §3.1,
extended by the T81 addendum: default-method bodies and OPTIONS-AND-
METHODS are now supported, not just a bare declaration).  Installs NAME
as a dispatching function, then defines, in source order, every method
`nelisp-cl-generic--parse-defgeneric-methods' finds in BODY -- see that
function's docstring for the full per-option grammar (`(declare ...)',
`(:documentation ...)', `(:method ...)', `(:argument-precedence-order
...)', and the trailing default-method-body rule) and its two `error'
cases (a doc string or `declare' given twice).  A non-docstring,
non-option BODY form used to be an unconditional loud `error' before
this addendum; it is now the start of the default method body instead."
  (let ((precedence
         (nelisp-cl-generic--parse-argument-precedence-order arglist body)))
    `(prog1 ',name
       (nelisp-cl-generic--ensure ',name)
       (put ',name 'nelisp-cl-generic--argument-precedence-order
            ',precedence)
       ,@(mapcar (lambda (m) `(cl-defmethod ,name ,@m))
                 (nelisp-cl-generic--parse-defgeneric-methods name arglist body)))))

(defmacro cl-defmethod (name &rest args)
  "Define a method on generic NAME (Doc 185's subset of real
`cl-defmethod', extended by §2.2's `:extra' qualifier).  The supported
qualifier grammar is: no qualifier (a primary method); or `:extra
STRING', optionally followed by exactly one of `:before'/`:after'/
`:around' -- real Emacs's own grammar (`cl-generic--method-qualifier-p'
accepts any non-list leading token; `cl--generic-standard-method-
combination' only ever strips a LEADING `:extra STRING' pair, so
`:extra' must come first when combined with a combinator -- confirmed
this session: real Emacs itself rejects `:around :extra \"x\"', only
`:extra \"x\" :around' works).  Several `:extra' methods for the same
specializer coexist (rather than one replacing another) and chain via
`cl-call-next-method', ordered newest-defined-first, then the plain
\(non-`:extra') primary method if any -- redefining an `:extra' method
with the SAME string replaces it in place, not accumulating a duplicate
and not reordering its siblings (`nelisp-cl-generic--register-method').
Bare `:before'/`:after'/`:around' methods use the standard method
combination directly; the same qualifiers may also follow `:extra STRING'.
This remains a small standard-combination subset and does not implement
arbitrary `define-method-combination'.  Any other leading non-list token is
a loud `error' naming it.

Required arguments may carry a type name (builtin `cl-typep' symbol or
`cl-defstruct' name), an `(eql VALUE)' form, a `(head VALUE)' form (T59
addendum), or a `(subclass CLASS)' form.  Specializers at more than one
argument position are supported and are jointly applicable; a bare symbol
is unspecialized (§2.1/§3.1).  The method body can call
`cl-call-next-method'/`cl-next-method-p' (§2.2)."
  (let (extra combinator)
    (when (and args (eq (car args) :extra))
      (unless (and (cdr args) (stringp (cadr args)))
        (error "cl-defmethod %s: :extra must be followed by a string" name))
      (setq extra (cadr args) args (cddr args)))
    (when (and args (not (listp (car args))))
      (let ((q (car args)))
        (unless (memq q '(:before :after :around))
          (error "cl-defmethod %s: unsupported method-combination qualifier \
%S (Doc 185 subset: primary methods, bare :before/:after/:around, or \
`:extra STRING' optionally combined with one of those qualifiers)"
                 name q))
        (setq combinator q args (cdr args))))
    (let* ((arglist (car args))
           (body (cdr args))
           (parsed (nelisp-cl-generic--parse-arglist arglist name))
           (plain-arglist (car parsed))
           (spec (cdr parsed))
           (specializers (plist-get spec :specializers))
           (kind (plist-get spec :kind))
           (type-name (plist-get spec :type-name))
           (value-form (and (eq kind 'eql) (plist-get spec :value-form)))
           ;; A bare symbol in an EQL specializer is data (e.g. `markdown'),
           ;; while compound forms remain evaluated at method definition time.
           (value-expr (if (symbolp value-form)
                           (list 'quote value-form)
                         value-form))
           (head-value (and (eq kind 'head) (plist-get spec :head-value))))
      `(prog1 ',name
         (nelisp-cl-generic--ensure ',name)
         (nelisp-cl-generic--register-method
          ',name
          (list :kind ',kind :type-name ',type-name :value ,value-expr
                :head-value ',head-value
                :subclass-name ',(plist-get spec :subclass-name)
                :specializers
                (list
                 ,@(mapcar
                    (lambda (entry)
                      (let* ((pos (car entry))
                             (sp (cdr entry))
                             (skind (plist-get sp :kind)))
                        `(cons ,pos
                               (list :kind ',skind
                                     :type-name ',(plist-get sp :type-name)
                                     :value ,(let ((v (plist-get sp :value-form)))
                                               (if (symbolp v)
                                                   (list 'quote v)
                                                 v))
                                     :head-value ',(plist-get sp :head-value)
                                     :subclass-name ',(plist-get sp :subclass-name)))))
                    specializers))
                :extra ,extra :combinator ',combinator
                :fn (lambda ,plain-arglist ,@body)))))))

;; ---------------------------------------------------------------------------
;; Doc 49 Wave 7 follow-up (2026-05-22): minimal cl-lib subset wired into
;; the same module so `(require 'cl-lib)' resolves via featurep without
;; needing a separate `lisp/cl-lib.el' bake entry.  Coverage = what
;; `nelisp-aot-compiler.el' and `scripts/compile-elisp-objects.el'
;; need to run end-to-end under `nelisp --batch'.
;;
;; Already provided elsewhere (kept here for reference):
;;   `cl-defun'   — lisp/nelisp-stdlib-eval-special.el:432 (full &key)
;;   `cl-loop'    — line 230 above
;;   `cl-block' / `cl-return' / `cl-return-from' — lines 42-56
;;   `cl-defstruct' — line 352
;; ---------------------------------------------------------------------------

(defun cl-mapcar (fn seq &rest more-seqs)
  "Apply FN to corresponding elements of SEQ and MORE-SEQS, returning a list.
The walk stops at the shortest sequence.  Like Emacs `cl-mapcar'."
  (let ((all (cons seq more-seqs))
        (result nil)
        (done nil))
    (while (not done)
      (let ((heads nil) (tails nil) (any-empty nil) (cur all))
        (while (and cur (not any-empty))
          (let ((s (car cur)))
            (if (null s)
                (setq any-empty t)
              (setq heads (cons (car s) heads))
              (setq tails (cons (cdr s) tails))))
          (setq cur (cdr cur)))
        (if any-empty
            (setq done t)
          (setq result (cons (apply fn (nreverse heads)) result))
          (setq all (nreverse tails)))))
    (nreverse result)))

(defun cl-mapc (fn seq &rest more-seqs)
  "Apply FN to corresponding elements of SEQ and MORE-SEQS for side effect.
Returns SEQ (= first sequence) like Emacs `cl-mapc'."
  (let ((all (cons seq more-seqs))
        (done nil))
    (while (not done)
      (let ((heads nil) (tails nil) (any-empty nil) (cur all))
        (while (and cur (not any-empty))
          (let ((s (car cur)))
            (if (null s)
                (setq any-empty t)
              (setq heads (cons (car s) heads))
              (setq tails (cons (cdr s) tails))))
          (setq cur (cdr cur)))
        (if any-empty
            (setq done t)
          (apply fn (nreverse heads))
          (setq all (nreverse tails)))))
    seq))

(defun cl-subseq (seq start &optional end)
  "Return the subsequence of SEQ from START up to END (default end of SEQ).
Supports proper lists only (= what `nelisp-aot-compiler.el' uses)."
  (let ((tail (nthcdr start seq))
        (n (if end (- end start) nil))
        (acc nil))
    (if (null n)
        (copy-sequence tail)
      (let ((i 0))
        (while (and (< i n) tail)
          (setq acc (cons (car tail) acc))
          (setq tail (cdr tail))
          (setq i (1+ i)))
        (nreverse acc)))))

(defun cl-remove-if-not (pred seq)
  "Return a list of SEQ elements where (PRED ELT) is non-nil.
Linear, allocates a fresh list; preserves order."
  (let ((acc nil) (cur seq))
    (while cur
      (when (funcall pred (car cur))
        (setq acc (cons (car cur) acc)))
      (setq cur (cdr cur)))
    (nreverse acc)))

(defmacro cl-labels (bindings &rest body)
  "Bind locally-recursive functions BINDINGS and run BODY.
BINDINGS = ((NAME (ARGS...) BODY...) ...).  Expands to a `let'-bound
funarg + `flet'-style cl-flet substitution so each binding can call
itself by NAME.  This is the minimal shape used by
`nelisp-aot-compiler.el' (single-binding walk-helper recursion);
sibling cross-calls within a single `cl-labels' block are NOT
supported (= would need a forward-declared placeholder set, deferred)."
  (let ((let-bindings nil)
        (defalias-forms nil)
        (unalias-forms nil))
    (dolist (b bindings)
      (let* ((name (car b))
             (fn-formals (car (cdr b)))
             (fn-body (cdr (cdr b)))
             (saved (intern (format "--cl-labels-saved-%s" name))))
        (setq let-bindings
              (cons (list saved (list 'and (list 'fboundp (list 'quote name))
                                      (list 'symbol-function (list 'quote name))))
                    let-bindings))
        (setq defalias-forms
              (cons (list 'defalias (list 'quote name)
                          (cons 'lambda (cons fn-formals fn-body)))
                    defalias-forms))
        (setq unalias-forms
              (cons (list 'if saved
                          (list 'defalias (list 'quote name) saved)
                          (list 'fmakunbound (list 'quote name)))
                    unalias-forms))))
    (list 'let (nreverse let-bindings)
          (cons 'unwind-protect
                (cons (cons 'progn (append (nreverse defalias-forms) body))
                      (nreverse unalias-forms))))))

(defmacro cl-incf (place &optional delta)
  "Increment PLACE by DELTA (default 1).
Symbol PLACE expands to `(setq PLACE (+ PLACE DELTA))'; generalised
PLACE (= cl-defstruct accessor, `car' / `cdr' / `aref' / `nth')
delegates to `setf' so the same call works on records and lists.

Note: PLACE is read TWICE in the generalised path because that
matches what `setf' supports; if PLACE has side-effects, hoist
it into a `let' first."
  (if (symbolp place)
      (list 'setq place (list '+ place (or delta 1)))
    (list 'setf place (list '+ place (or delta 1)))))

(defmacro defsubst (name args &rest body)
  "Define NAME as an inline function.  Standalone NeLisp has no
byte-compiler so defsubst is a strict synonym for `defun'."
  (cons 'defun (cons name (cons args body))))

(defun cl-every (pred seq)
  "Return non-nil iff (PRED ELT) is non-nil for every ELT in SEQ."
  (let ((all t) (cur seq))
    (while (and all cur)
      (unless (funcall pred (car cur)) (setq all nil))
      (setq cur (cdr cur)))
    all))

;; ---------------------------------------------------------------------------
;; Doc 49 Wave 7 R6c (2026-05-22) — minimal `backquote' macro.
;;
;; The reader (`nelisp-stdlib-reader.el') desugars source-level `\`'
;; and `,' / `,@' into `(backquote FORM)' / `(comma X)' / `(comma-at X)'
;; cons forms.  Without a `backquote' macro, evaluating these dies with
;; `(void-function backquote)' — observed when loading
;; `nelisp-sexp-layout.el' whose final `defconst' uses `((NAME . ,V) ...)'.
;;
;; Scope (Minimal):
;;   `atom              =>  'atom
;;   `,X                =>  X
;;   `(A B C)           =>  (list 'A 'B 'C)
;;   `(A ,X B)          =>  (list 'A X 'B)
;;   `(A ,@X B)         =>  (append (list 'A) X (list 'B))
;;   `(A . ,X)          =>  (cons 'A X)
;;   `(A . X)           =>  (cons 'A 'X)
;;
;; Nested backquote (Doc <handoff> 2026-08-22 fix): a nested `` `X''
;; increments the quasiquote LEVEL for its own content; a `,'/`,@'
;; decrements it.  A comma only fires (evaluates its argument now) when
;; the level reaches 0 -- i.e. it is balanced by exactly as many commas
;; as enclosing backquotes.  While the level stays above 0 the marker is
;; rebuilt as inert `(comma ...)'/`(comma-at ...)'/`(backquote ...)' data
;; (its own content still recursively expanded at the adjusted level, so
;; a `,,X' double-unquote still cancels back down to 0 and evaluates X).
;; This matches host Emacs's own `backquote.el' depth semantics.
;; Still unsupported (signal):  vector quasi `[A ,X B].
;; ---------------------------------------------------------------------------

(defun nelisp--bq-expand (form &optional level)
  "Return the expansion of FORM under `backquote' at nesting LEVEL.
LEVEL defaults to 1 (directly inside one backquote).  A `,'/`,@' at
LEVEL 1 fires immediately (its argument is evaluated at macro-expanded
runtime); above LEVEL 1 it is preserved as inert marker data one level
shallower, so a matching further `,' can still cancel it down to 0."
  (let ((level (or level 1)))
    (cond
     ((vectorp form)
      (signal 'error (list "nelisp-bq: vector quasi not supported")))
     ((not (consp form))
      (list 'quote form))
     ((eq (car form) 'comma)
      (if (= level 1)
          (cadr form)
        (list 'list (list 'quote 'comma)
              (nelisp--bq-expand (cadr form) (1- level)))))
     ((eq (car form) 'comma-at)
      (if (= level 1)
          (signal 'error (list "nelisp-bq: top-level ,@ not allowed"))
        (list 'list (list 'quote 'comma-at)
              (nelisp--bq-expand (cadr form) (1- level)))))
     ((eq (car form) 'backquote)
      ;; A nested backquote increments the level for its own content and
      ;; is itself rebuilt as inert `(backquote ...)' data -- it is only
      ;; ever "consumed" by a comma at the matching depth, never by
      ;; simply appearing inside an outer backquote.
      (list 'list (list 'quote 'backquote)
            (nelisp--bq-expand (cadr form) (1+ level))))
     (t (nelisp--bq-expand-list form level)))))

(defun nelisp--bq-expand-list (form level)
  "Walk list FORM at nesting LEVEL, producing the expansion.
Recognises both (... ,X ...) interior unquote and (... . ,X) dotted
unquote / (... . ,@X) dotted splice patterns, at any LEVEL (see
`nelisp--bq-expand')."
  (let ((parts nil)        ; alist entries (KIND . EXPR) where KIND = list|splice
        (cur form)
        (tail-expr nil)
        (done nil)
        (has-splice nil))
    (while (and (not done) (consp cur))
      (let ((head (car cur)))
        (cond
         ;; cdr-position bare `comma' → source had `. ,X'.
         ((eq head 'comma)
          (if (= level 1)
              (setq tail-expr (cadr cur))
            (setq tail-expr (list 'list (list 'quote 'comma)
                                   (nelisp--bq-expand (cadr cur) (1- level)))))
          (setq done t))
         ;; cdr-position bare `comma-at' → source had `. ,@X'.
         ((eq head 'comma-at)
          (if (= level 1)
              (progn (setq tail-expr (cadr cur)) (setq has-splice t))
            (setq tail-expr (list 'list (list 'quote 'comma-at)
                                   (nelisp--bq-expand (cadr cur) (1- level)))))
          (setq done t))
         (t
          (let ((elem head))
            (cond
             ((and (consp elem) (eq (car elem) 'comma-at))
              (if (= level 1)
                  (progn
                    (setq has-splice t)
                    (push (cons 'splice (cadr elem)) parts))
                (push (cons 'list
                             (list 'list (list 'quote 'comma-at)
                                   (nelisp--bq-expand (cadr elem) (1- level))))
                      parts)))
             ((and (consp elem) (eq (car elem) 'comma))
              (if (= level 1)
                  (push (cons 'list (cadr elem)) parts)
                (push (cons 'list
                             (list 'list (list 'quote 'comma)
                                   (nelisp--bq-expand (cadr elem) (1- level))))
                      parts)))
             (t
              (push (cons 'list (nelisp--bq-expand elem level)) parts))))
          (setq cur (cdr cur))))))
    (when (and (not done) (not (null cur)) (not (consp cur)))
      (setq tail-expr (list 'quote cur)))
    (nelisp--bq-build (nreverse parts) tail-expr has-splice)))

(defun nelisp--bq-build (parts tail has-splice)
  "Build the final form from PARTS list, TAIL expression, HAS-SPLICE flag."
  (cond
   ((and (null parts) (null tail))
    (list 'quote nil))
   ((null parts) tail)
   ((and (not has-splice) (null tail))
    (cons 'list (mapcar 'cdr parts)))
   ((not has-splice)
    (let ((acc tail) (rp (reverse parts)))
      (while rp
        (setq acc (list 'cons (cdr (car rp)) acc))
        (setq rp (cdr rp)))
      acc))
   (t
    (let ((args nil) (p parts))
      (while p
        (let ((kind (car (car p))) (val (cdr (car p))))
          (cond
           ((eq kind 'list) (push (list 'list val) args))
           ((eq kind 'splice) (push val args))))
        (setq p (cdr p)))
      (setq args (nreverse args))
      (when tail (setq args (append args (list tail))))
      (cons 'append args)))))

(defmacro backquote (form)
  "Expand FORM as a quasiquoted template (NeLisp minimal subset).
See `nelisp--bq-expand' for the supported shapes."
  (nelisp--bq-expand form))

(unless (fboundp 'zerop) (defun zerop (n) "Return t if N is zero." (= n 0)))

;; ---------------------------------------------------------------------------
;; Wave A21-fix (2026-05-24) — cl-case / cl-position / cl-set-difference /
;; cl-gensym / cl-macrolet.  Standalone NeLisp loads `nelisp-bytecode.el'
;; which uses these five `cl-' helpers — host Emacs provides them through
;; `cl-lib.el', NeLisp ships them here so the same source compiles + runs
;; identically on both substrates (= byte-identity, Rust LOC delta = 0).
;; ---------------------------------------------------------------------------

(defmacro cl-case (expr &rest clauses)
  "Common Lisp `case' macro: dispatch on EXPR equality.
Each CLAUSE = (KEYS BODY...).  KEYS is either a single literal
(matched with `eql' = NeLisp `equal') or a list of literals
(matched with `memq'/`member'); `t' or `otherwise' matches any.
Expands to a `let' + `cond'."
  (let* ((sym (intern (format "--cl-case-%s"
                              (if (fboundp 'cl-gensym)
                                  (symbol-name (cl-gensym "v"))
                                "v"))))
         (cond-clauses
          (mapcar (lambda (clause)
                    (let ((keys (car clause)) (body (cdr clause)))
                      (cond
                       ((or (eq keys t) (eq keys 'otherwise))
                        (cons t body))
                       ((and (consp keys) (consp (cdr keys)))
                        ;; List of keys.
                        (cons (list 'or
                                    (cons 'and
                                          (mapcar (lambda (k)
                                                    (list 'eql sym
                                                          (list 'quote k)))
                                                  (list (car keys))))
                                    (list 'member sym (list 'quote keys)))
                              body))
                       ((consp keys)
                        ;; Single-element list (KEY).
                        (cons (list 'eql sym (list 'quote (car keys)))
                              body))
                       (t
                        ;; Bare symbol/atom = single key.
                        (cons (list 'eql sym (list 'quote keys))
                              body)))))
                  clauses)))
    (list 'let (list (list sym expr))
          (cons 'cond cond-clauses))))

(defun cl-position (item seq &rest keys)
  "Return the 0-based index of ITEM in SEQ (list), or nil if absent.
NeLisp minimal: list-only.  Recognised KEYS:
  :test FN   — predicate to use (default `equal').
Unknown keys are silently ignored."
  (let* ((test (or (let ((p keys) (v nil))
                     (while p
                       (when (eq (car p) :test)
                         (setq v (car (cdr p))))
                       (setq p (cdr (cdr p))))
                     v)
                   #'equal))
         (i 0) (cur seq) (found nil))
    (while (and cur (not found))
      (if (funcall test (car cur) item)
          (setq found i)
        (setq i (1+ i))
        (setq cur (cdr cur))))
    found))

(defun cl-set-difference (list1 list2)
  "Return elements of LIST1 not present in LIST2, preserving order.
NeLisp minimal: no `:test' / `:key' keywords; uses `equal'."
  (let ((acc nil) (cur list1))
    (while cur
      (unless (member (car cur) list2)
        (setq acc (cons (car cur) acc)))
      (setq cur (cdr cur)))
    (nreverse acc)))

(defvar nelisp-cl-macros--gensym-counter 0
  "Monotone counter used by `cl-gensym' for unique symbol names.")

(defun cl-gensym (&optional prefix)
  "Return a fresh uninterned symbol named PREFIX (default \"G\") + counter.
NeLisp uses `intern' (= the standalone runtime has no
`make-symbol' equivalent that yields a usable callable name)."
  (setq nelisp-cl-macros--gensym-counter
        (1+ nelisp-cl-macros--gensym-counter))
  (intern (format "%s%d"
                  (or prefix "G")
                  nelisp-cl-macros--gensym-counter)))

;; ---------------------------------------------------------------------------
;; cl-macrolet — lexical macro bindings.
;;
;; Strategy: at expansion time, walk BODY and replace each call to a
;; bound macro NAME with its expansion.  The macro body is evaluated
;; under a `let' that binds the macro's formal parameters to the raw
;; (unevaluated) argument forms — same contract as host `defmacro' /
;; `cl-macrolet'.  Result is spliced back into BODY in place of the
;; call.  Sub-forms of the call's args are walked first so nested
;; cl-macrolet calls expand inside-out.
;;
;; Walker honours common special forms: `quote' / `function' / `lambda'
;; pass their inert subforms through unchanged; other lists recurse.
;; ---------------------------------------------------------------------------

(defun nelisp-cl-macros--macrolet-expand-one (entry args)
  "Expand a single cl-macrolet call.
ENTRY = (NAME FORMALS BODY...).  ARGS = the raw (unevaluated)
argument forms from the call site.  Returns the expansion form."
  (let* ((formals (car (cdr entry)))
         (body    (cdr (cdr entry)))
         (bindings nil)
         (rest-args args)
         (rest-mode nil)
         (rest-var nil))
    (while formals
      (let ((f (car formals)))
        (cond
         ((eq f '&rest)
          (setq rest-mode t)
          (setq rest-var (car (cdr formals)))
          (setq formals nil))
         (t
          (setq bindings (cons (list f (list 'quote (car rest-args)))
                               bindings))
          (setq rest-args (cdr rest-args))
          (setq formals (cdr formals))))))
    (when rest-mode
      (setq bindings (cons (list rest-var (list 'quote rest-args))
                           bindings)))
    (eval (list 'let (nreverse bindings)
                (cons 'progn body)))))

(defun nelisp-cl-macros--macrolet-walk-bindings (bindings env)
  "Walk BINDINGS (a list of (VAR INIT) pairs or bare symbols) for `let'."
  (mapcar (lambda (b)
            (cond
             ((symbolp b) b)
             ((and (consp b) (consp (cdr b)))
              (list (car b)
                    (nelisp-cl-macros--macrolet-walk (car (cdr b)) env)))
             (t b)))
          bindings))

(defun nelisp-cl-macros--macrolet-walk-list (forms env)
  "Walk a list of FORMS."
  (mapcar (lambda (s) (nelisp-cl-macros--macrolet-walk s env)) forms))

(defun nelisp-cl-macros--macrolet-walk (form env)
  "Walk FORM, replacing calls to macros bound in ENV with their expansions.
ENV is an alist (NAME . (FORMALS BODY...))."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((eq (car form) 'function)
    ;; Recur into the body of a lambda inside #'(lambda ...), but
    ;; leave the wrapper intact.
    (let ((arg (car (cdr form))))
      (if (and (consp arg) (eq (car arg) 'lambda))
          (list 'function
                (cons 'lambda
                      (cons (car (cdr arg))
                            (nelisp-cl-macros--macrolet-walk-list
                             (cdr (cdr arg)) env))))
        form)))
   ((eq (car form) 'lambda)
    (cons 'lambda
          (cons (car (cdr form))
                (nelisp-cl-macros--macrolet-walk-list (cdr (cdr form)) env))))
   ((or (eq (car form) 'let) (eq (car form) 'let*))
    (cons (car form)
          (cons (nelisp-cl-macros--macrolet-walk-bindings (cadr form) env)
                (nelisp-cl-macros--macrolet-walk-list (cddr form) env))))
   ((eq (car form) 'condition-case)
    (let ((var (cadr form))
          (protected (car (cddr form)))
          (handlers (cdr (cddr form))))
      (cons 'condition-case
            (cons var
                  (cons (nelisp-cl-macros--macrolet-walk protected env)
                        (mapcar (lambda (h)
                                  (if (consp h)
                                      (cons (car h)
                                            (nelisp-cl-macros--macrolet-walk-list
                                             (cdr h) env))
                                    h))
                                handlers))))))
   ((eq (car form) 'cond)
    (cons 'cond
          (mapcar (lambda (clause)
                    (if (consp clause)
                        (nelisp-cl-macros--macrolet-walk-list clause env)
                      clause))
                  (cdr form))))
   ((eq (car form) 'pcase)
    ;; (pcase EXPR (PAT BODY...)...) — EXPR is a form, PAT is literal,
    ;; each clause BODY is walked.
    (cons 'pcase
          (cons (nelisp-cl-macros--macrolet-walk (cadr form) env)
                (mapcar (lambda (clause)
                          (if (consp clause)
                              (cons (car clause)
                                    (nelisp-cl-macros--macrolet-walk-list
                                     (cdr clause) env))
                            clause))
                        (cddr form)))))
   ((eq (car form) 'setq)
    ;; Even-position elements are forms; odd-position are symbol names.
    (let ((rest (cdr form)) (out nil))
      (while rest
        (push (car rest) out)
        (setq rest (cdr rest))
        (when rest
          (push (nelisp-cl-macros--macrolet-walk (car rest) env) out)
          (setq rest (cdr rest))))
      (cons 'setq (nreverse out))))
   (t
    (let* ((head (car form))
           (entry (and (symbolp head) (assq head env))))
      (cond
       (entry
        ;; Recur into the (raw) args first so inner cl-macrolet calls
        ;; expand inside-out, then expand this call.
        (let ((walked-args (nelisp-cl-macros--macrolet-walk-list
                            (cdr form) env)))
          (nelisp-cl-macros--macrolet-walk
           (nelisp-cl-macros--macrolet-expand-one entry walked-args)
           env)))
       ((symbolp head)
        ;; Ordinary function-like call: leave head, walk args.
        (cons head
              (nelisp-cl-macros--macrolet-walk-list (cdr form) env)))
       (t
        ;; Head is itself a list (= sub-form, e.g. a binding pair).
        ;; Recurse into both head and cdr so nested macro calls expand.
        (cons (nelisp-cl-macros--macrolet-walk head env)
              (nelisp-cl-macros--macrolet-walk-list (cdr form) env))))))))

(defmacro setf (&rest pairs)
  "Generalised assignment macro (NeLisp minimal).
Each pair PLACE VAL assigns VAL to PLACE.  Supported PLACE shapes:
  - SYMBOL                 → `(setq SYMBOL VAL)'
  - (ACCESSOR REC)         where ACCESSOR is a registered cl-defstruct
                            slot accessor → `(nelisp--record-set REC I VAL)'
  - (car X)  / (cdr X)     → `(setcar X VAL)' / `(setcdr X VAL)'
  - (aref V I) / (nth I L) → `(aset V I VAL)' / `(setcar (nthcdr I L) VAL)'
  - registered simple setter → calls setter with PLACE args + VAL
  - registered struct setter → calls setter with REC + VAL
Other shapes signal a host `error' at expand time."
  (when (null pairs) (signal 'error (list "setf: empty body")))
  (let ((forms nil))
    (while pairs
      (let ((place (car pairs))
            (val (cadr pairs)))
        (setq pairs (cdr (cdr pairs)))
        (push
         (cond
          ((symbolp place)
           (list 'setq place val))
          ((and (consp place) (eq (car place) 'car))
           (list 'setcar (cadr place) val))
          ((and (consp place) (eq (car place) 'cdr))
           (list 'setcdr (cadr place) val))
          ((and (consp place) (eq (car place) 'aref))
           (list 'aset (cadr place) (caddr place) val))
          ((and (consp place) (eq (car place) 'nth))
           (list 'setcar (list 'nthcdr (cadr place) (caddr place)) val))
          ((and (consp place) (symbolp (car place))
                (get (car place) 'cl-simple-setter))
           (cons 'funcall
                 (cons (list 'quote (get (car place) 'cl-simple-setter))
                       (append (cdr place) (list val)))))
          ((and (consp place) (symbolp (car place))
                (get (car place) 'cl-struct-setter))
           (list 'funcall
                 (list 'quote (get (car place) 'cl-struct-setter))
                 (cadr place)
                 val))
          ((and (consp place) (symbolp (car place))
                (assq (car place) nelisp-cl-macros--accessor-info))
           (let ((idx (cdr (assq (car place)
                                 nelisp-cl-macros--accessor-info))))
             (list 'nelisp--record-set (cadr place) idx val)))
          (t
           (signal 'error
                   (list "setf: unsupported place"
                         (and (consp place) (car place))))))
         forms)))
    (if (cdr forms)
        (cons 'progn (nreverse forms))
      (car forms))))

(defmacro cl-macrolet (bindings &rest body)
  "Locally bind macros for the lexical scope of BODY.
BINDINGS = ((NAME (ARGS...) BODY...) ...).  Each NAME is treated as
a macro: calls `(NAME a1 a2 ...)' inside BODY are replaced at
expansion time with the result of evaluating the macro BODY with
ARGS bound to the raw (unevaluated) call-site forms.

NeLisp minimal implementation: a code walker substitutes calls
in BODY.  Macros may use backquote; expansion produces code that
naturally lexically captures whatever the surrounding `let'
bindings provide.  &rest is honoured."
  (let ((env (mapcar (lambda (b)
                       (cons (car b) (cdr b)))
                     bindings)))
    (cons 'progn
          (mapcar (lambda (s)
                    (nelisp-cl-macros--macrolet-walk s env))
                  body))))

(defun nelisp-cl-macros--symbol-macrolet-walk (form env)
  "Replace symbol references in FORM according to ENV."
  (cond
   ((symbolp form)
    (let ((cell (assq form env)))
      (if cell (cdr cell) form)))
   ((not (consp form)) form)
   ((memq (car form) '(quote function)) form)
   ((eq (car form) 'setq)
    (let ((pairs (cdr form))
          (out nil))
      (while pairs
        (let* ((place (car pairs))
               (value (cadr pairs))
               (cell (and (symbolp place) (assq place env))))
          (setq out
                (append out
                        (list (if cell (cdr cell) place)
                              (nelisp-cl-macros--symbol-macrolet-walk
                               value env)))))
        (setq pairs (cddr pairs)))
      (cons 'setq out)))
   ((memq (car form) '(let let*))
    (let ((bindings (cadr form))
          (body (cddr form))
          (shadowed nil)
          (new-bindings nil)
          new-env)
      (dolist (binding bindings)
        (let ((var (if (symbolp binding) binding (car binding))))
          (push var shadowed)
          (push (if (symbolp binding)
                    binding
                  (list var
                        (nelisp-cl-macros--symbol-macrolet-walk
                         (cadr binding) env)))
                new-bindings)))
      (setq new-env
            (let ((cur env) (acc nil))
              (while cur
                (unless (memq (caar cur) shadowed)
                  (push (car cur) acc))
                (setq cur (cdr cur)))
              (nreverse acc)))
      (cons (car form)
            (cons (nreverse new-bindings)
                  (mapcar (lambda (body-form)
                            (nelisp-cl-macros--symbol-macrolet-walk
                             body-form new-env))
                          body)))))
   (t
    (mapcar (lambda (item)
              (nelisp-cl-macros--symbol-macrolet-walk item env))
            form))))

(defmacro cl-symbol-macrolet (bindings &rest body)
  "Minimal symbol macro substitution used by generator.el CPS rewrites."
  (let ((env (mapcar (lambda (binding)
                       (cons (car binding) (cadr binding)))
                     bindings)))
    (cons 'progn
          (mapcar (lambda (body-form)
                    (nelisp-cl-macros--symbol-macrolet-walk body-form env))
                  body))))

(provide 'cl-lib)
(provide 'nelisp-cl-macros)

;; nelisp-cl-macros.el ends here
