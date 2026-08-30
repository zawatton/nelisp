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
;;   for VAR from N to M                  numeric inclusive
;;   for VAR from N below M               numeric exclusive
;;   for VAR = INIT then UPDATE          accumulator (deferred)
;;   with VAR = VAL                       binding
;;   do FORM …                            unconditional side-effect
;;   collect FORM                         accumulate into list
;;   sum FORM                             accumulate sum
;;   count FORM                           count truthy
;;   when COND return FORM                early-exit with FORM
;;   when COND do FORM                    conditional side-effect
;;   while COND                           continue while COND non-nil
;;   until COND                           continue until COND non-nil
;;   bodyless (= no for/with/do keyword)  infinite loop with cl-return
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

(defun nelisp-cl-macros--loop-build (clauses)
  "Build expansion for `cl-loop' CLAUSES.

See header for supported shapes.  Returns a form that, when the
shape is unrecognised, expands to nil (= caller gets a no-op
expansion rather than a runtime error)."
  (let ((var nil) (list-form nil) (do-forms nil) (collect-form nil)
        (sum-form nil) (count-form nil) (with-bindings nil)
        (when-return-cond nil) (when-return-form nil)
        (when-do-cond nil) (when-do-forms nil)
        (when-collect-cond nil) (when-collect-form nil)
        (numeric-from nil) (numeric-to nil) (numeric-below nil)
        (repeat-count nil)
        (while-cond nil) (until-cond nil)
        (bodyless-forms nil)
        (cur clauses) (recognised t))
    ;; Detect bodyless form: first clause is NOT a known keyword.
    (when (and clauses
               (not (memq (car clauses)
                          '(for with do collect sum count when
                                while until repeat finally return
                                named))))
      (setq bodyless-forms clauses
            cur nil
            recognised t))
    (while (and cur recognised)
      (let ((kw (car cur)))
        (cond
         ((eq kw 'for)
          (setq var (car (cdr cur)))
          (cond
           ((eq (car (cdr (cdr cur))) 'in)
            (setq list-form (car (cdr (cdr (cdr cur)))))
            (setq cur (cdr (cdr (cdr (cdr cur))))))
           ((eq (car (cdr (cdr cur))) 'from)
            (setq numeric-from (car (cdr (cdr (cdr cur)))))
            (let ((kw2 (car (cdr (cdr (cdr (cdr cur))))))
                  (val2 (car (cdr (cdr (cdr (cdr (cdr cur))))))))
              (cond
               ((eq kw2 'to)
                (setq numeric-to val2)
                (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
               ((eq kw2 'below)
                (setq numeric-below val2)
                (setq cur (cdr (cdr (cdr (cdr (cdr (cdr cur))))))))
               (t (setq recognised nil)))))
           (t (setq recognised nil))))
         ((eq kw 'repeat)
          (setq repeat-count (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'do)
          (setq do-forms (cons (car (cdr cur)) do-forms))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'collect)
          (setq collect-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'sum)
          (setq sum-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'count)
          (setq count-form (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'with)
          (let ((wname (car (cdr cur))))
            (when (eq (car (cdr (cdr cur))) '=)
              (setq with-bindings
                    (append with-bindings
                            (list (list wname (car (cdr (cdr (cdr cur))))))))
              (setq cur (cdr (cdr (cdr (cdr cur))))))))
         ((eq kw 'while)
          (setq while-cond (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'until)
          (setq until-cond (car (cdr cur)))
          (setq cur (cdr (cdr cur))))
         ((eq kw 'when)
          (let ((cond-form (car (cdr cur)))
                (next-kw (car (cdr (cdr cur))))
                (next-form (car (cdr (cdr (cdr cur))))))
            (cond
             ((eq next-kw 'return)
              (setq when-return-cond cond-form
                    when-return-form next-form
                    cur (cdr (cdr (cdr (cdr cur))))))
             ((eq next-kw 'do)
              (setq when-do-cond cond-form
                    when-do-forms (cons next-form when-do-forms)
                    cur (cdr (cdr (cdr (cdr cur)))))
              (while (and cur (eq (car cur) 'and))
                (let ((and-kw (car (cdr cur)))
                      (and-form (car (cdr (cdr cur)))))
                  (cond
                   ((eq and-kw 'do)
                    (setq when-do-forms (cons and-form when-do-forms)
                          cur (cdr (cdr (cdr cur)))))
                   ((eq and-kw 'collect)
                    (setq when-collect-cond cond-form
                          when-collect-form and-form
                          cur (cdr (cdr (cdr cur)))))
                   (t (setq recognised nil
                            cur nil))))))
             ((eq next-kw 'collect)
              (setq when-collect-cond cond-form
                    when-collect-form next-form
                    cur (cdr (cdr (cdr (cdr cur))))))
             (t (setq recognised nil)))))
         (t (setq recognised nil)))))
    (cond
     ((not recognised) nil)
     ;; Bodyless infinite loop wrapped in cl-block nil.
     (bodyless-forms
      (list 'cl-block nil
            (cons 'while
                  (cons t bodyless-forms))))
     ;; Numeric `for VAR from N {to,below} M' [do/collect/sum/count FORM ...]
     ((and numeric-from (or numeric-to numeric-below))
      (let ((cmp (if numeric-to '<= '<))
            (limit (or numeric-to numeric-below)))
        (nelisp-cl-macros--loop-build-counted
         (list var numeric-from) (list cmp var limit)
         (list 'setq var (list '1+ var))
         collect-form sum-form count-form do-forms with-bindings)))
     ;; `repeat N' [do/collect/sum/count FORM ...] -- unconditional count,
     ;; no loop variable.
     (repeat-count
      (let ((n-sym (make-symbol "--loop-n--")))
        (nelisp-cl-macros--loop-build-counted
         (list n-sym repeat-count) (list '> n-sym 0)
         (list 'setq n-sym (list '1- n-sym))
         collect-form sum-form count-form do-forms with-bindings)))
     ;; While / until plain loops (= no iterator).
     (while-cond
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'while (cons while-cond rev)))))
     (until-cond
      (let ((rev nil))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (cons 'while (cons (list 'not until-cond) rev)))))
     ;; `for VAR in LIST when COND return FORM' — early exit pattern.
     (when-return-cond
      (let ((tag-sym (make-symbol "--loop-tag--"))
            (result-sym (make-symbol "--loop-r--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (list 'let (cons (list result-sym nil) with-bindings)
              (list 'catch (list 'quote tag-sym)
                    (list 'dolist (list loop-var list-form)
                          (nelisp-cl-macros--loop-wrap-body
                           var loop-var
                           (list (list 'when when-return-cond
                                       (list 'setq result-sym when-return-form)
                                       (list 'throw (list 'quote tag-sym) nil))))))
              result-sym)))
     ;; `for VAR in LIST collect FORM'
     ((or collect-form when-collect-cond)
      (let ((acc-sym (make-symbol "--loop-acc--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--")))
            (body nil)
            (rev nil))
        (when collect-form
          (setq body
                (append body
                        (list (list 'setq acc-sym
                                    (list 'cons collect-form acc-sym))))))
        (when when-do-cond
          (while when-do-forms
            (setq rev (cons (car when-do-forms) rev))
            (setq when-do-forms (cdr when-do-forms)))
          (setq body
                (append body
                        (list (cons 'when
                                    (cons when-do-cond rev))))))
        (when when-collect-cond
          (setq body
                (append body
                        (list (list 'when when-collect-cond
                                    (list 'setq acc-sym
                                          (list 'cons when-collect-form acc-sym)))))))
        (list 'let (cons (list acc-sym nil) with-bindings)
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body var loop-var body))
              (list 'nreverse acc-sym))))
     ;; `for VAR in LIST sum FORM'
     (sum-form
      (let ((acc-sym (make-symbol "--loop-sum--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (list 'let (cons (list acc-sym 0) with-bindings)
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body
                     var loop-var
                     (list (list 'setq acc-sym (list '+ acc-sym sum-form)))))
              acc-sym)))
     ;; `for VAR in LIST count FORM'
     (count-form
      (let ((acc-sym (make-symbol "--loop-count--"))
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (list 'let (cons (list acc-sym 0) with-bindings)
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body
                     var loop-var
                     (list (list 'when count-form
                                 (list 'setq acc-sym (list '+ acc-sym 1))))))
              acc-sym)))
     ;; `for VAR in LIST when COND do FORM …'
     (when-do-cond
      (let ((rev nil)
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (while when-do-forms
          (setq rev (cons (car when-do-forms) rev))
          (setq when-do-forms (cdr when-do-forms)))
        (list 'let with-bindings
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body
                     var loop-var
                     (list (cons 'when (cons when-do-cond rev))))))))
     ;; `for VAR in LIST do FORM …'
     (do-forms
      (let ((rev nil)
            (loop-var (if (symbolp var) var (make-symbol "--loop-item--"))))
        (while do-forms (setq rev (cons (car do-forms) rev))
               (setq do-forms (cdr do-forms)))
        (list 'let with-bindings
              (list 'dolist (list loop-var list-form)
                    (nelisp-cl-macros--loop-wrap-body var loop-var rev)))))
     (t (list 'let with-bindings nil)))))

(defmacro cl-loop (&rest clauses)
  "Loop CLAUSES — minimal CL-style iteration macro.

See `nelisp-cl-macros--loop-build' commentary for supported shapes.
Patterns this stub does not recognise expand to nil."
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
;; `cl-defgeneric'/`cl-defmethod' subset: type + eql specializers only
;; (struct dispatch via the already-working `nelisp-cl-macros--struct-isa'
;; ancestry walker above, NOT `cl-typep', which knows nothing about
;; `cl-defstruct'-generated types -- docs/design/185-cl-generic-subset.org
;; §1.2/§2.1a), primary methods only plus `cl-call-next-method'/
;; `cl-next-method-p' (§2.2), a lazily-built per-generic dispatch table
;; (§2.3/§3.4 -- this is the direct answer to Doc 157's 280-second eager
;; global-prefill wall, §1.4: nothing here does `eval'-based dispatcher
;; compilation, and no work happens for a generic until it is first
;; called).  Every unsupported form -- a method-combination qualifier
;; (`:before'/`:after'/`:around'/anything but none), a specializer kind
;; other than type/eql/unspecialized, a specializer on an argument
;; position other than 0 -- is a loud `error' at `cl-defmethod'
;; macroexpansion time, never a silent no-dispatch (§3.5).
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
    (put 'cl-no-next-method 'error-message "No next method")))

(defun nelisp-cl-generic--parse-specializer (arg-form)
  "Parse one position-0 arglist entry of a `cl-defmethod' form.
Return a plist: `(:kind unspecialized)' for a bare symbol,
`(:kind type :type-name TYPE)' for `(VAR TYPE)', or
`(:kind eql :value-form FORM)' for `(VAR (eql FORM))'.  Signals a loud
`error' for any other shape -- head/list-head specializers, `&context',
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
       ((symbolp spec) (list :kind 'type :type-name spec))
       (t (error "cl-defmethod: unsupported specializer form %S (Doc 185 \
§2.1 -- type name or (eql VALUE) only)"
                 arg-form)))))
   (t (error "cl-defmethod: unsupported specializer form %S (Doc 185 §2.1 \
-- type name or (eql VALUE) only)"
             arg-form))))

(defun nelisp-cl-generic--parse-arglist (arglist name)
  "Split a `cl-defmethod' ARGLIST into (PLAIN-ARGLIST . SPEC-PLIST).
SPEC-PLIST (from `nelisp-cl-generic--parse-specializer') describes
position 0 only -- Doc 185 §3.1 supports a single dispatch argument.  A
non-bare-symbol specializer at any later position, or any unrecognised
`&FOO' lambda-list keyword (e.g. `&context'), is a loud `error' naming
NAME and the position."
  (let ((plain nil) (spec nil) (i 0) (in-required t) (cur arglist))
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
            (push (if (consp item) (car item) item) plain))
          (setq i (1+ i)))
         (t
          (unless (symbolp item)
            (error "cl-defmethod %s: specializer on argument position %d \
not supported (Doc 185 §3.1 -- position 0 only)"
                   name i))
          (push item plain)
          (setq i (1+ i)))))
      (setq cur (cdr cur)))
    (cons (nreverse plain)
          (or spec (list :kind 'unspecialized)))))

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
  "Non-nil iff method entries A and B specialize the same way -- a new
entry this-equal to an existing one REPLACES it (`cl-defmethod'
redefinition, not accumulation; Doc 185 §3.4)."
  (and (eq (plist-get a :kind) (plist-get b :kind))
       (eq (plist-get a :type-name) (plist-get b :type-name))
       (eql (plist-get a :value) (plist-get b :value))))

(defun nelisp-cl-generic--register-method (name entry)
  "Install ENTRY on generic NAME's method list, replacing any existing
entry with the same specializer (`nelisp-cl-generic--same-specializer-p')
rather than accumulating a duplicate.  Invalidates NAME's cached
dispatch table (Doc 185 §3.4) so the next call rebuilds it."
  (let ((kept nil))
    (dolist (e (get name 'nelisp-cl-generic--methods))
      (unless (nelisp-cl-generic--same-specializer-p e entry)
        (push e kept)))
    (put name 'nelisp-cl-generic--methods (cons entry (nreverse kept)))
    (put name 'nelisp-cl-generic--dispatch-cache nil)))

(defun nelisp-cl-generic--build-dispatch-table (name)
  "Partition NAME's registered methods into the three static specializer
tiers (Doc 185 §3.3) and cache the result on NAME's plist (§3.4).  Struct
vs. builtin `:type' matching stays a per-call decision
(`nelisp-cl-generic--type-match') since a struct named by a specializer
may not have been `cl-defstruct'-registered yet at the time this table is
first built."
  (let (eql-methods type-methods unspecialized-methods)
    (dolist (e (get name 'nelisp-cl-generic--methods))
      (let ((k (plist-get e :kind)))
        (cond
         ((eq k 'eql) (push e eql-methods))
         ((eq k 'type) (push e type-methods))
         (t (push e unspecialized-methods)))))
    (let ((table (list :eql eql-methods :type type-methods
                        :unspecialized unspecialized-methods)))
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

(defun nelisp-cl-generic--eql-match (methods value)
  "The first (only, by construction -- redefinition replaces, never
duplicates) eql-tier method whose `:value' is `eql' to VALUE, or nil."
  (let (hit)
    (dolist (e methods)
      (when (and (not hit) (eql (plist-get e :value) value))
        (setq hit e)))
    hit))

(defun nelisp-cl-generic--ordered-type-matches (methods value)
  "The `:type'-tier METHODS applicable to VALUE, most specific first
(Doc 185 §3.3).  Struct matches are ordered by
`nelisp-cl-generic--struct-depth' from VALUE's own runtime type -- always
a strict total order for one value, since `cl-defstruct''s `:include' is
single-parent only (§6.1), so every applicable struct specializer for a
given VALUE necessarily lies on that value's one ancestry chain.  Builtin
`cl-typep' matches carry no such order in this subset (§2.1a does not add
a type lattice on top of `cl-typep''s ten-symbol contract): two or more
of them matching the same VALUE is an ambiguous dispatch, signalled
loudly here rather than guessed at (§3.5)."
  (let (struct-hits builtin-hits)
    (dolist (e methods)
      (let ((tn (plist-get e :type-name)))
        (when (nelisp-cl-generic--type-match value tn)
          (if (nelisp-cl-generic--builtin-type-p tn)
              (push e builtin-hits)
            (push (cons (nelisp-cl-generic--struct-depth
                         (nelisp--record-type value) tn)
                        e)
                  struct-hits)))))
    (when (> (length builtin-hits) 1)
      (error "cl-generic: ambiguous dispatch -- builtin-type methods %S \
all match %S"
             (mapcar (lambda (e) (plist-get e :type-name)) builtin-hits)
             value))
    (append
     (mapcar #'cdr (sort struct-hits (lambda (a b) (< (car a) (car b)))))
     builtin-hits)))

(defun nelisp-cl-generic--applicable-methods (name value)
  "NAME's methods applicable to VALUE, most specific first: an `eql'
match (if any) outranks every type match, which outranks the
unspecialized fallback (if any) -- Doc 185 §3.3."
  (let ((table (nelisp-cl-generic--dispatch-table name)))
    (append
     (let ((hit (nelisp-cl-generic--eql-match (plist-get table :eql) value)))
       (and hit (list hit)))
     (nelisp-cl-generic--ordered-type-matches (plist-get table :type) value)
     (plist-get table :unspecialized))))

(defun nelisp-cl-generic--invoke (name args)
  "Dispatch a call to generic NAME with ARGS (Doc 185 §3-§3.5): find the
applicable methods for `(car ARGS)', most specific first, and call the
first one with `cl-call-next-method'/`cl-next-method-p' able to walk the
rest.  Signals `cl-no-applicable-method' when nothing matches."
  (let* ((applicable (nelisp-cl-generic--applicable-methods name (car args)))
         (fns (mapcar (lambda (e) (plist-get e :fn)) applicable)))
    (if (null fns)
        ;; Data shape is `(cons name args)' -- `(GENERIC ARG1 ARG2 ...)',
        ;; flat -- measured against real Emacs 30.1 this session (not the
        ;; nested `(list generic-name args)' Doc 185 §3.5's table text
        ;; says; that reading does not match what real `cl-generic'
        ;; actually signals, confirmed empirically, so this follows the
        ;; measured behaviour over the doc's imprecise transcription).
        (signal 'cl-no-applicable-method (cons name args))
      (let ((nelisp-cl-generic--next-methods (cdr fns))
            (nelisp-cl-generic--call-args args)
            (nelisp-cl-generic--current-name name))
        (apply (car fns) args)))))

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

(defmacro cl-defgeneric (name arglist &rest body)
  "Declare NAME as a generic function over ARGLIST (Doc 185 §3.1).
Installs NAME as a dispatching function with no methods yet -- add
methods with `cl-defmethod'.  BODY may be a single leading docstring
only: this subset does not implement CLOS-style default-method bodies,
so a non-docstring BODY form is a loud macroexpansion-time `error'
rather than a silently-ignored default method."
  (ignore arglist)
  (when (and body (stringp (car body))) (setq body (cdr body)))
  (when body
    (error "cl-defgeneric %s: default-method body not supported by this \
subset -- declare with no body, add methods via cl-defmethod (Doc 185 \
§3.1)"
           name))
  `(prog1 ',name (nelisp-cl-generic--ensure ',name)))

(defmacro cl-defmethod (name &rest args)
  "Define a method on generic NAME (Doc 185's subset of real
`cl-defmethod').  No method-combination qualifier is supported --
`:before'/`:after'/`:around'/anything but none is a loud `error' naming
the qualifier (§3.5).  Exactly one specializer, on argument position 0
only: a type name (builtin `cl-typep' symbol or `cl-defstruct' name), an
`(eql VALUE)' form, or a bare (unspecialized) symbol (§2.1/§3.1).  The
method body can call `cl-call-next-method'/`cl-next-method-p' (§2.2)."
  (let (qualifier)
    (when (and args (not (listp (car args))))
      (setq qualifier (car args) args (cdr args)))
    (when qualifier
      (error "cl-defmethod %s: unsupported method-combination qualifier \
%S (Doc 185 subset: primary methods only)"
             name qualifier))
    (let* ((arglist (car args))
           (body (cdr args))
           (parsed (nelisp-cl-generic--parse-arglist arglist name))
           (plain-arglist (car parsed))
           (spec (cdr parsed))
           (kind (plist-get spec :kind))
           (type-name (plist-get spec :type-name))
           (value-form (and (eq kind 'eql) (plist-get spec :value-form))))
      `(prog1 ',name
         (nelisp-cl-generic--ensure ',name)
         (nelisp-cl-generic--register-method
          ',name
          (list :kind ',kind :type-name ',type-name :value ,value-form
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
