;;; nl-prelude.el --- Result/Option error handling for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 169 (nl-prelude / nl-condition) Phase 1: value-based error
;; handling for Elisp code running on NeLisp (or host Emacs).
;;
;; Public API (Phase 1 scope is locked to Result / Option / `nl-?'):
;;
;;   Construction:  `nl-ok' `nl-err' `nl-some' `nl-none'
;;   Predicates:    `nl-ok-p' `nl-err-p' `nl-result-p'
;;                  `nl-some-p' `nl-none-p' `nl-option-p'
;;   Extraction:    `nl-unwrap' `nl-unwrap-or' `nl-unwrap-or-else'
;;   Transform:     `nl-map' `nl-map-err' `nl-and-then'
;;                  `nl-ok->option' `nl-option->ok'
;;   Aggregation:   `nl-collect'
;;   Early return:  `nl-?' inside `nl-defun' / `nl-lambda' (+ `nl-let*')
;;
;; Representation (Doc 169 section 2.1): tagged conses, not cl-defstruct.
;;
;;   Result: (nl--ok . VALUE) / (nl--err . ERROR)
;;   Option: (nl--some . VALUE) / nl--none
;;
;; Tags are interned symbols compared with `eq'.
;;
;; `nl-?' constraints (Doc 169 section 2.3, enforced at expansion time):
;;   - using `nl-?' outside `nl-defun' / `nl-lambda' is an expansion
;;     error, not a runtime `cl-return-from' failure
;;   - `nl-?' must not cross a nested plain `lambda'; such uses are
;;     rejected when the enclosing `nl-defun' / `nl-lambda' expands.
;;     A nested `nl-lambda' / `nl-defun' opens its own return block.
;;
;; The rejection walker inspects the unexpanded body, so `nl-?' forms
;; produced by other macros inside the body are not seen.  This is a
;; documented v0.1 limitation (see README.org).
;;
;; Design constraints:
;;   - pure Lisp only, no dependency beyond core + cl-lib macros
;;   - must run unchanged on `target/nelisp' standalone (no ert there;
;;     see test/nl-prelude-standalone-smoke.el)

;;; Code:

(require 'cl-lib)
(require 'nl-prelude-trampoline)

;;;; Errors -----------------------------------------------------------

(define-error 'nl-error "nl-prelude error")
(define-error 'nl-unwrap-error "Unwrap of an err/none value" 'nl-error)
(define-error 'nl-type-error "Value is not a Result/Option" 'nl-error)

;;;; Representation ---------------------------------------------------

(defconst nl-none 'nl--none
  "The Option none value (Doc 169 section 2.1).")

(defun nl-ok (value)
  "Wrap VALUE as a successful Result."
  (cons 'nl--ok value))

(defun nl-err (error)
  "Wrap ERROR as a failed Result."
  (cons 'nl--err error))

(defun nl-some (value)
  "Wrap VALUE as a present Option."
  (cons 'nl--some value))

;;;; Predicates -------------------------------------------------------

(defun nl-ok-p (object)
  "Return non-nil when OBJECT is an ok Result."
  (eq (car-safe object) 'nl--ok))

(defun nl-err-p (object)
  "Return non-nil when OBJECT is an err Result."
  (eq (car-safe object) 'nl--err))

(defun nl-result-p (object)
  "Return non-nil when OBJECT is a Result (ok or err)."
  (or (nl-ok-p object) (nl-err-p object)))

(defun nl-some-p (object)
  "Return non-nil when OBJECT is a some Option."
  (eq (car-safe object) 'nl--some))

(defun nl-none-p (object)
  "Return non-nil when OBJECT is the none Option."
  (eq object 'nl--none))

(defun nl-option-p (object)
  "Return non-nil when OBJECT is an Option (some or none)."
  (or (nl-some-p object) (nl-none-p object)))

;;;; Internal helpers -------------------------------------------------

(defun nl--payload-or-signal (object caller)
  "Validate OBJECT as Result/Option for CALLER; return its kind symbol.
The kind is one of `ok', `err', `some', `none'.  Signal `nl-type-error'
when OBJECT is neither a Result nor an Option."
  (cond ((nl-ok-p object) 'ok)
        ((nl-err-p object) 'err)
        ((nl-some-p object) 'some)
        ((nl-none-p object) 'none)
        (t (signal 'nl-type-error (list caller object)))))

;;;; Shared syntax-walker primitives ---------------------------------

(defun nl--walk-proper-list (tail)
  "Return the proper-list prefix of TAIL, dropping a dotted terminator.
Static walkers must tolerate dotted source forms rather than iterating
them with `dolist', which signals `wrong-type-argument'."
  (let ((out nil))
    (while (consp tail)
      (push (car tail) out)
      (setq tail (cdr tail)))
    (nreverse out)))

(defun nl--walk-quoted-p (form)
  "Return non-nil when FORM is quoted data a syntax walker must skip.
`(function (lambda ...))' remains live code because walkers may need to
inspect its body for captures or annotated calls."
  (and (consp form) (memq (car form) '(quote function))
       (not (and (eq (car form) 'function)
                 (consp (car (cdr form)))
                 (eq (car (car (cdr form))) 'lambda)))))

(defun nl--walk-backquote-p (form)
  "Return non-nil when FORM is a backquote template."
  (and (consp form) (memq (car form) '(\` backquote))))

(defun nl--walk-unquoted (template acc)
  "Collect TEMPLATE's unquote-position forms onto ACC.
A backquote template is data except where unquote and
unquote-splicing introduce live code."
  (cond
   ((not (consp template)) acc)
   ((memq (car template) '(\, \,@ unquote unquote-splicing))
    (cons (car (cdr template)) acc))
   (t
    (let ((rest template))
      (while (consp rest)
        (setq acc (nl--walk-unquoted (car rest) acc))
        (setq rest (cdr rest)))
      acc))))

(defun nl--walk-live-parts (form)
  "Return the parts of FORM a syntax walker should treat as code.
For a backquote template these are its unquote positions; otherwise
the result is a one-element list containing FORM."
  (if (nl--walk-backquote-p form)
      (nreverse (nl--walk-unquoted (car (cdr form)) nil))
    (list form)))

;;;; Extraction -------------------------------------------------------

(defun nl-unwrap (result)
  "Return the value inside an ok/some RESULT.
Signal `nl-unwrap-error' on an err Result (with the err payload) or on
none.  Signal `nl-type-error' when RESULT is not a Result/Option."
  (pcase (nl--payload-or-signal result 'nl-unwrap)
    ((or 'ok 'some) (cdr result))
    ('err (signal 'nl-unwrap-error (list (cdr result))))
    ('none (signal 'nl-unwrap-error (list 'nl--none)))))

(defun nl-unwrap-or (result default)
  "Return the value inside an ok/some RESULT, else DEFAULT."
  (pcase (nl--payload-or-signal result 'nl-unwrap-or)
    ((or 'ok 'some) (cdr result))
    (_ default)))

(defun nl-unwrap-or-else (result fn)
  "Return the value inside an ok/some RESULT, else call FN.
On an err Result, FN receives the err payload; on none, FN receives no
arguments."
  (pcase (nl--payload-or-signal result 'nl-unwrap-or-else)
    ((or 'ok 'some) (cdr result))
    ('err (funcall fn (cdr result)))
    ('none (funcall fn))))

;;;; Transform --------------------------------------------------------

(defun nl-map (result fn)
  "Apply FN to the value inside an ok/some RESULT.
Return a Result/Option of the same kind.  err and none pass through
unchanged (the identical object)."
  (pcase (nl--payload-or-signal result 'nl-map)
    ('ok (nl-ok (funcall fn (cdr result))))
    ('some (nl-some (funcall fn (cdr result))))
    (_ result)))

(defun nl-map-err (result fn)
  "Apply FN to the error inside an err RESULT.
ok passes through unchanged.  RESULT must be a Result."
  (pcase (nl--payload-or-signal result 'nl-map-err)
    ('err (nl-err (funcall fn (cdr result))))
    ('ok result)
    (_ (signal 'nl-type-error (list 'nl-map-err result)))))

(defun nl-and-then (result fn)
  "Monadic bind: call FN on the value inside an ok/some RESULT.
FN must itself return a Result (for ok input) or an Option (for some
input).  err and none short-circuit: they are returned unchanged and FN
is not called."
  (pcase (nl--payload-or-signal result 'nl-and-then)
    ((or 'ok 'some) (funcall fn (cdr result)))
    (_ result)))

(defun nl-ok->option (result)
  "Convert a Result to an Option: ok -> some, err -> none (payload dropped)."
  (pcase (nl--payload-or-signal result 'nl-ok->option)
    ('ok (nl-some (cdr result)))
    ('err nl-none)
    (_ (signal 'nl-type-error (list 'nl-ok->option result)))))

(defun nl-option->ok (option error)
  "Convert an Option to a Result: some -> ok, none -> (nl-err ERROR)."
  (pcase (nl--payload-or-signal option 'nl-option->ok)
    ('some (nl-ok (cdr option)))
    ('none (nl-err error))
    (_ (signal 'nl-type-error (list 'nl-option->ok option)))))

;;;; Aggregation ------------------------------------------------------

(defun nl-collect (results)
  "Collect a list of Results into one Result.
When every element of RESULTS is ok, return (nl-ok LIST-OF-VALUES) in
order.  Return the first err encountered unchanged, without inspecting
the remaining elements (short-circuit)."
  (let ((acc nil)
        (rest results)
        (ret nil))
    (while (and rest (not ret))
      (let ((r (car rest)))
        (cond ((nl-err-p r) (setq ret r))
              ((nl-ok-p r) (push (cdr r) acc))
              (t (signal 'nl-type-error (list 'nl-collect r)))))
      (setq rest (cdr rest)))
    (or ret (nl-ok (nreverse acc)))))

;;;; nl-? early return ------------------------------------------------

(defmacro nl-? (_form)
  "Early-return operator for Result values (Doc 169 section 2.3).
Valid only inside the body of `nl-defun' / `nl-lambda', where the
enclosing macro rewrites it.  Reaching this global definition means the
constraint was violated, so expansion fails loudly instead of leaving a
runtime `cl-return-from' error behind."
  (error "nl-?: only valid inside the body of `nl-defun' or `nl-lambda'"))

(defun nl--expand-? (form in-lambda)
  "Rewrite (nl-? X) forms inside FORM for the enclosing return block.
IN-LAMBDA non-nil means FORM sits inside a nested plain `lambda'; any
`nl-?' found there is rejected at expansion time.  Nested `nl-defun' /
`nl-lambda' forms are left untouched -- they open their own block when
they expand.  Quoted forms are not descended into."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((eq (car form) 'nl-?)
    (when in-lambda
      (error "nl-?: must not cross a nested `lambda' boundary"))
    (unless (and (consp (cdr form)) (null (cddr form)))
      (error "nl-?: expects exactly one form, got %S" form))
    (let ((r (gensym "nl--r"))
          (inner (nl--expand-? (cadr form) in-lambda)))
      `(let ((,r ,inner))
         (cond ((nl-err-p ,r) (cl-return-from nl--fn-block ,r))
               ((nl-ok-p ,r) (cdr ,r))
               (t (signal 'nl-type-error (list 'nl-? ,r)))))))
   ((memq (car form) '(nl-defun nl-lambda))
    form)
   ((eq (car form) 'lambda)
    (nl--expand-?-seq form t))
   ((and (eq (car form) 'function)
         (eq (car-safe (car-safe (cdr form))) 'lambda))
    (nl--expand-?-seq form t))
   (t (nl--expand-?-seq form in-lambda))))

(defun nl--expand-?-seq (form in-lambda)
  "Map `nl--expand-?' over the cons tree FORM, preserving dotted tails.
IN-LAMBDA is passed through to `nl--expand-?'."
  (if (consp form)
      (cons (nl--expand-? (car form) in-lambda)
            (nl--expand-?-seq (cdr form) in-lambda))
    form))

(defun nl--wrap-?-body (body)
  "Wrap BODY forms in the `nl-?' return block.
Return a list whose head keeps any leading docstring / `declare' /
`interactive' forms outside the block."
  (let ((prefix nil))
    (when (and (stringp (car body)) (cdr body))
      (push (pop body) prefix))
    (while (and (consp (car body))
                (memq (caar body) '(declare interactive)))
      (push (pop body) prefix))
    (append (nreverse prefix)
            `((cl-block nl--fn-block
                ,@(mapcar (lambda (f) (nl--expand-? f nil)) body))))))

(defmacro nl-defun (name arglist &rest body)
  "Define NAME as a function whose BODY may use `nl-?'.
ARGLIST is a normal `defun' argument list.  The body runs inside a
return block: (nl-? R) unwraps an ok Result R inline and early-returns
an err R from the whole function."
  (declare (indent defun) (doc-string 3))
  `(defun ,name ,arglist
     ,@(nl--wrap-?-body body)))

(defmacro nl-lambda (arglist &rest body)
  "Return an anonymous function whose BODY may use `nl-?'.
ARGLIST is a normal `lambda' argument list.  See `nl-defun'."
  (declare (indent defun))
  `(lambda ,arglist
     ,@(nl--wrap-?-body body)))

(defmacro nl-let* (bindings &rest body)
  "Sequential `let*' spelled for `nl-?' pipelines (Doc 169 section 2.3).
BINDINGS and BODY behave exactly like `let*'; this alias only marks
intent in code written against the nl-prelude API."
  (declare (indent 1))
  `(let* ,bindings ,@body))

;;;; nl-strict --------------------------------------------------------

(defvar nl--strict nil
  "Non-nil while expansion-time checks should be strict (Doc 168 sec 3.4).
Set through the `nl-strict' macro.  v0.2 limitation: the flag is
load/compile-unit scoped, not file scoped -- a file that sets it
should not rely on another file resetting it.")

(defmacro nl-strict (flag)
  "Set strict expansion-time checking to FLAG for the rest of the unit.
Under strict mode an `nl-match' over unregistered variants is an
expansion error instead of falling back to an unchecked dispatch."
  `(eval-and-compile (setq nl--strict ,flag)))

(defun nl-strict-p ()
  "Return non-nil when strict expansion-time checking is in effect.
Public read API for the flag set by the `nl-strict' macro; sibling
packages' macros should call this instead of touching `nl--strict'."
  nl--strict)

;;;; Warnings ---------------------------------------------------------

(defun nl--warn (format-string &rest args)
  "Emit an nl-prelude warning built from FORMAT-STRING and ARGS.
During byte compilation this routes through `byte-compile-warn' so the
CI compile gate (`byte-compile-error-on-warn' t) catches it; otherwise
it uses `display-warning' when available (absent on standalone
NeLisp), falling back to `message'."
  (let ((msg (apply #'format format-string args)))
    (cond
     ((and (fboundp 'byte-compile-warn)
           (fboundp 'macroexp-compiling-p)
           (macroexp-compiling-p))
      (byte-compile-warn "%s" msg))
     ((fboundp 'display-warning)
      (display-warning 'nl-prelude msg))
     (t (message "Warning (nl-prelude): %s" msg)))))

;;;; Algebraic data types (Doc 169 section 3, Phase 2a) ---------------

(define-error 'nl-match-error "No nl-match clause matched" 'nl-error)

(defvar nl--data-registry (make-hash-table :test 'eq)
  "Map of TYPE symbol -> list of (VARIANT . ARITY) in declaration order.")

(defvar nl--data-variants (make-hash-table :test 'eq)
  "Map of VARIANT symbol -> (TYPE . ARITY) for `nl-match' expansion.")

(defun nl--data-register (type variants)
  "Register TYPE with VARIANTS, a list of (VARIANT . ARITY).
Re-registering TYPE replaces its previous variant set.  A variant name
already claimed by a DIFFERENT type is an error: constructors and
predicates are global defuns, so a silent cross-type takeover would
clobber the other type's functions and registry entry."
  (dolist (v variants)
    (let ((prev (gethash (car v) nl--data-variants)))
      (when (and prev (not (eq (car prev) type)))
        (error "nl-defdata: variant %s already belongs to type %s"
               (car v) (car prev)))))
  (dolist (old (gethash type nl--data-registry))
    (let ((rev (gethash (car old) nl--data-variants)))
      (when (and rev (eq (car rev) type))
        (remhash (car old) nl--data-variants))))
  (puthash type variants nl--data-registry)
  (dolist (v variants)
    (puthash (car v) (cons type (cdr v)) nl--data-variants))
  type)

(defun nl--data-p (object)
  "Return non-nil when OBJECT is an `nl-defdata' value."
  (and (vectorp object)
       (>= (length object) 3)
       (eq (aref object 0) 'nl--data)))

(defun nl--data-type (object)
  "Return the type tag of the `nl-defdata' value OBJECT."
  (aref object 1))

(defun nl--data-variant (object)
  "Return the variant tag of the `nl-defdata' value OBJECT."
  (aref object 2))

(defun nl--defdata-variant-defs (type variant fields)
  "Return the defuns for VARIANT of TYPE with FIELDS.
Generates constructor, predicate, and one accessor per field.
Representation: [nl--data TYPE VARIANT FIELD...] (Doc 169 sec 3.3)."
  (let ((pred (intern (format "%s-p" variant)))
        (defs nil)
        (idx 3))
    (push `(defun ,variant ,fields
             ,(format "Construct an `%s' value of type `%s'." variant type)
             (vector 'nl--data ',type ',variant ,@fields))
          defs)
    (push `(defun ,pred (object)
             ,(format "Return non-nil when OBJECT is an `%s' value." variant)
             (and (nl--data-p object)
                  (eq (nl--data-type object) ',type)
                  (eq (nl--data-variant object) ',variant)))
          defs)
    (dolist (field fields)
      (let ((accessor (intern (format "%s-%s" variant field)))
            (i idx))
        (push `(defun ,accessor (object)
                 ,(format "Return the `%s' field of an `%s' value." field variant)
                 (unless (,pred object)
                   (signal 'nl-type-error (list ',accessor object)))
                 (aref object ,i))
              defs))
      (setq idx (1+ idx)))
    (nreverse defs)))

(defmacro nl-defdata (type &rest variants)
  "Declare TYPE as a closed set of VARIANTS (Doc 169 section 3).
Each variant is (NAME FIELD...).  Generates, per variant, a
constructor NAME, a predicate NAME-p, and NAME-FIELD accessors, plus a
TYPE-p predicate; registers the variant set so `nl-match' can check
exhaustiveness at expansion time."
  (declare (indent defun))
  (let ((specs
         (mapcar (lambda (v)
                   (unless (and (consp v) (symbolp (car v)) (car v)
                                (listp (cdr v)))
                     (error "nl-defdata: bad variant spec %S" v))
                   v)
                 variants)))
    (unless specs
      (error "nl-defdata: %s needs at least one variant" type))
    (when (memq type (mapcar #'car specs))
      (error
       "nl-defdata: variant must not share the type name %s (its %s-p would clobber the type predicate)"
       type type))
    `(progn
       (eval-and-compile
         (nl--data-register
          ',type
          ',(mapcar (lambda (s) (cons (car s) (length (cdr s)))) specs)))
       (defun ,(intern (format "%s-p" type)) (object)
         ,(format "Return non-nil when OBJECT is a `%s' value." type)
         (and (nl--data-p object) (eq (nl--data-type object) ',type)))
       ,@(apply #'append
                (mapcar (lambda (s)
                          (nl--defdata-variant-defs type (car s) (cdr s)))
                        specs))
       ',type)))

;;;; nl-match ---------------------------------------------------------

(defun nl--match-parse-clause (clause)
  "Parse CLAUSE into (HEAD VARS BODY); HEAD is `_' for the wildcard."
  (unless (consp clause)
    (error "nl-match: bad clause %S" clause))
  (let ((pattern (car clause))
        (body (cdr clause)))
    (cond
     ((eq pattern '_) (list '_ nil body))
     ((and (consp pattern) (symbolp (car pattern)) (car pattern))
      (dolist (v (cdr pattern))
        (unless (symbolp v)
          (error "nl-match: pattern variable %S is not a symbol" v)))
      (list (car pattern) (cdr pattern) body))
     (t (error "nl-match: bad pattern %S" pattern)))))

(defun nl--match-check (parsed)
  "Check PARSED clauses against the variant registry.
Return non-nil when the checked (registry-backed) expansion applies;
nil requests the unchecked fallback (unknown variants, non-strict).
Signals expansion errors for missing variants, arity mismatches, and
mixed types; warns about unreachable clauses."
  (let* ((heads (delq '_ (mapcar #'car parsed)))
         (wildcard (assq '_ parsed))
         (unknown (delq nil (mapcar (lambda (h)
                                      (and (not (gethash h nl--data-variants)) h))
                                    heads))))
    (cond
     (unknown
      (when nl--strict
        (error "nl-match: unregistered variant(s) %S under (nl-strict t)"
               unknown))
      nil)
     ((null heads)
      ;; Only a wildcard clause: nothing to check.
      (null wildcard))
     (t
      (let* ((type (car (gethash (car heads) nl--data-variants)))
             (registered (gethash type nl--data-registry))
             (seen nil))
        (dolist (c parsed)
          (unless (eq (car c) '_)
            (let* ((head (car c))
                   (info (gethash head nl--data-variants)))
              (unless (eq (car info) type)
                (error "nl-match: variant %s belongs to type %s, not %s"
                       head (car info) type))
              (unless (= (length (nth 1 c)) (cdr info))
                (error "nl-match: %s takes %d field(s), pattern %S has %d"
                       head (cdr info) (cons head (nth 1 c))
                       (length (nth 1 c))))
              (when (memq head seen)
                (nl--warn "nl-match: duplicate clause for %s is unreachable"
                          head))
              (push head seen))))
        (when (and wildcard
                   (not (eq (car (car (last parsed))) '_)))
          (nl--warn "nl-match: clauses after _ are unreachable"))
        (let ((missing (delq nil (mapcar (lambda (v)
                                           (and (not (memq (car v) heads))
                                                (car v)))
                                         registered))))
          (when (and missing (not wildcard))
            (error "nl-match: non-exhaustive match on %s; missing %S"
                   type missing)))
        t)))))

(defun nl--match-emit-clause (clause val)
  "Emit a `cond' clause for the parsed CLAUSE matching against VAL."
  (let ((head (car clause))
        (vars (nth 1 clause))
        (body (nth 2 clause)))
    (if (eq head '_)
        `(t ,@(or body '(nil)))
      (let ((bindings nil)
            (idx 3))
        (dolist (v vars)
          (unless (string-prefix-p "_" (symbol-name v))
            (push `(,v (aref ,val ,idx)) bindings))
          (setq idx (1+ idx)))
        `((and (nl--data-p ,val)
               (eq (nl--data-variant ,val) ',head))
          (let ,(nreverse bindings)
            ,@(or body '(nil))))))))

(defmacro nl-match (expr &rest clauses)
  "Match the `nl-defdata' value of EXPR against CLAUSES.
Each clause is ((VARIANT VAR...) BODY...) or (_ BODY...).  When every
variant head is registered, exhaustiveness and field arity are checked
at expansion time: missing variants (without a _ clause) stop the
compile.  Unregistered heads fall back to an unchecked dispatch, or
signal under (nl-strict t).  At runtime a value no clause matches
signals `nl-match-error'."
  (declare (indent 1))
  (unless clauses
    (error "nl-match: needs at least one clause"))
  (let* ((val (gensym "nl--val"))
         (parsed (mapcar #'nl--match-parse-clause clauses)))
    (nl--match-check parsed)
    `(let ((,val ,expr))
       (cond
        ,@(mapcar (lambda (c) (nl--match-emit-clause c val)) parsed)
        (t (signal 'nl-match-error (list ,val)))))))

;;;; Auto-gensym macros (Doc 169 section 4, Phase 2a) -----------------

(defun nl--auto-gensym-p (object)
  "Return non-nil when OBJECT is a symbol spelled with a trailing `#'."
  (and (symbolp object) object
       (let ((name (symbol-name object)))
         (and (> (length name) 1)
              (string-suffix-p "#" name)))))

(defun nl--rename-auto-gensyms-1 (form table)
  "Rewrite trailing-# symbols in FORM using TABLE for consistency.
Quoted forms are left untouched (matching the other tree walkers):
a trailing-# symbol used as literal DATA keeps its spelling."
  (cond
   ((and (consp form) (eq (car form) 'quote)) form)
   ((nl--auto-gensym-p form)
    (or (gethash form table)
        (puthash form
                 (gensym (concat "nl--"
                                 (substring (symbol-name form) 0 -1)
                                 "-"))
                 table)))
   ((consp form)
    (cons (nl--rename-auto-gensyms-1 (car form) table)
          (nl--rename-auto-gensyms-1 (cdr form) table)))
   ((vectorp form)
    (vconcat (mapcar (lambda (x) (nl--rename-auto-gensyms-1 x table))
                     form)))
   (t form)))

(defun nl--rename-auto-gensyms (form)
  "Replace every trailing-# symbol in FORM with a fresh gensym.
Occurrences of the same spelling share one gensym within a single call
(= a single macro expansion), Clojure syntax-quote style."
  (nl--rename-auto-gensyms-1 form (make-hash-table :test 'eq)))

(defmacro nl-defmacro (name arglist &rest body)
  "Define macro NAME with ARGLIST and BODY with auto-gensym support.
Symbols spelled `foo#' anywhere in the produced expansion are replaced
by gensyms, unique per expansion and consistent within one expansion.
This shields let-bound helper variables from capturing user bindings;
it is not a hygienic macro system (free-variable and function-position
capture are not prevented)."
  (declare (indent defun) (doc-string 3))
  (let ((prefix nil))
    (when (and (stringp (car body)) (cdr body))
      (push (pop body) prefix))
    (while (and (consp (car body)) (eq (caar body) 'declare))
      (push (pop body) prefix))
    `(defmacro ,name ,arglist
       ,@(nreverse prefix)
       (nl--rename-auto-gensyms (progn ,@body)))))

;;;; Tail-recursive loop (Doc 169 section 5, Phase 2a, L1) ------------

(defmacro nl-recur (&rest _args)
  "Rebind the enclosing `nl-loop' variables and restart the loop.
Valid only in tail position inside `nl-loop'; reaching this global
definition means the constraint was violated, so expansion fails."
  (error "nl-recur: only valid in tail position inside `nl-loop'"))

(defun nl--loop-scan (form)
  "Signal when FORM contains `nl-recur' outside a nested `nl-loop'.
Used for non-tail subtrees, which may not contain `nl-recur' at all."
  (cond
   ((not (consp form)) nil)
   ((eq (car form) 'quote) nil)
   ((eq (car form) 'nl-recur)
    (error "nl-recur: not in tail position: %S" form))
   ((eq (car form) 'nl-loop) nil)
   (t (nl--loop-scan (car form))
      (nl--loop-scan (cdr form)))))

(defun nl--loop-walk-body (forms regs continue)
  "Walk an implicit-progn FORMS list: all non-tail but the last.
REGS is the (VAR . REGISTER) alist; CONTINUE the loop-again flag."
  (let ((head (butlast forms))
        (tail (last forms)))
    (mapc #'nl--loop-scan head)
    (append head
            (list (nl--loop-walk (car tail) regs continue)))))

(defun nl--loop-walk (form regs continue)
  "Rewrite `nl-recur' calls in tail position of FORM.
REGS is an alist of (VAR . REGISTER): the user-visible loop variables
and the hidden gensym registers `nl-recur' assigns to.  Assigning
registers (not VARs) makes the rewrite immune to an inner `let'
shadowing a loop variable, and gives simultaneous-rebinding semantics
for free (the argument expressions still see the per-iteration VAR
bindings).  CONTINUE is the loop-again flag symbol.  Signals on
`nl-recur' in non-tail position or with the wrong argument count."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'quote) form)
   ((eq (car form) 'nl-loop) form)
   ((eq (car form) 'nl-recur)
    (let ((args (cdr form)))
      (unless (= (length args) (length regs))
        (error "nl-recur: expects %d value(s) for %S, got %d"
               (length regs) (mapcar #'car regs) (length args)))
      (mapc #'nl--loop-scan args)
      `(progn
         (setq ,@(let ((sets nil) (rs regs) (as args))
                   (while rs
                     (push (cdr (car rs)) sets) (push (car as) sets)
                     (setq rs (cdr rs) as (cdr as)))
                   (nreverse sets))
               ,continue t)
         nil)))
   ((memq (car form) '(progn when unless))
    (if (eq (car form) 'progn)
        `(progn ,@(nl--loop-walk-body (cdr form) regs continue))
      (nl--loop-scan (nth 1 form))
      `(,(car form) ,(nth 1 form)
        ,@(nl--loop-walk-body (cddr form) regs continue))))
   ((eq (car form) 'if)
    (nl--loop-scan (nth 1 form))
    `(if ,(nth 1 form)
         ,(nl--loop-walk (nth 2 form) regs continue)
       ,@(if (cdddr form)
             (nl--loop-walk-body (cdddr form) regs continue)
           nil)))
   ((eq (car form) 'cond)
    `(cond
      ,@(mapcar (lambda (clause)
                  (nl--loop-scan (car clause))
                  (if (cdr clause)
                      (cons (car clause)
                            (nl--loop-walk-body (cdr clause)
                                                regs continue))
                    clause))
                (cdr form))))
   ((memq (car form) '(let let*))
    (dolist (b (nth 1 form)) (nl--loop-scan b))
    `(,(car form) ,(nth 1 form)
      ,@(nl--loop-walk-body (cddr form) regs continue)))
   ((memq (car form) '(and or))
    ;; Zero-argument (and) / (or) has no tail sub-form to rewrite;
    ;; (car (last form)) would be the head symbol itself and the
    ;; rewrite would emit the bogus (and and) / (or or).
    (if (null (cdr form))
        form
      (mapc #'nl--loop-scan (butlast (cdr form)))
      `(,(car form) ,@(butlast (cdr form))
        ,(nl--loop-walk (car (last form)) regs continue))))
   (t (nl--loop-scan form) form)))

(defmacro nl-loop (bindings &rest body)
  "Loop with BINDINGS rebound by `nl-recur' in tail position of BODY.
BINDINGS is a list of (VAR INIT) evaluated like `let'.  BODY is an
implicit progn; (nl-recur NEW...) in tail position rebinds all VARs
simultaneously and restarts, in constant stack space; any other value
ends the loop and is returned (Doc 169 section 5).

The loop state lives in hidden registers; each iteration binds the
VARs fresh from them.  `nl-recur' therefore works correctly even when
an inner `let' shadows a loop variable, and Clojure loop/recur
semantics apply: mutating a VAR directly never affects the next
iteration -- only `nl-recur' does."
  (declare (indent 1))
  (dolist (b bindings)
    (unless (and (consp b) (symbolp (car b)) (car b)
                 (consp (cdr b)) (null (cddr b)))
      (error "nl-loop: binding must be (VAR INIT), got %S" b)))
  (unless body
    (error "nl-loop: empty body"))
  (let* ((vars (mapcar #'car bindings))
         (regs (mapcar (lambda (v) (cons v (gensym (format "nl--reg-%s-" v))))
                       vars))
         (continue (gensym "nl--continue"))
         (result (gensym "nl--result"))
         (walked (nl--loop-walk-body body regs continue)))
    `(let (,@(mapcar (lambda (b) (list (cdr (assq (car b) regs)) (nth 1 b)))
                     bindings)
           (,continue t) (,result nil))
       (while ,continue
         (setq ,continue nil)
         (setq ,result
               (let ,(mapcar (lambda (r) (list (car r) (cdr r))) regs)
                 ,@walked)))
       ,result)))

(provide 'nl-prelude)

;;; nl-prelude.el ends here
