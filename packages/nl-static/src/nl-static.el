;;; nl-static.el --- Opt-in syntactic guarantees for NeLisp -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Doc 197's L1, macroexpansion-time static layer.  Nothing here advises,
;; hooks, or rewrites unannotated Elisp.  A caller must require this
;; library and use one of its annotation macros explicitly:
;;
;;   (nl-total defun NAME ARGLIST BODY...)
;;   (nl-borrow-scope BODY...)
;;   (nl-typed defun NAME ((ARG TYPE) ...) BODY...)
;;
;; The checks are deliberately Tier 1 and syntactic.  `nl-total' names
;; `nl-match''s already-shipped registry/exhaustiveness check.
;; `nl-borrow-scope' sees only literally nested borrow macros on the same
;; lexical cell symbol.  `nl-typed' checks literals and declared arguments
;; in annotated function bodies; it performs no inference.  Opaque calls,
;; macro-produced syntax, aliases, and cross-function flow remain the job
;; of nl-safe's runtime checks.

;;; Code:

(require 'nl-prelude)
(require 'nl-safe)

;;;; Errors and disable flag ------------------------------------------

(define-error 'nl-static-error "nl-static expansion-time violation" 'nl-error)
(define-error 'nl-total-error "nl-total expansion-time violation"
              'nl-static-error)
(define-error 'nl-borrow-scope-error
              "nl-borrow-scope expansion-time violation" 'nl-static-error)
(define-error 'nl-typed-error "nl-typed expansion-time violation"
              'nl-static-error)

(defvar nl-static--enabled t
  "Non-nil enables checks in nl-static annotation macros.
The flag is read at macroexpansion time.  When nil, each annotation
expands to its documented plain Elisp counterpart with no checker
residue.  Requiring `nl-static' alone never changes another form's
expansion or runtime behaviour.")

(defun nl-static--fail (condition rule form detail)
  "Signal CONDITION for RULE at FORM with explanatory DETAIL."
  (signal condition (list :rule rule :form form :detail detail)))

;;;; nl-total ---------------------------------------------------------

(defun nl-static--total-check-match (form)
  "Run the shipped `nl-match' checker strictly on FORM.
Any error is re-signalled as the named `nl-total-error' condition."
  (condition-case err
      (let ((nl--strict t))
        (macroexpand form))
    (error
     (nl-static--fail
      'nl-total-error 'registered-exhaustive-nl-match form err))))

(defun nl-static--total-scan-seq (forms)
  "Check every live form in the possibly dotted list FORMS."
  (dolist (form (nl--walk-proper-list forms))
    (nl-static--total-scan form)))

(defun nl-static--total-scan (form)
  "Check every syntactically visible `nl-match' in FORM."
  (cond
   ((not (consp form)) nil)
   ((nl--walk-quoted-p form) nil)
   ((nl--walk-backquote-p form)
    (nl-static--total-scan-seq (nl--walk-live-parts form)))
   ((eq (car form) 'nl-match)
    (nl-static--total-check-match form)
    ;; Clause patterns are data.  Only EXPR and each clause body are live.
    (nl-static--total-scan (car (cdr form)))
    (let ((clauses (cdr (cdr form))))
      (while (consp clauses)
        (when (consp (car clauses))
          (nl-static--total-scan-seq (cdr (car clauses))))
        (setq clauses (cdr clauses)))))
   (t
    (nl-static--total-scan-seq form))))

(defmacro nl-total (definition name arglist &rest body)
  "Define a total function, checking visible `nl-match' forms now.
DEFINITION must be the symbol `defun'.  NAME, ARGLIST, and BODY are
otherwise the corresponding `defun' parts.  Each literal `nl-match' in
BODY must use registered variants and pass nl-match's shipped
exhaustiveness and arity checker.  The emitted form is the plain
`defun'; only expansion gains an earlier, named failure point."
  (declare (indent defun) (doc-string 4))
  (unless (eq definition 'defun)
    (error "nl-total: expected (nl-total defun NAME ARGLIST ...), got %S"
           definition))
  (unless (and name (symbolp name))
    (error "nl-total: NAME must be a non-nil symbol, got %S" name))
  (unless (listp arglist)
    (error "nl-total: ARGLIST must be a list, got %S" arglist))
  (when nl-static--enabled
    (nl-static--total-scan-seq body))
  `(defun ,name ,arglist ,@body))

;;;; nl-borrow-scope --------------------------------------------------

(defun nl-static--env-without (environment names)
  "Return ENVIRONMENT without entries whose keys occur in NAMES."
  (let ((out nil))
    (while environment
      (unless (memq (car (car environment)) names)
        (setq out (cons (car environment) out)))
      (setq environment (cdr environment)))
    (nreverse out)))

(defun nl-static--binding-names (bindings)
  "Return symbol names introduced by the possibly dotted BINDINGS."
  (let ((names nil))
    (while (consp bindings)
      (let ((binding (car bindings)))
        (cond ((symbolp binding) (setq names (cons binding names)))
              ((and (consp binding) (symbolp (car binding)))
               (setq names (cons (car binding) names)))))
      (setq bindings (cdr bindings)))
    names))

(defun nl-static--borrow-scan-seq (forms active)
  "Scan each live element of FORMS with ACTIVE lexical borrows."
  (dolist (form (nl--walk-proper-list forms))
    (nl-static--borrow-scan form active)))

(defun nl-static--borrow-scan-let (form active sequential)
  "Scan let FORM under ACTIVE; SEQUENTIAL means `let*' semantics."
  (let ((bindings (car (cdr form)))
        (body (cdr (cdr form)))
        (inside active))
    (if sequential
        (while (consp bindings)
          (let ((binding (car bindings)))
            (when (consp binding)
              (nl-static--borrow-scan-seq (cdr binding) inside))
            (setq inside
                  (nl-static--env-without
                   inside (nl-static--binding-names (list binding)))))
          (setq bindings (cdr bindings)))
      (let ((rest bindings))
        (while (consp rest)
          (when (consp (car rest))
            (nl-static--borrow-scan-seq (cdr (car rest)) active))
          (setq rest (cdr rest))))
      (setq inside
            (nl-static--env-without
             active (nl-static--binding-names bindings))))
    (nl-static--borrow-scan-seq body inside)))

(defun nl-static--borrow-scan-borrow (form active kind)
  "Scan borrow FORM under ACTIVE, requesting KIND (`shared' or `mutable')."
  (let ((spec (car (cdr form)))
        (body (cdr (cdr form))))
    ;; A malformed spec remains nl-safe's own expansion error.  This
    ;; checker only adds the conflicting-nesting rule it owns.
    (if (not (and (consp spec) (symbolp (car spec))
                  (consp (cdr spec)) (null (cdr (cdr spec)))))
        (nl-static--borrow-scan-seq (cdr form) active)
      (let* ((var (car spec))
             (cell (car (cdr spec)))
             (existing (and (symbolp cell) cell (assq cell active))))
        (nl-static--borrow-scan cell active)
        (when (and existing
                   (or (eq kind 'mutable) (eq (cdr existing) 'mutable)))
          (nl-static--fail
           'nl-borrow-scope-error 'conflicting-lexical-borrow form
           (list :cell cell :existing (cdr existing) :requested kind)))
        (let ((inside (nl-static--env-without active (list var))))
          ;; SPEC's VAR shadows a same-named CELL inside BODY, so the
          ;; borrowed cell no longer has that lexical spelling there.
          (when (and (symbolp cell) cell (not (eq var cell)))
            (setq inside (cons (cons cell kind) inside)))
          (nl-static--borrow-scan-seq body inside))))))

(defun nl-static--borrow-scan (form active)
  "Reject visible conflicting borrow nesting in FORM under ACTIVE.
ACTIVE is an alist of (CELL-SYMBOL . shared|mutable)."
  (cond
   ((not (consp form)) nil)
   ((nl--walk-quoted-p form) nil)
   ((nl--walk-backquote-p form)
    (nl-static--borrow-scan-seq (nl--walk-live-parts form) active))
   ((eq (car form) 'nl-with-borrow)
    (nl-static--borrow-scan-borrow form active 'shared))
   ((eq (car form) 'nl-with-borrow-mut)
    (nl-static--borrow-scan-borrow form active 'mutable))
   ((eq (car form) 'let)
    (nl-static--borrow-scan-let form active nil))
   ((eq (car form) 'let*)
    (nl-static--borrow-scan-let form active t))
   ;; A closure or nested definition is a flow boundary: its body does
   ;; not run merely because this surrounding syntax runs.
   ((memq (car form) '(lambda closure defun nl-defun defmacro nl-defmacro))
    nil)
   ((and (eq (car form) 'function)
         (consp (car (cdr form))))
    nil)
   (t
    ;; For an ordinary call, the function-position form and arguments
    ;; are evaluated now even though the opaque callee body is unseen.
    (when (consp (car form))
      (nl-static--borrow-scan (car form) active))
    (nl-static--borrow-scan-seq (cdr form) active))))

(defmacro nl-borrow-scope (&rest body)
  "Run BODY after checking syntactically visible borrow nesting.
Two borrows on the same lexical cell symbol conflict when either is
mutable and one is literally nested in the other's body.  Shared/shared
nesting is allowed.  Aliases, closure bodies, macro-produced borrows,
and opaque callees are deliberately not claimed; nl-safe still checks
those cases at runtime."
  (declare (indent 0))
  (when nl-static--enabled
    (nl-static--borrow-scan-seq body nil))
  `(progn ,@body))

;;;; nl-typed ---------------------------------------------------------

(defvar nl-static--typed-signatures (make-hash-table :test 'eq)
  "Map annotated function names to ((ARG . TYPE) ...) signatures.")

(defun nl-static--typed-parse-declarations (name declarations)
  "Validate DECLARATIONS for NAME and return ((ARG . TYPE) ...)."
  (unless (listp declarations)
    (error "nl-typed: declarations for %S must be a list, got %S"
           name declarations))
  (let ((signature nil)
        (seen nil))
    (dolist (declaration declarations)
      (unless (and (consp declaration) (symbolp (car declaration))
                   (car declaration) (consp (cdr declaration))
                   (null (cdr (cdr declaration))))
        (error "nl-typed: declaration must be (ARG TYPE), got %S"
               declaration))
      (when (memq (car declaration) seen)
        (error "nl-typed: duplicate argument %S in %S"
               (car declaration) name))
      (setq seen (cons (car declaration) seen))
      (setq signature
            (cons (cons (car declaration) (car (cdr declaration)))
                  signature)))
    (nreverse signature)))

(defun nl-static--typed-register (name signature)
  "Register NAME with the already validated SIGNATURE."
  (puthash name signature nl-static--typed-signatures)
  name)

(defun nl-static--literal (form)
  "Return (t . VALUE) when FORM is a literal, else nil."
  (cond
   ((or (numberp form) (stringp form) (vectorp form)
        (keywordp form) (null form) (eq form t))
    (cons t form))
   ((and (consp form) (eq (car form) 'quote)
         (consp (cdr form)) (null (cdr (cdr form))))
    (cons t (car (cdr form))))
   (t nil)))

(defun nl-static--literal-type-result (value type)
  "Return `yes', `no', or `unknown' for VALUE against declared TYPE."
  (cond
   ((eq type t) 'yes)
   ((null type) 'no)
   ((eq type 'integer) (if (integerp value) 'yes 'no))
   ((eq type 'float) (if (floatp value) 'yes 'no))
   ((eq type 'number) (if (numberp value) 'yes 'no))
   ((eq type 'string) (if (stringp value) 'yes 'no))
   ((eq type 'symbol) (if (symbolp value) 'yes 'no))
   ((eq type 'keyword) (if (keywordp value) 'yes 'no))
   ((eq type 'null) (if (null value) 'yes 'no))
   ((eq type 'boolean) (if (memq value '(nil t)) 'yes 'no))
   ((eq type 'cons) (if (consp value) 'yes 'no))
   ((eq type 'list) (if (listp value) 'yes 'no))
   ((eq type 'vector) (if (vectorp value) 'yes 'no))
   ((eq type 'sequence)
    (if (or (listp value) (stringp value) (vectorp value)) 'yes 'no))
   ((eq type 'array)
    (if (or (stringp value) (vectorp value)) 'yes 'no))
   ((and (consp type) (eq (car type) 'member))
    (if (member value (cdr type)) 'yes 'no))
   ((and (consp type) (eq (car type) 'eql)
         (consp (cdr type)) (null (cdr (cdr type))))
    (if (equal value (car (cdr type))) 'yes 'no))
   ((and (consp type) (eq (car type) 'or))
    (let ((rest (cdr type)) (answer 'no))
      (while (and rest (not (eq answer 'yes)))
        (let ((one (nl-static--literal-type-result value (car rest))))
          (cond ((eq one 'yes) (setq answer 'yes))
                ((eq one 'unknown) (setq answer 'unknown))))
        (setq rest (cdr rest)))
      answer))
   ((and (consp type) (eq (car type) 'and))
    (let ((rest (cdr type)) (answer 'yes))
      (while (and rest (not (eq answer 'no)))
        (let ((one (nl-static--literal-type-result value (car rest))))
          (cond ((eq one 'no) (setq answer 'no))
                ((eq one 'unknown) (setq answer 'unknown))))
        (setq rest (cdr rest)))
      answer))
   ((and (consp type) (eq (car type) 'not)
         (consp (cdr type)) (null (cdr (cdr type))))
    (let ((one (nl-static--literal-type-result value (car (cdr type)))))
      (cond ((eq one 'yes) 'no) ((eq one 'no) 'yes) (t 'unknown))))
   (t 'unknown)))

(defun nl-static--type-categories (type)
  "Return abstract value categories accepted by TYPE, or nil if unknown."
  (cond
   ((eq type t) '(any))
   ((eq type 'integer) '(integer))
   ((eq type 'float) '(float))
   ((eq type 'number) '(integer float))
   ((eq type 'string) '(string))
   ((eq type 'symbol) '(nil symbol))
   ((eq type 'keyword) '(symbol))
   ((eq type 'null) '(nil))
   ((eq type 'boolean) '(nil symbol))
   ((eq type 'cons) '(cons))
   ((eq type 'list) '(nil cons))
   ((eq type 'vector) '(vector))
   ((eq type 'sequence) '(nil cons string vector))
   ((eq type 'array) '(string vector))
   ((and (consp type) (eq (car type) 'or))
    (let ((rest (cdr type)) (all nil) (known t))
      (while (and rest known)
        (let ((one (nl-static--type-categories (car rest))))
          (if (null one)
              (setq known nil)
            (dolist (category one)
              (unless (memq category all)
                (setq all (cons category all))))))
        (setq rest (cdr rest)))
      (and known all)))
   (t nil)))

(defun nl-static--types-disjoint-p (left right)
  "Return non-nil only when declared LEFT and RIGHT are provably disjoint."
  (let ((a (nl-static--type-categories left))
        (b (nl-static--type-categories right))
        (overlap nil))
    (when (and a b (not (memq 'any a)) (not (memq 'any b)))
      (while (and a (not overlap))
        (setq overlap (memq (car a) b))
        (setq a (cdr a)))
      (not overlap))))

(defun nl-static--assigned-vars (form vars)
  "Collect direct `setq' target symbols in live FORM onto VARS."
  (cond
   ((not (consp form)) vars)
   ((nl--walk-quoted-p form) vars)
   ((nl--walk-backquote-p form)
    (let ((parts (nl--walk-live-parts form)))
      (while parts
        (setq vars (nl-static--assigned-vars (car parts) vars))
        (setq parts (cdr parts)))
      vars))
   ((eq (car form) 'setq)
    (let ((tail (cdr form)))
      (while (consp tail)
        (when (symbolp (car tail))
          (setq vars (cons (car tail) vars)))
        (when (consp (cdr tail))
          (setq vars (nl-static--assigned-vars (car (cdr tail)) vars)))
        (setq tail (cdr (cdr tail))))
      vars))
   (t
    (let ((tail form))
      (while (consp tail)
        (setq vars (nl-static--assigned-vars (car tail) vars))
        (setq tail (cdr tail)))
      vars))))

(defun nl-static--typed-check-call (form environment)
  "Check typed call FORM using declared binding ENVIRONMENT."
  (when (symbolp (car form))
    (let ((signature (gethash (car form) nl-static--typed-signatures))
          (arguments (cdr form)))
      (while (and signature (consp arguments))
        (let* ((expected (cdr (car signature)))
               (argument (car arguments))
               (literal (nl-static--literal argument))
               (declared (and (symbolp argument)
                              (assq argument environment))))
          (cond
           ((and literal
                 (eq (nl-static--literal-type-result
                      (cdr literal) expected)
                     'no))
            (nl-static--fail
             'nl-typed-error 'literal-shape-mismatch form
             (list :callee (car form) :argument argument
                   :expected expected)))
           ((and declared
                 (nl-static--types-disjoint-p (cdr declared) expected))
            (nl-static--fail
             'nl-typed-error 'declared-binding-shape-mismatch form
             (list :callee (car form) :argument argument
                   :declared (cdr declared) :expected expected)))))
        (setq signature (cdr signature))
        (setq arguments (cdr arguments))))))

(defun nl-static--typed-scan-seq (forms environment)
  "Check typed calls in FORMS using declared binding ENVIRONMENT."
  (dolist (form (nl--walk-proper-list forms))
    (nl-static--typed-scan form environment)))

(defun nl-static--typed-scan-let (form environment sequential)
  "Check typed calls in let FORM; SEQUENTIAL means `let*'."
  (let ((bindings (car (cdr form)))
        (body (cdr (cdr form)))
        (inside environment))
    (if sequential
        (while (consp bindings)
          (let ((binding (car bindings)))
            (when (consp binding)
              (nl-static--typed-scan-seq (cdr binding) inside))
            (setq inside
                  (nl-static--env-without
                   inside (nl-static--binding-names (list binding)))))
          (setq bindings (cdr bindings)))
      (let ((rest bindings))
        (while (consp rest)
          (when (consp (car rest))
            (nl-static--typed-scan-seq (cdr (car rest)) environment))
          (setq rest (cdr rest))))
      (setq inside
            (nl-static--env-without
             environment (nl-static--binding-names bindings))))
    (nl-static--typed-scan-seq body inside)))

(defun nl-static--typed-lambda-args (arglist)
  "Return ordinary symbol bindings introduced by ARGLIST."
  (let ((names nil))
    (while (consp arglist)
      (let ((arg (car arglist)))
        (cond ((and (symbolp arg) (not (memq arg '(&optional &rest))))
               (setq names (cons arg names)))
              ((and (consp arg) (symbolp (car arg)))
               (setq names (cons (car arg) names)))))
      (setq arglist (cdr arglist)))
    names))

(defun nl-static--typed-scan (form environment)
  "Check syntactically visible typed calls in FORM under ENVIRONMENT."
  (cond
   ((not (consp form)) nil)
   ((nl--walk-quoted-p form) nil)
   ((nl--walk-backquote-p form)
    (nl-static--typed-scan-seq (nl--walk-live-parts form) environment))
   ((eq (car form) 'let)
    (nl-static--typed-scan-let form environment nil))
   ((eq (car form) 'let*)
    (nl-static--typed-scan-let form environment t))
   ((eq (car form) 'lambda)
    (nl-static--typed-scan-seq
     (cdr (cdr form))
     (nl-static--env-without
      environment (nl-static--typed-lambda-args (car (cdr form))))))
   ((and (eq (car form) 'function)
         (consp (car (cdr form)))
         (eq (car (car (cdr form))) 'lambda))
    (nl-static--typed-scan (car (cdr form)) environment))
   ;; A nested annotation owns its own declarations and check.
   ((memq (car form) '(nl-typed nl-total)) nil)
   (t
    (nl-static--typed-check-call form environment)
    (when (consp (car form))
      (nl-static--typed-scan (car form) environment))
    (nl-static--typed-scan-seq (cdr form) environment))))

(defun nl-static--definition-prefix (body)
  "Return (PREFIX . REST) for definition BODY.
PREFIX contains a leading docstring (when not the sole value),
`declare', and `interactive' forms, which must remain before inserted
runtime checks."
  (let ((prefix nil))
    (when (and (stringp (car body)) (cdr body))
      (setq prefix (cons (car body) prefix))
      (setq body (cdr body)))
    (while (and (consp (car body))
                (memq (car (car body)) '(declare interactive)))
      (setq prefix (cons (car body) prefix))
      (setq body (cdr body)))
    (cons (nreverse prefix) body)))

(defmacro nl-typed (definition name declarations &rest body)
  "Define NAME with declared argument shapes and local static checks.
DEFINITION is `defun' or `nl-defun'.  DECLARATIONS is ((ARG TYPE) ...).
The emitted function has the ordinary ARG list and begins with a
runtime `cl-check-type' assertion for every argument.  Under
`nl-strict', literal arguments and still-visible declared arguments in
BODY are compared with signatures of prior `nl-typed' definitions at
macroexpansion time.  This is declared-shape checking, not inference."
  (declare (indent defun) (doc-string 4))
  (unless (memq definition '(defun nl-defun))
    (error "nl-typed: DEFINITION must be defun or nl-defun, got %S"
           definition))
  (unless (and name (symbolp name))
    (error "nl-typed: NAME must be a non-nil symbol, got %S" name))
  (let* ((signature (nl-static--typed-parse-declarations name declarations))
         (arglist (mapcar #'car signature)))
    (if (not nl-static--enabled)
        `(,definition ,name ,arglist ,@body)
      (nl-static--typed-register name signature)
      (when (nl-strict-p)
        (let ((assigned nil))
          (dolist (form body)
            (setq assigned (nl-static--assigned-vars form assigned)))
          (nl-static--typed-scan-seq
           body (nl-static--env-without signature assigned))))
      (let* ((parts (nl-static--definition-prefix body))
             (prefix (car parts))
             (rest (cdr parts)))
        `(progn
           (eval-and-compile
             (nl-static--typed-register ',name ',signature))
           (,definition ,name ,arglist
             ,@prefix
             ,@(mapcar (lambda (entry)
                         `(cl-check-type ,(car entry) ,(cdr entry)))
                       signature)
             ,@rest))))))

(provide 'nl-static)

;;; nl-static.el ends here
