;;; nelisp-special-forms.el --- Phase 7+B special-forms (Doc 40 §3.B)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; Author: zawatton <kurozawawo@gmail.com>

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Phase 7+B basic special-forms (Doc 40 §3.B LOCKED v2).  Provides a
;; self-contained tree-walking evaluator covering the core 11 forms
;; of Emacs Lisp plus the additional 11 forms enumerated in Doc 40
;; §3.B (save-* family, setq-default / setq-local, eval-when-compile
;; / eval-and-compile, let-when-compile, inline-let, declare).
;;
;; This file is parallel to (and independent from) `nelisp-eval.el',
;; the Phase 1 Week 5-24 evaluator core.  Both evaluators MUST stay
;; semantically compatible; the split exists because Doc 40 §4.2
;; freezes a fresh public API surface (`nelisp-special-forms-eval' /
;; `nelisp-special-forms-funcall' / `nelisp-special-forms-apply')
;; and the legacy file's `nelisp-` prefixed surface is byte-perfect
;; pinned by the Phase 3 SHIPPED ERT corpus.
;;
;; Public API:
;;
;;   (nelisp-special-forms-eval FORM &optional ENV)
;;       Evaluate FORM in lexical environment ENV (alist of
;;       (symbol . value)).  ENV defaults to nil = empty.
;;
;;   (nelisp-special-forms-funcall FN &rest ARGS)
;;       Apply callable FN to ARGS.  FN may be a closure, a host
;;       Emacs function symbol, or a function value.
;;
;;   (nelisp-special-forms-apply FN ARGLIST)
;;       Like `apply' — last element of ARGLIST may be a list whose
;;       elements are spliced as additional arguments.
;;
;; Special-form coverage (Doc 40 §3.B v2):
;;
;;   T143 core 11:   quote function setq if cond and or progn
;;                   let let* lambda
;;
;;   §3.B add. 11:   save-excursion save-restriction
;;                   save-current-buffer save-match-data
;;                   setq-default setq-local
;;                   eval-when-compile eval-and-compile
;;                   let-when-compile inline-let declare
;;
;; Closures are represented as (nelisp-sf-closure ENV PARAMS BODY).
;; The tag is distinct from `nelisp-eval.el's `nelisp-closure' so
;; the two evaluators can run side-by-side without function-table
;; collisions.  Bridge translation between the two tags is left to
;; Phase 7+C (Doc 40 §3.3 closure / lexical compatibility).
;;
;; Out of scope (carry-forward to later sub-phases per Doc 40):
;;
;;   - macro expansion / defmacro      Phase 7+D (§3.4)
;;   - condition-case / unwind-protect Phase 7+E (§3.5)
;;   - catch / throw                   Phase 7+E (§3.5)
;;   - advice add/remove               Phase 7+E (§3.5)
;;   - defun/defvar/defconst global    Phase 7+B emits the side-effect
;;                                     into a private global table; the
;;                                     full anvil-server cold-init path
;;                                     waits for Phase 7+F (§3.6)
;;   - pcase                           Phase 7+D (§3.4, B1 reflow)
;;
;; The evaluator is intentionally minimal: lambda parameter binding
;; stays lexical (Doc 40 §1.6 / §3.5 footnote), &rest is supported,
;; &optional is supported with default nil (default value evaluation
;; is Phase 7+C scope), and dynamic vs lexical name discipline reuses
;; `nelisp-special-forms--specials' — independent from the legacy
;; `nelisp--specials' table.

;;; Code:

(define-error 'nelisp-special-forms-error
  "NeLisp special-forms error (Phase 7+B)")

(define-error 'nelisp-special-forms-unbound-variable
  "NeLisp special-forms: symbol's value is void"
  'nelisp-special-forms-error)

(define-error 'nelisp-special-forms-void-function
  "NeLisp special-forms: symbol's function is void"
  'nelisp-special-forms-error)

;;; Global state -------------------------------------------------------

(defvar nelisp-special-forms--functions (make-hash-table :test 'eq)
  "Symbol -> NeLisp special-forms closure.
Populated by `defun' style top-level definitions.  Independent from
`nelisp--functions' so the two evaluators do not contend.")

(defvar nelisp-special-forms--globals (make-hash-table :test 'eq)
  "Symbol -> value for top-level global definitions.
Populated by `setq' on a name not in any lexical frame, by
`setq-default', and by future `defvar' / `defconst' wiring.")

(defvar nelisp-special-forms--buffer-locals
  (make-hash-table :test 'eq)
  "Symbol -> value for `setq-local' bindings.
Phase 7+B keeps this as a single global table; the per-buffer
semantics arrive with the buffer subsystem (Phase 9b / Doc 40 §6.2
risks footnote).  This stub preserves the lexical contract: a
`setq-local' performed before any `setq-default' shadows the global
on subsequent reads from inside `nelisp-special-forms-eval'.")

(defvar nelisp-special-forms--specials (make-hash-table :test 'eq)
  "Set of symbols declared dynamic-special.
Reserved for Phase 7+C.  Phase 7+B treats every name as lexical
unless it has been touched via `setq-default'.")

(defconst nelisp-special-forms--unbound
  (make-symbol "nelisp-sf-unbound")
  "Sentinel returned from hash-table lookups when a key is missing.")

;;; Closure representation --------------------------------------------

(defsubst nelisp-special-forms--closure-p (x)
  "Return non-nil when X is a Phase 7+B closure object."
  (and (consp x) (eq (car x) 'nelisp-sf-closure)))

(defun nelisp-special-forms--make-closure (env params body)
  "Build a closure object capturing ENV / PARAMS / BODY."
  (list 'nelisp-sf-closure env params body))

(defsubst nelisp-special-forms--closure-env    (c) (nth 1 c))
(defsubst nelisp-special-forms--closure-params (c) (nth 2 c))
(defsubst nelisp-special-forms--closure-body   (c) (nth 3 c))

;;; Lookup -------------------------------------------------------------

(defun nelisp-special-forms--lookup (sym env)
  "Return SYM's value resolving ENV (alist) then the global tables.
Order: lexical ENV, then `setq-local' table, then `setq-default'
global table.  Signal `nelisp-special-forms-unbound-variable' if
nothing binds SYM."
  (cond
   ((eq sym nil) nil)
   ((eq sym t) t)
   ((keywordp sym) sym)
   (t
    (let ((cell (assq sym env)))
      (if cell
          (cdr cell)
        (let ((bl (gethash sym nelisp-special-forms--buffer-locals
                           nelisp-special-forms--unbound)))
          (if (not (eq bl nelisp-special-forms--unbound))
              bl
            (let ((g (gethash sym nelisp-special-forms--globals
                              nelisp-special-forms--unbound)))
              (if (eq g nelisp-special-forms--unbound)
                  (signal 'nelisp-special-forms-unbound-variable
                          (list sym))
                g)))))))))

(defun nelisp-special-forms--function-of (sym)
  "Return the callable bound to SYM in the function tables."
  (let ((f (gethash sym nelisp-special-forms--functions
                    nelisp-special-forms--unbound)))
    (cond
     ((not (eq f nelisp-special-forms--unbound)) f)
     ((fboundp sym) (symbol-function sym))
     (t (signal 'nelisp-special-forms-void-function (list sym))))))

;;; Special-form dispatcher table ------------------------------------

(defvar nelisp-special-forms--dispatch
  (let ((h (make-hash-table :test 'eq :size 32)))
    (puthash 'quote               'nelisp-special-forms--eval-quote h)
    (puthash 'function            'nelisp-special-forms--eval-function h)
    (puthash 'if                  'nelisp-special-forms--eval-if h)
    (puthash 'cond                'nelisp-special-forms--eval-cond h)
    (puthash 'and                 'nelisp-special-forms--eval-and h)
    (puthash 'or                  'nelisp-special-forms--eval-or h)
    (puthash 'progn               'nelisp-special-forms--eval-progn h)
    (puthash 'let                 'nelisp-special-forms--eval-let h)
    (puthash 'let*                'nelisp-special-forms--eval-let* h)
    (puthash 'lambda              'nelisp-special-forms--eval-lambda h)
    (puthash 'setq                'nelisp-special-forms--eval-setq h)
    (puthash 'save-excursion      'nelisp-special-forms--eval-save-excursion h)
    (puthash 'save-restriction    'nelisp-special-forms--eval-save-restriction h)
    (puthash 'save-current-buffer 'nelisp-special-forms--eval-save-current-buffer h)
    (puthash 'save-match-data     'nelisp-special-forms--eval-save-match-data h)
    (puthash 'setq-default        'nelisp-special-forms--eval-setq-default h)
    (puthash 'setq-local          'nelisp-special-forms--eval-setq-local h)
    (puthash 'eval-when-compile   'nelisp-special-forms--eval-eval-when-compile h)
    (puthash 'eval-and-compile    'nelisp-special-forms--eval-eval-and-compile h)
    (puthash 'let-when-compile    'nelisp-special-forms--eval-let-when-compile h)
    (puthash 'inline-let          'nelisp-special-forms--eval-inline-let h)
    (puthash 'declare             'nelisp-special-forms--eval-declare h)
    h)
  "Hash table mapping special-form head symbol -> handler function.
Doc 40 §3.B refactors the legacy `cond' chain to a hash lookup so
the eventual Phase 7+E expansion to ~35 forms keeps O(1) dispatch.")

;;; Evaluator core ----------------------------------------------------

(defun nelisp-special-forms-eval (form &optional env)
  "Evaluate FORM in lexical ENV (alist of (symbol . value)).
ENV defaults to nil.  Public API per Doc 40 §4.2.

Atoms that are not symbols and not cons are self-evaluating
(numbers, strings, vectors, keywords, characters, hash-tables,
buffers, opaque host objects, etc.) — this matches Emacs Lisp
semantics and lets opaque host values (e.g. a `current-buffer'
object captured in a quoted lambda) round-trip through the
evaluator unchanged."
  (cond
   ((symbolp form)
    (cond
     ((eq form nil) nil)
     ((eq form t) t)
     ((keywordp form) form)
     (t (nelisp-special-forms--lookup form env))))
   ((consp form)
    (let* ((head (car form))
           (args (cdr form))
           (handler (and (symbolp head)
                         (gethash head nelisp-special-forms--dispatch))))
      (if handler
          (funcall handler args env)
        (nelisp-special-forms--eval-call head args env))))
   (t
    ;; Self-evaluating: numbers, strings, vectors, characters, opaque
    ;; objects (buffers, hash-tables, processes, etc.).
    form)))

(defun nelisp-special-forms--eval-body (forms env)
  "Evaluate FORMS sequentially in ENV, return the last value."
  (let ((last nil))
    (while forms
      (setq last (nelisp-special-forms-eval (car forms) env)
            forms (cdr forms)))
    last))

;;; Special-form handlers — T143 core 11 -----------------------------

(defun nelisp-special-forms--eval-quote (args _env)
  "(quote X) — return X unevaluated."
  (car args))

(defun nelisp-special-forms--eval-function (args env)
  "(function FORM) — quote a lambda as a closure over ENV."
  (let ((form (car args)))
    (cond
     ((and (consp form) (eq (car form) 'lambda))
      (nelisp-special-forms--make-closure env (cadr form) (cddr form)))
     ((symbolp form) (nelisp-special-forms--function-of form))
     (t (signal 'nelisp-special-forms-error
                (list "cannot take function value of" form))))))

(defun nelisp-special-forms--eval-if (args env)
  "(if TEST THEN ELSE...) — ELSE is an implicit progn."
  (if (nelisp-special-forms-eval (car args) env)
      (nelisp-special-forms-eval (cadr args) env)
    (nelisp-special-forms--eval-body (cddr args) env)))

(defun nelisp-special-forms--eval-cond (args env)
  "(cond (TEST BODY...) ...) — first truthy test wins.
If a clause has no body, the test value itself is returned."
  (catch 'nelisp-sf--cond-done
    (dolist (clause args)
      (unless (consp clause)
        (signal 'nelisp-special-forms-error
                (list "cond clause must be a list" clause)))
      (let ((test-val (nelisp-special-forms-eval (car clause) env)))
        (when test-val
          (throw 'nelisp-sf--cond-done
                 (if (cdr clause)
                     (nelisp-special-forms--eval-body (cdr clause) env)
                   test-val)))))
    nil))

(defun nelisp-special-forms--eval-and (args env)
  "(and FORM...) — short-circuit; (and) returns t."
  (if (null args)
      t
    (catch 'nelisp-sf--and-done
      (let ((last nil))
        (while args
          (setq last (nelisp-special-forms-eval (car args) env))
          (unless last
            (throw 'nelisp-sf--and-done nil))
          (setq args (cdr args)))
        last))))

(defun nelisp-special-forms--eval-or (args env)
  "(or FORM...) — first truthy value, else nil."
  (catch 'nelisp-sf--or-done
    (dolist (form args)
      (let ((v (nelisp-special-forms-eval form env)))
        (when v
          (throw 'nelisp-sf--or-done v))))
    nil))

(defun nelisp-special-forms--eval-progn (args env)
  "(progn BODY...) — sequential, returns last value."
  (nelisp-special-forms--eval-body args env))

(defun nelisp-special-forms--eval-lambda (args env)
  "(lambda PARAMS BODY...) — capture ENV into a closure."
  (nelisp-special-forms--make-closure env (car args) (cdr args)))

(defun nelisp-special-forms--split-binding (b env)
  "Return (SYM . VAL) from a let binding spec B, evaluating in ENV."
  (cond
   ((symbolp b) (cons b nil))
   ((and (consp b) (symbolp (car b)))
    (cons (car b) (nelisp-special-forms-eval (cadr b) env)))
   (t (signal 'nelisp-special-forms-error
              (list "malformed let binding" b)))))

(defun nelisp-special-forms--eval-let (args env)
  "(let ((VAR VAL) ...) BODY...) — parallel binding."
  (let ((bindings (car args))
        (body (cdr args))
        (lex-pairs nil))
    (dolist (b bindings)
      (let ((p (nelisp-special-forms--split-binding b env)))
        (push (cons (car p) (cdr p)) lex-pairs)))
    (nelisp-special-forms--eval-body
     body
     (append (nreverse lex-pairs) env))))

(defun nelisp-special-forms--eval-let* (args env)
  "(let* ((VAR VAL) ...) BODY...) — sequential binding."
  (let ((bindings (car args))
        (body (cdr args))
        (new-env env))
    (dolist (b bindings)
      (let ((p (nelisp-special-forms--split-binding b new-env)))
        (setq new-env (cons (cons (car p) (cdr p)) new-env))))
    (nelisp-special-forms--eval-body body new-env)))

(defun nelisp-special-forms--eval-setq (args env)
  "(setq SYM1 VAL1 SYM2 VAL2 ...) — return last assigned value.
Lexical names mutate the leftmost ENV cell that binds SYM (via
`setcdr').  Names with no lexical binding fall through to the
global table."
  (let ((last nil))
    (while args
      (let ((sym (car args))
            (form (cadr args)))
        (unless (symbolp sym)
          (signal 'nelisp-special-forms-error
                  (list "setq target must be a symbol" sym)))
        (when (null (cdr args))
          (signal 'nelisp-special-forms-error
                  (list "setq missing value for" sym)))
        (let ((val (nelisp-special-forms-eval form env)))
          (let ((cell (assq sym env)))
            (if cell
                (setcdr cell val)
              (puthash sym val nelisp-special-forms--globals)))
          (setq last val))
        (setq args (cddr args))))
    last))

;;; Special-form handlers — Doc 40 §3.B add. 11 ----------------------

(defun nelisp-special-forms--eval-save-excursion (args env)
  "(save-excursion BODY...) — host-Emacs save/restore around BODY.
Phase 7+B delegates to the host primitive; the buffer-state
boundary that Doc 40 §3.B risks calls out is Phase 9b scope."
  (save-excursion
    (nelisp-special-forms--eval-body args env)))

(defun nelisp-special-forms--eval-save-restriction (args env)
  "(save-restriction BODY...) — host-Emacs delegate."
  (save-restriction
    (nelisp-special-forms--eval-body args env)))

(defun nelisp-special-forms--eval-save-current-buffer (args env)
  "(save-current-buffer BODY...) — host-Emacs delegate."
  (save-current-buffer
    (nelisp-special-forms--eval-body args env)))

(defun nelisp-special-forms--eval-save-match-data (args env)
  "(save-match-data BODY...) — host-Emacs delegate."
  (save-match-data
    (nelisp-special-forms--eval-body args env)))

(defun nelisp-special-forms--eval-setq-default (args env)
  "(setq-default SYM1 VAL1 ...) — write to the default-global table.
Phase 7+B has no per-buffer dispatch yet; the value is persisted
in `nelisp-special-forms--globals' and is visible to subsequent
`nelisp-special-forms--lookup' calls in any environment."
  (let ((last nil))
    (while args
      (let ((sym (car args))
            (form (cadr args)))
        (unless (symbolp sym)
          (signal 'nelisp-special-forms-error
                  (list "setq-default target must be a symbol" sym)))
        (when (null (cdr args))
          (signal 'nelisp-special-forms-error
                  (list "setq-default missing value for" sym)))
        (let ((val (nelisp-special-forms-eval form env)))
          (puthash sym val nelisp-special-forms--globals)
          (setq last val))
        (setq args (cddr args))))
    last))

(defun nelisp-special-forms--eval-setq-local (args env)
  "(setq-local SYM1 VAL1 ...) — write to the buffer-local stub table.
Phase 7+B keeps a single per-process buffer-local map; per-buffer
semantics arrive with Phase 9b.  Reads via
`nelisp-special-forms--lookup' favour this table over the global."
  (let ((last nil))
    (while args
      (let ((sym (car args))
            (form (cadr args)))
        (unless (symbolp sym)
          (signal 'nelisp-special-forms-error
                  (list "setq-local target must be a symbol" sym)))
        (when (null (cdr args))
          (signal 'nelisp-special-forms-error
                  (list "setq-local missing value for" sym)))
        (let ((val (nelisp-special-forms-eval form env)))
          (puthash sym val nelisp-special-forms--buffer-locals)
          (setq last val))
        (setq args (cddr args))))
    last))

(defun nelisp-special-forms--eval-eval-when-compile (args env)
  "(eval-when-compile BODY...) — interpret path evaluates immediately.
Doc 40 §3.B: byte-compile path is Phase 7.1 native compiler scope."
  (nelisp-special-forms--eval-body args env))

(defun nelisp-special-forms--eval-eval-and-compile (args env)
  "(eval-and-compile BODY...) — interpret path evaluates immediately."
  (nelisp-special-forms--eval-body args env))

(defun nelisp-special-forms--eval-let-when-compile (args env)
  "(let-when-compile BINDINGS BODY...) — interpret path == let.
Doc 40 §3.B: byte-compile-time visibility is Phase 7.1 scope."
  (nelisp-special-forms--eval-let args env))

(defun nelisp-special-forms--eval-inline-let (args env)
  "(inline-let BINDINGS BODY...) — interpret path == let*.
Phase 7+B treats the inline hint as a no-op; Phase 7.1 native
compile honours the inlining."
  (nelisp-special-forms--eval-let* args env))

(defun nelisp-special-forms--eval-declare (_args _env)
  "(declare CLAUSE...) — declarative-only, returns nil."
  nil)

;;; Function call -----------------------------------------------------

(defun nelisp-special-forms--eval-call (head args env)
  "Evaluate a function call (HEAD ARGS...) under ENV.

When HEAD is the symbol `funcall' or `apply', the call is routed
through `nelisp-special-forms--apply' so NeLisp closures resolve
correctly even though the host Emacs `funcall' / `apply' do not
recognise the `nelisp-sf-closure' tag."
  (let ((vals (mapcar (lambda (a) (nelisp-special-forms-eval a env))
                      args)))
    (cond
     ((eq head 'funcall)
      (nelisp-special-forms--apply (car vals) (cdr vals)))
     ((eq head 'apply)
      (nelisp-special-forms-apply (car vals) (cdr vals)))
     (t
      (let ((fn (cond
                 ((symbolp head)
                  (nelisp-special-forms--function-of head))
                 ((nelisp-special-forms--closure-p head)
                  head)
                 ((and (consp head) (eq (car head) 'lambda))
                  (nelisp-special-forms--make-closure env
                                                      (cadr head)
                                                      (cddr head)))
                 ((and (consp head) (eq (car head) 'function))
                  (nelisp-special-forms-eval head env))
                 (t
                  (signal 'nelisp-special-forms-error
                          (list "not a callable head" head))))))
        (nelisp-special-forms--apply fn vals))))))

(defun nelisp-special-forms-funcall (fn &rest args)
  "Apply callable FN to ARGS.  Public API per Doc 40 §4.2."
  (nelisp-special-forms--apply fn args))

(defun nelisp-special-forms-apply (fn arglist)
  "Apply callable FN to ARGLIST.  Public API per Doc 40 §4.2.
The last element of ARGLIST is spliced in (Emacs `apply' semantics)."
  (let* ((reversed (reverse arglist))
         (last-elt (car reversed))
         (head     (nreverse (cdr reversed)))
         (effective
          (cond
           ((null arglist) nil)
           ((null reversed) nil)
           ((listp last-elt) (append head last-elt))
           (t (append head (list last-elt))))))
    (nelisp-special-forms--apply fn effective)))

(defun nelisp-special-forms--apply (fn args)
  "Apply FN to ARGS, dispatching on closure tag or host fboundp.

Recognises three callable shapes:
  1. Phase 7+B `(nelisp-sf-closure ENV PARAMS BODY)' tag —
     dispatched via `nelisp-special-forms--apply-closure'.
  2. Phase 7+C `nelisp-closure' record (cl-defstruct) — dispatched
     via `nelisp-closure-apply' when that module is loaded.  This
     is a *runtime* `fboundp' check so this file imposes no
     load-time dependency on `nelisp-closure.el'.
  3. Host-Emacs symbol or function value — falls through to the
     host `apply'."
  (cond
   ((nelisp-special-forms--closure-p fn)
    (nelisp-special-forms--apply-closure fn args))
   ((and (fboundp 'nelisp-closure-p)
         (funcall (symbol-function 'nelisp-closure-p) fn))
    (funcall (symbol-function 'nelisp-closure-apply) fn args))
   ((symbolp fn)
    (let ((resolved (nelisp-special-forms--function-of fn)))
      (cond
       ((nelisp-special-forms--closure-p resolved)
        (nelisp-special-forms--apply-closure resolved args))
       ((and (fboundp 'nelisp-closure-p)
             (funcall (symbol-function 'nelisp-closure-p) resolved))
        (funcall (symbol-function 'nelisp-closure-apply) resolved args))
       (t (apply resolved args)))))
   ((functionp fn)
    (apply fn args))
   (t
    (signal 'nelisp-special-forms-error
            (list "not applicable" fn)))))

(defun nelisp-special-forms--apply-closure (closure args)
  "Apply a CLOSURE object to ARGS."
  (let* ((env    (nelisp-special-forms--closure-env closure))
         (params (nelisp-special-forms--closure-params closure))
         (body   (nelisp-special-forms--closure-body closure))
         (frame  (nelisp-special-forms--bind-params params args)))
    (nelisp-special-forms--eval-body body (append frame env))))

(defun nelisp-special-forms--bind-params (params args)
  "Bind PARAMS (lambda list) to ARGS, return alist frame.
Supports &optional (default nil) and &rest."
  (let ((frame nil)
        (mode 'positional))
    (while params
      (let ((p (car params)))
        (cond
         ((eq p '&optional) (setq mode 'optional))
         ((eq p '&rest)
          (setq mode 'rest)
          (setq params (cdr params))
          (when params
            (push (cons (car params) args) frame)
            (setq args nil)))
         (t
          (cond
           ((eq mode 'positional)
            (when (null args)
              (signal 'nelisp-special-forms-error
                      (list "missing positional arg for" p)))
            (push (cons p (car args)) frame)
            (setq args (cdr args)))
           ((eq mode 'optional)
            (push (cons p (car args)) frame)
            (setq args (cdr args)))))))
      (setq params (cdr params)))
    (nreverse frame)))

;;; Reset helper (test harness convenience) --------------------------

(defun nelisp-special-forms-reset ()
  "Clear all global tables.  Test-harness convenience helper."
  (clrhash nelisp-special-forms--functions)
  (clrhash nelisp-special-forms--globals)
  (clrhash nelisp-special-forms--buffer-locals)
  (clrhash nelisp-special-forms--specials)
  nil)

(provide 'nelisp-special-forms)

;;; nelisp-special-forms.el ends here
