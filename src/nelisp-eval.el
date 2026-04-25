;;; nelisp-eval.el --- Elisp evaluator in pure Elisp  -*- lexical-binding: t; -*-

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

;; Phase 1 Week 5-24 evaluator core.  Enough of Elisp to run `fib',
;; `factorial', `cond'-driven recursion, short-circuiting `and' / `or',
;; `when' / `unless' guards, closures that capture + mutate their
;; lexical environment, tagged non-local exits, error handling via
;; `condition-case', and true dynamic binding for symbols declared
;; with `defvar' or `defconst'.
;;
;; Implemented special forms:
;;   quote, function, if, cond, and, or, when, unless,
;;   progn, prog1, prog2, let, let*, lambda, defun, defvar, defconst,
;;   setq, while, catch, throw, unwind-protect, condition-case
;;
;; Installed builtins (delegated to the host Elisp functions):
;;   List:   car cdr caar cadr cdar cddr
;;           cons list null not atom consp listp
;;           length nth nthcdr last reverse nreverse append
;;           member memq assq assoc setcar setcdr
;;   Eq:     eq eql equal identity ignore
;;   Arith:  + - * / mod /= < <= > >= =
;;           1+ 1- abs max min zerop numberp integerp
;;   String: stringp concat substring string=
;;           string-to-number number-to-string upcase downcase
;;           format prin1-to-string
;;   Symbol: symbolp keywordp intern make-symbol symbol-name gensym
;;   Hash:   make-hash-table gethash puthash remhash clrhash
;;           hash-table-p hash-table-count
;;   Error:  error signal user-error define-error
;; Plus NeLisp-aware wrappers that must see our closure tag or our
;; hash tables: funcall apply mapcar mapc mapconcat boundp fboundp
;; symbol-value.
;;
;; Core macros (installed via `nelisp--install-core-macros', sources
;; in `nelisp--core-macro-source' — written in NeLisp itself thanks
;; to the Phase 2 backquote reader): defsubst declare-function push
;; pop dolist dotimes.
;;
;; The macro system (defmacro / macroexpand / macroexpand-all) lives
;; in `nelisp-macro.el'; this file only hosts the two hooks that make
;; it work: the `nelisp--macros' registry and the evaluator dispatch
;; arm that routes macro calls through `nelisp-macroexpand'.
;;
;; Deferred (still out of scope for this file):
;;   dolist / dotimes (trivial once macros land via nelisp-macro.el)
;;   princ / prin1 / message side-effect output (tests prefer pure
;;   `prin1-to-string' / `format' variants anyway)
;;   dynamic binding on function parameters (only `let' / `let*'
;;   bindings go dynamic for special names; lambda params stay
;;   lexical regardless, a deliberate Phase 1 simplification)
;;
;; `catch' / `throw' / `unwind-protect' delegate to the host Elisp
;; equivalents.  The semantics are identical, and threading through
;; Elisp's implementation means NeLisp errors that bubble up through
;; any number of host-level cleanup layers unwind correctly.
;;
;; Dynamic binding follows Emacs's special-variable discipline: a
;; symbol becomes dynamic by appearing in `nelisp--specials' (set by
;; `defvar' / `defconst').  `let' / `let*' inspect that flag per
;; binding and, for specials, save the previous global value, install
;; the new one, and restore on every exit path via `unwind-protect'.
;; Variable lookup checks `nelisp--specials' first so dynamic names
;; never shadow-through a lexical environment frame.
;;
;; NeLisp keeps its own function / global tables rather than writing to
;; the host Emacs symbol cells, so running `(nelisp-eval '(defun fib …))'
;; does not pollute the host environment and vice versa.  Primitive
;; function bindings are still borrowed from Emacs — they will be
;; re-implemented natively when Phase 5 rewrites the relevant C code
;; in NeLisp.

;;; Code:

(require 'nelisp-read)
;; Phase 6.2.0 — anvil-http port preparation. `url-host' / `url-port' /
;; `url-filename' / `url-type' are cl-defstruct accessors defined in
;; `url-parse'; without an explicit require they remain unbound and the
;; primitive install loop trips on `symbol-function'.
(require 'url-parse)

(define-error 'nelisp-eval-error
  "NeLisp evaluation error")

(define-error 'nelisp-unbound-variable
  "NeLisp: symbol's value is void"
  'nelisp-eval-error)

(define-error 'nelisp-void-function
  "NeLisp: symbol's function is void"
  'nelisp-eval-error)

;;; Global state -------------------------------------------------------

(defvar nelisp--functions (make-hash-table :test 'eq)
  "Symbol -> primitive function or NeLisp closure.
Populated by `defun' and by `nelisp--install-primitives'.")

(defvar nelisp--globals (make-hash-table :test 'eq)
  "Symbol -> value for top-level definitions (defvar, global setq).")

(defvar nelisp--specials (make-hash-table :test 'eq)
  "Set of symbols declared special via `defvar'.
Reserved for the Week 17+ lexical/dynamic binding split — currently
populated but not yet consulted during binding.")

(defvar nelisp--macros (make-hash-table :test 'eq)
  "Symbol -> NeLisp closure used as a macro expander.
Populated by `defmacro' (see `nelisp-macro.el').  Keys stay distinct
from `nelisp--functions': a name is either a function or a macro,
never both.")

(declare-function nelisp--eval-defmacro "nelisp-macro" (args env))
(declare-function nelisp-macroexpand "nelisp-macro" (form))
(declare-function nelisp-bc-run "nelisp-bytecode" (bcl &optional args))

(defconst nelisp--unbound (make-symbol "nelisp-unbound")
  "Sentinel returned from hash-table lookups when a key is missing.")

;;; Closure representation --------------------------------------------

(defsubst nelisp--closure-p (x)
  (and (consp x) (eq (car x) 'nelisp-closure)))

(defun nelisp--make-closure (env params body)
  "Build a callable from ENV / PARAMS / BODY.
When `nelisp-bytecode' is loaded and the body compiles cleanly,
the result is a `nelisp-bcl' that runs on the VM.  Otherwise the
interpreter closure form `(nelisp-closure ENV PARAMS BODY)' is
returned and `nelisp--apply-closure' handles it."
  (or (and (fboundp 'nelisp-bc-try-compile-lambda)
           (funcall 'nelisp-bc-try-compile-lambda env params body))
      (list 'nelisp-closure env params body)))

(defsubst nelisp--closure-env    (c) (nth 1 c))
(defsubst nelisp--closure-params (c) (nth 2 c))
(defsubst nelisp--closure-body   (c) (nth 3 c))

;;; Lookup -------------------------------------------------------------

(defun nelisp--lookup (sym env)
  "Return SYM's value in ENV (alist) then in globals.
Special (dynamic) variables bypass ENV and read directly from
`nelisp--globals' — that is what makes them dynamic.  Signal
`nelisp-unbound-variable' if nothing binds SYM."
  (cond
   ((eq sym nil) nil)
   ((eq sym t) t)
   ((keywordp sym) sym)
   ((gethash sym nelisp--specials)
    (let ((g (gethash sym nelisp--globals nelisp--unbound)))
      (if (eq g nelisp--unbound)
          (signal 'nelisp-unbound-variable (list sym))
        g)))
   (t
    (let ((cell (assq sym env)))
      (if cell
          (cdr cell)
        (let ((g (gethash sym nelisp--globals nelisp--unbound)))
          (if (eq g nelisp--unbound)
              (signal 'nelisp-unbound-variable (list sym))
            g)))))))

(defun nelisp--function-of (sym)
  "Return the callable bound to SYM in `nelisp--functions'.
Signal `nelisp-void-function' if none is bound."
  (let ((f (gethash sym nelisp--functions nelisp--unbound)))
    (if (eq f nelisp--unbound)
        (signal 'nelisp-void-function (list sym))
      f)))

;;; Evaluator ----------------------------------------------------------

(defun nelisp-eval-form (form env)
  "Evaluate FORM in lexical ENV (alist of (symbol . value))."
  (cond
   ((or (numberp form) (stringp form) (keywordp form)
        (eq form nil) (eq form t))
    form)
   ((symbolp form)
    (nelisp--lookup form env))
   ((consp form)
    (let ((head (car form))
          (args (cdr form)))
      (cond
       ((eq head 'quote)     (car args))
       ((eq head 'function)  (nelisp--eval-function (car args) env))
       ((eq head 'if)        (nelisp--eval-if args env))
       ((eq head 'cond)      (nelisp--eval-cond args env))
       ((eq head 'and)       (nelisp--eval-and args env))
       ((eq head 'or)        (nelisp--eval-or args env))
       ((eq head 'when)      (nelisp--eval-when args env))
       ((eq head 'unless)    (nelisp--eval-unless args env))
       ((eq head 'progn)     (nelisp--eval-body args env))
       ((eq head 'prog1)     (nelisp--eval-prog1 args env))
       ((eq head 'prog2)     (nelisp--eval-prog2 args env))
       ((eq head 'let)       (nelisp--eval-let args env))
       ((eq head 'let*)      (nelisp--eval-let* args env))
       ((eq head 'lambda)
        (nelisp--make-closure env (car args) (cdr args)))
       ((eq head 'defun)     (nelisp--eval-defun args env))
       ((eq head 'defvar)    (nelisp--eval-defvar args env))
       ((eq head 'defconst)  (nelisp--eval-defconst args env))
       ((eq head 'setq)      (nelisp--eval-setq args env))
       ((eq head 'while)     (nelisp--eval-while args env))
       ((eq head 'catch)           (nelisp--eval-catch args env))
       ((eq head 'throw)           (nelisp--eval-throw args env))
       ((eq head 'unwind-protect)  (nelisp--eval-unwind-protect args env))
       ((eq head 'condition-case)  (nelisp--eval-condition-case args env))
       ((eq head 'defmacro)        (nelisp--eval-defmacro args env))
       ((and (symbolp head)
             (not (eq (gethash head nelisp--macros nelisp--unbound)
                      nelisp--unbound)))
        (nelisp-eval-form (nelisp-macroexpand (cons head args)) env))
       (t (nelisp--eval-call head args env)))))
   (t
    (signal 'nelisp-eval-error (list "cannot evaluate" form)))))

(defun nelisp--eval-body (forms env)
  "Evaluate FORMS sequentially in ENV, return the last value."
  (let ((last nil))
    (while forms
      (setq last (nelisp-eval-form (car forms) env)
            forms (cdr forms)))
    last))

(defun nelisp--eval-if (args env)
  "(if TEST THEN ELSE...) — ELSE is an implicit progn."
  (if (nelisp-eval-form (car args) env)
      (nelisp-eval-form (cadr args) env)
    (nelisp--eval-body (cddr args) env)))

(defun nelisp--eval-cond (args env)
  "(cond (TEST BODY...) ...) — first truthy test wins.
If a clause has no body, the test value itself is returned."
  (catch 'nelisp--cond-done
    (dolist (clause args)
      (unless (consp clause)
        (signal 'nelisp-eval-error
                (list "cond clause must be a list" clause)))
      (let ((test-val (nelisp-eval-form (car clause) env)))
        (when test-val
          (throw 'nelisp--cond-done
                 (if (cdr clause)
                     (nelisp--eval-body (cdr clause) env)
                   test-val)))))
    nil))

(defun nelisp--eval-and (args env)
  "(and FORM...) — short-circuit; `(and)' returns t."
  (if (null args)
      t
    (catch 'nelisp--and-done
      (let ((last nil))
        (while args
          (setq last (nelisp-eval-form (car args) env))
          (unless last
            (throw 'nelisp--and-done nil))
          (setq args (cdr args)))
        last))))

(defun nelisp--eval-or (args env)
  "(or FORM...) — return first truthy value, else nil."
  (catch 'nelisp--or-done
    (dolist (form args)
      (let ((v (nelisp-eval-form form env)))
        (when v
          (throw 'nelisp--or-done v))))
    nil))

(defun nelisp--eval-when (args env)
  "(when TEST BODY...) — body as implicit progn, or nil if TEST false."
  (when (nelisp-eval-form (car args) env)
    (nelisp--eval-body (cdr args) env)))

(defun nelisp--eval-unless (args env)
  "(unless TEST BODY...) — inverse of `when'."
  (unless (nelisp-eval-form (car args) env)
    (nelisp--eval-body (cdr args) env)))

(defun nelisp--eval-prog1 (args env)
  "(prog1 FIRST BODY...) — return FIRST's value after evaluating BODY."
  (let ((first (nelisp-eval-form (car args) env)))
    (nelisp--eval-body (cdr args) env)
    first))

(defun nelisp--eval-prog2 (args env)
  "(prog2 FIRST SECOND BODY...) — return SECOND's value."
  (nelisp-eval-form (car args) env)
  (let ((second (nelisp-eval-form (cadr args) env)))
    (nelisp--eval-body (cddr args) env)
    second))

(defun nelisp--eval-function (form env)
  "(function FORM) — quote a lambda as a NeLisp closure over ENV."
  (cond
   ((and (consp form) (eq (car form) 'lambda))
    (nelisp--make-closure env (cadr form) (cddr form)))
   ((symbolp form) (nelisp--function-of form))
   (t (signal 'nelisp-eval-error
              (list "cannot take function value of" form)))))

(defun nelisp--split-binding (b env)
  "Return (SYM . VAL) from a let binding spec B, evaluating in ENV.
B is either a bare symbol or a two-element list (SYM INIT)."
  (cond
   ((symbolp b) (cons b nil))
   ((and (consp b) (symbolp (car b)))
    (cons (car b) (nelisp-eval-form (cadr b) env)))
   (t (signal 'nelisp-eval-error
              (list "malformed let binding" b)))))

(defun nelisp--restore-dynamic (saves)
  "Restore dynamic bindings from SAVES, a list of (SYM NEW OLD).
Entries recorded via `nelisp--unbound' become `remhash' restores."
  (dolist (d saves)
    (let ((sym (car d))
          (old (nth 2 d)))
      (if (eq old nelisp--unbound)
          (remhash sym nelisp--globals)
        (puthash sym old nelisp--globals)))))

(defun nelisp--eval-let (args env)
  "(let ((VAR VAL) ...) BODY...) — parallel binding.
Lexical names extend ENV.  Names in `nelisp--specials' save their
previous global value, set the new value globally, and are restored
on every exit path from BODY (normal, throw, or error)."
  (let ((bindings (car args))
        (body (cdr args))
        (lex-pairs nil)
        (dyn-saves nil))
    (dolist (b bindings)
      (let* ((p (nelisp--split-binding b env))
             (sym (car p))
             (val (cdr p)))
        (if (gethash sym nelisp--specials)
            (push (list sym val
                        (gethash sym nelisp--globals nelisp--unbound))
                  dyn-saves)
          (push (cons sym val) lex-pairs))))
    (dolist (d dyn-saves)
      (puthash (car d) (nth 1 d) nelisp--globals))
    (unwind-protect
        (nelisp--eval-body body (append (nreverse lex-pairs) env))
      (nelisp--restore-dynamic dyn-saves))))

(defun nelisp--eval-let* (args env)
  "(let* ((VAR VAL) ...) BODY...) — sequential binding.
Each successive init form sees the prior bindings — lexical ones
via the extended ENV, dynamic ones via the global table that `let'
has just mutated."
  (let ((bindings (car args))
        (body (cdr args))
        (new-env env)
        (dyn-saves nil))
    (unwind-protect
        (progn
          (dolist (b bindings)
            (let* ((p (nelisp--split-binding b new-env))
                   (sym (car p))
                   (val (cdr p)))
              (if (gethash sym nelisp--specials)
                  (progn
                    (push (list sym val
                                (gethash sym nelisp--globals
                                         nelisp--unbound))
                          dyn-saves)
                    (puthash sym val nelisp--globals))
                (setq new-env (cons (cons sym val) new-env)))))
          (nelisp--eval-body body new-env))
      (nelisp--restore-dynamic dyn-saves))))

(defun nelisp--eval-defun (args env)
  "(defun NAME (PARAMS) BODY...) — install a global closure."
  (let ((name (car args))
        (params (cadr args))
        (body (cddr args)))
    (unless (symbolp name)
      (signal 'nelisp-eval-error (list "defun needs a symbol" name)))
    (puthash name (nelisp--make-closure env params body)
             nelisp--functions)
    name))

(defun nelisp--eval-defvar (args env)
  "(defvar NAME [INITVAL [DOCSTRING]]) — declare special, maybe init."
  (let ((name (car args)))
    (unless (symbolp name)
      (signal 'nelisp-eval-error (list "defvar needs a symbol" name)))
    (puthash name t nelisp--specials)
    (when (and (cdr args)
               (eq (gethash name nelisp--globals nelisp--unbound)
                   nelisp--unbound))
      (puthash name (nelisp-eval-form (cadr args) env) nelisp--globals))
    name))

(defun nelisp--eval-defconst (args env)
  "(defconst NAME VALUE [DOCSTRING]) — always (re-)initializes unlike `defvar'."
  (let ((name (car args)))
    (unless (symbolp name)
      (signal 'nelisp-eval-error (list "defconst needs a symbol" name)))
    (unless (cdr args)
      (signal 'nelisp-eval-error (list "defconst needs a value" name)))
    (puthash name t nelisp--specials)
    (puthash name (nelisp-eval-form (cadr args) env) nelisp--globals)
    name))

(defun nelisp--eval-setq (args env)
  "(setq SYM VAL [SYM VAL ...]) — update lexical if bound, else global."
  (let ((last nil))
    (while args
      (unless (cdr args)
        (signal 'nelisp-eval-error (list "setq with odd args")))
      (let* ((sym (car args))
             (val (nelisp-eval-form (cadr args) env))
             (cell (assq sym env)))
        (unless (symbolp sym)
          (signal 'nelisp-eval-error (list "setq non-symbol" sym)))
        (if cell
            (setcdr cell val)
          (puthash sym val nelisp--globals))
        (setq last val)
        (setq args (cddr args))))
    last))

(defun nelisp--eval-while (args env)
  "(while TEST BODY...) — returns nil."
  (while (nelisp-eval-form (car args) env)
    (nelisp--eval-body (cdr args) env))
  nil)

(defun nelisp--eval-catch (args env)
  "(catch TAG-FORM BODY...) — tagged non-local exit sink."
  (let ((tag (nelisp-eval-form (car args) env)))
    (catch tag
      (nelisp--eval-body (cdr args) env))))

(defun nelisp--eval-throw (args env)
  "(throw TAG-FORM VALUE-FORM) — transfer control to matching catch."
  (let ((tag (nelisp-eval-form (car args) env))
        (val (nelisp-eval-form (cadr args) env)))
    (throw tag val)))

(defun nelisp--eval-unwind-protect (args env)
  "(unwind-protect BODYFORM UNWINDFORMS...) — cleanup runs on every exit.
Normal completion, NeLisp `throw', and any signaled error all unwind
through the cleanup block."
  (unwind-protect
      (nelisp-eval-form (car args) env)
    (nelisp--eval-body (cdr args) env)))

(defun nelisp--handler-matches-p (spec conditions)
  "Return non-nil if SPEC matches one of CONDITIONS.
SPEC is the car of a `condition-case' handler — a condition symbol,
a list of condition symbols, or `t' to catch anything.  CONDITIONS
is the error's `error-conditions' chain (most specific first)."
  (cond
   ((eq spec t) t)
   ((symbolp spec) (memq spec conditions))
   ((listp spec)
    (catch 'nelisp--handler-match
      (dolist (s spec)
        (when (memq s conditions)
          (throw 'nelisp--handler-match t)))
      nil))
   (t nil)))

(defun nelisp--eval-condition-case (args env)
  "(condition-case VAR BODYFORM (CONDITION HANDLER-BODY...)...).
VAR is bound to the signaled error data inside a matching handler
(or left unbound when VAR is nil).  Handlers are tried in order;
the first match wins.  Unmatched errors propagate to the caller."
  (let ((var (car args))
        (bodyform (cadr args))
        (handlers (cddr args)))
    (unless (or (null var) (symbolp var))
      (signal 'nelisp-eval-error
              (list "condition-case VAR must be symbol or nil" var)))
    (condition-case err
        (nelisp-eval-form bodyform env)
      (error
       (let* ((conditions (get (car err) 'error-conditions))
              (matched nil))
         (catch 'nelisp--cc-done
           (dolist (h handlers)
             (unless (consp h)
               (signal 'nelisp-eval-error
                       (list "condition-case handler must be a list" h)))
             (when (nelisp--handler-matches-p (car h) conditions)
               (setq matched h)
               (throw 'nelisp--cc-done nil))))
         (if matched
             (let ((handler-env (if var
                                    (cons (cons var err) env)
                                  env)))
               (nelisp--eval-body (cdr matched) handler-env))
           (signal (car err) (cdr err))))))))

(defun nelisp--eval-call (head args env)
  "Evaluate a function call (HEAD ARGS...) in ENV."
  (let ((fn (cond
             ((symbolp head) (nelisp--function-of head))
             ((and (consp head) (eq (car head) 'lambda))
              (nelisp--make-closure env (cadr head) (cddr head)))
             ((nelisp--closure-p head) head)
             (t (signal 'nelisp-eval-error
                        (list "not callable" head))))))
    (nelisp--apply fn
                   (mapcar (lambda (a) (nelisp-eval-form a env)) args))))

;;; Apply --------------------------------------------------------------

(defun nelisp--apply (fn args)
  "Apply FN to the already-evaluated ARGS list.
A symbol FN looks in `nelisp--functions' first so NeLisp-only defuns
passed to higher-order primitives (e.g. `mapcar') dispatch correctly;
it falls through to the host Elisp binding only when the symbol is
not registered in our table, which keeps host helper symbols like
`nelisp--builtin-mapcar' callable when they appear inline.

A `nelisp-bcl' (bytecode closure built by `nelisp-bytecode.el')
dispatches straight into the VM."
  (cond
   ;; Inline the bcl check rather than introducing a defsubst — the
   ;; self-host probe evaluates this very file at NeLisp level, and a
   ;; named helper would add one host stack frame per NeLisp-on-NeLisp
   ;; `nelisp--apply' call, which is enough to trip
   ;; `max-lisp-eval-depth' inside `nelisp--install-core-macros'.
   ((and (consp fn) (eq (car fn) 'nelisp-bcl))
    (if (fboundp 'nelisp-bc-run)
        ;; Direct call rather than `funcall 'nelisp-bc-run' — byte-compile
        ;; can resolve this to a constant jump, saving a symbol-indirection
        ;; per CALL on the hot recursive path (fib 21k invocations).
        (nelisp-bc-run fn args)
      (signal 'nelisp-eval-error
              (list "bcl received without bytecode VM loaded" fn))))
   ((nelisp--closure-p fn)
    (nelisp--apply-closure fn args))
   ((symbolp fn)
    (let ((nfn (gethash fn nelisp--functions nelisp--unbound)))
      (if (eq nfn nelisp--unbound)
          (if (functionp fn)
              (apply fn args)
            (signal 'nelisp-void-function (list fn)))
        (nelisp--apply nfn args))))
   ((functionp fn)
    (apply fn args))
   (t
    (signal 'nelisp-eval-error (list "not a function" fn)))))

(defun nelisp--apply-closure (fn args)
  (let* ((params (nelisp--closure-params fn))
         (body   (nelisp--closure-body fn))
         (cenv   (nelisp--closure-env fn))
         (call-env (nelisp--bind-params params args cenv)))
    (nelisp--eval-body body call-env)))

(defun nelisp--bind-params (params args env)
  "Extend ENV by binding PARAMS to ARGS; support &optional and &rest."
  (let ((state 'required)
        (done nil))
    (while (and params (not done))
      (let ((p (car params)))
        (cond
         ((eq p '&optional)
          (setq state 'optional)
          (setq params (cdr params)))
         ((eq p '&rest)
          (setq params (cdr params))
          (unless (and params (symbolp (car params)))
            (signal 'nelisp-eval-error (list "&rest without symbol")))
          (setq env (cons (cons (car params) args) env))
          (setq args nil)
          (setq params nil)
          (setq done t))
         (t
          (cond
           ((and (eq state 'required) (null args))
            (signal 'nelisp-eval-error (list "too few args")))
           (t
            (setq env (cons (cons p (car args)) env))
            (setq args (cdr args))
            (setq params (cdr params))))))))
    (when args
      (signal 'nelisp-eval-error (list "too many args")))
    env))

;;; Primitive install --------------------------------------------------

(defconst nelisp--primitive-symbols
  '(;; Pair / list constructors + shape predicates
    car cdr car-safe caar cadr cdar cddr caddr cdddr
    cons list null not atom consp listp
    length nth nthcdr last butlast reverse nreverse append
    member memq assq assoc
    ;; List mutation (cons cell slot writes)
    setcar setcdr
    ;; General sequence / list helpers (Phase 5-B.0)
    copy-sequence elt nconc delq
    ;; Equality / identity / type predicates
    eq eql equal identity ignore functionp vectorp
    ;; Vector constructors
    vector make-vector
    ;; Arithmetic
    + - * / mod /= < <= > >= =
    1+ 1- abs max min zerop numberp integerp float
    ;; Bit arithmetic
    ash logand logior
    ;; String / format
    stringp concat substring string= string-to-number number-to-string
    upcase downcase format prin1-to-string string make-string
    aref aset string-match-p string-match string-empty-p
    char-or-string-p
    ;; String search / split (Phase 5-B.0)
    string-search split-string
    ;; Symbol surface (interning side; variable / function cells handled
    ;; via NeLisp-aware wrappers below for our own table — but the
    ;; host `symbol-function' is a useful escape hatch for bootstrap)
    symbolp keywordp intern make-symbol symbol-name gensym
    symbol-function
    ;; Property lists (used by condition-case to read error-conditions)
    get put plist-get plist-put
    ;; Hash tables — raw data, safe to delegate wholesale
    make-hash-table gethash puthash remhash clrhash
    hash-table-p hash-table-count
    ;; Feature registry — `featurep' lets NeLisp `require' trust the
    ;; host when NeLisp source is evaluated inside an Emacs session
    ;; that has already loaded the same feature (self-host + cycle)
    featurep
    ;; I/O (side-effect, but used for NeLisp-internal diagnostics)
    message
    ;; Terminal & frame metrics (Phase 5-B.0 — redisplay/eventloop 下地)
    send-string-to-terminal frame-width frame-height
    ;; Timer scheduling (Phase 5-B.0 — eventloop fallback と diagnostics)
    run-at-time
    ;; Subprocess primitives (Phase 5-C.0)
    make-process process-send-string process-send-eof
    process-live-p process-status process-exit-status
    kill-process delete-process accept-process-output
    process-id process-name process-command process-buffer
    set-process-sentinel set-process-filter
    ;; Network primitives (Phase 5-C.0)
    make-network-process
    ;; File system primitives (Phase 5-C.0)
    file-attributes file-exists-p file-directory-p
    delete-file rename-file
    ;; Host-buffer helpers used by HTTP parsing / file-notify (Phase 5-C.0)
    goto-char point point-min point-max
    buffer-substring-no-properties re-search-forward
    ;; List util (Phase 5-C.0)
    assq-delete-all
    ;; Time + numeric rounding + RNG (Phase 5-D.0 — worker metrics
    ;; and correlation id generation)
    float-time format-time-string truncate random
    ;; Stdio I/O (Phase 5-E.0 — MCP server line-delimited JSON-RPC)
    princ terpri read-from-minibuffer
    ;; File I/O + path parsing (Phase 5-E.0 — file-read / file-outline
    ;; tool handlers)
    insert-file-contents buffer-string file-name-extension
    ;; Host-buffer construction for process captures (Phase 5-E.0 —
    ;; git-log / git-status tool handlers)
    generate-new-buffer kill-buffer
    ;; Regex capture + line metadata (Phase 5-E.0 — file-outline
    ;; tool dispatcher)
    line-number-at-pos match-string
    ;; Alist access (Phase 5-E.0 — JSON-RPC params / MCP tool args)
    alist-get
    ;; SQLite primitives (Phase 5-F.1.0 — anvil-state port 前提、
    ;; host 委譲 SBCL-style。with-sqlite-transaction は移植性不安定
    ;; のため primitive 化せず、手書き BEGIN/COMMIT で回避)
    sqlite-available-p sqlitep sqlite-open sqlite-close
    sqlite-execute sqlite-select
    ;; URL + crypto primitives (Phase 6.2.0 — anvil-http port 前提、
    ;; SBCL-style host 委譲。url package は Emacs 29 built-in、
    ;; url-host / url-port / url-filename / url-type は url-parse の
    ;; cl-defstruct accessors なので require 'url-parse 済 (上記)。
    ;; 動的バインド変数 url-request-method / url-request-extra-headers
    ;; は host symbol cell に住むため NeLisp 側の dynamic-let では
    ;; 触れない — 高水準 wrapper (nelisp-http-fetch 等、Phase 6.2.1)
    ;; が host で let-bind してから url-retrieve-synchronously を呼ぶ
    ;; 設計とする)
    url-retrieve-synchronously url-generic-parse-url
    url-encode-url url-hexify-string url-unhex-string
    url-recreate-url
    url-host url-port url-filename url-type
    secure-hash
    ;; Error plumbing — `error' / `signal' / `user-error' / `define-error'
    ;; all hook into the host condition system that `condition-case'
    ;; already knows how to catch.
    error signal user-error define-error)
  "Host Elisp primitives borrowed wholesale by Phase 1 NeLisp.
Each entry is copied by `symbol-function' at install time so the
host's bytecode / subr implementation runs unchanged.")

(defun nelisp--builtin-funcall (fn &rest args)
  "NeLisp-aware `funcall': accepts NeLisp closures and primitives."
  (nelisp--apply fn args))

(defun nelisp--builtin-apply (fn &rest args)
  "NeLisp-aware `apply': splices the last argument like Elisp `apply'."
  (let ((flat (cond
               ((null args) nil)
               (t (append (butlast args) (car (last args)))))))
    (nelisp--apply fn flat)))

(defun nelisp--builtin-mapcar (fn seq)
  "NeLisp-aware `mapcar': FN may be a NeLisp closure, a symbol
resolved through `nelisp--functions', or a host Elisp primitive."
  (let ((result nil))
    (while seq
      (push (nelisp--apply fn (list (car seq))) result)
      (setq seq (cdr seq)))
    (nreverse result)))

(defun nelisp--builtin-mapc (fn seq)
  "NeLisp-aware `mapc': side-effect only, returns SEQ."
  (let ((orig seq))
    (while seq
      (nelisp--apply fn (list (car seq)))
      (setq seq (cdr seq)))
    orig))

(defun nelisp--builtin-mapconcat (fn seq separator)
  "NeLisp-aware `mapconcat'."
  (let ((parts nil))
    (while seq
      (push (nelisp--apply fn (list (car seq))) parts)
      (setq seq (cdr seq)))
    (mapconcat #'identity (nreverse parts) separator)))

(defun nelisp--builtin-maphash (fn table)
  "NeLisp-aware `maphash': FN receives (KEY VALUE) for each entry.
FN may be a NeLisp closure, a symbol routed through
`nelisp--functions', or a host Elisp primitive.  Returns nil like
the host `maphash'."
  (maphash (lambda (k v) (nelisp--apply fn (list k v))) table)
  nil)

(defun nelisp--builtin-boundp (sym)
  "Non-nil if SYM has a value in the NeLisp global table.
Self-evaluating atoms (nil, t, keywords) are always bound."
  (cond
   ((memq sym '(nil t)) t)
   ((keywordp sym) t)
   (t (not (eq (gethash sym nelisp--globals nelisp--unbound)
               nelisp--unbound)))))

(defun nelisp--builtin-fboundp (sym)
  "Non-nil if SYM has a function (or macro) in the NeLisp tables."
  (or (not (eq (gethash sym nelisp--functions nelisp--unbound)
               nelisp--unbound))
      (not (eq (gethash sym nelisp--macros nelisp--unbound)
               nelisp--unbound))))

(defun nelisp--builtin-symbol-value (sym)
  "Return SYM's NeLisp value — dynamic / global only, not lexical.
Matches Elisp `symbol-value' which never sees lexical bindings."
  (nelisp--lookup sym nil))

(defun nelisp--builtin-require (feature &optional _filename _noerror)
  "Phase 2 NeLisp `require' stub.
NeLisp does not yet maintain a module table; the dependents are
expected to have been loaded by the host before NeLisp evaluates
the source.  Return FEATURE unchanged so callers see the same
shape as Elisp's own `require'."
  feature)

(defun nelisp--builtin-provide (feature &optional _subfeatures)
  "Phase 2 NeLisp `provide' stub.
No module registry yet; just acknowledge the symbol so source files
that end with `(provide ...)' work as-is."
  feature)

(defun nelisp--install-primitives ()
  "Bind every primitive symbol in `nelisp--functions'.
Host Emacs functions cover pure data ops; higher-order primitives
and tables-specific queries go through NeLisp-aware wrappers so
closures, NeLisp-only defuns, and our own hash tables stay visible."
  (dolist (sym nelisp--primitive-symbols)
    (puthash sym (symbol-function sym) nelisp--functions))
  (puthash 'funcall      #'nelisp--builtin-funcall      nelisp--functions)
  (puthash 'apply        #'nelisp--builtin-apply        nelisp--functions)
  (puthash 'mapcar       #'nelisp--builtin-mapcar       nelisp--functions)
  (puthash 'mapc         #'nelisp--builtin-mapc         nelisp--functions)
  (puthash 'mapconcat    #'nelisp--builtin-mapconcat    nelisp--functions)
  (puthash 'maphash      #'nelisp--builtin-maphash      nelisp--functions)
  (puthash 'boundp       #'nelisp--builtin-boundp       nelisp--functions)
  (puthash 'fboundp      #'nelisp--builtin-fboundp      nelisp--functions)
  (puthash 'symbol-value #'nelisp--builtin-symbol-value nelisp--functions)
  (puthash 'require      #'nelisp--builtin-require      nelisp--functions)
  (puthash 'provide      #'nelisp--builtin-provide      nelisp--functions)
  ;; Doc 12 §3.4: re-register the NeLisp macroexpand family if
  ;; `nelisp-macro.el' is loaded.  fboundp guard keeps this safe during
  ;; the bootstrap of `nelisp-eval.el' itself.
  (when (fboundp 'nelisp-macro--install-primitives)
    (nelisp-macro--install-primitives))
  ;; Bytecode VM dispatch — registered conditionally so the
  ;; NeLisp-installed `nelisp--apply' can route bcls to the host VM
  ;; via NeLisp `fboundp' / `funcall' even after `nelisp--reset'.
  ;; Only present once nelisp-bytecode.el has been loaded.
  (when (fboundp 'nelisp-bc-run)
    (puthash 'nelisp-bc-run (symbol-function 'nelisp-bc-run)
             nelisp--functions)))

(defconst nelisp--core-macro-source
  "\
(defmacro defsubst (name params &rest body)
  (cons (quote defun) (cons name (cons params body))))

(defmacro declare-function (&rest _ignored)
  nil)

(defmacro defgroup (&rest _ignored)
  nil)

(defmacro defcustom (name default &rest _ignored)
  `(defvar ,name ,default))

(defmacro defface (&rest _ignored)
  nil)

(defmacro push (elt place)
  `(setq ,place (cons ,elt ,place)))

(defmacro pop (place)
  `(prog1 (car ,place) (setq ,place (cdr ,place))))

(defmacro dolist (spec &rest body)
  (let ((var (car spec))
        (seq-form (cadr spec))
        (result (nth 2 spec))
        (seq-sym (gensym \"dolist-seq-\")))
    `(let ((,seq-sym ,seq-form))
       (while ,seq-sym
         (let ((,var (car ,seq-sym)))
           ,@body)
         (setq ,seq-sym (cdr ,seq-sym)))
       ,result)))

(defmacro dotimes (spec &rest body)
  (let ((var (car spec))
        (count-form (cadr spec))
        (result (nth 2 spec))
        (limit-sym (gensym \"dotimes-n-\")))
    `(let ((,limit-sym ,count-form)
           (,var 0))
       (while (< ,var ,limit-sym)
         ,@body
         (setq ,var (+ ,var 1)))
       ,result)))
"
  "Core macros installed on top of the NeLisp primitive substrate.
Written as a string so the NeLisp reader parses the embedded
backquote / unquote / splice directly — we get to use the reader
features landed in Phase 2 Week 3-6 to bootstrap the macros that
Phase 1 deferred.  `nelisp--install-core-macros' evaluates every
top-level form here in order against the NeLisp global tables.")

(defun nelisp--install-core-macros ()
  "Parse `nelisp--core-macro-source' and evaluate every top-level form.
Must run after `nelisp--install-primitives' since some macro bodies
call `gensym' to avoid lexical capture.  The body uses a plain
`while' loop rather than `dolist' so the install bootstrap stays
self-sufficient — `dolist' is one of the macros being installed."
  (let ((forms (nelisp-read-all nelisp--core-macro-source)))
    (while forms
      (nelisp-eval (car forms))
      (setq forms (cdr forms)))))

;;; Public API ---------------------------------------------------------

;;;###autoload
(defun nelisp-eval (form)
  "Evaluate FORM (as returned by `nelisp-read') and return its value."
  (nelisp-eval-form form nil))

;;;###autoload
(defun nelisp-eval-string (str)
  "Read STR as one sexp and evaluate it."
  (nelisp-eval (nelisp-read str)))

(defun nelisp--reset ()
  "Clear all global NeLisp state and reinstall primitives + core macros.
Intended for test hygiene; callers should expect to re-run every
`defun' / `defvar' from scratch afterwards."
  (clrhash nelisp--functions)
  (clrhash nelisp--globals)
  (clrhash nelisp--specials)
  (clrhash nelisp--macros)
  (nelisp--install-primitives)
  (nelisp--install-core-macros)
  (when (fboundp 'nelisp-load--reset-registry)
    (nelisp-load--reset-registry)))

(nelisp--install-primitives)
;; Core macros (dolist / push / defsubst / …) reach into the evaluator
;; via the macro system, which lives in `nelisp-macro.el'.  That file
;; has not been loaded yet when this file is read, so
;; `nelisp--install-core-macros' is called from `src/nelisp.el' once
;; every module is in place — see the `(nelisp--install-core-macros)'
;; call there.  `nelisp--reset' already covers test hygiene because
;; tests only run after `nelisp.el' has completed loading.

(provide 'nelisp-eval)

;;; nelisp-eval.el ends here
