;;; nelisp-jit.el --- NeLisp JIT to host Elisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton

;; This file is not part of GNU Emacs.

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Phase 3b.8a ships the minimal JIT.  Bcl (Phase 3b's bytecode VM)
;; remains as the semantic reference — this path is the performance
;; path.  The translator walks a NeLisp `lambda' form and emits an
;; equivalent host Elisp form, which `byte-compile' lowers to a host
;; closure.  A JIT closure has shape `(nelisp-jit HOST-LAMBDA)' and is
;; dispatched by `nelisp--apply' like any other callable.
;;
;; Scope after Phase 3b.8b:
;;   - top-level defuns AND captured-env closures (let-wrapped lambdas,
;;     partial application).  The captured values are embedded as a
;;     literal vector in the compiled constants pool and read via
;;     `aref' — see `nelisp-jit-try-compile-lambda'.
;;   - captured-env closures that MUTATE their captured variables via
;;     `setq' are rejected (return nil) so bcl / interpreter handle
;;     the counter-pattern.  The let-wrapped read form re-binds on
;;     every call, so mutation wouldn't survive across invocations.
;;   - inline primitives are emitted as direct host calls (see
;;     `nelisp-jit--inline-primitives').  User-defined functions and
;;     unknown symbols are dispatched through `nelisp--apply' so
;;     NeLisp-level redefinitions stay observable.
;;   - a known set of special forms gets per-form handling; anything
;;     else signals `nelisp-jit-unsupported' and the caller falls back.
;;
;; The opt-in is `nelisp-jit-enabled'; when set, `nelisp--make-closure'
;; tries the JIT before bcl compilation.

;;; Code:

(require 'cl-lib)
(require 'nelisp-eval)

(declare-function nelisp-bc-run "nelisp-bytecode" (bcl &optional args))

(defvar nelisp-jit-enabled t
  "When non-nil, `nelisp--make-closure' tries JIT compilation first.
Failure on unsupported forms falls through to bcl / interpreter.

Default became t on 2026-08-16.  It was nil while the translation path
had never been run over the suite: the first full run with it on found
three semantic differences from the interpreter -- untranslated special
forms mistranslated as calls, and arity violations carrying the host's
error.  Those are fixed and both paths run in CI (`make test-jit' and
`make test-nojit'), which is what the default rests on.

Bind it nil around anything that asserts the interpreter's or bcl's own
representation; several tests do, and say why.")

(defvar nelisp-jit-untranslated-special-forms
  '(and or progn when unless
    catch throw unwind-protect condition-case
    defun defmacro defvar defconst defvar-local cl-defun)
  "NeLisp special forms the JIT does not translate.
These must DECLINE rather than reach the user-call arm: a special form
does not evaluate its arguments, so translating one as a call is not a
fallback but a mistranslation that runs.  Every name the interpreter
treats specially belongs here until the JIT grows a case for it -- the
list is the complement of the heads `nelisp-jit--translate-compound'
handles, taken from `nelisp-eval.el'.")

(define-error 'nelisp-jit-unsupported
  "NeLisp JIT cannot translate this form"
  'nelisp-eval-error)

;;;; Unverified code (Doc 170 section 10)

;; The JIT is the one place code arrives that no build step ever saw.
;; `nelisp-artifact-compile-file' checks every artifact, and `make
;; compile' checks every source, but a body reaching the JIT may have
;; come from `eval' or been assembled at runtime, so neither ran on it.
;;
;; Refusing to JIT such a body would not prevent the defect -- the
;; fallback interpreter runs the same code -- so refusal is not the
;; default.  What the default does is make the gap measurable: count
;; what goes by unchecked, so "nothing executes unverified" is a claim
;; with a number behind it rather than a hope.  A build that wants the
;; stricter answer sets the policy to `refuse'.

(defvar nelisp-jit-check-policy 'ignore
  "What the JIT does with a body carrying a `nl-check' finding.

`ignore'  do not check at all -- the default, and free.
`count'   check and tally, but compile anyway.
`refuse'  check and decline to JIT, falling through to bcl.

Checking costs a walk of every body the JIT sees, which is a hot path,
so it is off unless asked for.")

(defvar nelisp-jit-check-kinds
  '(must-use-discarded resource-untracked resource-leak resource-double)
  "Finding kinds the JIT counts.  Mirrors `nelisp-artifact-check-kinds'.")

(defvar nelisp-jit-check-seen 0
  "Bodies the JIT has examined since the last `nelisp-jit-check-reset'.")

(defvar nelisp-jit-check-flagged 0
  "Of those, how many carried a finding.")

(defun nelisp-jit-check-reset ()
  "Zero the unverified-code counters."
  (setq nelisp-jit-check-seen 0)
  (setq nelisp-jit-check-flagged 0))

(defun nelisp-jit-check-report ()
  "Return (:seen N :flagged N :policy SYM)."
  (list :seen nelisp-jit-check-seen
        :flagged nelisp-jit-check-flagged
        :policy nelisp-jit-check-policy))

(defun nelisp-jit-check-body (body)
  "Return non-nil when BODY carries a gated `nl-check' finding.
Returns nil under the `ignore' policy without looking, and nil when
`nl-check' is absent -- a soft require, as in the artifact path, so
this package gains no hard dependency on one Doc 170 section 10 says
nothing may depend on."
  (when (not (eq nelisp-jit-check-policy 'ignore))
    (require 'nl-check nil t)
    (when (fboundp 'nl-check-forms)
      (setq nelisp-jit-check-seen (1+ nelisp-jit-check-seen))
      (let ((flagged nil))
        (dolist (finding (nl-check-forms body))
          (when (memq (plist-get finding :kind) nelisp-jit-check-kinds)
            (setq flagged t)))
        (when flagged
          (setq nelisp-jit-check-flagged (1+ nelisp-jit-check-flagged)))
        flagged))))

(defconst nelisp-jit--inline-primitives
  '(+ - * / mod 1+ 1- < > = <= >=
    eq equal null not
    cons car cdr caar cadr cdar cddr
    list length nth nthcdr append reverse
    member memq assoc assq
    consp symbolp numberp integerp floatp stringp listp atom
    zerop)
  "Primitives that the JIT emits as direct host calls.
Redefining these at the NeLisp level will not affect JIT-compiled
bodies — they are inlined at compile time.  The set deliberately
excludes anything mutable (e.g. `message') to keep side effects
observable through `nelisp--apply'.")

(defvar nelisp-jit--primitive-set (make-hash-table :test 'eq)
  "O(1) membership check for `nelisp-jit--inline-primitives'.")

(defun nelisp-jit--refresh-primitive-set ()
  "Rebuild `nelisp-jit--primitive-set' from the defconst list."
  (clrhash nelisp-jit--primitive-set)
  (dolist (p nelisp-jit--inline-primitives)
    (puthash p t nelisp-jit--primitive-set)))
(nelisp-jit--refresh-primitive-set)

(defsubst nelisp-jit--primitive-p (sym)
  (gethash sym nelisp-jit--primitive-set))

(defsubst nelisp-jit--call-user (sym args)
  "Fast-path dispatch for user-defined calls from JIT-compiled bodies.
Skip `nelisp--apply's symbolp arm (cond + gethash + recursive call)
by looking up SYM directly and invoking `nelisp-bc-run' when the
cell is a bcl (which covers bcl-compiled defuns AND JIT closures —
`nelisp-bc-run' has its own JIT marker fast-path).  Anything else
defers to `nelisp--apply' so dispatch semantics stay identical for
interpreter closures / subrs / void bindings / etc.

`defsubst' so byte-compile inlines this into every JIT body and the
dispatch becomes one conditional + one tail-style call."
  (let ((fn (gethash sym nelisp--functions nelisp--unbound)))
    (if (and (consp fn) (eq (car fn) 'nelisp-bcl))
        (nelisp-bc-run fn args)
      (nelisp--apply sym args))))

(defun nelisp-jit--param-symbols (params)
  "Return lexical symbol names declared by PARAMS.
Filters out `&optional' and `&rest' markers so the result is a
plain list suitable for membership checks during translation."
  (let (out)
    (dolist (p params)
      (unless (memq p '(&optional &rest))
        (push p out)))
    (nreverse out)))

(defun nelisp-jit--translate-body (forms env)
  "Translate each of FORMS with ENV; return the list of translated forms."
  (mapcar (lambda (f) (nelisp-jit--translate f env)) forms))

(defun nelisp-jit--translate (form env)
  "Translate NeLisp FORM to host Elisp.
ENV is the list of lexical symbols in scope.  Unknown symbols are
emitted as NeLisp-globals lookups; unknown function heads are
dispatched through `nelisp--apply'.  Signals `nelisp-jit-unsupported'
for constructs the MVP does not handle."
  (cond
   ;; Self-evaluating.
   ((or (integerp form) (floatp form) (stringp form)
        (vectorp form) (booleanp form) (null form))
    form)
   ;; Symbol reference.
   ((symbolp form)
    (cond
     ((or (eq form t) (eq form nil) (keywordp form)) form)
     ((memq form env) form)
     (t
      ;; Global NeLisp binding.  `nelisp--unbound' sentinel discriminates
      ;; "no binding" from "bound to nil".
      `(let ((v (gethash ',form nelisp--globals nelisp--unbound)))
         (if (eq v nelisp--unbound)
             (signal 'nelisp-unbound-variable (list ',form))
           v)))))
   ;; Compound form.
   ((consp form)
    (nelisp-jit--translate-compound (car form) (cdr form) env))
   (t
    (signal 'nelisp-jit-unsupported (list form)))))

(defun nelisp-jit--translate-compound (head rest env)
  "Translate (HEAD . REST) given lexical ENV."
  (cond
   ;; ----- special forms -------------------------------------------------
   ((eq head 'quote) (cons 'quote rest))
   ((eq head 'function)
    ;; `(function X)'.  If X is a lambda, recursively translate its body
    ;; as a host lambda (still subject to JIT rules).  Bare symbols pass
    ;; through and caller can funcall them.
    (let ((arg (car rest)))
      (if (and (consp arg) (eq (car arg) 'lambda))
          (let ((sub-params (cadr arg))
                (sub-body (cddr arg)))
            `(lambda ,sub-params
               ,@(nelisp-jit--translate-body
                  sub-body
                  (append (nelisp-jit--param-symbols sub-params) env))))
        `(function ,arg))))
   ((eq head 'if)
    `(if ,(nelisp-jit--translate (car rest) env)
         ,(nelisp-jit--translate (cadr rest) env)
       ,@(nelisp-jit--translate-body (cddr rest) env)))
   ((eq head 'cond)
    `(cond ,@(mapcar (lambda (clause)
                       (nelisp-jit--translate-body clause env))
                     rest)))
   ((memq head '(when unless))
    `(,head ,(nelisp-jit--translate (car rest) env)
            ,@(nelisp-jit--translate-body (cdr rest) env)))
   ((memq head '(and or progn prog1 prog2))
    `(,head ,@(nelisp-jit--translate-body rest env)))
   ((eq head 'while)
    `(while ,(nelisp-jit--translate (car rest) env)
       ,@(nelisp-jit--translate-body (cdr rest) env)))
   ((eq head 'let)
    (let* ((bindings (car rest))
           (new-syms (mapcar (lambda (b) (if (consp b) (car b) b)) bindings))
           (translated
            (mapcar (lambda (b)
                      (cond ((symbolp b) b)
                            (t (list (car b)
                                     (nelisp-jit--translate (cadr b) env)))))
                    bindings))
           (new-env (append new-syms env)))
      `(let ,translated
         ,@(nelisp-jit--translate-body (cdr rest) new-env))))
   ((eq head 'let*)
    (let ((working env)
          (out nil))
      (dolist (b (car rest))
        (cond
         ((symbolp b) (push b out) (push b working))
         (t (push (list (car b) (nelisp-jit--translate (cadr b) working)) out)
            (push (car b) working))))
      `(let* ,(nreverse out)
         ,@(nelisp-jit--translate-body (cdr rest) working))))
   ((eq head 'setq)
    (let (forms (pairs rest))
      (while pairs
        (let ((var (car pairs))
              (val (nelisp-jit--translate (cadr pairs) env)))
          (push (if (memq var env)
                    `(setq ,var ,val)
                  `(puthash ',var ,val nelisp--globals))
                forms)
          (setq pairs (cddr pairs))))
      (if (cdr forms) `(progn ,@(nreverse forms)) (car forms))))
   ((eq head 'lambda)
    ;; Bare (lambda params body) used as a value (e.g. for mapcar arg).
    ;; Treat like (function (lambda ...)).
    (let ((sub-params (car rest))
          (sub-body (cdr rest)))
      `(lambda ,sub-params
         ,@(nelisp-jit--translate-body
            sub-body
            (append (nelisp-jit--param-symbols sub-params) env)))))
   ;; ----- special forms this MVP does not translate ---------------------
   ;; A special form does not evaluate its arguments, so routing one to
   ;; the user-call arm below is not a fallback -- it is a mistranslation
   ;; that runs.  `(condition-case err (/ a b) (arith-error :x))' became a
   ;; call with `err' evaluated as a variable, which signalled
   ;; `nelisp-unbound-variable' where the interpreter returns :x.  Decline
   ;; instead, and the caller falls through to bcl / interpreter, which
   ;; handle these correctly.
   ((and (symbolp head) (memq head nelisp-jit-untranslated-special-forms))
    (signal 'nelisp-jit-unsupported (list (cons head rest))))
   ;; ----- call dispatch -------------------------------------------------
   ((and (symbolp head) (nelisp-jit--primitive-p head))
    ;; Direct host call on an assumed-stable primitive.
    `(,head ,@(nelisp-jit--translate-body rest env)))
   ((symbolp head)
    ;; User-defined call: dispatch through `nelisp-jit--call-user' which
    ;; hits the bcl / JIT fast-path directly (gethash + `nelisp-bc-run')
    ;; and only falls back to the full `nelisp--apply' cond when the
    ;; symbol resolves to something other than a bcl (closure, subr,
    ;; nil, …).  Redefinitions stay observable because we look up on
    ;; every call — we never cache the function cell.  Phase 3b.8c.
    `(nelisp-jit--call-user ',head
                            (list ,@(nelisp-jit--translate-body rest env))))
   (t
    (signal 'nelisp-jit-unsupported (list (cons head rest))))))

(defun nelisp-jit--wrap-as-bcl (host-fn)
  "Package HOST-FN as a bcl-shaped closure so `nelisp-bc-run'
dispatches through its JIT fast-path instead of the VM loop.
Reusing the bcl tag means `nelisp--apply' needs no changes — a
JIT callable dispatches via the existing bcl cond arm, which
keeps the self-host `max-lisp-eval-depth' budget unchanged."
  ;; Shape: (nelisp-bcl MARKER nil nil HOST-FN nil nil).
  ;; `nelisp-bc-env' returns MARKER; `nelisp-bc-code' returns HOST-FN.
  (list 'nelisp-bcl 'nelisp-jit-marker nil nil host-fn nil nil))

(defun nelisp-jit--setq-on-env-p (form env-syms)
  "Return non-nil if FORM contains (setq SYM ...) for any SYM in ENV-SYMS.
Used as a pre-translation guard: captured-env closures that mutate
their captures cannot use the read-only `aref'-in-let embedding
because the binding is re-established on every call."
  (cond
   ((atom form) nil)
   ((and (consp form) (eq (car form) 'setq))
    (let ((pairs (cdr form)))
      (catch 'found
        (while pairs
          (when (memq (car pairs) env-syms)
            (throw 'found t))
          (setq pairs (cddr pairs)))
        nil)))
   ((consp form)
    (or (nelisp-jit--setq-on-env-p (car form) env-syms)
        (nelisp-jit--setq-on-env-p (cdr form) env-syms)))
   (t nil)))

(defun nelisp-jit--body-mutates-env-p (body env-syms)
  "Return non-nil if any form in BODY mutates an ENV-SYMS variable."
  (catch 'found
    (dolist (f body)
      (when (nelisp-jit--setq-on-env-p f env-syms)
        (throw 'found t)))
    nil))

(defun nelisp-jit-try-compile-lambda (env params body)
  "Attempt to JIT-compile a NeLisp lambda.
Returns a bcl-shaped JIT closure on success (see
`nelisp-jit--wrap-as-bcl'), or nil on unsupported forms /
captured-env closures that mutate their captures."
  (condition-case _err
      (let* ((lexical (nelisp-jit--param-symbols params))
             (env-syms (mapcar #'car env))
             (full-env (append env-syms lexical)))
        (cond
         ;; Captured-env closures that write to their captures cannot use
         ;; the `aref'-in-let embedding (the let re-initialises on every
         ;; call, so a `setq' would not persist).  Fall through to bcl /
         ;; interpreter which handle the counter pattern correctly.
         ((and env (nelisp-jit--body-mutates-env-p body env-syms)) nil)
         ;; Under the `refuse' policy a body carrying a finding falls
         ;; through to bcl / interpreter, the same way an untranslatable
         ;; form does.  Under `count' the tally is kept and we compile.
         ((and (eq nelisp-jit-check-policy 'refuse)
               (nelisp-jit-check-body body))
          nil)
         (t
          (unless (eq nelisp-jit-check-policy 'refuse)
            (nelisp-jit-check-body body))
          (let* ((translated (nelisp-jit--translate-body body full-env))
                 (host-form
                  (if env
                      (let* ((env-vec (vconcat (mapcar #'cdr env)))
                             (binds
                              (cl-loop for sym in env-syms
                                       for i from 0
                                       collect `(,sym (aref ,env-vec ,i)))))
                        `(lambda ,params
                           (let ,binds ,@translated)))
                    `(lambda ,params ,@translated)))
                 (host-fn (byte-compile host-form)))
            (nelisp-jit--wrap-as-bcl host-fn)))))
    (nelisp-jit-unsupported nil)))

(defun nelisp-jit--bc-try-advice (orig-fn env params body)
  "When `nelisp-jit-enabled', try JIT before bcl compilation.
On unsupported forms the JIT returns nil and we fall through to
the original bcl path (which itself may fall through to
interpreter closure via `nelisp--make-closure')."
  ;; Deliberately does NOT consult `nelisp-bc-auto-compile'.  That flag
  ;; is bcl's own -- its docstring scopes it to materialising bodies as
  ;; "bytecode closures" -- and it is checked inside
  ;; `nelisp-bc-try-compile-lambda', which this advice runs ahead of.
  ;; The JIT has its own switch.  Making the JIT honour the bcl flag was
  ;; tried and broke `nelisp-jit-compile-runs-fib', which sets
  ;; auto-compile nil precisely to isolate the JIT from bcl.
  (if nelisp-jit-enabled
      (or (nelisp-jit-try-compile-lambda env params body)
          (funcall orig-fn env params body))
    (funcall orig-fn env params body)))

(defun nelisp-jit-install ()
  "Wire the JIT into `nelisp-bc-try-compile-lambda' via advice.
Call this once per session after loading `nelisp-jit'.
Set `nelisp-jit-enabled' to toggle JIT on/off without uninstalling."
  (interactive)
  (advice-add 'nelisp-bc-try-compile-lambda
              :around #'nelisp-jit--bc-try-advice))

(defun nelisp-jit-uninstall ()
  "Remove the JIT advice from `nelisp-bc-try-compile-lambda'."
  (interactive)
  (advice-remove 'nelisp-bc-try-compile-lambda
                 #'nelisp-jit--bc-try-advice))

(defsubst nelisp-jit-bcl-p (obj)
  "Non-nil if OBJ is a JIT-wrapped bcl closure."
  (and (consp obj)
       (eq (car obj) 'nelisp-bcl)
       (eq (cadr obj) 'nelisp-jit-marker)))

;; Install on load.  `nelisp-jit-enabled' alone decides nothing until
;; the advice is in place, so leaving installation to an explicit call
;; would make the default a value nobody acts on.  The advice itself
;; reads the flag on every call, so installing here does not commit the
;; session to the JIT -- binding the flag nil still takes the bcl path.
;;
;; The JIT emits host Elisp for `byte-compile' and is wired in through
;; nadvice, so it only exists where nadvice does (host Emacs, where
;; nadvice is preloaded).  The NeLisp standalone has no nadvice, and
;; `nelisp-bytecode' soft-requires this file: calling `advice-member-p'
;; unconditionally there signalled `(void-function advice-member-p)'
;; from inside that `require' (its NOERROR covers only a missing file),
;; so `(require 'nelisp-bytecode)' -- and with it every standalone
;; `extend-runtime-image' run, whose embedded command source requires
;; the bytecode lane -- aborted before reading its arguments.  Without
;; nadvice the bcl path simply stays in charge, exactly as in a tree
;; without this package.
(when (fboundp 'advice-add)
  (unless (advice-member-p #'nelisp-jit--bc-try-advice
                           'nelisp-bc-try-compile-lambda)
    (nelisp-jit-install)))

(provide 'nelisp-jit)
;;; nelisp-jit.el ends here
