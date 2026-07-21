;;; emacs-parity-cc.el --- cc-mode c-lang-defconst hard-abort parity fix -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fix (b1k21 frontier).  In the standalone NeLisp
;; self-host runtime, loading CC Mode (`cc-langs.el' via `cc-mode') aborted
;; every one of its ~225 `c-lang-defconst' forms with `nelisp-bare-abort',
;; and the missing language constants then produced ~20 downstream
;; "might be a cyclic reference" errors from `c-matchers-1/2/3'.  Runtime
;; marker-localization probing (bootstrap replay + `nelisp--eval-source-string'
;; on isolated forms) pinned TWO independent native-evaluator defects, each of
;; which is hit by EVERY `c-lang-defconst':
;;
;; ROOT 1 -- `byte-compile' returns a non-callable symbol.
;;   The standalone `byte-compile' primitive (not a subr; installed in the
;;   runtime image) returns a bare SYMBOL, not a function object; funcalling
;;   that symbol hard-aborts.  cc-defs.el:2449 does
;;
;;     (defalias 'cc-bytecomp-compiling-or-loading
;;       (cc-eval-when-compile
;;         (let ((f (symbol-function 'cc-bytecomp-compiling-or-loading)))
;;           (if (byte-code-function-p f) f (byte-compile f)))))
;;
;;   so at load time it installs `(byte-compile f)' -- a non-callable symbol --
;;   as `cc-bytecomp-compiling-or-loading'.  `c-get-current-file' (cc-defs.el
;;   :2451, a defsubst) calls that function; and BOTH the `c-lang-defconst'
;;   macro expander (cc-defs.el:2532) and `c-define-lang-constant'
;;   (cc-defs.el:2612) call `c-get-current-file'.  So the very first thing
;;   every `c-lang-defconst' does is call a function whose body hard-aborts.
;;   Isolation proof: `(funcall (byte-compile (lambda () 42)))' hard-aborts and
;;   `(type-of (byte-compile (lambda () 42)))' => `symbol'.
;;
;;   FIX: since NeLisp executes interpreted, the correct byte-compilation of a
;;   function IS the function itself.  Override `byte-compile' to return a
;;   callable object (the function unchanged, or a symbol's function cell)
;;   instead of the broken native symbol.  This must be in effect before
;;   cc-defs.el:2449 runs, so it is installed at shim load; it is global and
;;   order-independent (cc-defs never redefines `byte-compile').
;;
;; ROOT 2 -- `macroexpand'/`macroexpand-1'/`macroexpand-all' hard-abort when
;;   expanding ANY macro under the `nelisp--eval-source-string' evaluator.
;;   The `c-lang-defconst' macro runs `(c--macroexpand-all val)' (=
;;   `(macroexpand-all val)') on every VAL.  Under the self-host source
;;   evaluator that CC Mode is loaded with, invoking a `(macro . closure)'
;;   expander (via `macroexpand-1' internally, or even a manual
;;   `(apply (cdr (symbol-function 'when)) ...)') hard-aborts -- `when',
;;   `backquote', `c-lang-const', all of them.  (Normal evaluation of the same
;;   backquote works fine: `\`(a ,b)' evaluates correctly; it is only the
;;   macro-EXPANSION functions under source-eval that abort.)  A trivial VAL
;;   such as `42' has no macro to expand, so it survives ROOT 1's fix; but any
;;   VAL containing backquote or `c-lang-const' -- i.e. almost all of
;;   cc-langs.el -- aborts.
;;
;;   FIX: neutralize CC Mode's `c--macroexpand-all' to an identity macro that
;;   returns VAL unexpanded.  This is behaviour-preserving for an interpreter:
;;   the unexpanded backquote / `c-lang-const' / `c-lang-defconst-eval-immediately'
;;   forms are stored inside the `(lambda () VAL)' binding and expand LAZILY,
;;   through normal evaluation, when the constant is first requested via
;;   `c-get-lang-constant' (which itself records the inter-constant
;;   dependencies at request time).  Validated: `c-symbol-start' for `c-mode'
;;   resolves to "[a-zA-Z_$]" and `c-keywords'/`c-operators' register with the
;;   fix in place, with zero `c-lang-defconst' aborts and zero cyclic-reference
;;   errors on `(require 'cc-langs)'.
;;
;;   Because cc-defs.el itself defines `c--macroexpand-all' (cc-defs.el:214),
;;   loading it AFTER this shim would clobber the override.  This runtime's
;;   `with-eval-after-load' does not fire on `provide' (verified by probe), so
;;   the neutralization is (re)installed by wrapping `require': cc-mode.el /
;;   cc-langs.el call `(require 'cc-defs)' (via `cc-require') before their own
;;   `c-lang-defconst' forms run, and the wrapper runs the install right after
;;   `cc-defs' is required -- and also immediately if cc-defs is already loaded.
;;
;; This shim is a no-op outside the standalone NeLisp runtime: the helper
;; definitions are inert and nothing is activated, so host Emacs keeps its real
;; `byte-compile', `require', and CC Mode macro-expansion behaviour.

;;; Code:

(defconst emacs-parity-cc--standalone-p
  (fboundp 'nelisp--eval-source-string)
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards activation so host Emacs is left untouched (it keeps the real
`byte-compile', `require', and CC Mode macro-expansion behaviour).")

(defvar emacs-parity-cc--orig-require nil
  "The `require' implementation in effect before this shim wrapped it.")

(defvar emacs-parity-cc--orig-c-get-lang-constant nil
  "Original `c-get-lang-constant' function before standalone memoization.")

(defvar emacs-parity-cc--orig-c-define-lang-constant nil
  "Original `c-define-lang-constant' function before cache invalidation wrap.")

(defvar emacs-parity-cc--lang-constant-cache nil
  "Hash table caching standalone `c-get-lang-constant' results.")

;; --- ROOT 1: interpreter-safe `byte-compile' -----------------------------
;; The standalone `byte-compile' returns a non-callable symbol; funcalling it
;; hard-aborts.  For an interpreter, the byte-compiled form of a function is
;; the function itself, so return a callable object instead.  Defined
;; unconditionally (inert unless installed) so the byte-compiler knows it.
(defun emacs-parity-cc--byte-compile (form &rest _ignored)
  "Interpreter-safe replacement for `byte-compile'.
Return a callable object for FORM: a function is returned unchanged, and a
bound symbol yields its function cell.  Never returns the non-callable symbol
that the standalone `byte-compile' primitive produces."
  (cond
   ((and (symbolp form) (fboundp form)) (symbol-function form))
   (t form)))

(defun emacs-parity-cc--backquote-head-p (head)
  "Return non-nil when HEAD denotes a backquote form head.
The standalone reader emits `(backquote FORM)'; some host readers retain the
raw backquote symbol instead."
  (or (eq head 'backquote)
      (eq head '\`)))

(defun emacs-parity-cc--unquote-head-p (head)
  "Return non-nil when HEAD denotes an unquote form head."
  (or (eq head 'comma)
      (eq head '\,)))

(defun emacs-parity-cc--unquote-splice-head-p (head)
  "Return non-nil when HEAD denotes an unquote-splicing form head."
  (or (eq head 'comma-at)
      (eq head '\,@)))

(defun emacs-parity-cc--qq-active-p (qq-depth)
  "Return non-nil when QQ-DEPTH is inside an active quasiquote."
  (and (integerp qq-depth)
       (> qq-depth 0)))

(defun emacs-parity-cc--rewrite-c-lang-const (form &optional qq-depth)
  "Rewrite runtime `c-lang-const' calls inside FORM.
Only `(c-lang-const NAME . ARGS)' subforms are rewritten to
`(c-get-lang-constant 'NAME . ARGS)'.  Other macro forms are left untouched so
no general macro expansion is attempted."
  (cond
   ((consp form)
    (let ((head (car form)))
      (cond
       ((eq head 'quote)
        form)
       ((eq head 'function)
        form)
       ((emacs-parity-cc--backquote-head-p head)
        (if (emacs-parity-cc--qq-active-p qq-depth)
            (cons head
                  (emacs-parity-cc--rewrite-c-lang-const-list
                   (cdr form)
                   (1+ qq-depth)))
          (cons head
                (emacs-parity-cc--rewrite-c-lang-const-list
                 (cdr form)
                 1))))
       ((and (emacs-parity-cc--qq-active-p qq-depth)
             (emacs-parity-cc--unquote-head-p head))
        (cons head
              (emacs-parity-cc--rewrite-c-lang-const-list
               (cdr form)
               (max 0 (1- qq-depth)))))
       ((and (emacs-parity-cc--qq-active-p qq-depth)
             (emacs-parity-cc--unquote-splice-head-p head))
        (cons head
              (emacs-parity-cc--rewrite-c-lang-const-list
               (cdr form)
               (max 0 (1- qq-depth)))))
       ((emacs-parity-cc--qq-active-p qq-depth)
        (cons (emacs-parity-cc--rewrite-c-lang-const head qq-depth)
              (emacs-parity-cc--rewrite-c-lang-const-list (cdr form) qq-depth)))
       ((eq head 'c-lang-const)
        (let ((name (car-safe (cdr form)))
              (args (cdr (cdr form))))
          (cons 'c-get-lang-constant
                (cons (list 'quote name)
                      (emacs-parity-cc--rewrite-c-lang-const-list args)))))
       (t
        (cons (emacs-parity-cc--rewrite-c-lang-const head qq-depth)
              (emacs-parity-cc--rewrite-c-lang-const-list (cdr form) qq-depth))))))
   ((vectorp form)
    (apply #'vector
           (emacs-parity-cc--rewrite-c-lang-const-list (append form nil) qq-depth)))
   (t form)))

(defun emacs-parity-cc--rewrite-c-lang-const-list (forms &optional qq-depth)
  "Map `emacs-parity-cc--rewrite-c-lang-const' over FORMS.
Preserve dotted tails."
  (cond
   ((consp forms)
    (cons (emacs-parity-cc--rewrite-c-lang-const (car forms) qq-depth)
          (emacs-parity-cc--rewrite-c-lang-const-list (cdr forms) qq-depth)))
   ((null forms)
    nil)
   (t
    (emacs-parity-cc--rewrite-c-lang-const forms qq-depth))))

(defun emacs-parity-cc--record-lang-constant-dependency (name)
  "Mirror `c-get-lang-constant' dependency recording for cached NAME."
  (let ((eval-in-sym (and (boundp 'c-lang-constants-under-evaluation)
                          c-lang-constants-under-evaluation
                          (caar c-lang-constants-under-evaluation))))
    (when eval-in-sym
      (let ((sym (intern (symbol-name name) c-lang-constants)))
        (unless (memq eval-in-sym (get sym 'dependents))
          (put sym 'dependents (cons eval-in-sym (get sym 'dependents))))))))

(defun emacs-parity-cc--cached-c-get-lang-constant (name &optional source-files mode)
  "Standalone memoizing wrapper for `c-get-lang-constant'.
Cache successful resolutions by `(NAME . MODE)' while preserving dependency
recording for callers that resolve constants during another constant's
evaluation."
  (let* ((resolved-mode (or mode c-buffer-is-cc-mode))
         (cache-key (cons name resolved-mode))
         (cached (and resolved-mode
                      emacs-parity-cc--lang-constant-cache
                      (gethash cache-key emacs-parity-cc--lang-constant-cache))))
    (if cached
        (progn
          (emacs-parity-cc--record-lang-constant-dependency name)
          (cdr cached))
      (let ((value (funcall emacs-parity-cc--orig-c-get-lang-constant
                            name source-files mode)))
        (when (and resolved-mode emacs-parity-cc--lang-constant-cache)
          (puthash cache-key (cons 'ok value) emacs-parity-cc--lang-constant-cache))
        value))))

(defun emacs-parity-cc--cached-c-define-lang-constant (name source-file newentries)
  "Wrapper for `c-define-lang-constant' that invalidates memoized values."
  (when emacs-parity-cc--lang-constant-cache
    (clrhash emacs-parity-cc--lang-constant-cache))
  (funcall emacs-parity-cc--orig-c-define-lang-constant
           name source-file newentries))

(defun emacs-parity-cc--install ()
  "Install the CC-Mode-specific parity overrides.
Idempotent; safe to call more than once (immediately if cc-defs is already
loaded, and again from the `require' wrapper when cc-defs loads later)."
  ;; ROOT 1 residual: if cc-defs already installed the broken byte-compiled
  ;; `cc-bytecomp-compiling-or-loading', replace it with a plain interpreted
  ;; version.  The full upstream version walks the Lisp stack with
  ;; `backtrace-frame' only when load AND compile are both active; in the
  ;; standalone runtime `byte-compile-dest-file' is never a live compile
  ;; target, so returning 'loading / nil is exactly correct here.
  (defalias 'cc-bytecomp-compiling-or-loading
    (lambda ()
      (cond
       ((and (boundp 'load-in-progress) load-in-progress) 'loading)
       ((and (boundp 'byte-compile-dest-file)
             (stringp byte-compile-dest-file))
        'compiling)
       (t nil))))
  ;; ROOT 2: replace `c--macroexpand-all' with a safe partial expander that
  ;; rewrites only `c-lang-const' calls to the equivalent runtime
  ;; `c-get-lang-constant' calls.  This gives `c-lang-defconst' the one-time
  ;; registration rewrite that `c-get-lang-constant' relies on without ever
  ;; invoking the memory-unsafe general macroexpander.
  ;;
  ;; NOTE (2026-07-20): tried restoring the real upstream `macroexpand-all'
  ;; definition here after fixing the `(macro CLOSURE)' macroexpand abort at the
  ;; source.  A faithful `(require 'cc-mode)' reproduction showed it did NOT
  ;; reintroduce the ~225 `c-lang-defconst' aborts (so the macroexpand fix is
  ;; sound), but it also did NOT change the residual 15 `rx-syntax' + 3
  ;; `java-font-lock-keywords' bare-aborts: those come from `c-get-lang-constant'
  ;; evaluating the complex `c-matchers-*' constants at request time, a separate
  ;; root unrelated to the macroexpand deferral.  Kept the identity override.
  ;; THROUGHPUT NOTE (2026-07-21): this identity deferral has a large hidden
  ;; cost -- profiling the full init showed the raw `(c-lang-const ...)'/backquote
  ;; VAL is re-walked by normal eval at every `c-get-lang-constant' resolution,
  ;; uncached, at ~5s each x ~160 resolutions = ~819s = the single dominant
  ;; full-init crawl block (~35%).  Restoring CC Mode's REAL `c--macroexpand-all'
  ;; (= `macroexpand-all') to expand+cache once at registration was TESTED via
  ;; `NELISP_CC_REAL_MACROEXPAND' and DETERMINISTICALLY SEGFAULTS this binary
  ;; (exit 139) before user-init even starts -- `macroexpand-all' under the
  ;; source evaluator is not merely a catchable abort here, it is memory-unsafe.
  ;; So the real one is NOT viable.  The safe fix here is a hand-written walk
  ;; that rewrites only `(c-lang-const X ...)' to
  ;; `(c-get-lang-constant 'X ...)' and leaves all other forms untouched.
  (defmacro c--macroexpand-all (form &optional _environment)
    "Rewrite only `c-lang-const' calls inside FORM.
This intentionally avoids all general macro expansion."
    (emacs-parity-cc--rewrite-c-lang-const form))
  ;; Some NeLisp images resolve macros through a side table rather than the
  ;; function cell; mirror the override there when that table exists.
  (when (and (boundp 'nelisp--macros) (hash-table-p nelisp--macros))
    (puthash 'c--macroexpand-all
             (symbol-function 'c--macroexpand-all)
             nelisp--macros))
  ;; Complementary throughput fix: successful `c-get-lang-constant'
  ;; resolutions are deterministic for a given `(name . mode)' until new
  ;; language definitions are registered, so memoize them and invalidate on
  ;; each `c-define-lang-constant'.
  (unless emacs-parity-cc--lang-constant-cache
    (setq emacs-parity-cc--lang-constant-cache (make-hash-table :test 'equal)))
  (unless (eq (symbol-function 'c-get-lang-constant)
              #'emacs-parity-cc--cached-c-get-lang-constant)
    (setq emacs-parity-cc--orig-c-get-lang-constant
          (symbol-function 'c-get-lang-constant))
    (defalias 'c-get-lang-constant #'emacs-parity-cc--cached-c-get-lang-constant))
  (unless (eq (symbol-function 'c-define-lang-constant)
              #'emacs-parity-cc--cached-c-define-lang-constant)
    (setq emacs-parity-cc--orig-c-define-lang-constant
          (symbol-function 'c-define-lang-constant))
    (defalias 'c-define-lang-constant
      #'emacs-parity-cc--cached-c-define-lang-constant)))

(defun emacs-parity-cc--require (feature &rest args)
  "Wrapper around `require' that installs CC parity fixes after `cc-defs'.
See `emacs-parity-cc' commentary for the two roots addressed."
  (prog1 (apply emacs-parity-cc--orig-require feature args)
    (when (eq feature 'cc-defs)
      (emacs-parity-cc--install))))

;; --- Activation: standalone runtime only ---------------------------------
(when emacs-parity-cc--standalone-p

  ;; ROOT 1: install before cc-defs.el:2449 runs.
  (defalias 'byte-compile #'emacs-parity-cc--byte-compile)

  ;; Install now if cc-defs is already loaded (shim-after-cc ordering).
  (when (featurep 'cc-defs)
    (emacs-parity-cc--install))

  ;; Otherwise cc-defs loads later, mid-`require' chain: cc-mode.el /
  ;; cc-langs.el both do `(require 'cc-defs)' (via `cc-require') BEFORE their
  ;; own `c-lang-defconst' forms run.  Wrap `require' so the install lands the
  ;; moment `cc-defs' has been required.  Guarded to wrap once so a reload
  ;; cannot capture the wrapper as its own original (infinite recursion).
  (unless (get 'emacs-parity-cc--require 'installed)
    (setq emacs-parity-cc--orig-require (symbol-function 'require))
    (defalias 'require #'emacs-parity-cc--require)
    (put 'emacs-parity-cc--require 'installed t))

  ;; Secondary trigger; harmless if `with-eval-after-load' is inert here.
  (when (fboundp 'with-eval-after-load)
    (with-eval-after-load 'cc-defs (emacs-parity-cc--install))))

(provide 'emacs-parity-cc)

;;; emacs-parity-cc.el ends here
