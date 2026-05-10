;;; nelisp-stdlib-env-shim.el --- Doc 86 §86.3.c env shim user API  -*- lexical-binding: t; -*-

;;; Commentary:

;; Doc 86 §86.3.c / Doc 89 §4.3 — Tier 0 env shim, switch-over stage.
;;
;; Layered build-up (file lifecycle):
;;
;;   §86.3.a — *skeleton*: define `nelisp--global-env' placeholder + 11
;;             thin wrappers (`nelisp--global-env-*') around the Rust
;;             slim primitives (`nelisp--env-globals-*' from
;;             `build-tool/src/eval/env_shim.rs').  Wrappers were
;;             dormant; existing callsites still went through the Rust
;;             `Env::*' methods unchanged.
;;
;;   §86.3.b — *shadow path*: cfg-gated `verify_elisp_mirror_*' helpers
;;             in `eval/env.rs' compared the shim view of `Env::globals'
;;             against the canonical `Env::*' result on every Tier 3
;;             callsite.  CI ran the gate green; no divergences were
;;             ever observed because the slim primitive and `Env::*'
;;             read the same `HashMap'.
;;
;;   §86.3.c — *switch-over* (THIS FILE'S CURRENT STAGE): the slim
;;             primitive IS now the canonical path for user-visible env
;;             ops (`symbol-value' / `symbol-function' / `boundp' /
;;             `fboundp' / `defalias' / `fset' / `set' / `fmakunbound' /
;;             `makunbound').  Each user-visible name is `fset' to an
;;             elisp wrapper that funcalls the corresponding slim
;;             primitive — so subsequent stdlib files (= stdlib.el /
;;             eval-special.el / etc.) calling `(fset 'X ...)' route
;;             through the elisp shim.
;;
;;             The Rust `Env::*' methods + dispatch arms in
;;             `builtins.rs' remain Tier 0 (= internal hot path) so
;;             `eval/mod.rs' / `eval/special_forms.rs' do NOT pay
;;             elisp round-trip overhead.  Only the user-visible names
;;             (= reachable from elisp source code) route through this
;;             shim.
;;
;; Bootstrap order constraint (Doc 89 §6.2.1): this file MUST load
;; AFTER `nelisp-jit-substrate.el' (= so `defun' / `defmacro' /
;; `cond' / `let' macros are live) and BEFORE
;; `nelisp-stdlib-eval-special.el' (= so the next stage of the
;; bootstrap can rely on the shim symbols already being interned).
;; The slim Rust primitives are installed by `Env::new_global' BEFORE
;; the STDLIB_IMAGES load loop, so any of the wrappers below can
;; `funcall' the underlying primitive name at load time.
;;
;; Out of scope: `defvar' / `setq' user-visible API rebinding.
;; `defvar' is provided as a Tier 2 macro by
;; `nelisp-stdlib-eval-special.el' which loads AFTER this file; `setq'
;; is a Tier 1 special form that stays Rust-side because its
;; lexical-frame-walk semantics are not slim-primitive-expressible.
;;
;; Tier 1 substrate constraint: this file loads BEFORE `stdlib.el' so
;; the convenience predicates `symbolp' / `consp' / `listp' / etc. are
;; NOT yet defined.  Bodies use `(eq (type-of X) 'symbol)' instead.

;;; Code:

;;;; --- 1. Mirror placeholder (deferred until hash-tables are live) ---
;;
;; In §86.3.b shadow path we left `nelisp--global-env' as a placeholder
;; nil because `make-hash-table' had not been bootstrapped yet at this
;; load order.  §86.3.c keeps the same deferral — there is no functional
;; need for the elisp-side mirror once the slim primitive is canonical
;; (= they read the same `Env::globals' HashMap).  The variable remains
;; for future Phase 3 D-B scope where the canonical may flip the other
;; way (= elisp owns the table, Rust mirrors); reserving the symbol now
;; avoids a churn breaking-change.

(setq nelisp--global-env nil)

;;;; --- 2. Slim primitive wrappers (1:1 with Rust env_shim.rs) -------
;;
;; The 11 `nelisp--global-env-*' wrappers carry forward from §86.3.a.
;; They remain `defun' (= not `defalias') so future hooks can adjust
;; the body without touching call sites that already use the wrapper
;; name.  The current bodies are direct forwarders.

(defun nelisp--global-env-lookup-value (sym)
  "Return the global value cell of SYM (Doc 86 §86.3 wrapper)."
  (nelisp--env-globals-get-value sym))

(defun nelisp--global-env-set-value (sym val)
  "Overwrite the global value cell of SYM with VAL."
  (nelisp--env-globals-set-value sym val))

(defun nelisp--global-env-lookup-function (sym)
  "Return the global function cell of SYM."
  (nelisp--env-globals-get-function sym))

(defun nelisp--global-env-set-function (sym def)
  "Overwrite the global function cell of SYM with DEF."
  (nelisp--env-globals-set-function sym def))

(defun nelisp--global-env-clear-value (sym)
  "Drop the global value cell of SYM."
  (nelisp--env-globals-clear-value sym))

(defun nelisp--global-env-clear-function (sym)
  "Drop the global function cell of SYM."
  (nelisp--env-globals-clear-function sym))

(defun nelisp--global-env-is-bound (sym)
  "Return t iff SYM has a global value cell."
  (nelisp--env-globals-is-bound sym))

(defun nelisp--global-env-is-fbound (sym)
  "Return t iff SYM has a global function cell."
  (nelisp--env-globals-is-fbound sym))

(defun nelisp--global-env-is-constant (sym)
  "Return t iff SYM is flagged as a constant."
  (nelisp--env-globals-is-constant sym))

(defun nelisp--global-env-set-constant (sym flag)
  "Set the constant flag of SYM to FLAG."
  (nelisp--env-globals-set-constant sym flag))

(defun nelisp--global-env-capture-lexical ()
  "Capture the current lexical frames as alist."
  (nelisp--env-globals-capture-lexical))

;;;; --- 3. §86.3.c — let-binding parser delegate --------------------
;;
;; Doc 89 §5.2 lists `parse_let_binding' (special_forms.rs) as a -30 LOC
;; reduction candidate.  The elisp implementation below mirrors the
;; Rust `parse_let_binding' (= `eval/special_forms.rs:204-234') so the
;; Rust special form `sf_let' / `sf_let_star' can stay short while the
;; parsing semantics are visible / overrideable from elisp.
;;
;; NOT yet wired from Rust at switch-over — the Rust sf_let / sf_let_star
;; remain hot-path Tier 1 special forms that retain the inline
;; `parse_let_binding' helper for performance (Doc 89 §10.1).  This
;; shim stands ready for Phase 3 D-B which will lift sf_let into elisp.

(defun nelisp--parse-let-binding (binding)
  "Parse a `let' / `let*' BINDING form into (NAME . VALUE-FORM).
BINDING is one of:
  - SYM                       => (SYM . nil)
  - (SYM)                     => (SYM . nil)
  - (SYM VALUE-FORM)          => (SYM . VALUE-FORM)
Mirrors Rust `eval/special_forms.rs::parse_let_binding'.  The Rust
caller evaluates the returned VALUE-FORM in the appropriate scope (=
outer for `let', incremental for `let*')."
  (cond
   ((eq (type-of binding) 'symbol)
    (cons binding nil))
   ((eq (type-of binding) 'cons)
    (let ((sym (car binding))
          (rest (cdr binding)))
      (when (not (eq (type-of sym) 'symbol))
        (signal 'wrong-type-argument (cons 'symbolp (cons sym nil))))
      (cond
       ((eq rest nil) (cons sym nil))
       ((eq (type-of rest) 'cons) (cons sym (car rest)))
       (t (signal 'wrong-type-argument (cons 'listp (cons rest nil)))))))
   (t
    (signal 'wrong-type-argument
            (cons 'symbol-or-cons (cons binding nil))))))

;;;; --- 4. §86.3.c — user-visible env builtins overrides ------------
;;
;; Per Doc 89 §8.4: install elisp wrappers for the 9 user-visible
;; Tier 3 names so user code routes through the shim, while internal
;; Rust callsites still go through `Env::*' direct.  Each `fset'
;; install replaces the dispatch arm's `(builtin NAME)' sentinel with
;; an elisp `(closure ...)' that delegates back to the slim primitive.
;;
;; Implementation note: each wrapper is a thin redispatch — it cannot
;; literally call `(symbol-value SYM)' here because that name now
;; resolves to the wrapper itself (= infinite recursion).  Instead it
;; routes through the slim primitive name (`nelisp--env-globals-*')
;; which always invokes the Rust body via the extern-builtin registry.
;;
;; Type-check policy (parity with Rust dispatch arms):
;;   `symbol-value' / `symbol-function' / `defalias' / `fset' / `set'
;;       / `fmakunbound' / `makunbound' — error on non-symbol arg
;;   `boundp' / `fboundp' — return nil on non-symbol arg (= Emacs
;;       behaviour, returns nil rather than signalling)
;;
;; The slim primitive `bi_set_value' / `bi_get_value' / etc. itself
;; signals `wrong-type-argument' on non-symbol args (= via
;; `env_shim.rs::sym_arg'), so the wrappers below can mostly forward
;; directly; only `boundp' / `fboundp' need an explicit symbol guard
;; to suppress the signal and return nil instead.

(defun nelisp--shim-symbol-value (sym)
  "Doc 86 §86.3.c shim — `symbol-value' user-visible API.
Delegates to the Rust slim primitive `nelisp--env-globals-get-value'."
  (nelisp--env-globals-get-value sym))

(defun nelisp--shim-symbol-function (sym)
  "Doc 86 §86.3.c shim — `symbol-function' user-visible API."
  (nelisp--env-globals-get-function sym))

(defun nelisp--shim-fboundp (sym)
  "Doc 86 §86.3.c shim — `fboundp' user-visible API.
Returns nil on non-symbol arg (= host-Emacs parity)."
  (if (eq (type-of sym) 'symbol)
      (nelisp--env-globals-is-fbound sym)
    nil))

(defun nelisp--shim-boundp (sym)
  "Doc 86 §86.3.c shim — `boundp' user-visible API.
Returns nil on non-symbol arg (= host-Emacs parity)."
  (if (eq (type-of sym) 'symbol)
      (nelisp--env-globals-is-bound sym)
    nil))

(defun nelisp--shim-defalias (sym def &optional _docstring)
  "Doc 86 §86.3.c shim — `defalias' user-visible API.
The optional DOCSTRING is accepted for API parity with host Emacs but
discarded (= no doc-cell yet).  When DEF is a symbol, follows the
function-cell chain so the alias resolves to a callable form."
  (let ((resolved (if (eq (type-of def) 'symbol)
                      (nelisp--env-globals-get-function def)
                    def)))
    (nelisp--env-globals-set-function sym resolved)
    sym))

(defun nelisp--shim-fset (sym def)
  "Doc 86 §86.3.c shim — `fset' user-visible API.
Returns DEF (= host-Emacs contract).  Follows the function-cell chain
when DEF is a symbol."
  (let ((resolved (if (eq (type-of def) 'symbol)
                      (nelisp--env-globals-get-function def)
                    def)))
    (nelisp--env-globals-set-function sym resolved)
    resolved))

(defun nelisp--shim-set (sym val)
  "Doc 86 §86.3.c shim — `set' user-visible API.
Returns VAL.  The slim primitive bypasses the constant-rejection path;
constant-flag enforcement happens in the Rust `Env::set_value' which
this shim's caller does NOT pass through (= shim is the canonical
user path).  We re-add the check here so user code observing
`(set 'nil 1)' still gets the right signal."
  (when (nelisp--env-globals-is-constant sym)
    (signal 'setting-constant (cons sym nil)))
  (nelisp--env-globals-set-value sym val)
  val)

(defun nelisp--shim-fmakunbound (sym)
  "Doc 86 §86.3.c shim — `fmakunbound' user-visible API."
  (nelisp--env-globals-clear-function sym)
  sym)

(defun nelisp--shim-makunbound (sym)
  "Doc 86 §86.3.c shim — `makunbound' user-visible API."
  (nelisp--env-globals-clear-value sym)
  sym)

;;;; --- 5. §86.3.c — `fset' install of the 9 user-visible names ----
;;
;; Doc 89 §8.4: replace the Rust dispatch arms' `(builtin NAME)'
;; sentinel with the elisp wrapper, so the next time elisp source
;; references `(boundp X)' / `(symbol-value X)' / etc. the call goes
;; through the shim above.
;;
;; CRITICAL: this `fset' loop happens DURING this file's load, which
;; means the wrappers' bodies must NOT themselves reference
;; user-visible names that have already been overridden.  All bodies
;; route through `nelisp--env-globals-*' (= slim primitive direct), so
;; the load is self-contained.
;;
;; We use the slim primitive `nelisp--env-globals-set-function' rather
;; than `(fset ...)' because at this load point `fset' itself is one
;; of the names we are about to override — using the slim primitive is
;; the bootstrap-safe way regardless of `fset' override order.  Note
;; that `(symbol-function 'nelisp--shim-X)' below also routes through
;; the Rust dispatch arm `bi_symbol_function' until the corresponding
;; install line below replaces it; that ordering is fine because the
;; reads happen BEFORE the install of `symbol-function' on this file.

(nelisp--env-globals-set-function 'symbol-value (symbol-function 'nelisp--shim-symbol-value))
(nelisp--env-globals-set-function 'symbol-function (symbol-function 'nelisp--shim-symbol-function))
(nelisp--env-globals-set-function 'fboundp (symbol-function 'nelisp--shim-fboundp))
(nelisp--env-globals-set-function 'boundp (symbol-function 'nelisp--shim-boundp))
(nelisp--env-globals-set-function 'defalias (symbol-function 'nelisp--shim-defalias))
(nelisp--env-globals-set-function 'fset (symbol-function 'nelisp--shim-fset))
(nelisp--env-globals-set-function 'set (symbol-function 'nelisp--shim-set))
(nelisp--env-globals-set-function 'fmakunbound (symbol-function 'nelisp--shim-fmakunbound))
(nelisp--env-globals-set-function 'makunbound (symbol-function 'nelisp--shim-makunbound))

;;;; --- 6. Provide ---------------------------------------------------
;;
;; `provide' is installed by `nelisp-stdlib.el' which loads AFTER us.
;; Skipping here matches the convention in `nelisp-jit-substrate.el'
;; / `nelisp-stdlib-error.el' (= top of the bootstrap chain).

;;; nelisp-stdlib-env-shim.el ends here
