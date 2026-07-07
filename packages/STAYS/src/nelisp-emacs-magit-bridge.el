;;; nelisp-emacs-magit-bridge.el --- load the real vendor Magit chain into a live session  -*- lexical-binding: t; -*-

;;; Commentary:

;; Task #17 (M1).  Session-side loader that brings the unpatched vendor
;; Magit/transient/with-editor chain into a live NeLisp session (a
;; persistent `nelisp --repl' session, or a runtime-image bake via
;; `extend-runtime-image').  This is a bridge, not a reimplementation: it
;; never patches or reimplements Magit, transient, or with-editor source.
;;
;; Mechanism: `scripts/build-nelisp-emacs-magit-bridge-bundle.el' (run under
;; host Emacs, mirroring `scripts/build-nelisp-bootstrap.el') normalizes the
;; real vendor source (same `standalone-source-normalize' path already
;; proven by `scripts/vendor-repl-standalone-replay.el' in
;; docs/design/33-emacs-core-substrate-priority-plan.org) into one
;; plain-Elisp bundle, `build/nelisp-emacs-magit-bridge-bundle.el', with each
;; file's forms wrapped in `(unless (featurep FEATURE) ...)'.  This module
;; only has to `load' that pre-normalized bundle from inside NeLisp itself;
;; normalization never runs inside the standalone reader.
;;
;; Ownership: this file owns "bring the Magit vendor closure into a NeLisp
;; session" as a reusable adapter step, per the library-first CLAUDE.md/
;; AGENTS.md rule that command/session semantics stay out of app glue.
;; `apps/nemacs-next' and `scripts/nemacs-runtime-image-preload.el' call into
;; this module; they do not duplicate its logic.

;;; Code:

(defvar nelisp-emacs-magit-bridge-repo-root nil
  "Repository root used to resolve the generated bundle path.
When nil, `nelisp-emacs-magit-bridge-load' derives it from
`load-file-name'/`buffer-file-name' (this file lives at REPO/src/).")

(defvar nelisp-emacs-magit-bridge-bundle-file nil
  "Explicit path to the generated magit bridge bundle.
When nil, resolved as build/nelisp-emacs-magit-bridge-bundle.el under
`nelisp-emacs-magit-bridge-repo-root'.")

(defvar nelisp-emacs-magit-bridge-loaded nil
  "Non-nil once `nelisp-emacs-magit-bridge-load' has run in this session.")

(defun nelisp-emacs-magit-bridge--repo-root ()
  "Return the resolved repository root."
  (or nelisp-emacs-magit-bridge-repo-root
      (expand-file-name ".." (file-name-directory
                              (or load-file-name buffer-file-name "")))))

(defun nelisp-emacs-magit-bridge--bundle-file ()
  "Return the resolved bundle file path."
  (or nelisp-emacs-magit-bridge-bundle-file
      (expand-file-name "build/nelisp-emacs-magit-bridge-bundle.el"
                        (nelisp-emacs-magit-bridge--repo-root))))

(defun nelisp-emacs-magit-bridge--ensure-process-substrate ()
  "Ensure the sync process substrate is loaded before Magit needs git.

Magit's git plumbing (`magit-git-string', `magit-process-file', ...)
goes through `call-process'/`process-file'.  The interactive/batch nemacs
bootstrap already provides these; this is a defensive no-op there and a
real `require' when the bridge runs against a bare NeLisp session that
has not loaded the bootstrap (e.g. a from-scratch `nelisp --repl' spike)."
  (unless (featurep 'emacs-process)
    (require 'emacs-process))
  (unless (featurep 'emacs-process-builtins)
    (require 'emacs-process-builtins)))

(defun nelisp-emacs-magit-bridge--ensure-emacs-version-identity ()
  "Ensure `emacs-version'/-major-/-minor-version' hold real version strings.

The nemacs batch/interactive bootstrap leaves `emacs-version' as an
unbound-marker placeholder rather than a real dotted version string
(a substrate gap, not a Magit-specific one).  Compat's own
`compat-require' macro evaluates `(version< emacs-version VERSION)' at
*macro-expansion time* while loading compat-31/compat-30 layers; with
the placeholder value that comparison spuriously succeeds and the file
tries to require a compat layer this repo's vendor snapshot never
needed to carry (real host Emacs 30.1 never loads it either, since the
same guard is false there).  Declaring a real, current identity here
keeps that guard's real-Emacs behavior; this never overwrites a
already-real value, so it becomes a no-op once the nemacs substrate
sets one itself."
  (unless (stringp emacs-version)
    (setq emacs-version "30.1"))
  (unless (integerp emacs-major-version)
    (setq emacs-major-version 30))
  (unless (integerp emacs-minor-version)
    (setq emacs-minor-version 1)))

(defun nelisp-emacs-magit-bridge--ensure-buffer-defaults ()
  "Ensure `buffer-read-only' defaults to nil, matching real Emacs.

After `nemacs-init', this repo's batch/interactive bootstrap leaves the
global default value of `buffer-read-only' at `t' (another substrate
gap, not Magit-specific): every freshly-created buffer, including the
ones `with-temp-buffer' creates, inherits it and any `insert' into that
buffer signals `text-read-only'.  Compat's own docstring formatting
helper (`compat-macs--docstring') builds its formatted string inside a
`with-temp-buffer', so this alone was enough to silently break the
first vendor file in the chain (`compat-31.el') before Magit is ever
reached.  Restoring the real-Emacs default here is a session-level
precondition fix, not a vendor patch."
  (unless (eq (default-value 'buffer-read-only) nil)
    (setq-default buffer-read-only nil)))

(defun nelisp-emacs-magit-bridge--ensure-static-if ()
  "Ensure `static-if' (Emacs 29's own preloaded macro) is available.

Real Emacs 29+ defines `static-if' in `subr.el' as part of the dumped,
always-preloaded core, so it never shows up as a `require'd feature and
Compat never needs to shim it itself.  NeLisp's substrate does not
preload it, so `compat-31.el' (which uses `static-if' directly, relying
on the host to already provide it) hits a `void-function' otherwise.
Copied verbatim from `subr.el' (not reimplemented) and only installed
when absent."
  (unless (fboundp 'static-if)
    (defmacro static-if (condition then-form &rest else-forms)
      "A conditional compilation macro.
Evaluate CONDITION at macro-expansion time.  If it is non-nil,
expand the macro to THEN-FORM.  Otherwise expand it to ELSE-FORMS
enclosed in a `progn' form.  ELSE-FORMS may be empty."
      (declare (indent 2) (debug (sexp sexp &rest sexp)))
      (if (eval condition lexical-binding)
          then-form
        (cons 'progn else-forms)))))

(defun nelisp-emacs-magit-bridge--ensure-cl-generic-define-generalizer ()
  "Ensure `cl-generic-define-generalizer' (real Emacs `cl-generic.el') exists.

NeLisp's native `cl-generic'/`cl-defmethod'/`cl-generic-make-generalizer'
substrate is otherwise present (P0 in Doc 33), but this one small
convenience macro around `cl-generic-make-generalizer' is missing;
EIEIO's `eieio-core.el' uses it directly to teach `cl-defmethod'
dispatch about EIEIO classes (`magit-section' and friends).  Copied
verbatim from `cl-generic.el' (not reimplemented) and only installed
when absent."
  (unless (fboundp 'cl-generic-define-generalizer)
    (defmacro cl-generic-define-generalizer
        (name priority tagcode-function specializers-function)
      "Define a new kind of generalizer.
NAME is the name of the variable that will hold it.
PRIORITY defines which generalizer takes precedence.
  The catch-all generalizer has priority 0.
  Then `eql' generalizer has priority 100.
TAGCODE-FUNCTION takes as first argument a varname and should return
  a chunk of code that computes the tag of the value held in that variable.
  Further arguments are reserved for future use.
SPECIALIZERS-FUNCTION takes as first argument a tag value TAG
  and should return a list of specializers that match TAG.
  Further arguments are reserved for future use."
      (declare (indent 1) (debug (symbolp body)))
      `(defconst ,name
         (cl-generic-make-generalizer
          ',name ,priority ,tagcode-function ,specializers-function)))))

(defun nelisp-emacs-magit-bridge--ensure-defalias-forward-reference ()
  "Ensure `defalias' tolerates a not-yet-defined symbol DEFINITION.

`(defalias SYM (function NOT-YET-DEFINED))' is a routine forward-alias
pattern (`define-obsolete-function-alias' uses it throughout EIEIO and
Magit, e.g. `object-class-fast' aliased to `eieio-object-class' before
that function is defined later in the same file).  `(function SYM)'
for a plain symbol evaluates to the bare symbol here exactly as in
real Emacs (confirmed: `(consp (function foo))' is nil), so this is
not a `function'-special-form bug; the native `defalias' eagerly
resolves/dereferences its DEFINITION argument and signals a
`void-variable' error when the target symbol has no function cell
yet.  `src/emacs-eval.el' already carries an Elisp polyfill for this
exact case (a late-bound forwarder via `symbolp'+`fboundp'), but it is
guarded by `(unless (fboundp (quote defalias)) ...)' and a native
`defalias' already satisfies that guard, so the polyfill never
installs.  This bridge precondition re-installs that same already-written
logic unconditionally (copied, not reimplemented) so the forward-reference
case works without waiting on the native/polyfill guard order to change
in `src/emacs-eval.el' itself."
  (defun defalias (symbol definition &optional docstring)
    "Alias SYMBOL to DEFINITION, tolerating a not-yet-defined DEFINITION.
DOCSTRING is accepted for arglist parity and currently ignored."
    (ignore docstring)
    (if (and (symbolp definition)
             (not (fboundp definition)))
        (eval (list 'defun symbol '(&rest args)
                    (list 'apply (list 'quote definition) 'args)))
      (fset symbol definition))
    symbol))

(defun nelisp-emacs-magit-bridge--ensure-cl-declaim ()
  "Ensure `cl-declaim' exists as the compiler-hint no-op it effectively is.

Real `cl-declaim' (`cl-lib.el') records byte-compiler optimization
declarations (`cl-proclaim') and, when compiling, wraps them in
`cl-eval-when'.  NeLisp has no byte/native compiler for these hints to
affect, so a no-op macro is a faithful runtime stand-in rather than a
partial reimplementation of `cl-proclaim' bookkeeping nothing here ever
reads."
  (unless (fboundp 'cl-declaim)
    (defmacro cl-declaim (&rest _specs) nil)))

(defun nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub ()
  "Ensure `ansi-color--update-face-vec' exists as a documented no-op.

Real `ansi-color.el' (loaded by this bridge for its ordinary 8/16-color
SGR handling, used by `magit-process'/`magit-log' to render colored git
output) contains one function, `ansi-color--update-face-vec', with a
bool-vector reader literal (`#&8\" \"') that the NeLisp reader/evaluator
cannot get through cleanly on its own (confirmed in isolation: reaching
that literal silently truncates the remainder of that `load' call, with
no relation to Magit); the bundle generator drops only that one
definition (see `nelisp-emacs-magit-bridge-bundle-excluded-defuns'),
keeping the rest of `ansi-color.el' real.  That function implements only
the extended 256-color/24-bit (SGR 38/48 with a 5- or 2-parameter
sub-sequence) face-vector bookkeeping path; ordinary 8/16-color SGR
codes (what plain `git' plumbing output realistically emits) never reach
it.  This stub is a documented, narrow functionality gap for that
extended path, not a Magit or git-correctness gap: a no-op here means an
extended-color escape sequence's *face* bookkeeping silently no-ops
(cosmetic — the affected text still renders, just without that specific
color update), not a data-correctness or crash risk."
  (unless (fboundp 'ansi-color--update-face-vec)
    (defun ansi-color--update-face-vec (_face-vec _iterator)
      "Stub: extended 256-color/24-bit SGR face bookkeeping is not supported.
See `nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub'."
      nil)))

(defun nelisp-emacs-magit-bridge--ensure-compat-maybe-require ()
  "Ensure `compat--maybe-require' exists as the harmless no-op it already is.

`compat.el' defines this helper macro inside a top-level
`eval-when-compile', which the shared normalizer (correctly, in
general) drops as compile-time-only.  Real `compat--maybe-require'
only conditionally `require's `compat-31' when
`(< emacs-major-version 31)'; this bridge already force-loads
`compat-31.el' earlier in the fixed load order regardless, so by the
time `compat.el' calls `(compat--maybe-require)' the real macro would
have been a no-op here anyway.  A literal no-op stands in for it."
  (unless (fboundp 'compat--maybe-require)
    (defmacro compat--maybe-require () nil)))

(defun nelisp-emacs-magit-bridge--ensure-default-process-coding-system ()
  "Ensure `default-process-coding-system' is a real dynamic (special) variable.

Real Emacs declares this as a built-in, always-special defvar (default
value a DECODING . ENCODING cons set during startup).  NeLisp's
substrate never declares it at all.  Magit's own lexical-binding files
only ever *bind* it locally around a process call
(`(let ((default-process-coding-system (magit--process-coding-system)))
...)' in `vendor/magit/lisp/magit-process.el'/`magit-git.el') and never
`defvar' it themselves, exactly like real Emacs magit assumes the name
is already special.  Without a prior `defvar', a lexical-binding `let'
on an unknown name creates a plain LEXICAL binding instead of a dynamic
one, so a *different* function reading the same name later in the same
dynamic extent (e.g. `magit-git.el:1342', not textually nested inside
that `let') hits `void-variable' instead of seeing Magit's own binding
-- this is what M2 probing hit running `magit-git-string' for real.
Declaring the name here with a value shaped like real Emacs' own
default (a cons of decoding and encoding coding systems, both nil =
`no-conversion') is a session-level precondition fix, not a vendor
patch: it only makes the name a genuine special variable so Magit's
existing dynamic `let' behaves the way it already assumes."
  (unless (boundp 'default-process-coding-system)
    (defvar default-process-coding-system (cons nil nil))))

(defun nelisp-emacs-magit-bridge--ensure-coding-system-change-eol-conversion ()
  "Ensure `coding-system-change-eol-conversion' exists.

`magit--process-coding-system' (`vendor/magit/lisp/magit-process.el')
calls this real-Emacs function to derive an EOL-specific variant of a
coding system (e.g. mapping a generic symbol + `unix' to `utf-8-unix')
whenever `magit-process-ensure-unix-line-ending' is non-nil (the
default).  NeLisp's substrate does not model coding systems as objects
with an EOL-conversion axis at all, and this bridge's process substrate
already always reads/writes subprocess bytes as given (no CRLF
translation applied anywhere), so returning CODING-SYSTEM unchanged (or
`utf-8-unix' when CODING-SYSTEM is nil, i.e. no coding system was
configured) is a faithful-enough stand-in for M2/M3's read-only status-
buffer use: it lets Magit's own process setup finish instead of
`void-function' aborting before any git process is even started, and it
does not change what bytes get inserted -- only what symbol Magit
records for later (here, unused) coding introspection."
  (unless (fboundp 'coding-system-change-eol-conversion)
    (defun coding-system-change-eol-conversion (coding-system eol-type)
      "See `nelisp-emacs-magit-bridge--ensure-coding-system-change-eol-conversion'."
      (ignore eol-type)
      (or coding-system 'utf-8-unix))))

(defun nelisp-emacs-magit-bridge--ensure-backquote-marker-symbols ()
  "Ensure the three advertised backquote/unquote/splice marker constants exist.

Real Emacs's `backquote.el' (always preloaded/dumped, so it never shows
up as a \"newly loaded\" file when this bridge's bundle generator
records `load-history' diffs from a clean host Emacs session -- hence
it is absent from `nelisp-emacs-magit-bridge-bundle-files') advertises
three `defconst's other packages use to introspect or build raw,
un-expanded backquote forms: `backquote-backquote-symbol' (the reader
head for `` ` ''), `backquote-unquote-symbol' (`` , ''), and
`backquote-splice-symbol' (`` ,@ '').  `vendor/llama/llama.el' (the
`##'/`llama' macro transient/magit/with-editor use pervasively for
short lambdas) and `vendor/emacs-lisp/emacs-lisp/pp.el' both reference
these three names directly as plain variables at macro-expansion or
load time, e.g. `(eq (car-safe fn) backquote-backquote-symbol)' in
`llama--collect'.  This NeLisp reader expands backquote templates
fully at READ time (`nelisp-reader--read-backquote') instead of
leaving a `` (\\=` ...) ''-headed literal for a `backquote' MACRO to
expand later the way real Emacs does, so it never produces a runtime
value whose `car' is `eq' to one of these markers -- but the mere act
of *evaluating* the bare variable reference to compare against still
requires the variable to be bound, and this bridge never provided it,
so any `##...' call site anywhere in the real vendor chain signalled
`void-variable: backquote-backquote-symbol' the first time the
enclosing (uncompiled, so lazily macro-expanded) function was actually
called (found probing M2's `magit-toplevel', which reaches
`magit-ignore-submodules-p's `(cl-find-if (##string-prefix-p ...) ...)'
by way of ordinary git-status plumbing, not anything Magit-status-
buffer specific -- this affects the `##' shorthand generally).
Declaring the three constants with real Emacs's own values is a
session-level precondition fix, not a vendor patch: because this
reader's backquote representation never round-trips through the
tagged-list shape these constants exist to recognize, the comparisons
that use them will correctly always take the \"not a nested backquote
template\" branch, which is the semantically right answer here."
  (unless (boundp 'backquote-backquote-symbol)
    (defconst backquote-backquote-symbol '\`))
  (unless (boundp 'backquote-unquote-symbol)
    (defconst backquote-unquote-symbol '\,))
  (unless (boundp 'backquote-splice-symbol)
    (defconst backquote-splice-symbol '\,@)))

(defun nelisp-emacs-magit-bridge--ensure-files-el-globals ()
  "Ensure the `files.el'-defined globals the vendor chain reads directly exist.

Real Emacs preloads/dumps `files.el' as part of the core, exactly like
`backquote.el' (see
`nelisp-emacs-magit-bridge--ensure-backquote-marker-symbols' above), so it
never shows up as a newly-loaded file when this bridge's bundle generator
diffs `load-history' against a clean host Emacs session -- `files.el'
itself is therefore correctly absent from
`nelisp-emacs-magit-bridge-bundle-files', but the NeLisp substrate never
preloads its globals either, so any of these names signals `void-variable'
the first time evaluation actually reaches it, at whatever call depth that
happens to be (M2 probing hit `find-file-visit-truename' this way, deep
inside `magit-toplevel' -- by direct code inspection this is one of many,
not an isolated case).  This list was built by a static cross-reference of
every top-level `files.el' defcustom/defvar/defconst name against the full
vendor chain (magit+transient+with-editor+compat+cond-let+llama), narrowed
to the subset a live `boundp' probe against this bridge's own baked
runtime image confirmed both referenced AND still missing -- notably,
`auto-mode-alist'/`file-name-history'/`find-file-hook'/`font-lock-keywords'/
`kill-buffer-hook'/`magic-fallback-mode-alist' also match the reference
search but are already bound elsewhere in this substrate (or, for some
Compat-shimmed names, become bound as a side effect of Compat's own
version-gated logic once `emacs-version' reads \"30.1\"; not blindly
ported here since they are not gaps).  Declaring each with real Emacs's
own default value (copied from `files.el', not reinterpreted) is a
session-level precondition fix, not a vendor patch.  A few defaults are
deliberately simplified stand-ins rather than literal copies because the
real value either names a function this read-only M2/M3 bridge never
calls (`revert-buffer-function' defaults to nil here, not
`#'revert-buffer--default', since `files.el' -- and that function with
it -- is never loaded) or duplicates the caching keybinding-prompt logic
of a function `save-some-buffers' this bridge does not need to run
(`save-some-buffers-action-alist' is left nil rather than the real
value's `##'-heavy alist); both are noted inline."
  (dolist (spec
           '((after-revert-hook . nil)
             (after-save-hook . nil)
             (before-revert-hook . nil)
             (before-save-hook . nil)
             (backup-directory-alist . nil)
             (confirm-nonexistent-file-or-buffer . after-completion)
             (directory-abbrev-alist . nil)
             (directory-files-no-dot-files-regexp . "[^.]\\|\\.\\.\\.")
             (enable-local-variables . t)
             ;; Real default is platform-conditional; this bridge only
             ;; ever runs on the non-Windows branch of `files.el''s own
             ;; `(if (memq system-type '(windows-nt cygwin)) ...)'.
             (mounted-file-systems
              . "^\\(?:/\\(?:afs/\\|m\\(?:edia/\\|nt\\)\\|\\(?:ne\\|tmp_mn\\)t/\\)\\)")
             (find-file-literally . nil)
             (find-file-not-found-functions . nil)
             (find-file-visit-truename . nil)
             (lock-file-name-transforms . nil)
             (remote-file-name-inhibit-cache . 10)
             ;; Simplified stand-in: real default is `#'revert-buffer--default',
             ;; a `files.el' function this bridge never loads or calls.
             (revert-buffer-function . nil)
             (revert-without-query . nil)
             ;; Simplified stand-in: real default is a keybinding/prompt alist
             ;; for the interactive `save-some-buffers' prompt loop, unused by
             ;; this bridge's read-only M2/M3 status-buffer path.
             (save-some-buffers-action-alist . nil)
             (trash-directory . nil)
             (trusted-content . nil)))
    (unless (boundp (car spec))
      ;; `defvar' is a special form and cannot take a runtime symbol as its
      ;; first argument, so a data-driven `dolist' has to go through `eval'
      ;; to build and run the equivalent `(defvar SYM 'VALUE)' form -- this
      ;; still makes SYM a genuine special (dynamically-scoped) variable,
      ;; unlike plain `set', which would only populate its global value
      ;; cell without marking it special for a later `let' to bind
      ;; dynamically (the same distinction
      ;; `nelisp-emacs-magit-bridge--ensure-default-process-coding-system'
      ;; documents above).
      (eval (list 'defvar (car spec) (list 'quote (cdr spec))) t)))
  ;; Two names need a non-literal default value (a hash table, a quoted
  ;; list), so they cannot live in the simple dotted-pair table above.
  (unless (boundp 'file-has-changed-p--hash-table)
    (defvar file-has-changed-p--hash-table (make-hash-table :test #'equal)))
  (unless (boundp 'ignored-local-variables)
    (defvar ignored-local-variables
      '(ignored-local-variables safe-local-variable-values
        file-local-variables-alist dir-local-variables-alist))))

(defun nelisp-emacs-magit-bridge--ensure-uniquify-globals ()
  "Ensure the `uniquify.el' names Magit's buffer naming touches exist.

`uniquify.el' is preloaded/dumped by real Emacs (same class as
`files.el' and `backquote.el' above: never appears in a load-history
diff, so it is correctly absent from the bundle, but this substrate
never preloads it either).  `magit--maybe-uniquify-buffer-names'
(`vendor/magit/lisp/magit-mode.el') runs unconditionally for every new
Magit buffer when `magit-uniquify-buffer-names' is non-nil (the
default): it pushes onto `uniquify-list-buffers-directory-modes',
let-binds `uniquify-buffer-name-style', sets the built-in buffer-local
`list-buffers-directory', and calls
`uniquify-rationalize-file-buffer-names'.  The two defvars and the
defcustom get real Emacs's own default values.  The rationalize
function is a documented no-op stand-in, NOT a copy: its only job is
cosmetic buffer-name disambiguation across same-named buffers, this
bridge's buffers are already unique via `generate-new-buffer', and
porting uniquify's full rationalize/rename machinery is out of scope
for a read-only status buffer."
  (unless (boundp 'uniquify-list-buffers-directory-modes)
    (defvar uniquify-list-buffers-directory-modes
      '(dired-mode cvs-mode vc-dir-mode)))
  (unless (boundp 'uniquify-buffer-name-style)
    (defvar uniquify-buffer-name-style 'post-forward-angle-brackets))
  (unless (boundp 'list-buffers-directory)
    (defvar list-buffers-directory nil))
  (unless (fboundp 'uniquify-rationalize-file-buffer-names)
    (defun uniquify-rationalize-file-buffer-names (_base _dirname _newbuf)
      "Stub: cosmetic buffer-name disambiguation is not modeled.
See `nelisp-emacs-magit-bridge--ensure-uniquify-globals'."
      nil)))

(defun nelisp-emacs-magit-bridge--ensure-simple-el-globals ()
  "Ensure the `simple.el'-defined globals the vendor chain reads exist.

Same host-preload gap class as
`nelisp-emacs-magit-bridge--ensure-files-el-globals' above, for
`simple.el' (also dumped into real Emacs, so also correctly absent from
the bundle).  Built the same way: static cross-reference of simple.el's
top-level defcustom/defvar/defconst names against the vendor chain,
narrowed by a live `boundp' probe against the baked runtime image
(`kill-ring'/`minibuffer-history'/`shell-command-switch' matched the
reference scan but are already bound here).  M2 hit
`line-move-visual' first, via `magit-section-mode''s own
`(setq-local line-move-visual t)' body.  Values are real Emacs's own
defaults; the two `redisplay-*-region-function' defaults name simple.el
functions this bridge never loads, so they are nil here (their only
consumer is region highlighting, not modeled by this substrate), and
that simplification is deliberately documented rather than silent."
  (dolist (spec
           '((deactivate-mark-hook . nil)
             (line-move-visual . t)
             (minibuffer-default . nil)
             (minibuffer-default-add-function . nil)
             (prefix-command-preserve-state-hook . nil)
             (read-extended-command-predicate . nil)
             ;; Real defaults are #'redisplay--highlight-overlay-function /
             ;; #'redisplay--unhighlight-overlay-function (simple.el
             ;; functions not loaded here); region highlighting is not
             ;; modeled, so nil.
             (redisplay-highlight-region-function . nil)
             (redisplay-unhighlight-region-function . nil)
             (shell-command-default-error-buffer . nil)
             (shift-select-mode . t)
             (tabulated-list-entries . nil)
             (tabulated-list-format . nil)
             (tabulated-list-sort-key . nil)
             (widen-automatically . t)))
    (unless (boundp (car spec))
      ;; Same `eval'-built `defvar' as -ensure-files-el-globals: makes
      ;; the name genuinely special so vendor `let'/`setq-local' work.
      (eval (list 'defvar (car spec) (list 'quote (cdr spec))) t))))

(defun nelisp-emacs-magit-bridge--ensure-third-party-soft-vars ()
  "Ensure third-party variables Magit declares value-less exist with nil.

`vendor/magit/lisp/magit-section.el' carries `(defvar
symbol-overlay-inhibit-map)' -- a value-less special declaration for an
optional third-party package -- and then does `(setq-local
symbol-overlay-inhibit-map t)' unconditionally in
`magit-section-mode''s body.  Real Emacs accepts a `setq-local' (or
`set') of a symbol that has no binding yet; this substrate's
`setq-local' fallback expands through `set', which signals
`void-variable' for a never-bound name instead of creating it.  Giving
the declared name a real nil default here matches what any Emacs
session without the symbol-overlay package effectively observes."
  (unless (boundp 'symbol-overlay-inhibit-map)
    (defvar symbol-overlay-inhibit-map nil))
  ;; Same class, found by auditing every `setq-local' target in the
  ;; magit-section/magit-mode/magit-status mode bodies against a live
  ;; `boundp' probe: hook-function variables owned by host-preloaded or
  ;; optional packages (bookmark.el, imenu.el, isearch.el) that the mode
  ;; bodies overwrite unconditionally.  Real defaults name functions
  ;; from files this bridge never loads, so nil (= "package facility not
  ;; active", exactly what the vendor code then replaces) is the honest
  ;; default here.
  (unless (boundp 'bookmark-make-record-function)
    (defvar bookmark-make-record-function nil))
  (unless (boundp 'imenu-create-index-function)
    (defvar imenu-create-index-function nil))
  (unless (boundp 'imenu-default-goto-function)
    (defvar imenu-default-goto-function nil))
  (unless (boundp 'isearch-filter-predicate)
    (defvar isearch-filter-predicate nil))
  ;; `magit-mode''s body calls this `files.el' function unconditionally.
  ;; Dir-local variables are not modeled by this substrate at all, so a
  ;; documented no-op stand-in (NOT a copy) is faithful: with no
  ;; .dir-locals machinery there is nothing to hack in.
  (unless (fboundp 'hack-dir-local-variables-non-file-buffer)
    (defun hack-dir-local-variables-non-file-buffer ()
      "Stub: dir-local variables are not modeled by this substrate.
See `nelisp-emacs-magit-bridge--ensure-third-party-soft-vars'."
      nil))
  ;; `magit-mode''s body also calls `face-remap-add-relative'
  ;; (face-remap.el, host-preloaded) to restyle the header line.  Face
  ;; remapping is purely cosmetic display state with no consumer in this
  ;; substrate, so a documented no-op stand-in returning nil (the shape
  ;; of a remapping cookie consumers may later pass to
  ;; `face-remap-remove-relative') is faithful for M2/M3.
  (unless (fboundp 'face-remap-add-relative)
    (defun face-remap-add-relative (_face &rest _specs)
      "Stub: face remapping is not modeled by this substrate.
See `nelisp-emacs-magit-bridge--ensure-third-party-soft-vars'."
      nil)))

(defun nelisp-emacs-magit-bridge--ensure-docstring-fill-helpers ()
  "Ensure subr.el's docstring fill helpers used by `defclass' exist.

EIEIO's `defclass' macro calls `internal--format-docstring-line' (a
host-preloaded `subr.el' helper, same absence class as `static-if'
above) while building each accessor's docstring -- at MACRO EXPANSION
time, so with it missing every `(defclass ...)' in the whole vendor
chain fails, and inside the bundle's per-form load tolerance those
failures were completely silent: `magit-section'/`magit-status' classes
simply never registered, leaving `magit-insert-section--create' void
and every status-buffer refresh empty (0 bytes, root section nil).
Both helpers are copied verbatim from `vendor/emacs-lisp/subr.el' (not
reimplemented) and installed only when absent.  `fill-column' gets its
real Emacs default when unbound, since the fill helper reads it."
  (unless (boundp 'fill-column)
    (defvar fill-column 70))
  (unless (fboundp 'internal--fill-string-single-line)
    (defun internal--fill-string-single-line (str)
      "Fill string STR to `fill-column'.
This is intended for very simple filling while bootstrapping
Emacs itself, and does not support all the customization options
of fill.el (for example `fill-region')."
      (if (< (length str) fill-column)
          str
        (let* ((limit (min fill-column (length str)))
               (fst (substring str 0 limit))
               (lst (substring str limit)))
          (cond ((string-match "\\( \\)$" fst)
                 (setq fst (replace-match "\n" nil nil fst 1)))
                ((string-match "^ \\(.*\\)" lst)
                 (setq fst (concat fst "\n"))
                 (setq lst (match-string 1 lst)))
                ((string-match ".*\\( \\(.+\\)\\)$" fst)
                 (setq lst (concat (match-string 2 fst) lst))
                 (setq fst (replace-match "\n" nil nil fst 1))))
          (concat fst (internal--fill-string-single-line lst))))))
  (unless (fboundp 'internal--format-docstring-line)
    (defun internal--format-docstring-line (string &rest objects)
      "Format a single line from a documentation string out of STRING and OBJECTS.
Signal an error if STRING contains a newline.
This is intended for internal use only.  Avoid using this for the
first line of a docstring; the first line should be a complete
sentence (see Info node `(elisp) Documentation Tips')."
      (when (string-match "\n" string)
        (error "Unable to fill string containing newline: %S" string))
      (internal--fill-string-single-line (apply #'format string objects)))))

(defun nelisp-emacs-magit-bridge--ensure-special-mode ()
  "Ensure `special-mode' (real Emacs `simple.el') exists as a parent mode.

`simple.el' is preloaded/dumped by real Emacs (same class as `files.el'
and `uniquify.el' above), and `magit-section-mode' is
`(define-derived-mode magit-section-mode special-mode ...)' -- so the
first `(magit-status-mode)' activation walks its parent chain into a
`void-function special-mode'.  Body and `mode-class' property are
copied from `vendor/emacs-lisp/simple.el' (not reimplemented); the
keymap is built with plain `define-key' instead of `defvar-keymap'
(which this substrate lacks) but binds the same keys to the same
commands."
  (unless (fboundp 'special-mode)
    (unless (boundp 'special-mode-map)
      (defvar special-mode-map
        (let ((map (make-sparse-keymap)))
          (when (fboundp 'suppress-keymap)
            (suppress-keymap map))
          (define-key map "q" 'quit-window)
          (define-key map " " 'scroll-up-command)
          (define-key map "\d" 'scroll-down-command)
          (define-key map "?" 'describe-mode)
          (define-key map "h" 'describe-mode)
          (define-key map ">" 'end-of-buffer)
          (define-key map "<" 'beginning-of-buffer)
          (define-key map "g" 'revert-buffer)
          map)))
    (put 'special-mode 'mode-class 'special)
    (define-derived-mode special-mode nil "Special"
      "Parent major mode from which special major modes should inherit.

A special major mode is intended to view specially formatted data
rather than files.  These modes usually use read-only buffers."
      (setq buffer-read-only t))))

(defun nelisp-emacs-magit-bridge--ensure-current-buffer ()
  "Ensure the session has a live current buffer, like real Emacs always does.

Real Emacs guarantees `(current-buffer)' is never nil -- a bare session
starts inside *scratch*.  The nemacs batch bootstrap leaves this image
with an empty `buffer-list' and a nil `current-buffer' until the first
explicit buffer switch, so any code path that hands the \"currently
displayed\" buffer around hits `wrong-type-argument (nelisp-ec-buffer-p
nil)' -- M2 probing hit this inside `magit-get-mode-buffer', whose
window-scan maps `window-buffer' over `window-list' and passes each
result straight to `with-current-buffer' (with the window substrate's
own buffer slot falling back to `current-buffer', i.e. nil here).
Creating and selecting *scratch* is a session-level precondition fix
mirroring the real Emacs startup invariant, not a vendor patch."
  (when (and (fboundp 'current-buffer)
             (fboundp 'get-buffer-create)
             (fboundp 'set-buffer)
             (null (current-buffer)))
    (set-buffer (get-buffer-create "*scratch*"))))

(defun nelisp-emacs-magit-bridge--ensure-save-some-buffers ()
  "Ensure `save-some-buffers' (real Emacs `files.el') exists.

`files.el' is preloaded/dumped by real Emacs (same host-preload-gap
class as `-ensure-files-el-globals' above), so it never shows up as a
newly-loaded file in this bridge's bundle, but the substrate never
preloads it either.  `magit-save-repository-buffers'
(`vendor/magit/lisp/magit-mode.el') calls `(save-some-buffers ARG
PRED)' as a precondition before refreshing/opening the status buffer
-- found once the M2 buffer-local swap-engine fix (Doc 33 §8 item 242)
unblocked the earlier `text-read-only' abort and status-buffer setup
ran far enough to reach it.  Real `save-some-buffers' walks every live,
file-visiting, modified buffer PRED selects and interactively prompts
to save each one; this bridge's M2/M3 status-buffer smoke never opens
an Emacs buffer on a modified file (the fixture's unstaged edit and
untracked files are plain filesystem state the whole time) and has no
minibuffer-prompt loop to drive one anyway, so a no-op returning nil
-- matching what real Emacs itself returns when there is nothing to
save -- is a faithful stand-in for this read-only path, not a vendor
patch."
  (unless (fboundp 'save-some-buffers)
    (defun save-some-buffers (&optional _arg _pred)
      "Stub: no interactive buffer-save prompt loop is modeled.
See `nelisp-emacs-magit-bridge--ensure-save-some-buffers'."
      nil)))

;; Doc 155 §8.13 / nelisp Policy B (retained-generation growth-chunk boxes,
;; vendor/nelisp commit range fix/gc-retention-edge-magit) removed the need
;; for a bridge-side raw memory poke here.  Doc 33 item 244 found that
;; replaying the full vendor Magit bundle with real EIEIO class registration
;; live made the standalone runtime SIGSEGV partway through the load with
;; the Doc 155 signature (`nl_vector_slot_ptr' NULL deref under
;; `nelisp_frame_stack_find_in_frame': a live lexframe's hash-table/buckets
;; child reclaimed by the form-boundary collector while the frame record
;; survives) -- a further instance of the marker-gap class Path B
;; (b31179ce) had already closed once.  Formerly this bridge worked around
;; it with `nelisp-emacs-magit-bridge--ensure-gc-collect-disabled', a
;; session-scoped `ptr-read-u64'/`ptr-write-u64' poke of the vendor
;; binary's RECLAIMER GATE (base+160) -- effectively disabling the
;; standalone runtime's GC reclaimer for the whole session (Doc 155 §8.8
;; "Fix A", the sound-but-blunt stopgap).  The vendor core now ships the
;; real fix instead: the form-boundary collector treats every growth-chunk
;; (non-chunk-0) box it would otherwise free as belonging to a retained
;; generation, exactly like the existing chunk-0 boot watermark, so it can
;; never wrongly free a live growth-chunk child.  `(garbage-collect)' and
;; the mid-form loop collector are untouched by this policy and keep
;; reclaiming growth-chunk garbage normally.  With the core sound by
;; construction, this bridge no longer needs to (and no longer does) touch
;; the vendor binary's internal GC gate at all.

(defun nelisp-emacs-magit-bridge--ensure-lambda-documentation-form ()
  "Make a lambda-body-leading `(:documentation ...)' form evaluate harmlessly.

Real Emacs treats `(:documentation FORM)' at the head of a lambda body
as a special dynamic-docstring construct (handled by cconv/oclosure
machinery, never actually *called*).  EIEIO generates exactly that
shape for every class it defines -- `eieio-defclass-internal''s
backward-compatible `NAME-list-p' defalias and the class predicates
funnel through `(lambda (obj) (:documentation ...) ...)' -- and the
standalone NeLisp evaluator, which has no special handling for the
construct, evaluates it as an ordinary function call of the keyword
`:documentation' the first time such a predicate is INVOKED (found on
the M2 status-buffer path right after the item 244 `copy-alist' fix
let those EIEIO predicates be defined for real:
`(void-function :documentation)').  Binding the keyword's function
cell to an ignore-everything lambda makes the doc form a cheap no-op
(its argument is a docstring literal or a pure formatting call) while
leaving the body's real forms untouched -- the same observable
behavior real Emacs has at call time, where the construct contributes
nothing to the function's return value."
  (unless (fboundp :documentation)
    (defalias :documentation (lambda (&rest _) nil))))

(defun nelisp-emacs-magit-bridge--ensure-magit-insert-headers ()
  "Install a hook-and-closure-free `magit-insert-headers' after the bundle loads.

Real `magit-insert-headers' (`vendor/magit/lisp/magit-section.el',
excluded from this bundle — see `nelisp-emacs-magit-bridge-bundle-
excluded-defuns' in `scripts/build-nelisp-emacs-magit-bridge-bundle.el')
collects the top-level sections a header-hook run inserted by
`add-hook'ing a short closure onto `magit-insert-section-hook' at depth
-90 that does `(push magit-insert-section--current header-sections)',
then regroups those sections (making the first one the parent of the
rest) once the hook run finishes.

Doc 33 item 244 bisection found that this closure — created while
`magit-insert-section--current' already has an ACTIVE outer dynamic
binding (the enclosing status-buffer root section) — always reads back
that OUTER (root) value instead of the correctly re-bound INNER value
once actually invoked from within a still more deeply nested re-binding
of the same variable (each individual header line's own section): a
NeLisp interpreter gap in how closures resolve a special variable
across more than one level of nested dynamic re-binding, reproduced
with a minimal repro that needs neither Magit, EIEIO, nor `add-hook'/
`run-hooks' (plain nested `let' forms over an ordinary `defvar' already
exhibit it — see `docs/design/33-emacs-core-substrate-priority-
plan.org' item 244 for the full bisection).  Every section the original
closure collected therefore turns out to be the (status) root itself,
whose own `parent' slot is nil, so the regroup step's `(oset
header-parent children ...)' aborts with `(wrong-type-argument (or
eieio-object cl-structure-object oclosure) nil)' the moment it tries to
write a slot on that nil `header-parent'.  This is a core interpreter
gap, out of this bridge's scope to fix.

This replacement collects the SAME set of sections a different, hook-
and-closure-free way that sidesteps the gap entirely: `magit-insert-
section--finish' (an ordinary function, not a closure) already appends
each newly finished section directly onto its real parent's `children'
slot before returning — a plain, non-closure `oref'/`oset' round trip
that Doc 33 item 244 confirmed is reliable.  Snapshotting the parent's
child count before running HOOK and taking the newly appended tail
afterward yields exactly the sections HOOK inserted, in the same
creation order the original closure's `nreverse'd accumulator produced
(assumes normal append-order insertion; `magit-section-insert-in-
reverse' is a log-rendering knob that headers never bind).  The
regroup logic below is copied verbatim from the original; only the
collection mechanism differs."
  (unless (fboundp 'magit-insert-headers)
    (defun magit-insert-headers (hook)
      (let* ((parent magit-insert-section--current)
             (before (length (oref parent children)))
             header-sections)
        (magit-run-section-hook hook)
        (setq header-sections (nthcdr before (oref parent children)))
        (when header-sections
          (insert "\n")
          (when (cdr header-sections)
            (let* ((1st-header (pop header-sections))
                   (header-parent (oref 1st-header parent)))
              (oset header-parent children (list 1st-header))
              (oset 1st-header children header-sections)
              (oset 1st-header content (oref (car header-sections) start))
              (oset 1st-header end (oref (car (last header-sections)) end))
              (dolist (sub-header header-sections)
                (oset sub-header parent 1st-header))
              (magit-section-maybe-add-heading-map 1st-header))))))))

(defun nelisp-emacs-magit-bridge--ensure-preconditions ()
  "Ensure every session precondition the vendor chain assumes is live."
  (nelisp-emacs-magit-bridge--ensure-lambda-documentation-form)
  (nelisp-emacs-magit-bridge--ensure-current-buffer)
  (nelisp-emacs-magit-bridge--ensure-process-substrate)
  (nelisp-emacs-magit-bridge--ensure-emacs-version-identity)
  (nelisp-emacs-magit-bridge--ensure-buffer-defaults)
  (nelisp-emacs-magit-bridge--ensure-static-if)
  (nelisp-emacs-magit-bridge--ensure-cl-generic-define-generalizer)
  (nelisp-emacs-magit-bridge--ensure-cl-declaim)
  (nelisp-emacs-magit-bridge--ensure-defalias-forward-reference)
  (nelisp-emacs-magit-bridge--ensure-ansi-color-update-face-vec-stub)
  (nelisp-emacs-magit-bridge--ensure-compat-maybe-require)
  (nelisp-emacs-magit-bridge--ensure-default-process-coding-system)
  (nelisp-emacs-magit-bridge--ensure-coding-system-change-eol-conversion)
  (nelisp-emacs-magit-bridge--ensure-backquote-marker-symbols)
  (nelisp-emacs-magit-bridge--ensure-files-el-globals)
  (nelisp-emacs-magit-bridge--ensure-uniquify-globals)
  (nelisp-emacs-magit-bridge--ensure-simple-el-globals)
  (nelisp-emacs-magit-bridge--ensure-third-party-soft-vars)
  (nelisp-emacs-magit-bridge--ensure-docstring-fill-helpers)
  (nelisp-emacs-magit-bridge--ensure-special-mode)
  (nelisp-emacs-magit-bridge--ensure-save-some-buffers))

(defun nelisp-emacs-magit-bridge-load ()
  "Load the real vendor Magit chain into the current NeLisp session.

Idempotent: a second call is a no-op once `nelisp-emacs-magit-bridge-loaded'
is set.  Signals an error naming the missing file when the generated
bundle has not been built yet (`make bake-magit-runtime-image' or
`make -f Makefile build/nelisp-emacs-magit-bridge-bundle.el' builds it via
`scripts/build-nelisp-emacs-magit-bridge-bundle.el' under host Emacs)."
  (unless nelisp-emacs-magit-bridge-loaded
    (nelisp-emacs-magit-bridge--ensure-preconditions)
    (let ((bundle (nelisp-emacs-magit-bridge--bundle-file)))
      (unless (file-readable-p bundle)
        (error "nelisp-emacs-magit-bridge: bundle not built: %s (run scripts/build-nelisp-emacs-magit-bridge-bundle.el under host Emacs)"
               bundle))
      (load bundle nil 'no-message t t)
      ;; Post-load fixup: `magit-insert-headers' is excluded from the
      ;; bundle (see `nelisp-emacs-magit-bridge--ensure-magit-insert-
      ;; headers'), so its replacement needs `oref'/`oset'/`magit-run-
      ;; section-hook' already defined, unlike the pre-load precondition
      ;; steps above.
      (nelisp-emacs-magit-bridge--ensure-magit-insert-headers)
      (setq nelisp-emacs-magit-bridge-loaded t)))
  nelisp-emacs-magit-bridge-loaded)

(defun nelisp-emacs-magit-bridge-loaded-p ()
  "Return non-nil once the real vendor Magit chain is live in this session.

Checks `commandp' on the two known silent-drop-prone modes (per Doc 33's
established finding: `featurep' alone can be true for a mode whose
`define-derived-mode' body was silently dropped), not just `featurep'."
  (and (featurep 'magit)
       (fboundp 'magit-status)
       (commandp 'magit-status)
       (fboundp 'magit-status-mode)
       (commandp 'magit-status-mode)
       (fboundp 'magit-run-git)
       (boundp 'magit-mode-map)
       (keymapp magit-mode-map)))

(provide 'nelisp-emacs-magit-bridge)

;;; nelisp-emacs-magit-bridge.el ends here
