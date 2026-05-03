;;; emacs-stub.el --- residual no-op shims (Doc 51 Phase 3-A''-3 → 10)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 3-A''-3 origin / Phase 10 trim — temporary no-op shims
;; for the long tail of Emacs C primitives that vendored `subr.el' /
;; `cl-lib.el' / friends reference at load time.  Without these,
;; NeLisp standalone fails to load any nontrivial Emacs library.
;;
;; This file is INTENTIONALLY DISPOSABLE — it should disappear as the
;; real implementations land in nelisp-emacs's L2 ports
;; (`emacs-keymap.el', `emacs-frame.el', etc.) or via `nelisp-ec-*'
;; aliasing.  See `project_phase4_emacs_c_primitives_todo' memory entry
;; for the full migration checklist.
;;
;; **Phase 10 split-out (2026-05-03)**: the three biggest blocks were
;; promoted out of this file into dedicated modules:
;;
;;   - pcase placeholder        → `emacs-pcase.el'
;;   - cl-* macros / fns subset → `emacs-cl-macros.el'
;;   - time / truncate polyfills → `emacs-time.el'
;;
;; That trim removed ~670 LOC.  Future phases will likely promote
;; `bytecomp / runtime metadata' (= ~290 LOC) and `numeric / bitwise'
;; into their own modules too.  See the `;;;; --- ' section markers
;; below for the remaining tail.
;;
;; Functions here are no-ops (= return nil / fixed sentinel).  Calling
;; them at runtime does NOTHING; library code that relies on actual
;; behavior (e.g. real keybindings, real frame manipulation) will fail
;; silently.  This is acceptable for the use cases nelisp-emacs targets
;; (= anvil tool dispatch, MCP server) where keymap / frame / display
;; primitives are never reached at the data path.
;;
;; Each shim is gated on `unless (fboundp ...)' so loading under host
;; Emacs is a cheap no-op.

;;; Code:

;;;; --- keymap.c -----------------------------------------------------------
;; Phase 11.C'' (2026-05-03) deleted the redundant make-keymap /
;; make-sparse-keymap / keymapp / define-key / lookup-key / key-binding
;; / set-keymap-parent / keymap-parent / current-global-map /
;; current-local-map / use-global-map / use-local-map /
;; where-is-internal nil-stubs that were shadowing
;; `emacs-keymap-builtins.el's bridges to `emacs-keymap-*' under
;; standalone NeLisp.  `define-key-after' has no prefixed equivalent
;; yet, so its no-op stub stays.

(unless (fboundp 'define-key-after)
  (defun define-key-after (keymap key definition &optional after)
    "Stub: no-op; returns DEFINITION."
    (ignore keymap key after)
    definition))


;;;; --- frame.c ------------------------------------------------------------
;; Phase 11.C'' (2026-05-03) deleted the redundant make-frame / framep
;; / frame-live-p / frame-list / selected-frame / frame-parameter /
;; frame-parameters / set-frame-parameter / modify-frame-parameters /
;; delete-frame nil-stubs — `emacs-frame-builtins.el' bridges them to
;; `emacs-frame-*'.  display-* probes have no prefixed substrate yet
;; (= would need a display capability map), so their no-op stubs stay.

(unless (fboundp 'display-graphic-p)
  (defun display-graphic-p (&optional display) (ignore display) nil))

(unless (fboundp 'display-color-p)
  (defun display-color-p (&optional display) (ignore display) nil))

(unless (fboundp 'display-multi-frame-p)
  (defun display-multi-frame-p (&optional display) (ignore display) nil))


;;;; --- window.c -----------------------------------------------------------
;; Phase 11.C'' (2026-05-03) deleted the redundant selected-window /
;; windowp / window-list / window-buffer / set-window-buffer nil-stubs
;; — `emacs-window-builtins.el' bridges them to `emacs-window-*'.
;; window-live-p and frame-selected-window have no prefixed substrate
;; yet (= no live-flag tracking / no per-frame selected-window slot in
;; the prefixed model), so their no-op stubs stay.

(unless (fboundp 'window-live-p)
  (defun window-live-p (window) (windowp window)))

(unless (fboundp 'frame-selected-window)
  (defun frame-selected-window (&optional frame) (ignore frame) (selected-window)))


;;;; --- font-lock ----------------------------------------------------------

(unless (fboundp 'font-lock-mode)
  (defun font-lock-mode (&optional arg) (ignore arg) nil))

(unless (boundp 'font-lock-defaults)
  (defvar font-lock-defaults nil))

(unless (boundp 'font-lock-keywords)
  (defvar font-lock-keywords nil))

(unless (fboundp 'font-lock-fontify-buffer)
  (defun font-lock-fontify-buffer () nil))


;;;; --- bytecomp / runtime metadata ---------------------------------------

(unless (fboundp 'set-advertised-calling-convention)
  (defun set-advertised-calling-convention (function arglist when)
    "Stub: drop the metadata."
    (ignore function arglist when) nil))

(unless (fboundp 'byte-code-function-p)
  (defun byte-code-function-p (object) (ignore object) nil))

(unless (fboundp 'compiled-function-p)
  (defun compiled-function-p (object) (ignore object) nil))

(unless (fboundp 'subrp)
  (defun subrp (object) (ignore object) nil))

(unless (fboundp 'special-form-p)
  (defun special-form-p (object) (ignore object) nil))

(unless (fboundp 'macrop)
  (defun macrop (object) (ignore object) nil))

(unless (fboundp 'symbol-value)
  (defun symbol-value (symbol)
    (if (boundp symbol)
        (eval symbol)
      (signal 'void-variable (list symbol)))))

(unless (fboundp 'default-value)
  (defalias 'default-value 'symbol-value))

(unless (fboundp 'default-boundp)
  (defalias 'default-boundp 'boundp))

(unless (fboundp 'set-default)
  (defun set-default (symbol value)
    (set symbol value)))

(unless (fboundp 'make-variable-buffer-local)
  (defun make-variable-buffer-local (variable)
    "Stub: no-op (NeLisp standalone has no buffer-local subsystem)."
    (ignore variable) nil))

(unless (fboundp 'make-local-variable)
  (defun make-local-variable (variable) (ignore variable) nil))

(unless (fboundp 'local-variable-p)
  (defun local-variable-p (variable &optional buffer) (ignore variable buffer) nil))

(unless (fboundp 'kill-local-variable)
  (defun kill-local-variable (variable) (ignore variable) nil))

;; condition-case variants used by subr.el
(unless (fboundp 'condition-case-unless-debug)
  (defmacro condition-case-unless-debug (var bodyform &rest handlers)
    "Stub: route through plain condition-case (= NeLisp has no debug-on-error toggle)."
    (cons 'condition-case (cons var (cons bodyform handlers)))))

;; Quoting helpers
(unless (fboundp 'kbd)
  (defun kbd (keys) (ignore) keys))

(unless (fboundp 'defvaralias)
  (defun defvaralias (new-alias base-variable &optional docstring)
    "Stub: copy current value (no live aliasing)."
    (ignore docstring)
    (when (boundp base-variable)
      (set new-alias (symbol-value base-variable)))
    new-alias))

(unless (fboundp 'make-symbol)
  (defun make-symbol (name) (intern name)))

(unless (fboundp 'gensym)
  (let ((counter 0))
    (defun gensym (&optional prefix)
      (setq counter (+ counter 1))
      (intern (format "%s%d" (or prefix "g") counter)))))

(unless (fboundp 'cl-gensym)
  (defalias 'cl-gensym 'gensym))

(unless (fboundp 'consing-uses-no-pure-list)
  (defvar consing-uses-no-pure-list nil))

(unless (boundp 'inhibit-changing-match-data)
  (defvar inhibit-changing-match-data nil))

(unless (boundp 'noninteractive)
  (defvar noninteractive t))

(unless (boundp 'inhibit-debugger)
  (defvar inhibit-debugger t))

;; defvar-local = defvar + make-variable-buffer-local
(unless (fboundp 'defvar-local)
  (defmacro defvar-local (var val &optional docstring)
    `(progn (defvar ,var ,val ,docstring)
            (make-variable-buffer-local ',var))))

;; Buffer search primitives.  Phase 11.B' (2026-05-03) extracted the
;; bridgeable subset (= re-search-forward / re-search-backward /
;; search-forward / search-backward / looking-at / match-data /
;; match-beginning / match-end / match-string + -no-properties) into
;; `emacs-search-builtins.el', wired to `nelisp-ec-*' substrate.  The
;; stubs that remain below cover the surface that has no L1.5 impl yet
;; (= string-match returns plist not int, replace-* needs a buffer
;; mutator, looking-back needs reverse scan).

(unless (fboundp 'set-match-data)
  (defun set-match-data (list &optional reseat) (ignore list reseat) nil))

(unless (fboundp 'string-match)
  ;; Emacs 27+ added 4th arg INHIBIT-MODIFY (= don't update match data).
  ;; Vendor subr.el's string-match-p calls (string-match RE STR START t) so
  ;; our polyfill must accept all 4.
  (defun string-match (regexp string &optional start inhibit-modify)
    (ignore regexp string start inhibit-modify)
    nil))

(unless (fboundp 'replace-regexp-in-string)
  (defun replace-regexp-in-string (regexp rep string &rest _)
    (ignore regexp rep) string))

(unless (fboundp 'replace-match)
  (defun replace-match (newtext &optional fixedcase literal string subexp)
    (ignore newtext fixedcase literal subexp) string))

(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit greedy) (ignore regexp limit greedy) nil))

;; Line / column primitives.  Phase 11.A' (2026-05-03) deleted the
;; redundant `point' / `point-min' / `point-max' / `goto-char' /
;; `forward-char' / `backward-char' nil-stubs that were shadowing
;; `emacs-buffer-builtins.el's bridges to `nelisp-ec-*' under standalone
;; NeLisp.  The line-grain stubs below remain as no-ops (= no L1.5
;; line-iterator yet).

(unless (fboundp 'forward-line)
  (defun forward-line (&optional n) (ignore n) 0))

(unless (fboundp 'beginning-of-line)
  (defun beginning-of-line (&optional n) (ignore n) nil))

(unless (fboundp 'end-of-line)
  (defun end-of-line (&optional n) (ignore n) nil))

(unless (fboundp 'line-beginning-position)
  (defun line-beginning-position (&optional n) (ignore n) 1))

(unless (fboundp 'line-end-position)
  (defun line-end-position (&optional n) (ignore n) 1))

(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos absolute) (ignore pos absolute) 1))

(unless (fboundp 'eobp)
  (defun eobp () t))

(unless (fboundp 'bobp)
  (defun bobp () t))

(unless (fboundp 'eolp)
  (defun eolp () t))

(unless (fboundp 'bolp)
  (defun bolp () t))

;; save-match-data — regex-related, only stub left after Phase 11.A'.
;; The buffer-text and save-excursion/save-restriction/with-current-buffer
;; / with-temp-buffer / narrow-to-region / widen forms moved into
;; `emacs-buffer-builtins.el' (Phase 9) and are deleted here.

(unless (fboundp 'save-match-data)
  (defmacro save-match-data (&rest body) (cons 'progn body)))





;; Syntax tables
(unless (fboundp 'standard-syntax-table)
  (defun standard-syntax-table () nil))

(unless (fboundp 'syntax-table)
  (defun syntax-table () nil))

(unless (fboundp 'set-syntax-table)
  (defun set-syntax-table (table) (ignore table) nil))

(unless (fboundp 'modify-syntax-entry)
  (defun modify-syntax-entry (char newentry &optional table) (ignore char newentry table) nil))

;; `set' is a special form in NeLisp bootstrap but appears as void-function
;; in some funcall contexts.  Polyfill by routing through `eval' + `setq'.
(unless (fboundp 'set)
  (defun set (symbol newval)
    "Polyfill: dynamic indirect setq via `eval'."
    (eval (list 'setq symbol (list 'quote newval)))
    newval))

(unless (fboundp 'eq)
  (defalias 'eq 'equal))  ;; conservative — bootstrap should have eq, but harmless

(unless (fboundp 'memql)
  (defun memql (element list)
    "Stub: like memq but uses eql."
    (let ((c list) (found nil))
      (while (and c (not found))
        (if (or (eq (car c) element) (equal (car c) element))
            (setq found c)
          (setq c (cdr c))))
      found)))

;;;; --- format / message helpers ----------------------------------------

(unless (fboundp 'format-message)
  (defun format-message (string &rest objects)
    "Stub: route through plain `format' (= no curly-quote substitution)."
    (apply #'format string objects)))

(unless (fboundp 'message)
  (defun message (format-string &rest args)
    "Stub: print to stderr via `princ' (NeLisp standalone has no echo area)."
    (let ((s (apply #'format format-string args)))
      (princ s)
      (princ "\n")
      s)))

(unless (fboundp 'error)
  (defun error (format-string &rest args)
    "Stub: signal `error' with formatted message."
    (signal 'error (list (apply #'format format-string args)))))

;;;; --- numeric primitives + bitwise ops -----------------------------------
;; Phase E (2026-05-03) extracted the `min' / `max' / `abs' / `zerop' /
;; `plusp' / `minusp' / `oddp' / `evenp' / `natnump' / `1+' / `1-'
;; numeric polyfills and the `logior' / `logand' / `logxor' / `lognot' /
;; `ash' / `lsh' bitwise polyfills into `emacs-numeric.el'.  Same
;; semantics, just promoted out of this file.


;;;; --- char.c / fns.c -----------------------------------------------------

(unless (fboundp 'clear-string)
  (defun clear-string (string) (ignore string) nil))

(unless (fboundp 'store-substring)
  (defun store-substring (string idx obj) (ignore idx obj) string))


;;;; --- display.c ----------------------------------------------------------

(unless (fboundp 'redraw-display)
  (defun redraw-display (&rest _) nil))

(unless (fboundp 'redisplay)
  (defun redisplay (&optional force) (ignore force) nil))

(unless (fboundp 'force-mode-line-update)
  (defun force-mode-line-update (&optional all) (ignore all) nil))


;;;; --- buffer.c (minimal subset; nelisp-ec-* covers the rest) ------------




(unless (fboundp 'get-buffer)
  (defun get-buffer (buffer-or-name) (ignore buffer-or-name) nil))

(unless (fboundp 'get-buffer-create)
  (defun get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
    (ignore buffer-or-name inhibit-buffer-hooks)
    (cons 'buffer nil)))


(unless (fboundp 'buffer-list)
  (defun buffer-list (&optional frame) (ignore frame) nil))


;;;; --- minor-mode helpers -------------------------------------------------

(unless (fboundp 'add-hook)
  (defun add-hook (hook function &optional depth local)
    "Stub: no-op (NeLisp standalone has no hook dispatch)."
    (ignore hook function depth local)
    nil))

(unless (fboundp 'remove-hook)
  (defun remove-hook (hook function &optional local)
    (ignore hook function local) nil))

(unless (fboundp 'run-hooks)
  (defun run-hooks (&rest hooks) (ignore hooks) nil))

(unless (fboundp 'run-hook-with-args)
  (defun run-hook-with-args (hook &rest args) (ignore hook args) nil))


;;;; --- list helpers ------------------------------------------------------

(unless (fboundp 'add-to-list)
  (defun add-to-list (list-var element &optional append compare-fn)
    "Stub: prepend (or append) ELEMENT to LIST-VAR if not already present."
    (ignore compare-fn)
    (let ((cur (and (boundp list-var) (symbol-value list-var))))
      (unless (member element cur)
        (set list-var (if append
                          (append cur (list element))
                        (cons element cur))))
      (and (boundp list-var) (symbol-value list-var)))))

(unless (fboundp 'add-to-ordered-list)
  (defun add-to-ordered-list (list-var element &optional order)
    (ignore order)
    (add-to-list list-var element)))


(provide 'emacs-stub)

;;; emacs-stub.el ends here

;;;; --- gv.el placeholder (avoid the NeLisp-eval scoping bug in real gv.el) ---

(unless (fboundp 'gv-define-expander)
  (defmacro gv-define-expander (name handler)
    "Stub: no-op (NeLisp standalone has no setf customization)."
    (ignore name handler) nil))

(unless (fboundp 'gv-define-setter)
  (defmacro gv-define-setter (name arglist &rest body)
    "Stub: no-op."
    (ignore name arglist body) nil))

(unless (fboundp 'gv-define-simple-setter)
  (defmacro gv-define-simple-setter (name setter &optional fix)
    "Stub: no-op."
    (ignore name setter fix) nil))

(unless (fboundp 'gv-letplace)
  (defmacro gv-letplace (vars place &rest body)
    "Stub: just eval BODY (= no real getter/setter binding)."
    (ignore vars place) (cons 'progn body)))

(unless (fboundp 'gv-get)
  (defun gv-get (place do)
    "Stub: invoke DO with PLACE as both getter and trivial setter."
    (funcall do place (lambda (v) (list 'setq place v)))))

(unless (fboundp 'gv-setter)
  (defun gv-setter (name)
    "Stub: synthesize setf-name symbol."
    (intern (format "(setf %s)" name))))

(unless (fboundp 'gv-ref)
  (defun gv-ref (place) place))

(unless (boundp 'defun-declarations-alist)
  (defvar defun-declarations-alist nil))
(unless (boundp 'macro-declarations-alist)
  (defvar macro-declarations-alist nil))

;; Provide gv as a feature so cl-lib's `(require 'gv)' (if any) succeeds.
(unless (featurep 'gv) (provide 'gv))



;;;; --- standard error symbols ---------------------------------------------
;; Common Emacs error symbols that subr / cl-lib / vendor code signals;
;; bootstrap eval may not have them pre-installed.

(when (fboundp 'define-error)
  (define-error 'end-of-file "End of file during parsing")
  (define-error 'end-of-buffer "End of buffer")
  (define-error 'beginning-of-buffer "Beginning of buffer")
  (define-error 'wrong-number-of-arguments "Wrong number of arguments")
  (define-error 'invalid-function "Invalid function")
  (define-error 'no-catch "No catch for tag")
  (define-error 'arith-error "Arithmetic error")
  (define-error 'range-error "Arithmetic range error")
  (define-error 'overflow-error "Arithmetic overflow error")
  (define-error 'cyclic-list "List contains a loop")
  (define-error 'circular-list "List contains a loop")
  (define-error 'permission-denied "Permission denied")
  (define-error 'file-error "File error")
  (define-error 'file-missing "File missing")
  (define-error 'file-already-exists "File already exists")
  (define-error 'json-error "JSON error")
  (define-error 'json-readtable-error "JSON readtable error")
  (define-error 'json-parse-error "JSON parse error")
  (define-error 'search-failed "Search failed")
  (define-error 'invalid-read-syntax "Invalid read syntax")
  (define-error 'user-error "User error")
  (define-error 'quit "Quit"))

;;;; --- rx.el placeholder (= regex DSL not used by anvil dispatch) ---

(unless (fboundp 'rx-define)
  (defmacro rx-define (name &rest body)
    "Stub: no-op (= NeLisp standalone uses raw regex strings)."
    (ignore name body) nil))

(unless (fboundp 'rx-let)
  (defmacro rx-let (bindings &rest body)
    "Stub: drop BINDINGS, eval BODY."
    (ignore bindings) (cons 'progn body)))

(unless (fboundp 'rx-let-eval)
  (defmacro rx-let-eval (bindings &rest body)
    (ignore bindings) (cons 'progn body)))

(unless (fboundp 'rx)
  (defmacro rx (&rest forms)
    "Stub: return empty regex string (= placeholder, never matches)."
    (ignore forms) ""))

(unless (fboundp 'rx-to-string)
  (defun rx-to-string (form &optional no-group)
    (ignore form no-group) ""))

(unless (featurep 'rx) (provide 'rx))

;;;; --- url stack pre-provide (= avoid url-vars `(append "STR" nil)` choke) ---
;; nelisp-eval requires url-parse only for cl-defstruct accessor names
;; (url-host/url-port/url-filename/url-type) when running URL retrievals.
;; FFI standalone path doesn't issue URL retrievals, so we satisfy the
;; (require 'url-parse) by pre-providing it + defining empty accessors.

(unless (fboundp 'url-host)
  (defun url-host (&rest _) nil)
  (defun url-port (&rest _) nil)
  (defun url-filename (&rest _) nil)
  (defun url-type (&rest _) nil)
  (defun url-user (&rest _) nil)
  (defun url-password (&rest _) nil)
  (defun url-target (&rest _) nil)
  (defun url-attributes (&rest _) nil)
  (defun url-fullness (&rest _) nil)
  (defun url-generic-parse-url (&rest _) nil)
  (defun url-encode-url (&rest _) nil)
  (defun url-hexify-string (&rest _) nil)
  (defun url-unhex-string (&rest _) nil)
  (defun url-retrieve-synchronously (&rest _) nil))

(unless (boundp 'url-request-method) (defvar url-request-method nil))
(unless (boundp 'url-request-extra-headers) (defvar url-request-extra-headers nil))
(unless (boundp 'url-request-data) (defvar url-request-data nil))
(unless (boundp 'url-mime-separator-chars) (defvar url-mime-separator-chars nil))
(unless (boundp 'url-bad-port-list) (defvar url-bad-port-list nil))

(unless (featurep 'url-vars) (provide 'url-vars))
(unless (featurep 'url-parse) (provide 'url-parse))
(unless (featurep 'url) (provide 'url))

;;;; --- char-or-string-p (= simple type predicate combo) ---

(unless (fboundp 'char-or-string-p)
  (defun char-or-string-p (obj)
    "Return t if OBJ is a character (= integer) or string."
    (or (integerp obj) (stringp obj))))

;;;; --- file path utility polyfills --------------------------------------

;; The bulk auto-stub returns nil for file-name-sans-extension, which
;; breaks anvil-memory--fallback-display-name when handed a basename
;; without an extension.  Real impl: strip last `.EXT' suffix, return
;; original string when no `.' present in the basename.
(defun file-name-sans-extension (filename)
  "Return FILENAME with its extension (the last `.EXT' suffix) removed.
Returns FILENAME unchanged when no extension is present."
  (cond
   ((null filename) nil)
   (t
    ;; Use string-match to find the last `.' after the last `/'.  Walk
    ;; backwards: scan from the end looking for `.', stop at `/' or
    ;; start.
    (let* ((n (length filename))
           (i (- n 1))
           (dot-pos nil))
      (while (and (>= i 0) (null dot-pos))
        (let ((c (aref filename i)))
          (cond
           ((eq c ?/) (setq i -1))            ; passed last directory sep
           ((eq c ?.) (setq dot-pos i) (setq i -1))
           (t (setq i (- i 1))))))
      (if dot-pos
          (substring filename 0 dot-pos)
        filename)))))



;;;; --- file IO (= with-temp-buffer / -file family superseded by Phase 9) -

;; Phase 9 (= `emacs-buffer-builtins.el') replaced the Phase 8 string-
;; accumulator approximations of `with-temp-buffer' / `with-temp-file' /
;; `insert' / `erase-buffer' / `buffer-string' with real `nelisp-ec'
;; buffer wrappers.  The accumulator could only host ONE active buffer
;; at a time, which broke `anvil-worklog-export-org' (= multi-buffer
;; pattern: `(generate-new-buffer ...)' inside `(with-temp-file ...)').
;; The new module honors the full upstream Emacs contract via
;; `nelisp-ec-generate-new-buffer' + `nelisp-ec-with-current-buffer' +
;; `nelisp-ec-kill-buffer' inside an `unwind-protect'.

(defun make-directory (dir &optional parents)
  "Create DIR (recursive when PARENTS non-nil)."
  (ignore parents)
  (when (fboundp 'nl-make-directory)
    (nl-make-directory dir t)))


(defun downcase (string-or-char)
  "Phase 8 polyfill: lowercase via nl-downcase Rust builtin."
  (cond
   ((null string-or-char) nil)
   ((stringp string-or-char)
    (if (fboundp 'nl-downcase) (nl-downcase string-or-char) string-or-char))
   ((integerp string-or-char)
    ;; Naive ASCII-only char downcase (= sufficient for tokenizer use cases
    ;; that go through string downcase first).
    (if (and (>= string-or-char ?A) (<= string-or-char ?Z))
        (+ string-or-char (- ?a ?A))
      string-or-char))
   (t string-or-char)))

(defun upcase (string-or-char)
  "Phase 8 polyfill: uppercase via nl-upcase."
  (cond
   ((null string-or-char) nil)
   ((stringp string-or-char)
    (if (fboundp 'nl-upcase) (nl-upcase string-or-char) string-or-char))
   ((integerp string-or-char)
    (if (and (>= string-or-char ?a) (<= string-or-char ?z))
        (- string-or-char (- ?a ?A))
      string-or-char))
   (t string-or-char)))

(defun split-string (string &optional separators omit-nulls trim)
  "Phase 8 polyfill: split STRING by SEPARATORS regexp.
Supports the common `[^[:alnum:]]+' / `[ \\t\\n]+' / nil regexp shapes
via specialized fast paths.  TRIM is accepted for API compat but only
applied when whitespace separator is given."
  (ignore trim)
  (cond
   ((null string) nil)
   ((not (stringp string)) nil)
   ((or (null separators)
        (string-empty-p separators)
        (and (stringp separators)
             (or (string-equal separators "[ \t\n]+")
                 (string-equal separators "[ \t\n\r]+")
                 (string-equal separators "[ \t\n]"))))
    ;; Whitespace split (= Emacs default when SEPARATORS is nil).
    (if (fboundp 'nl-split-by-non-alnum)
        (let ((all (nl-split-by-non-alnum string omit-nulls)))
          ;; nl-split-by-non-alnum splits on punctuation too — for
          ;; whitespace-only intent, fall back to manual split.
          (let ((parts nil) (i 0) (n (length string)) (start 0))
            (while (< i n)
              (let ((c (aref string i)))
                (when (or (eq c ?\s) (eq c ?\t) (eq c ?\n) (eq c ?\r))
                  (when (< start i)
                    (setq parts (cons (substring string start i) parts)))
                  (setq start (+ i 1))))
              (setq i (+ i 1)))
            (when (< start n)
              (setq parts (cons (substring string start n) parts)))
            (let ((rev nil))
              (while parts (setq rev (cons (car parts) rev)) (setq parts (cdr parts)))
              (if omit-nulls
                  (let ((nn nil) (cur rev))
                    (while cur (unless (string-empty-p (car cur)) (setq nn (cons (car cur) nn))) (setq cur (cdr cur)))
                    (let ((rr nil)) (while nn (setq rr (cons (car nn) rr)) (setq nn (cdr nn))) rr))
                rev))))))
   ((and (stringp separators) (string-equal separators "[^[:alnum:]]+"))
    (and (fboundp 'nl-split-by-non-alnum)
         (nl-split-by-non-alnum string omit-nulls)))
   ((and (stringp separators) (= 1 (length separators)))
    ;; Single literal char delimiter.
    (let ((sep (aref separators 0))
          (parts nil) (i 0) (n (length string)) (start 0))
      (while (< i n)
        (when (eq (aref string i) sep)
          (setq parts (cons (substring string start i) parts))
          (setq start (+ i 1)))
        (setq i (+ i 1)))
      (setq parts (cons (substring string start n) parts))
      (let ((rev nil))
        (while parts (setq rev (cons (car parts) rev)) (setq parts (cdr parts)))
        (if omit-nulls
            (let ((nn nil) (cur rev))
              (while cur (unless (string-empty-p (car cur)) (setq nn (cons (car cur) nn))) (setq cur (cdr cur)))
              (let ((rr nil)) (while nn (setq rr (cons (car nn) rr)) (setq nn (cdr nn))) rr))
          rev))))
   (t
    ;; Unknown regex — fall back to single string return (= no split).
    (list string))))

(defun string-to-number (string &optional base)
  "Phase 6 polyfill: parse STRING as decimal integer.
BASE optional (default 10).  Negative strings supported.  Returns 0
for unparseable input (= matches Emacs semantics).  Float parsing is
limited to integer truncation when no `.' is present."
  (ignore base)
  (cond
   ((not (stringp string)) 0)
   ((= 0 (length string)) 0)
   (t
    (let ((sign 1)
          (i 0)
          (n (length string))
          (acc 0)
          (saw-digit nil))
      (when (and (> n 0) (eq (aref string 0) ?-))
        (setq sign -1)
        (setq i 1))
      (when (and (> n i) (eq (aref string i) ?+))
        (setq i (+ i 1)))
      (while (and (< i n)
                  (let ((c (aref string i)))
                    (and (>= c ?0) (<= c ?9))))
        (setq acc (+ (* acc 10) (- (aref string i) ?0)))
        (setq saw-digit t)
        (setq i (+ i 1)))
      (if saw-digit (* sign acc) 0)))))

(defun system-name ()
  "Phase 6 polyfill: return host name.
Falls back to `localhost' when neither `nl-system-name' (= future
builtin) nor the HOSTNAME env var is available.  worklog-add uses
this to scope per-host log files."
  (or (and (fboundp 'nl-system-name) (nl-system-name))
      (and (fboundp 'getenv) (getenv "HOSTNAME"))
      "localhost"))

(defun system-type ()
  "Phase 6 polyfill: return system-type symbol.
Reads the `system-type' variable seeded by anvil-runtime's
`seed_host_constants' (= gnu/linux on Linux, darwin on macOS,
windows-nt on Windows)."
  (if (boundp 'system-type) system-type 'gnu/linux))

(defun format-time-string (format-string &optional time zone)
  "Phase 6 polyfill: format Unix epoch via nl-format-unix-time.
TIME may be nil (= current time), an integer (= unix epoch), or a list
whose `car' is a Unix epoch integer (= the Phase 6 simplified
`current-time' shape).  ZONE is accepted for API compat but ignored
(= always UTC; anvil callsites use %Y-%m-%d which is timezone-stable
within a single day for our purposes)."
  (ignore zone)
  (let ((epoch (cond
                ((null time) (float-time))
                ((integerp time) time)
                ((floatp time) (truncate time))
                ((listp time) (or (car time) (float-time)))
                (t (float-time)))))
    (if (fboundp 'nl-format-unix-time)
        (nl-format-unix-time format-string epoch)
      (number-to-string epoch))))

(defun secure-hash (algorithm object &optional start end binary)
  "Compute the hash of OBJECT under ALGORITHM symbol.
START / END / BINARY are accepted for API compat but only the
(algo string) two-argument shape is wired up."
  (ignore start end binary)
  (cond
   ((not (fboundp 'nl-secure-hash))
    (error "secure-hash: nl-secure-hash builtin not available"))
   ((not (stringp object))
    (error "secure-hash: OBJECT must be a string (Phase 6 limitation)"))
   (t (nl-secure-hash algorithm object))))


;;;; --- terminal/IO no-op stubs (= avoid void-function during process load) ---

(unless (fboundp 'send-string-to-terminal)
  (defun send-string-to-terminal (s &optional terminal)
    (ignore terminal)
    (when (stringp s) (princ s)) nil))
(unless (fboundp 'discard-input) (defun discard-input () nil))
(unless (fboundp 'open-termscript) (defun open-termscript (&rest _) nil))
(unless (fboundp 'set-input-method) (defun set-input-method (&rest _) nil))

;;;; --- timers no-op stubs (= no scheduler in NeLisp standalone yet) ---

(unless (fboundp 'run-at-time)
  (defun run-at-time (&rest _) nil))
(unless (fboundp 'run-with-timer)
  (defun run-with-timer (&rest _) nil))
(unless (fboundp 'run-with-idle-timer)
  (defun run-with-idle-timer (&rest _) nil))
(unless (fboundp 'cancel-timer)
  (defun cancel-timer (&rest _) nil))
(unless (fboundp 'cancel-function-timers)
  (defun cancel-function-timers (&rest _) nil))
(unless (fboundp 'timerp)
  (defun timerp (_) nil))
(unless (fboundp 'timer-create)
  (defun timer-create (&rest _) nil))
(unless (fboundp 'timer-set-time)
  (defun timer-set-time (&rest _) nil))
(unless (fboundp 'timer-set-function)
  (defun timer-set-function (&rest _) nil))
(unless (fboundp 'timer-activate)
  (defun timer-activate (&rest _) nil))
(unless (fboundp 'sit-for)
  (defun sit-for (&rest _) nil))
(unless (fboundp 'sleep-for)
  (defun sleep-for (&rest _) nil))
(unless (boundp 'timer-list) (defvar timer-list nil))
(unless (boundp 'timer-idle-list) (defvar timer-idle-list nil))
