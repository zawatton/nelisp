;;; emacs-parity-skk.el --- ddskk real-load parity fixes -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fix (b1k21 frontier).  In the standalone NeLisp
;; self-host runtime, the user init's `(require 'ddskk)' / `(require 'skk)'
;; was historically short-circuited by a `site-elpa.el' bypass (four bogus
;; `provide' forms) put in place while the ddskk deep-load still segfaulted.
;; The evaluator memory-safety work (bf_aset / GC) removed that segfault:
;; runtime-verified on the b1k21 binary, `skk.el' and every ddskk submodule
;; (`skk-vars', `skk-macs', `ccc', `skk-cdb', `skk-emacs', `skk-cursor',
;; `skk-version', `skk-cus', `skk-comp', `skk-tankan', `ddskk') load top to
;; bottom with rc=0 and `skk-mode' fboundp/callable.
;;
;; What remained after the crash was gone was a set of ~115 catchable
;; `void-function' / `void-variable' errors emitted while ddskk loads.  They
;; are NOT skk-internal symbols (those are left to skk's own source, per the
;; parity rule): every one is an EXTERNAL core/`custom'/`faces'/`mule'
;; dependency that the reduced standalone runtime does not yet ship.  Marker
;; localisation (bootstrap replay + `skk-repro' step harness, run against the
;; b1k21 binary; cross-checked against the full-init audit stderr `err-c9'
;; where each of these symbols has ZERO occurrences with skk bypassed) pinned
;; the roots below.  Each is filled with its faithful Emacs 30.1 definition
;; or default; nothing here fakes a skk-owned symbol.
;;
;; ROOT 1 -- `documentation-stringp' (62 of the errors, the largest class).
;;   `custom-declare-face' (cus-face.el:35, reached by every ddskk `defface')
;;   does `(when (and doc (not (documentation-stringp doc))) (error ...))'.
;;   `documentation-stringp' is a C subr in Emacs 30.1 and is absent from the
;;   runtime, so every ddskk `defface' (35 in skk-vars.el alone, ~62 across
;;   the loaded custom/skk files) raised `void-function'.  FIX: the faithful
;;   C predicate -- a docstring object is a string, a fixnum (offset into the
;;   DOC file), or a cons of a string with a fixnum/string reference.
;;
;; ROOT 2 -- `window-system' / `frame-background-mode' (the skk-* var cascade).
;;   These two standard variables are void in the runtime.  ddskk reads them
;;   inside `defcustom' :STANDARD default forms: `skk-background-mode'
;;   (skk-vars.el:237) computes `(or frame-background-mode (cond ((and
;;   window-system ...))))', and `skk-use-face' (skk-vars.el:1982) is
;;   `(or window-system ...)'.  With the variables void the default form
;;   errors, so the `defcustom' never binds the skk variable, which then
;;   cascades into `void-variable: skk-background-mode' (read by the
;;   `skk-cursor-*-color' defaults) and `void-variable: skk-use-face'.
;;   FIX: provide both as their faithful batch defaults (nil).  This is the
;;   root that makes skk's OWN defcustoms bind their OWN variables -- we do
;;   not shim any skk-* symbol.
;;
;; ROOT 3 -- `charsetp' + `charset-list' (skk-tankan radical vector).
;;   `skk-tankan.el:121' guards optional JIS X 0213 glyph substitution with
;;   `(when (charsetp 'japanese-jisx0213-1) (... (make-char 'japanese-jisx0213-1 ...)))'.
;;   With `charsetp' void the guard itself errored and left
;;   `skk-tankan-radical-vector' unset.  The runtime has no mule charset
;;   registry for these Japanese charsets, so the FAITHFUL answer is that
;;   they are absent: `charsetp' consults `charset-list' (the runtime's
;;   representable set -- ascii/eight-bit/unicode) and returns nil for
;;   jisx0213, so skk-tankan keeps its base radical vector and never calls
;;   the unsupported `make-char'.  (`skk-setup-charset-list', skk.el:354,
;;   runs at skk-mode activation, not load, and likewise degrades cleanly.)
;;
;; ROOT 4 -- `internal-lisp-face-attribute-values' (faces.el widget values).
;;   `faces.el:1217/1221/1225' calls this C subr for the boolean-ish face
;;   attributes when a custom face widget is built.  FIX: the faithful C
;;   enumeration -- `(t nil)' for :underline/:overline/:strike-through/
;;   :inverse-video/:extend, nil for the numeric/other attributes (verified
;;   against Emacs 30.1).
;;
;; ROOT 5 -- residual `custom'/minibuffer/startup variables and `cus-edit'
;;   commands referenced while `custom'/`cus-edit'/`wid-edit' load under
;;   ddskk: `custom-file', `custom-raised-buttons', `custom-field-keymap',
;;   `custom-dirlocals-field-map', `minibuffer-prompt-properties',
;;   `read-file-name-completion-ignore-case', `after-init-time', `dump-mode',
;;   and the `customize-option' / `customize-option-other-window' /
;;   `customize-unsaved' commands.  Each is provided as its faithful Emacs
;;   30.1 default (values checked against the host oracle); the customize
;;   commands delegate to the canonical `customize-variable' family, of which
;;   `customize-option' is literally the primary name in cus-edit.el.
;;
;; Every definition here is `unless'-guarded (installed only when the symbol
;; is genuinely absent) and the activation block is gated on
;; `emacs-parity-skk--standalone-p', so host Emacs -- which ships all of
;; these as C subrs / real defvars -- is left completely untouched.

;;; Code:

(defconst emacs-parity-skk--standalone-p
  (fboundp 'nelisp--eval-source-string)
  "Non-nil only inside the standalone NeLisp self-host runtime.
Guards activation so host Emacs is left untouched (it already provides
`documentation-stringp', `charsetp', `window-system', and the rest as C
subrs / real defvars).")

;; --- ROOT 3 helper: faithful `charsetp' predicate ------------------------
;; Defined unconditionally (inert on host, where the real subr shadows it via
;; the `unless' guard below) so the symbol is always resolvable in the image.
(defun emacs-parity-skk--charsetp (object)
  "Return non-nil if OBJECT names a charset the runtime can represent.
Faithful to the runtime's actual capability: consults `charset-list'
\(the representable set), so charsets with no runtime backing -- e.g. the
JIS X 0213 charsets ddskk probes optionally -- correctly report absent."
  (and (symbolp object)
       (boundp 'charset-list)
       (memq object charset-list)
       t))

;; --- ROOT 1 helper: faithful `documentation-stringp' ---------------------
(defun emacs-parity-skk--documentation-stringp (object)
  "Return non-nil if OBJECT is a well-formed docstring object.
Faithful port of the Emacs 30.1 C subr: a docstring is a string, a fixnum
\(a byte offset into the DOC file), or a cons of a string (a .elc/DOC
reference) with a fixnum or string tail."
  (or (stringp object)
      (integerp object)
      (and (consp object)
           (stringp (car object))
           (or (integerp (cdr object)) (stringp (cdr object))))))

;; --- ROOT 4 helper: faithful `internal-lisp-face-attribute-values' -------
(defun emacs-parity-skk--face-attribute-values (attribute)
  "Return the display-independent value list for face ATTRIBUTE.
Faithful to the Emacs 30.1 C subr: the boolean-ish attributes enumerate
to `(t nil)'; the numeric/font attributes return nil (their values come
from the font tables / colour list, handled elsewhere in faces.el)."
  (if (memq attribute '(:underline :overline :strike-through
                        :inverse-video :extend))
      (list t nil)
    nil))

;; --- ROOT 6 helpers: vendor faces.el C-backend bridge --------------------
;; ddskk pulls in vendor `faces.el' (through `skk-cus' -> `cus-edit' ->
;; `(require 'faces)'; the runtime provides its own face system only under
;; the `emacs-faces-*' names and `(provide 'emacs-faces)', so the vendor file
;; still loads).  Vendor `faces.el' expects the xfaces.c primitives, none of
;; which exist in the standalone runtime -- `facep' (faces.el:268) calls
;; `internal-lisp-face-p', `make-face' (faces.el:214) calls
;; `internal-make-lisp-face', and `set-face-attribute' calls
;; `internal-set-lisp-face-attribute' -- so every ddskk `defface' aborts at
;; `custom-declare-face' -> `face-spec-set' -> `make-empty-face' -> `facep'.
;; Bridge each missing primitive to the runtime's real `emacs-faces-*'
;; registry backend so `defface' actually registers the face and its
;; attributes instead of aborting.  These are NOT no-ops: they route to the
;; live `emacs-redisplay--face-registry' face store.
(defun emacs-parity-skk--internal-lisp-face-p (face &optional _frame)
  "Bridge `internal-lisp-face-p' to the runtime `emacs-faces' registry."
  (and (fboundp 'emacs-faces-facep) (emacs-faces-facep face) t))

(defun emacs-parity-skk--internal-make-lisp-face (face &optional _frame)
  "Bridge `internal-make-lisp-face' to `emacs-faces-make-face'."
  (if (fboundp 'emacs-faces-make-face) (emacs-faces-make-face face) face))

(defun emacs-parity-skk--internal-lisp-face-empty-p (face &optional _frame)
  "Bridge `internal-lisp-face-empty-p': a registered face is non-empty."
  (not (and (fboundp 'emacs-faces-facep) (emacs-faces-facep face))))

(defun emacs-parity-skk--internal-set-lisp-face-attribute
    (face attribute value &optional _frame)
  "Bridge `internal-set-lisp-face-attribute' to `emacs-faces-set-attribute'."
  (when (fboundp 'emacs-faces-set-attribute)
    (emacs-faces-set-attribute face nil attribute value))
  value)

(defun emacs-parity-skk--internal-get-lisp-face-attribute
    (face attribute &optional _frame)
  "Bridge `internal-get-lisp-face-attribute' to `emacs-faces-attribute'."
  (if (fboundp 'emacs-faces-attribute)
      (emacs-faces-attribute face attribute)
    'unspecified))

(defun emacs-parity-skk--internal-merge-in-global-face (face &optional _frame)
  "Bridge `internal-merge-in-global-face': the registry has no per-frame split."
  face)

;; --- ROOT 7 helpers: custom.el keyword handlers --------------------------
;; Vendor `cus-face.el' loads (`skk-cus' -> `cus-edit' -> `(require 'cus-face)';
;; the bundled minimal `custom' does not `provide' `cus-face') and CLOBBERS
;; the bundled `custom-declare-face' with the full version.  That version, at
;; the tail of every ddskk `defface', calls `(custom-handle-all-keywords face
;; args 'custom-face)'.  In Emacs 30.1 `custom-handle-all-keywords' /
;; `custom-handle-keyword' are C subrs, absent here, so every ddskk `defface'
;; aborted at the keyword-handling step.  These are pure-Lisp handlers
;; (verbatim behaviour of the pre-C-promotion `custom.el' definitions), plus
;; the `custom-add-*' property recorders they dispatch to that the bundled
;; `custom' does not already ship.  No display/C backend is involved.
(defun emacs-parity-skk--custom-add-to-group (group option widget)
  "Add OPTION (of custom type WIDGET) to customization GROUP."
  (let ((members (get group 'custom-group))
        (entry (list option widget)))
    (unless (member entry members)
      (put group 'custom-group (nconc members (list entry))))))

(defun emacs-parity-skk--custom-add-version (symbol version)
  "Record that SYMBOL was first introduced in Emacs VERSION."
  (put symbol 'custom-version version))

(defun emacs-parity-skk--custom-add-package-version (symbol version)
  "Record SYMBOL's introducing package VERSION."
  (put symbol 'custom-package-version version))

(defun emacs-parity-skk--custom-add-link (symbol widget)
  "Add external link WIDGET to customization option SYMBOL."
  (let ((links (get symbol 'custom-links)))
    (unless (member widget links)
      (put symbol 'custom-links (append links (list widget))))))

(defun emacs-parity-skk--custom-add-dependencies (symbol dependencies)
  "Record that SYMBOL should be set after each of DEPENDENCIES."
  (let ((deps (get symbol 'custom-dependencies)))
    (while dependencies
      (unless (member (car dependencies) deps)
        (setq deps (cons (car dependencies) deps)))
      (setq dependencies (cdr dependencies)))
    (put symbol 'custom-dependencies deps)))

(defun emacs-parity-skk--custom-handle-keyword (symbol keyword value type)
  "Handle one custom KEYWORD/VALUE pair for option SYMBOL of TYPE.
Verbatim behaviour of the Emacs `custom.el' Lisp handler."
  (cond ((eq keyword :group) (custom-add-to-group value symbol type))
        ((eq keyword :version) (custom-add-version symbol value))
        ((eq keyword :package-version)
         (custom-add-package-version symbol value))
        ((eq keyword :link) (custom-add-link symbol value))
        ((eq keyword :load) (custom-add-load symbol value))
        ((eq keyword :tag) (put symbol 'custom-tag value))
        ((eq keyword :set-after) (custom-add-dependencies symbol value))
        (t (error "Unknown keyword %s" keyword))))

(defun emacs-parity-skk--custom-handle-all-keywords (symbol keywords type)
  "Handle the KEYWORDS argument list for custom option SYMBOL of TYPE."
  (while keywords
    (let ((keyword (car keywords)))
      (setq keywords (cdr keywords))
      (unless (symbolp keyword)
        (error "Junk in keyword list for symbol `%s'" symbol))
      (unless keywords
        (error "Keyword %s is missing an argument" keyword))
      (let ((value (car keywords)))
        (setq keywords (cdr keywords))
        (custom-handle-keyword symbol keyword value type)))))

;; --- ROOT 5 helpers: faithful cus-edit command delegation ----------------
(defun emacs-parity-skk--customize-option (symbol)
  "Customize the user option SYMBOL.
`customize-option' is the primary name of `customize-variable' in
cus-edit.el; delegate to whichever canonical entry point exists."
  (interactive
   (list (if (fboundp 'custom-variable-prompt)
             (car (custom-variable-prompt))
           (intern (read-string "Customize option: ")))))
  (cond ((fboundp 'customize-variable) (customize-variable symbol))
        (t (signal 'void-function '(customize-variable)))))

(defun emacs-parity-skk--customize-option-other-window (symbol)
  "Customize the user option SYMBOL in another window."
  (interactive
   (list (if (fboundp 'custom-variable-prompt)
             (car (custom-variable-prompt))
           (intern (read-string "Customize option: ")))))
  (cond ((fboundp 'customize-variable-other-window)
         (customize-variable-other-window symbol))
        ((fboundp 'customize-variable) (customize-variable symbol))
        (t (signal 'void-function '(customize-variable)))))

(defun emacs-parity-skk--customize-unsaved ()
  "Customize all options and faces set outside Customize but not saved.
Delegates to `customize-unsaved-options' when available (the modern
alias target), otherwise to `customize-customized'."
  (interactive)
  (cond ((fboundp 'customize-unsaved-options) (customize-unsaved-options))
        ((fboundp 'customize-customized) (customize-customized))
        (t nil)))

;; --- ROOT 2 + ROOT 5 variables ------------------------------------------
;; These must be installed as GLOBALLY-SPECIAL variables.  ddskk loads under
;; the NeLisp source evaluator with lexical-binding on, and a free reference
;; such as ddskk's `(defcustom skk-use-face (or window-system ...))' default
;; only resolves when the symbol is `special-variable-p'.  Runtime probing
;; showed that a `defvar' evaluated by the source evaluator (the evaluator a
;; plain `load' of THIS file uses) sets the value cell but does NOT mark the
;; symbol special: `(boundp 'window-system)' returned t yet
;; `(special-variable-p 'window-system)' stayed nil and every read of
;; `window-system' still raised `void-variable', which is exactly what aborts
;; `skk-background-mode' / `skk-use-face' and cascades into their downstream
;; `void-variable' errors.  The bundled bootstrap installs its own globals
;; through `nelisp--eval-source-string' (the FULL evaluator, whose `defvar'
;; DOES register global special-ness), so mirror that here.  Each is only
;; installed when genuinely absent (`special-variable-p' guard), so nothing
;; already provided by the runtime is disturbed.  Values are faithful Emacs
;; 30.1 defaults checked against the host oracle.
(defconst emacs-parity-skk--special-vars
  '(;; ROOT 2 -- the two roots of the skk-* variable cascade.
    ("window-system" . "nil")
    ("frame-background-mode" . "nil")
    ;; ROOT 3 -- representable charset set consulted by `charsetp'.
    ("charset-list" . "'(ascii eight-bit unicode)")
    ;; ROOT 6 -- vendor `make-face-x-resource-internal' skips X-resource
    ;; application when this is non-nil.  Faithful batch default is t (no X
    ;; display), which turns the per-`defface' X-resource pass into a no-op.
    ("inhibit-x-resources" . "t")
    ;; ROOT 5 -- custom/minibuffer/startup vars pulled in via cus-edit /
    ;; wid-edit / cus-start.
    ("custom-file" . "nil")
    ("custom-raised-buttons" . "nil")
    ("minibuffer-prompt-properties" . "'(read-only t face minibuffer-prompt)")
    ("read-file-name-completion-ignore-case" . "nil")
    ("after-init-time" . "nil")
    ("dump-mode" . "nil")
    ("custom-dirlocals-field-map" . "(make-sparse-keymap)")
    ("custom-field-keymap"
     . "(if (and (boundp 'widget-field-keymap) (keymapp widget-field-keymap)) (copy-keymap widget-field-keymap) (make-sparse-keymap))"))
  "Alist of (VAR-NAME . INIT-STRING) provided as globally-special defvars.
See the commentary above for why these go through the full evaluator.
Installed only in the standalone runtime; host Emacs already binds every
one of these, so this file leaves the host untouched.")

;; --- Activation: standalone runtime only ---------------------------------
(when emacs-parity-skk--standalone-p

  ;; ROOT 2 + ROOT 3 + ROOT 5 variables -- install as globally-special
  ;; defvars through the full evaluator (see commentary above).
  (dolist (cell emacs-parity-skk--special-vars)
    (unless (special-variable-p (intern (car cell)))
      (nelisp--eval-source-string
       (format "(defvar %s %s)" (car cell) (cdr cell)))))

  ;; ROOT 1
  (unless (fboundp 'documentation-stringp)
    (defalias 'documentation-stringp #'emacs-parity-skk--documentation-stringp))

  ;; ROOT 3 -- faithful charsetp (charset-list is the top-level defvar above).
  (unless (fboundp 'charsetp)
    (defalias 'charsetp #'emacs-parity-skk--charsetp))

  ;; ROOT 4
  (unless (fboundp 'internal-lisp-face-attribute-values)
    (defalias 'internal-lisp-face-attribute-values
      #'emacs-parity-skk--face-attribute-values))

  ;; ROOT 6 -- vendor faces.el C-backend bridged to the runtime emacs-faces.
  (unless (fboundp 'internal-lisp-face-p)
    (defalias 'internal-lisp-face-p
      #'emacs-parity-skk--internal-lisp-face-p))
  (unless (fboundp 'internal-make-lisp-face)
    (defalias 'internal-make-lisp-face
      #'emacs-parity-skk--internal-make-lisp-face))
  (unless (fboundp 'internal-lisp-face-empty-p)
    (defalias 'internal-lisp-face-empty-p
      #'emacs-parity-skk--internal-lisp-face-empty-p))
  (unless (fboundp 'internal-set-lisp-face-attribute)
    (defalias 'internal-set-lisp-face-attribute
      #'emacs-parity-skk--internal-set-lisp-face-attribute))
  (unless (fboundp 'internal-get-lisp-face-attribute)
    (defalias 'internal-get-lisp-face-attribute
      #'emacs-parity-skk--internal-get-lisp-face-attribute))
  (unless (fboundp 'internal-merge-in-global-face)
    (defalias 'internal-merge-in-global-face
      #'emacs-parity-skk--internal-merge-in-global-face))

  ;; ROOT 7 -- custom.el keyword handlers (C subrs upstream) + the
  ;; `custom-add-*' recorders they dispatch to that the bundled custom lacks.
  (unless (fboundp 'custom-add-to-group)
    (defalias 'custom-add-to-group #'emacs-parity-skk--custom-add-to-group))
  (unless (fboundp 'custom-add-version)
    (defalias 'custom-add-version #'emacs-parity-skk--custom-add-version))
  (unless (fboundp 'custom-add-package-version)
    (defalias 'custom-add-package-version
      #'emacs-parity-skk--custom-add-package-version))
  (unless (fboundp 'custom-add-link)
    (defalias 'custom-add-link #'emacs-parity-skk--custom-add-link))
  (unless (fboundp 'custom-add-dependencies)
    (defalias 'custom-add-dependencies
      #'emacs-parity-skk--custom-add-dependencies))
  (unless (fboundp 'custom-handle-keyword)
    (defalias 'custom-handle-keyword
      #'emacs-parity-skk--custom-handle-keyword))
  (unless (fboundp 'custom-handle-all-keywords)
    (defalias 'custom-handle-all-keywords
      #'emacs-parity-skk--custom-handle-all-keywords))

  ;; ROOT 5 commands
  (unless (fboundp 'customize-option)
    (defalias 'customize-option #'emacs-parity-skk--customize-option))
  (unless (fboundp 'customize-option-other-window)
    (defalias 'customize-option-other-window
      #'emacs-parity-skk--customize-option-other-window))
  (unless (fboundp 'customize-unsaved)
    (defalias 'customize-unsaved #'emacs-parity-skk--customize-unsaved)))

(provide 'emacs-parity-skk)

;;; emacs-parity-skk.el ends here
