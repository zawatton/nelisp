;;; emacs-faces.el --- Face attribute API (Track F)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track F (2026-05-03) — Layer 2.
;;
;; Substrate for the user-facing face API.  Shares the face registry
;; variable with `emacs-redisplay', but does not require redisplay at
;; load time: batch/bootstrap paths need face definitions without paying
;; the full renderer load cost.
;;
;; The data model:
;; - Each face is a symbol whose attribute plist lives in
;;   `emacs-redisplay--face-registry'.
;; - Custom themes follow Emacs's storage model closely enough for
;;   standalone init.el use: theme declarations and settings live on the
;;   theme symbol, and enabled face specs live on each face symbol's
;;   `theme-face' property.
;; - Attribute keys are Emacs-standard keywords:
;;     :foreground :background :weight :slant :underline
;;     :overline :strike-through :inverse-video :inherit :family
;;     :height :box.
;; - Unset attributes return the symbol `unspecified' (= matches
;;   `face-attribute' contract).
;;
;; The bridge layer (`emacs-faces-builtins') exposes the
;; conventional unprefixed names — `face-attribute',
;; `set-face-attribute', `face-foreground', `defface' (macro),
;; etc. — gated on `unless (fboundp ...)' so loading inside a
;; host Emacs is a no-op.
;;
;; Out of scope (= deferred to later γ phases): frame parameter
;; integration, X-resource fallback, `face-spec-set-2'-style display-class
;; precedence (= only the catch-all entry is honoured).

;;; Code:

(require 'cl-lib)

(defvar emacs-redisplay--face-registry (make-hash-table :test 'eq)
  "Shared face registry used by `emacs-faces' and `emacs-redisplay'.")

(defvar emacs-redisplay--face-cache (make-hash-table :test 'equal)
  "Shared realized-face cache used when `emacs-redisplay' is loaded.")

(unless (fboundp 'emacs-redisplay-face-cache-clear)
  (defun emacs-redisplay-face-cache-clear ()
    "Clear the shared face realization cache."
    (clrhash emacs-redisplay--face-cache)))

(define-error 'emacs-faces-error "Face error")

;;;; --- predicates / lifecycle -----------------------------------------

(defconst emacs-faces--unset (make-symbol "emacs-faces--unset")
  "Sentinel returned by `gethash' when a face is not registered.")

(defun emacs-faces-facep (x)
  "Return X (= face symbol) if X names a registered face, else nil.
A face with no attributes (= empty plist) still counts as registered;
we distinguish via a `gethash' sentinel rather than the value, since
the empty plist is `nil'."
  (and (symbolp x)
       (not (eq emacs-faces--unset
                (gethash x emacs-redisplay--face-registry
                         emacs-faces--unset)))
       x))

(defun emacs-faces-make-face (name)
  "Define a new face NAME with no attributes (= empty plist).
Returns NAME.  Idempotent: re-registering an existing face is a
no-op; existing attributes are preserved."
  (unless (symbolp name)
    (signal 'wrong-type-argument (list 'symbolp name)))
  (when (eq emacs-faces--unset
            (gethash name emacs-redisplay--face-registry
                     emacs-faces--unset))
    (puthash name nil emacs-redisplay--face-registry))
  name)

;;;; --- attribute accessors --------------------------------------------

(defun emacs-faces--plist-attr (plist attribute)
  "Return ATTRIBUTE from PLIST, or `unspecified' when absent."
  (if (and plist (plist-member plist attribute))
      (plist-get plist attribute)
    'unspecified))

(defun emacs-faces--theme-face-spec (face)
  "Return the active theme face spec for FACE, or nil."
  (let ((settings (get face 'theme-face))
        spec)
    (dolist (theme custom-enabled-themes)
      (when (and (null spec) (assq theme settings))
        (setq spec (cdr (assq theme settings)))))
    spec))

(defun emacs-faces--direct-attribute (face attribute)
  "Return FACE's direct ATTRIBUTE after enabled themes and defaults."
  (let* ((theme-spec (emacs-faces--theme-face-spec face))
         (theme-plist (and theme-spec
                           (emacs-faces--default-attrs-from-spec theme-spec)))
         (registry-plist (and (boundp 'emacs-redisplay--face-registry)
                              (gethash face emacs-redisplay--face-registry)))
         (theme-value (emacs-faces--plist-attr theme-plist attribute)))
    (if (not (eq theme-value 'unspecified))
        theme-value
      (emacs-faces--plist-attr registry-plist attribute))))

(defun emacs-faces--attribute-recursive (face attribute inherit depth)
  "Resolve FACE ATTRIBUTE, following :inherit when INHERIT is non-nil."
  (if (> depth 16)
      'unspecified
    (let ((value (emacs-faces--direct-attribute face attribute)))
      (if (or (not (eq value 'unspecified)) (not inherit))
          value
        (let ((parent (emacs-faces--direct-attribute face :inherit))
              found)
          (cond
           ((symbolp parent)
            (emacs-faces--attribute-recursive
             parent attribute inherit (1+ depth)))
           ((consp parent)
            (dolist (candidate parent)
              (when (and (eq found nil) (symbolp candidate))
                (let ((candidate-value
                       (emacs-faces--attribute-recursive
                        candidate attribute inherit (1+ depth))))
                  (unless (eq candidate-value 'unspecified)
                    (setq found candidate-value)))))
            (or found 'unspecified))
           (t 'unspecified)))))))

(defun emacs-faces-attribute (face attribute &optional _frame inherit)
  "Return FACE's ATTRIBUTE, or `unspecified' if not set.
Enabled custom themes take precedence over the `defface' default
stored in the local registry.  When INHERIT is non-nil, an
unspecified attribute follows FACE's `:inherit' chain."
  (emacs-faces--attribute-recursive face attribute inherit 0))

(defun emacs-faces-set-attribute (face _frame &rest props)
  "Update FACE's plist with PROPS (= alternating keyword/value).
Returns FACE.  Invalidates the realization cache so subsequent
lookups via `emacs-redisplay-realize-face' see the new state."
  (unless (symbolp face)
    (signal 'wrong-type-argument (list 'symbolp face)))
  (unless (zerop (mod (length props) 2))
    (signal 'emacs-faces-error
            (list 'odd-length-attribute-list props)))
  (emacs-faces-make-face face)
  (let ((plist (gethash face emacs-redisplay--face-registry)))
    (while props
      (let ((k (car props))
            (v (cadr props)))
        (setq plist (plist-put plist k v)))
      (setq props (cddr props)))
    (puthash face plist emacs-redisplay--face-registry)
    (emacs-redisplay-face-cache-clear)
    face))

;;;; --- convenience accessors -----------------------------------------

(defun emacs-faces-foreground (face &optional frame inherit)
  "Return FACE's :foreground, or nil when unspecified."
  (let ((v (emacs-faces-attribute face :foreground frame inherit)))
    (and (not (eq v 'unspecified)) v)))

(defun emacs-faces-background (face &optional frame inherit)
  "Return FACE's :background, or nil when unspecified."
  (let ((v (emacs-faces-attribute face :background frame inherit)))
    (and (not (eq v 'unspecified)) v)))

(defun emacs-faces-set-foreground (face color &optional frame)
  "Set FACE's :foreground to COLOR."
  (emacs-faces-set-attribute face frame :foreground color))

(defun emacs-faces-set-background (face color &optional frame)
  "Set FACE's :background to COLOR."
  (emacs-faces-set-attribute face frame :background color))

;;;; --- enumeration ----------------------------------------------------

(defun emacs-faces-list ()
  "Return all registered face names as a list (= unsorted)."
  (let ((out nil))
    (maphash (lambda (k _v) (push k out))
             emacs-redisplay--face-registry)
    out))

;;;; --- defface macro --------------------------------------------------

(defun emacs-faces--entry-attrs (entry)
  "Return the attribute plist stored in a face spec ENTRY.
Emacs accepts both `(t :foreground \"red\")' and
`(t (:foreground \"red\"))' shapes in `defface' specs.  The
substrate stores the normalized flat plist."
  (let ((attrs (cdr entry)))
    (if (and (= (length attrs) 1)
             (listp (car attrs)))
        (car attrs)
      attrs)))

(defun emacs-faces--default-attrs-from-spec (spec)
  "Extract a flat attribute plist from a SPEC value.

SPEC is the value handed to `defface' — a list of entries
`(DISPLAY . ATTRS)' where ATTRS is a flat plist.  We honour:

  default     →  always-applied attributes
  t           →  catch-all
  (((class color))) etc. → conditional (= ignored for MVP, only
                            checked if no t / default entry)

Returns a flat plist or nil."
  (let ((entries (cond
                  ((and (consp spec) (eq (car spec) 'quote))
                   (cadr spec))
                  ((listp spec) spec)
                  (t nil)))
        (default-entry nil)
        (t-entry nil)
        (first-entry nil))
    (dolist (e entries)
      (when (consp e)
        (cond
         ((eq (car e) 'default) (setq default-entry e))
         ((eq (car e) t)        (setq t-entry e))
         ((null first-entry)    (setq first-entry e)))))
    (let ((entry (or default-entry t-entry first-entry)))
      (and entry (emacs-faces--entry-attrs entry)))))

(defmacro emacs-faces-defface (name spec _doc &rest _opts)
  "Register face NAME with the SPEC's catch-all attributes.
DOC and OPTS (= :group / :version / :package-version) are
accepted for API parity but ignored in the MVP."
  (let ((attrs (emacs-faces--default-attrs-from-spec spec)))
    `(progn
       (emacs-faces-make-face ',name)
       ,@(when attrs
           `((apply #'emacs-faces-set-attribute
                    ',name nil ',attrs)))
	 ',name)))

;;;; --- Custom theme subset -----------------------------------------------

(defvar custom-known-themes '(user)
  "Loaded custom theme names known to the standalone face substrate.")

(defvar custom-enabled-themes nil
  "Enabled custom themes, highest precedence first.")

(defvar custom-theme-directory
  (or (and (boundp 'user-emacs-directory) user-emacs-directory)
      "~/.emacs.d/")
  "Default user directory for standalone custom theme files.")

(defvar custom-theme-load-path (list 'custom-theme-directory t)
  "Directories searched for THEME-theme.el files.")

(defvar custom-safe-themes '(default)
  "Standalone theme safety policy.
The current standalone loader cannot prompt or persist hashes, so callers
should pass NO-CONFIRM to `load-theme' from init.el or set this to t.")

(defvar custom--inhibit-theme-enable nil
  "When non-nil, theme setters record settings without applying them.")

(defvar data-directory nil
  "Compatibility variable for built-in data lookup.")

(defun custom-theme-name-valid-p (name)
  "Return non-nil when NAME is a valid Custom theme name."
  (and (symbolp name)
       (not (memq name '(nil user changed)))
       (not (equal (symbol-name name) ""))))

(defun custom-make-theme-feature (theme)
  "Return THEME's conventional THEME-theme feature symbol."
  (intern (concat (symbol-name theme) "-theme")))

(defun custom-declare-theme (theme feature &optional doc properties)
  "Declare THEME with FEATURE, DOC, and PROPERTIES."
  (unless (custom-theme-name-valid-p theme)
    (signal 'emacs-faces-error (list 'invalid-theme-name theme)))
  (unless (memq theme custom-known-themes)
    (setq custom-known-themes (cons theme custom-known-themes)))
  (put theme 'theme-feature feature)
  (when doc
    (put theme 'theme-documentation doc))
  (when properties
    (put theme 'theme-properties properties))
  theme)

(defmacro deftheme (theme &optional doc &rest properties)
  "Declare THEME to be a Custom theme."
  (declare (doc-string 2) (indent 1))
  `(custom-declare-theme ',theme
                         ',(custom-make-theme-feature theme)
                         ,doc
                         ',properties))

(defun custom-theme-p (theme)
  "Return non-nil when THEME has been declared."
  (and (symbolp theme) (memq theme custom-known-themes)))

(defun custom-check-theme (theme)
  "Signal unless THEME has been declared."
  (unless (or (eq theme 'user) (custom-theme-p theme))
    (signal 'emacs-faces-error (list 'undefined-theme theme)))
  theme)

(defun provide-theme (theme)
  "Provide THEME's THEME-theme feature."
  (custom-check-theme theme)
  (provide (or (get theme 'theme-feature)
               (custom-make-theme-feature theme))))

(defun custom-theme--load-path ()
  "Expand `custom-theme-load-path' into existing directories."
  (let (out)
    (dolist (entry custom-theme-load-path)
      (let ((dir (cond
                  ((eq entry 'custom-theme-directory) custom-theme-directory)
                  ((eq entry t)
                   (and data-directory
                        (expand-file-name "themes" data-directory)))
                  (t entry))))
        (when (and (stringp dir) (file-directory-p dir))
          (setq out (cons dir out)))))
    (nreverse out)))

(defun custom-available-themes ()
  "Return custom themes found in `custom-theme-load-path'."
  (let (themes)
    (dolist (dir (custom-theme--load-path))
      (dolist (file (directory-files dir nil "-theme\\.el\\'"))
        (let ((theme (intern (substring file 0 (string-match "-theme\\.el\\'" file)))))
          (when (and (custom-theme-name-valid-p theme)
                     (not (memq theme themes)))
            (setq themes (cons theme themes))))))
    (nreverse themes)))

(defun custom-theme--locate-file (theme)
  "Return THEME's file path from `custom-theme-load-path', or nil."
  (let ((file-name (concat (symbol-name theme) "-theme.el"))
        found)
    (dolist (dir (custom-theme--load-path))
      (let ((candidate (expand-file-name file-name dir)))
        (when (and (null found) (file-readable-p candidate))
          (setq found candidate))))
    found))

(defun custom-theme--record-setting (theme setting)
  "Append SETTING to THEME's `theme-settings' list."
  (let ((settings (get theme 'theme-settings)))
    (put theme 'theme-settings (append settings (list setting)))))

(defun custom-theme-set-faces (theme &rest args)
  "Initialize THEME face settings from ARGS.
Each entry has the Emacs shape (FACE SPEC [NOW [COMMENT]]).  SPEC is
stored on THEME's `theme-settings' property and, when enabled, on FACE's
`theme-face' property as (THEME . SPEC)."
  (custom-check-theme theme)
  (dolist (entry args)
    (unless (and (consp entry) (symbolp (car entry)))
      (signal 'emacs-faces-error (list 'bad-theme-face entry)))
    (let ((face (nth 0 entry))
          (spec (nth 1 entry)))
      (custom-theme--record-setting
       theme (list 'theme-face face theme spec))
      (when (or (eq theme 'user)
                (memq theme custom-enabled-themes)
                (not custom--inhibit-theme-enable))
        (put face 'theme-face
             (cons (cons theme spec)
                   (assq-delete-all theme (get face 'theme-face))))
        (emacs-redisplay-face-cache-clear))))
  theme)

(defun custom-set-faces (&rest args)
  "Install user face customizations from ARGS."
  (apply #'custom-theme-set-faces 'user args))

(defun custom-theme-set-variables (theme &rest args)
  "Record THEME variable settings from ARGS.
This MVP stores `theme-value' rows for inspection but only applies
settings for already-bound variables."
  (custom-check-theme theme)
  (dolist (entry args)
    (let ((symbol (car entry))
          (value-form (cadr entry)))
      (custom-theme--record-setting
       theme (list 'theme-value symbol theme value-form))
      (when (and (or (eq theme 'user)
                     (memq theme custom-enabled-themes)
                     (not custom--inhibit-theme-enable))
                 (boundp symbol))
        (set symbol (eval value-form)))))
  theme)

(defun custom-set-variables (&rest args)
  "Install user variable customizations from ARGS."
  (apply #'custom-theme-set-variables 'user args))

(defun enable-theme (theme)
  "Enable THEME with highest precedence among non-user themes."
  (custom-check-theme theme)
  (unless (eq theme 'user)
    (setq custom-enabled-themes
          (cons theme (delq theme custom-enabled-themes))))
  (dolist (setting (get theme 'theme-settings))
    (let ((prop (nth 0 setting))
          (symbol (nth 1 setting))
          (value (nth 3 setting)))
      (cond
       ((eq prop 'theme-face)
        (put symbol 'theme-face
             (cons (cons theme value)
                   (assq-delete-all theme (get symbol 'theme-face)))))
       ((and (eq prop 'theme-value) (boundp symbol))
        (set symbol (eval value))))))
  (emacs-redisplay-face-cache-clear)
  theme)

(defun disable-theme (theme)
  "Disable THEME and expose lower-precedence theme or defface defaults."
  (when (memq theme custom-enabled-themes)
    (dolist (setting (get theme 'theme-settings))
      (let ((prop (nth 0 setting))
            (symbol (nth 1 setting)))
        (when (eq prop 'theme-face)
          (put symbol 'theme-face
               (assq-delete-all theme (get symbol 'theme-face))))))
    (setq custom-enabled-themes (delq theme custom-enabled-themes))
    (emacs-redisplay-face-cache-clear))
  theme)

(defun load-theme (theme &optional no-confirm no-enable)
  "Load THEME from `custom-theme-load-path' and enable it.
Standalone policy: pass NO-CONFIRM from init.el, or set
`custom-safe-themes' to t.  Hash prompts are intentionally not
implemented in the noninteractive NeLisp loader."
  (unless (custom-theme-name-valid-p theme)
    (signal 'emacs-faces-error (list 'invalid-theme-name theme)))
  (let ((file (custom-theme--locate-file theme)))
    (unless file
      (signal 'file-missing
              (list "Unable to find theme file" (symbol-name theme))))
    (unless (or no-confirm (eq custom-safe-themes t))
      (signal 'emacs-faces-error
              (list 'unsafe-theme-requires-no-confirm theme)))
    (when (custom-theme-p theme)
      (disable-theme theme)
      (put theme 'theme-settings nil)
      (put theme 'theme-feature nil)
      (put theme 'theme-documentation nil))
    (let ((custom--inhibit-theme-enable t))
      (load file nil t))
    (unless no-enable
      (enable-theme theme))
    t))

;;;; --- reset (test helper) -------------------------------------------

(defun emacs-faces-reset ()
  "Drop every face from the registry + invalidate realize cache.
Test helper — production code shouldn't call this."
  (maphash (lambda (face _attrs)
             (put face 'theme-face nil))
           emacs-redisplay--face-registry)
  (clrhash emacs-redisplay--face-registry)
  (dolist (theme custom-known-themes)
    (put theme 'theme-settings nil))
  (setq custom-known-themes '(user)
        custom-enabled-themes nil)
  (emacs-redisplay-face-cache-clear))

(provide 'emacs-faces)

;;; emacs-faces.el ends here
