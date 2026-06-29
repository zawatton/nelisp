;;; emacs-stub.el --- no-op shims for Emacs C primitives (Phase 3-A''-3)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Phase 3-A''-3 — temporary no-op shims for the long tail of
;; Emacs C primitives that vendored `subr.el' / `cl-lib.el' / friends
;; reference at load time.  Without these, NeLisp standalone fails
;; to load any nontrivial Emacs library.
;;
;; This file is INTENTIONALLY DISPOSABLE — it should disappear as the
;; real implementations land in nelisp-emacs's L2 ports
;; (`emacs-keymap.el', `emacs-frame.el', etc.) or via `nelisp-ec-*'
;; aliasing.  See `project_phase4_emacs_c_primitives_todo' memory entry
;; for the full migration checklist.
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

(defconst emacs-stub--load-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory that contains the stub facade and its sibling features.")

(defun emacs-stub--load-feature (feature)
  "Load FEATURE from the stub facade directory."
  (load (expand-file-name (concat (symbol-name feature) ".el")
                          emacs-stub--load-directory)
        nil t))

;;;; --- keymap.c -----------------------------------------------------------

(unless (fboundp 'make-keymap)
  (defun make-keymap (&optional string)
    "Stub: returns a synthetic keymap sentinel cons.
NeLisp standalone has no keybinding subsystem; the returned object is
only useful for `keymapp' / `eq' identity checks."
    (ignore string)
    (cons 'keymap nil)))

(unless (fboundp 'make-sparse-keymap)
  (defun make-sparse-keymap (&optional string)
    "Stub: same shape as `make-keymap'."
    (ignore string)
    (cons 'keymap nil)))

(unless (fboundp 'keymapp)
  (defun keymapp (object)
    "Stub: recognise the `make-keymap' sentinel."
    (and (consp object) (eq (car object) 'keymap))))

(unless (fboundp 'define-key)
  (defun define-key (keymap key def &optional remove)
    "Stub: no-op; returns DEF."
    (ignore keymap key remove)
    def))

(unless (fboundp 'define-key-after)
  (defun define-key-after (keymap key definition &optional after)
    "Stub: no-op; returns DEFINITION."
    (ignore keymap key after)
    definition))

(unless (fboundp 'lookup-key)
  (defun lookup-key (keymap key &optional accept-default)
    "Stub: always returns nil (= no binding)."
    (ignore keymap key accept-default)
    nil))

(unless (fboundp 'key-binding)
  (defun key-binding (key &optional accept-default no-remap position)
    "Stub: always returns nil."
    (ignore key accept-default no-remap position)
    nil))

(unless (fboundp 'set-keymap-parent)
  (defun set-keymap-parent (keymap parent)
    "Stub: no-op; returns PARENT."
    (ignore keymap)
    parent))

(unless (fboundp 'keymap-parent)
  (defun keymap-parent (keymap) (ignore keymap) nil))

(unless (fboundp 'current-global-map)
  (defun current-global-map () (cons 'keymap nil)))

(unless (fboundp 'current-local-map)
  (defun current-local-map () nil))

(unless (fboundp 'use-global-map)
  (defun use-global-map (keymap) (ignore keymap) nil))

(unless (fboundp 'use-local-map)
  (defun use-local-map (keymap) (ignore keymap) nil))

(unless (fboundp 'where-is-internal)
  (defun where-is-internal (definition &optional keymap firstonly noindirect no-remap)
    "Stub: returns nil (= no key bound)."
    (ignore definition keymap firstonly noindirect no-remap)
    nil))


;;;; --- frame.c ------------------------------------------------------------

(unless (fboundp 'make-frame)
  (defun make-frame (&optional parameters)
    "Stub: returns a synthetic frame sentinel."
    (ignore parameters)
    (cons 'frame nil)))

(unless (fboundp 'framep)
  (defun framep (object)
    (and (consp object) (eq (car object) 'frame))))

(unless (fboundp 'frame-live-p)
  (defun frame-live-p (frame) (framep frame)))

(unless (fboundp 'frame-list)
  (defun frame-list () nil))

(unless (fboundp 'selected-frame)
  (defun selected-frame () (cons 'frame nil)))

(unless (fboundp 'frame-parameter)
  (defun frame-parameter (frame parameter)
    (ignore frame parameter)
    nil))

(unless (fboundp 'frame-parameters)
  (defun frame-parameters (&optional frame) (ignore frame) nil))

(unless (fboundp 'set-frame-parameter)
  (defun set-frame-parameter (frame parameter value)
    (ignore frame parameter)
    value))

(unless (fboundp 'modify-frame-parameters)
  (defun modify-frame-parameters (frame alist)
    (ignore frame alist) nil))

(unless (fboundp 'delete-frame)
  (defun delete-frame (&optional frame force) (ignore frame force) nil))

(unless (fboundp 'display-graphic-p)
  (defun display-graphic-p (&optional display) (ignore display) nil))

(unless (fboundp 'display-color-p)
  (defun display-color-p (&optional display) (ignore display) nil))

(unless (fboundp 'display-multi-frame-p)
  (defun display-multi-frame-p (&optional display) (ignore display) nil))


;;;; --- frame.c / display capability map -----------------------------------
;; Phase 1.E (2026-05-05) — display-* / `window-system' now consult a
;; central `emacs-display-system' defvar instead of returning a hard-
;; coded nil.  Display backends (= nelisp-emacs-gtk, the future curses
;; TUI) `setq' that defvar at bootstrap so init.el / startup code that
;; branches on `(display-graphic-p)' / `(window-system)' picks the
;; right path.  Default `nil' = batch / headless / pre-bootstrap so
;; existing stubbed-out call sites keep their previous behaviour.
;;
;; Earlier comment: "display-* probes have no prefixed substrate yet
;; (= would need a display capability map), so their no-op stubs stay."
;; The map landed here.

(unless (boundp 'emacs-display-system)
  (defvar emacs-display-system nil
    "Symbol naming the active display backend, or nil for batch / no
display.  Display backends (= nelisp-emacs-gtk, the future curses TUI,
…) set this at bootstrap time before any code that branches on
`(window-system)' / `(display-graphic-p)' runs.

Recognised values (list grows as backends ship):
  nil    — no display (= batch, headless, or pre-bootstrap)
  'gtk   — nelisp-emacs-gtk (GTK4 GUI)
  'tui   — emacs-tui-backend (curses-style TUI)"))

(unless (boundp 'initial-window-system)
  (defvar initial-window-system nil
    "Mirror of `emacs-display-system' captured at frame-realise time.
Provided for parity with the canonical Emacs name (= startup code
reads it to detect GUI mode without round-tripping `(window-system)')."))

(unless (boundp 'user-mail-address)
  (defvar user-mail-address nil))

(unless (boundp 'user-full-name)
  (defvar user-full-name nil))

(defun emacs-display-window-system (&optional frame)
  "Return the active window-system symbol (= `emacs-display-system'),
ignoring FRAME (= future per-frame override slot)."
  (ignore frame)
  emacs-display-system)

(defun emacs-display-graphic-p (&optional display)
  "Return non-nil when `emacs-display-system' is a graphic backend.
'tui is treated as non-graphic; nil means no display at all.  DISPLAY
is accepted for API parity but ignored."
  (ignore display)
  (and emacs-display-system
       (not (eq emacs-display-system 'tui))))

(defun emacs-display-color-p (&optional display)
  "MVP: any graphic backend implies colour.  Future capability bits
(= mono / grayscale displays, terminal palette depth) will refine
this; for now we follow `display-graphic-p'."
  (emacs-display-graphic-p display))

(defun emacs-display-multi-frame-p (&optional display)
  "MVP: any non-nil backend can host multiple frames.  Refined when
single-frame backends (= some bare-minimum TUIs) ship."
  (ignore display)
  (not (null emacs-display-system)))

(defun emacs-stub--install-function-p (symbol)
  "Return non-nil when SYMBOL should be installed by this shim layer."
  (or (not (boundp 'emacs-version))
      (not (fboundp symbol))))

(when (emacs-stub--install-function-p 'window-system)
  (defalias 'window-system #'emacs-display-window-system))

(when (emacs-stub--install-function-p 'display-graphic-p)
  (defalias 'display-graphic-p #'emacs-display-graphic-p))

(when (emacs-stub--install-function-p 'display-color-p)
  (defalias 'display-color-p #'emacs-display-color-p))

(when (emacs-stub--install-function-p 'display-multi-frame-p)
  (defalias 'display-multi-frame-p #'emacs-display-multi-frame-p))


;;;; --- window.c -----------------------------------------------------------
;;;; --- window.c -----------------------------------------------------------

(unless (fboundp 'selected-window)
  (defun selected-window () (cons 'window nil)))

(unless (fboundp 'windowp)
  (defun windowp (object) (and (consp object) (eq (car object) 'window))))

(unless (fboundp 'window-live-p)
  (defun window-live-p (window) (windowp window)))

(unless (fboundp 'window-list)
  (defun window-list (&optional frame minibuf window) (ignore frame minibuf window) nil))

(unless (fboundp 'frame-selected-window)
  (defun frame-selected-window (&optional frame) (ignore frame) (selected-window)))

(unless (fboundp 'set-window-buffer)
  (defun set-window-buffer (window buffer-or-name &optional keep-margins)
    (ignore window buffer-or-name keep-margins) nil))

(unless (fboundp 'window-buffer)
  (defun window-buffer (&optional window) (ignore window) nil))


;;;; --- font-lock ----------------------------------------------------------

(unless (fboundp 'font-lock-mode)
  (defun font-lock-mode (&optional arg) (ignore arg) nil))

(unless (boundp 'font-lock-defaults)
  (defvar font-lock-defaults nil))

(unless (boundp 'font-lock-keywords)
  (defvar font-lock-keywords nil))

(unless (boundp 'cpp-font-lock-keywords)
  (defvar cpp-font-lock-keywords nil))

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

(when (or (not (boundp 'emacs-version))
          (get 'make-variable-buffer-local 'emacs-stub-bulk)
          (not (fboundp 'make-variable-buffer-local)))
  (defun make-variable-buffer-local (variable)
    "Stub: accept VARIABLE and return it.
Even without a full buffer-local subsystem, callers such as
`setq-local' depend on Emacs's contract that this returns the symbol
that can then be passed to `set'."
    variable))

(when (or (not (boundp 'emacs-version))
          (get 'make-local-variable 'emacs-stub-bulk)
          (not (fboundp 'make-local-variable)))
  (defun make-local-variable (variable)
    "Stub: accept VARIABLE and return it.
This keeps `(set (make-local-variable 'foo) value)' from attempting to
set nil or another constant symbol in standalone NeLisp."
    variable))

(unless (fboundp 'local-variable-p)
  (defun local-variable-p (variable &optional buffer) (ignore variable buffer) nil))

(unless (fboundp 'kill-local-variable)
  (defun kill-local-variable (variable) (ignore variable) nil))

;; condition-case variants used by subr.el
(unless (fboundp 'condition-case-unless-debug)
  (defmacro condition-case-unless-debug (var bodyform &rest handlers)
    "Stub: route through plain condition-case (= NeLisp has no debug-on-error toggle)."
    (cons 'condition-case (cons var (cons bodyform handlers)))))

(unless (fboundp 'with-silent-modifications)
  (defmacro with-silent-modifications (&rest body)
    "Stub: evaluate BODY without modified-flag bookkeeping."
    (cons 'progn body)))

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

;;;; --- version helpers -----------------------------------------------------

(defun emacs-stub--version-skip-nondigits (string index)
  "Return first digit position in STRING at or after INDEX."
  (let ((len (length string)))
    (while (and (< index len)
                (let ((char (aref string index)))
                  (or (< char ?0) (> char ?9))))
      (setq index (1+ index)))
    index))

(defun emacs-stub--version-read-component (string index)
  "Read one numeric version component from STRING at INDEX.
Return (VALUE . NEXT-INDEX), where VALUE is nil when no component remains."
  (let ((len (length string))
        (value 0)
        (seen nil))
    (setq index (emacs-stub--version-skip-nondigits string index))
    (while (and (< index len)
                (let ((char (aref string index)))
                  (and (>= char ?0) (<= char ?9))))
      (setq value (+ (* value 10) (- (aref string index) ?0)))
      (setq seen t)
      (setq index (1+ index)))
    (cons (and seen value) index)))

(defun emacs-stub--version-compare (v1 v2)
  "Compare V1 and V2 as dotted numeric version strings.
Return -1, 0, or 1 when V1 is less than, equal to, or greater than V2."
  (let ((s1 (if (stringp v1) v1 (format "%s" v1)))
        (s2 (if (stringp v2) v2 (format "%s" v2)))
        (i1 0)
        (i2 0)
        (result 0)
        (done nil))
    (while (not done)
      (setq i1 (emacs-stub--version-skip-nondigits s1 i1))
      (setq i2 (emacs-stub--version-skip-nondigits s2 i2))
      (if (and (>= i1 (length s1)) (>= i2 (length s2)))
          (setq done t)
        (let* ((c1 (emacs-stub--version-read-component s1 i1))
               (c2 (emacs-stub--version-read-component s2 i2))
               (n1 (or (car c1) 0))
               (n2 (or (car c2) 0)))
          (setq i1 (cdr c1))
          (setq i2 (cdr c2))
          (cond
           ((< n1 n2) (setq result -1 done t))
           ((> n1 n2) (setq result 1 done t))))))
    result))

(unless (fboundp 'version<)
  (defun version< (v1 v2)
    "Return non-nil when version string V1 is older than V2."
    (< (emacs-stub--version-compare v1 v2) 0)))

(unless (fboundp 'version<=)
  (defun version<= (v1 v2)
    "Return non-nil when version string V1 is not newer than V2."
    (not (version< v2 v1))))

(unless (fboundp 'combine-change-calls)
  (defmacro combine-change-calls (_beg _end &rest body)
    "Standalone fallback: evaluate BODY without buffer-change coalescing."
    (cons 'progn body)))

(unless (fboundp 'define-advice)
  (defmacro define-advice (symbol args &rest body)
    "Standalone fallback for `nadvice.el' `define-advice'.
Define the generated advice function and call `advice-add'.  The current
standalone `advice-add' is load-time-only, so this preserves definitions
without attempting to weave advice into existing function cells."
    (let* ((how (car args))
           (arglist (cadr args))
           (name (caddr args))
           (props (cdddr args))
           (advice (intern (concat (symbol-name symbol)
                                   "@"
                                   (symbol-name name)))))
      `(prog1
           (defun ,advice ,arglist ,@body)
         (advice-add ',symbol ,how #',advice ,@(and props `(',props)))))))

;; Buffer search primitives — all stubs (= no real buffer text in standalone)
(unless (fboundp 're-search-forward)
  (defun re-search-forward (regexp &optional bound noerror count)
    (ignore regexp bound noerror count) nil))

(unless (fboundp 're-search-backward)
  (defun re-search-backward (regexp &optional bound noerror count)
    (ignore regexp bound noerror count) nil))

(unless (fboundp 'search-forward)
  (defun search-forward (string &optional bound noerror count)
    (ignore string bound noerror count) nil))

(unless (fboundp 'search-backward)
  (defun search-backward (string &optional bound noerror count)
    (ignore string bound noerror count) nil))

(unless (fboundp 'match-string)
  (defun match-string (num &optional string) (ignore num string) nil))

(unless (fboundp 'match-string-no-properties)
  (defalias 'match-string-no-properties 'match-string))

(unless (fboundp 'match-beginning)
  (defun match-beginning (subexp) (ignore subexp) nil))

(unless (fboundp 'match-end)
  (defun match-end (subexp) (ignore subexp) nil))

(unless (fboundp 'match-data)
  (defun match-data (&optional integers reuse reseat) (ignore integers reuse reseat) nil))

(unless (fboundp 'set-match-data)
  (defun set-match-data (list &optional reseat) (ignore list reseat) nil))

(unless (fboundp 'function-get)
  (defun function-get (f prop &optional _autoload)
    "Polyfill: value of F's function property PROP, following defalias chains.
AUTOLOAD is accepted for API parity and ignored (no autoload layer here).
Real `function-get' (subr.el) is relied on by `define-inline', cl-generic,
nadvice, etc.; the runtime previously left it void."
    (let ((val nil))
      (while (and (symbolp f)
                  (null (setq val (get f prop)))
                  (fboundp f))
        (let ((fundef (symbol-function f)))
          (setq f (and (symbolp fundef) fundef))))
      val)))

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

(unless (fboundp 'looking-at)
  (defun looking-at (regexp) (ignore regexp) nil))

(unless (fboundp 'looking-back)
  (defun looking-back (regexp &optional limit greedy) (ignore regexp limit greedy) nil))

;; Buffer cursor / point primitives — stubs returning sentinels
(unless (fboundp 'point)
  (defun point () 1))

(unless (fboundp 'point-min)
  (defun point-min () 1))

(unless (fboundp 'point-max)
  (defun point-max () 1))

(unless (fboundp 'goto-char)
  (defun goto-char (position) (ignore position) nil))

(unless (fboundp 'forward-char)
  (defun forward-char (&optional n) (ignore n) nil))

(unless (fboundp 'backward-char)
  (defun backward-char (&optional n) (ignore n) nil))

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

(unless (fboundp 'line-number-display-width)
  (defun line-number-display-width (&optional pixelwise)
    "Standalone fallback line-number display width.
Return one canonical column by default; callers such as
`display-line-numbers-update-width' only need a stable positive width
when no real redisplay window is available."
    (ignore pixelwise)
    1))

(unless (boundp 'display-line-numbers)
  (defvar display-line-numbers nil))

(unless (boundp 'display-line-numbers-width)
  (defvar display-line-numbers-width nil))

(unless (boundp 'display-line-numbers-widen)
  (defvar display-line-numbers-widen nil))

(unless (boundp 'display-line-numbers-current-absolute)
  (defvar display-line-numbers-current-absolute t))

(unless (fboundp 'eobp)
  (defun eobp () t))

(unless (fboundp 'bobp)
  (defun bobp () t))

(unless (fboundp 'eolp)
  (defun eolp () t))

(unless (fboundp 'bolp)
  (defun bolp () t))

;; Buffer text manipulation
(unless (fboundp 'insert)
  (defun insert (&rest args) (ignore args) nil))

(unless (fboundp 'delete-region)
  (defun delete-region (start end) (ignore start end) nil))

(unless (fboundp 'delete-char)
  (defun delete-char (n &optional killflag) (ignore n killflag) nil))

(unless (fboundp 'erase-buffer)
  (defun erase-buffer () nil))

(unless (fboundp 'buffer-substring)
  (defun buffer-substring (start end) (ignore start end) ""))

(unless (fboundp 'buffer-substring-no-properties)
  (defalias 'buffer-substring-no-properties 'buffer-substring))

(unless (fboundp 'buffer-string)
  (defun buffer-string () ""))

(unless (fboundp 'buffer-size)
  (defun buffer-size (&optional buffer) (ignore buffer) 0))

;; Save markers / regions
(unless (fboundp 'save-excursion)
  (defmacro save-excursion (&rest body) (cons 'progn body)))

(unless (fboundp 'save-restriction)
  (defmacro save-restriction (&rest body) (cons 'progn body)))

(unless (fboundp 'save-match-data)
  (defmacro save-match-data (&rest body) (cons 'progn body)))

(unless (fboundp 'with-current-buffer)
  (defmacro with-current-buffer (buffer &rest body)
    `(let ((--saved-buf-- (current-buffer)))
       (unwind-protect (progn ,@body) nil))))

(unless (fboundp 'with-temp-buffer)
  (defmacro with-temp-buffer (&rest body) (cons 'progn body)))

(when (or (emacs-stub--install-function-p 'bound-and-true-p)
          (get 'bound-and-true-p 'emacs-stub-bulk))
  (defmacro bound-and-true-p (var)
    "Return VAR's value if VAR is bound and non-nil."
    `(and (boundp ',var) ,var)))

(unless (fboundp 'narrow-to-region)
  (defun narrow-to-region (start end) (ignore start end) nil))

(unless (fboundp 'widen)
  (defun widen () nil))

;; Syntax tables
(unless (fboundp 'standard-syntax-table)
  (defun standard-syntax-table () '(syntax-table)))

(unless (fboundp 'syntax-table)
  (defun syntax-table () (standard-syntax-table)))

(unless (fboundp 'set-syntax-table)
  (defun set-syntax-table (table) (ignore table) nil))

(when (or (not (fboundp 'make-syntax-table))
          (get 'make-syntax-table 'emacs-stub-bulk))
  (defun make-syntax-table (&optional table)
    "Standalone load-time fallback for syntax table objects."
    (list 'syntax-table table)))

(unless (fboundp 'modify-syntax-entry)
  (defun modify-syntax-entry (char newentry &optional table) (ignore char newentry table) nil))

(unless (boundp 'outline-mode-syntax-table)
  (defvar outline-mode-syntax-table (standard-syntax-table)))
(unless (boundp 'text-mode-syntax-table)
  (defvar text-mode-syntax-table (standard-syntax-table)))

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

;;;; --- numeric primitives -------------------------------------------------

(unless (fboundp 'min)
  (defun min (&rest numbers)
    (let ((acc (car numbers)))
      (setq numbers (cdr numbers))
      (while numbers
        (when (< (car numbers) acc) (setq acc (car numbers)))
        (setq numbers (cdr numbers)))
      acc)))

(unless (fboundp 'max)
  (defun max (&rest numbers)
    (let ((acc (car numbers)))
      (setq numbers (cdr numbers))
      (while numbers
        (when (> (car numbers) acc) (setq acc (car numbers)))
        (setq numbers (cdr numbers)))
      acc)))

(unless (fboundp 'abs)
  (defun abs (n) (if (< n 0) (- n) n)))

(unless (fboundp 'zerop)
  (defun zerop (n) (= n 0)))

(unless (fboundp 'plusp)
  (defun plusp (n) (> n 0)))

(unless (fboundp 'minusp)
  (defun minusp (n) (< n 0)))

(unless (fboundp 'oddp)
  (defun oddp (n) (= 1 (mod n 2))))

(unless (fboundp 'evenp)
  (defun evenp (n) (= 0 (mod n 2))))

(unless (fboundp 'natnump)
  (defun natnump (n) (and (integerp n) (>= n 0))))

(unless (fboundp '1+)
  (defun 1+ (n) (+ n 1)))

(unless (fboundp '1-)
  (defun 1- (n) (- n 1)))


;;;; --- bitwise ops --------------------------------------------------------

(unless (fboundp 'logior)
  (defun logior (&rest ints)
    "Stub: bitwise OR of all INTS."
    (let ((acc 0))
      (while ints
        (setq acc (+ acc (- (car ints) (logand acc (car ints)))))
        (setq ints (cdr ints)))
      acc)))

(unless (fboundp 'logand)
  (defun logand (&rest ints)
    "Stub: bitwise AND of all INTS.  Approximation via min for non-negative."
    (if (null ints)
        -1
      (let ((acc (car ints)))
        (setq ints (cdr ints))
        (while ints
          ;; Conservative: use min as a lower bound; not strictly correct
          ;; but adequate for the bit-flag use cases in subr.el load path.
          (setq acc (min acc (car ints)))
          (setq ints (cdr ints)))
        acc))))

(unless (fboundp 'logxor)
  (defun logxor (&rest ints)
    "Stub: bitwise XOR (= using +/- proxy for non-overlapping flags)."
    (let ((acc 0))
      (while ints
        (setq acc (+ acc (car ints)))
        (setq ints (cdr ints)))
      acc)))

(unless (fboundp 'lognot)
  (defun lognot (int)
    "Stub: bitwise NOT."
    (- (- int) 1)))

(unless (fboundp 'ash)
  (defun ash (value count)
    "Stub: arithmetic shift (positive COUNT = left, negative = right)."
    (cond
     ((= count 0) value)
     ((> count 0)
      (let ((acc value))
        (while (> count 0) (setq acc (* acc 2)) (setq count (- count 1)))
        acc))
     (t
      (let ((acc value))
        (while (< count 0) (setq acc (/ acc 2)) (setq count (+ count 1)))
        acc)))))

(unless (fboundp 'lsh) (defalias 'lsh 'ash))


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

(unless (fboundp 'current-buffer)
  (defun current-buffer ()
    "Stub: synthetic placeholder.  Real impl needs nelisp-ec-current-buffer alias."
    (cons 'buffer nil)))

(unless (fboundp 'bufferp)
  (defun bufferp (object) (and (consp object) (eq (car object) 'buffer))))

(unless (fboundp 'buffer-live-p)
  (defun buffer-live-p (buffer) (bufferp buffer)))

(unless (fboundp 'get-buffer)
  (defun get-buffer (buffer-or-name) (ignore buffer-or-name) nil))

(unless (fboundp 'get-buffer-create)
  (defun get-buffer-create (buffer-or-name &optional inhibit-buffer-hooks)
    (ignore buffer-or-name inhibit-buffer-hooks)
    (cons 'buffer nil)))

(unless (fboundp 'buffer-name)
  (defun buffer-name (&optional buffer) (ignore buffer) ""))

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

;;;; --- regex/menu load-time helpers --------------------------------------

(unless (fboundp 'regexp-quote)
  (defun regexp-quote (string)
    "Minimal regexp quote for standalone load-time keyword tables."
    (let ((i 0)
          (out ""))
      (while (< i (length string))
        (let ((ch (aref string i)))
          (when (memq ch '(?\\ ?. ?* ?+ ?? ?^ ?$ ?\[ ?\] ?\( ?\) ?{ ?} ?|))
            (setq out (concat out "\\")))
          (setq out (concat out (string ch))))
        (setq i (1+ i)))
      out)))

;; Unconditional: an earlier no-op bulk stub may claim `regexp-opt' first
;; (closure (&rest _) nil), so always install the real implementation.
(defun regexp-opt (strings &optional paren)
  "Minimal `regexp-opt': alternation of regexp-quoted STRINGS.
PAREN nil -> shy group; t / string -> capture group; words / symbols add
word / symbol boundaries (matches the GNU `regexp-opt' grouping contract)."
  (let* ((body (mapconcat #'regexp-quote strings "\\|"))
         (open (cond ((stringp paren) paren)
                     ((eq paren 'words) "\\<\\(")
                     ((eq paren 'symbols) "\\_<\\(")
                     (paren "\\(")
                     (t "\\(?:")))
         (close (cond ((eq paren 'words) "\\)\\>")
                      ((eq paren 'symbols) "\\)\\_>")
                      (t "\\)"))))
    (concat open body close)))

(unless (fboundp 'easy-menu-define)
  (defmacro easy-menu-define (&rest _args)
    "Standalone load-time fallback: ignore menu declarations."
    nil))

(unless (fboundp 'easy-menu-add-item)
  (defun easy-menu-add-item (&rest _args)
    "Standalone load-time fallback: ignore menu mutations."
    nil))

(unless (fboundp 'current-idle-time)
  (defun current-idle-time ()
    "Standalone fallback: no UI event loop means no idle-time sample."
    nil))

(unless (fboundp 'shell-command-to-string)
  (defun shell-command-to-string (command)
    "Standalone fallback: do not run external shell commands."
    (ignore command)
    ""))

(unless (fboundp 'call-process-shell-command)
  (defun call-process-shell-command (&rest _args)
    "Standalone fallback: report external shell command failure."
    1))

(unless (fboundp 'syntax-propertize-rules)
  (defmacro syntax-propertize-rules (&rest _rules)
    "Standalone fallback: return an inert syntax propertizer."
    `(lambda (&rest _args) nil)))

(unless (fboundp 'cc-require)
  (defmacro cc-require (&rest _features)
    "Standalone load-time fallback: ignore CC Mode compile-time requires."
    nil))

(unless (fboundp 'cc-provide)
  (defmacro cc-provide (feature)
    "Standalone load-time fallback: provide FEATURE for CC Mode fragments."
    `(provide ,feature)))

(unless (boundp 'c-style-alist)
  (defvar c-style-alist nil))

(unless (fboundp 'c-add-style)
  (defun c-add-style (style description &optional set-p)
    "Standalone load-time fallback: remember a CC Mode STYLE."
    (ignore set-p)
    (let ((existing (assoc style c-style-alist)))
      (if existing
          (setcdr existing description)
        (setq c-style-alist (cons (cons style description) c-style-alist))))
    style))

;; Load the auto-generated bulk stubs last so emacs-stub.el's specific
;; (= more accurate) implementations above take precedence — bulk fills
;; only the remaining `(unless (fboundp X) ...)' gaps.  Without this
;; require the bulk file is an orphan and `macroexp-warn-and-return' /
;; other vendored-Emacs prerequisites stay void at standalone load
;; time, breaking `(require 'json)' / cl-lib / friends.
(emacs-stub--load-feature 'emacs-stub-bulk)

;;;; --- load-time machinery + misc (vendor-coverage 2026-06-06 batch) -------
;; Surfaced by bin/vendor-coverage as truly-missing top-level / load-time
;; callers across vendor/emacs-lisp.  No-ops / minimal degraded impls.

(unless (fboundp 'register-definition-prefixes)
  (defun register-definition-prefixes (file prefixes)
    "Stub: loaddefs prefix registration is unused in standalone."
    (ignore file prefixes) nil))

(unless (fboundp 'custom-add-load)
  (defun custom-add-load (symbol load)
    "Add LOAD to SYMBOL's `custom-loads' metadata."
    (let ((loads (get symbol 'custom-loads)))
      (unless (member load loads)
        (put symbol 'custom-loads (cons load loads))))))

(unless (fboundp 'custom--add-custom-loads)
  (defun custom--add-custom-loads (symbol loads)
    "Set SYMBOL's `custom-loads' metadata, preserving existing loads."
    (dolist (load (get symbol 'custom-loads))
      (unless (member load loads)
        (setq loads (cons load loads))))
    (put symbol 'custom-loads loads)))

(unless (fboundp 'custom-autoload)
  (defun custom-autoload (symbol load &optional noset)
    "Mark SYMBOL as a custom autoload and record LOAD."
    (put symbol 'custom-autoload (if noset 'noset t))
    (custom-add-load symbol load)))

(unless (fboundp 'setq-local)
  (defmacro setq-local (&rest pairs)
    "Stub: degrade to global `setq' (standalone has no buffer-local cells)."
    (cons 'setq pairs)))

(unless (fboundp 'default-value)
  (defun default-value (symbol)
    "Stub: standalone has no buffer-local cells; return the global value."
    (symbol-value symbol)))

(unless (fboundp 'set-default)
  (defun set-default (symbol value)
    "Stub: degrade to global `set'."
    (set symbol value)))

(unless (fboundp 'format-prompt)
  (defun format-prompt (prompt default &rest format-args)
    "Stub: minimal `format-prompt' — PROMPT plus an optional default hint."
    (concat (if format-args (apply #'format prompt format-args) prompt)
            (if (and default (not (equal default "")))
                (format " (default %s)"
                        (if (consp default) (car default) default))
              "")
            ": ")))

(unless (fboundp 'derived-mode-p)
  (defun derived-mode-p (&rest modes)
    "Stub: standalone has no major-mode hierarchy; always nil."
    (ignore modes) nil))

(unless (fboundp 'widget-get)
  (defun widget-get (widget property)
    "Stub: no widget subsystem; always nil."
    (ignore widget property) nil))

(unless (fboundp 'widget-put)
  (defun widget-put (widget property value)
    "Stub: no-op; return WIDGET."
    (ignore property value) widget))

(unless (fboundp 'debug)
  (defun debug (&rest args)
    "Stub: no debugger in standalone; no-op."
    (ignore args) nil))

;;;; --- markers (vendor-coverage 2026-06-06 batch) --------------------------
;; A marker is the vector [marker POSITION BUFFER INSERTION-TYPE].  Standalone
;; has no gap buffer, so markers do NOT auto-adjust when text is inserted or
;; deleted; they hold a position+buffer that code can create / set / read /
;; compare.  `current-buffer' returns a fresh sentinel each call, so
;; `marker-buffer' is only meaningful as a non-nil "is this marker set" flag.

(unless (fboundp 'make-marker)
  (defun make-marker ()
    "Return a new marker that points nowhere."
    (vector 'marker nil nil nil)))

(unless (fboundp 'markerp)
  (defun markerp (object)
    "Return non-nil if OBJECT is a marker created by this substrate."
    (and (vectorp object) (= (length object) 4) (eq (aref object 0) 'marker))))

(unless (fboundp 'marker-position)
  (defun marker-position (marker)
    "Return the position MARKER points to, or nil."
    (and (markerp marker) (aref marker 1))))

(unless (fboundp 'marker-buffer)
  (defun marker-buffer (marker)
    "Return the buffer MARKER points into, or nil."
    (and (markerp marker) (aref marker 2))))

(unless (fboundp 'marker-insertion-type)
  (defun marker-insertion-type (marker)
    "Return MARKER's insertion type."
    (and (markerp marker) (aref marker 3))))

(unless (fboundp 'set-marker-insertion-type)
  (defun set-marker-insertion-type (marker type)
    "Set MARKER's insertion type to TYPE."
    (when (markerp marker) (aset marker 3 type))
    type))

(unless (fboundp 'set-marker)
  (defun set-marker (marker position &optional buffer)
    "Point MARKER at POSITION in BUFFER (default current).  nil POSITION detaches."
    (when (markerp marker)
      (if (null position)
          (progn (aset marker 1 nil) (aset marker 2 nil))
        (aset marker 1 (if (markerp position) (marker-position position) position))
        (aset marker 2 (or buffer (and (fboundp 'current-buffer) (current-buffer))))))
    marker))

(unless (fboundp 'move-marker)
  (defalias 'move-marker 'set-marker))

(unless (fboundp 'copy-marker)
  (defun copy-marker (&optional position type)
    "Return a new marker at POSITION (integer or marker; default point)."
    (let ((m (make-marker)))
      (cond
       ((markerp position)
        (set-marker m (marker-position position) (marker-buffer position)))
       ((null position)
        (set-marker m (if (fboundp 'point) (point) 1)))
       (t (set-marker m position)))
      (when type (set-marker-insertion-type m type))
      m)))

(unless (fboundp 'point-marker)
  (defun point-marker ()
    "Return a new marker at point in the current buffer."
    (copy-marker (if (fboundp 'point) (point) 1))))

(unless (fboundp 'insert-before-markers)
  (defun insert-before-markers (&rest args)
    "Degraded alias for `insert' (markers do not auto-adjust in standalone)."
    (apply #'insert args)))

;;;; --- file-name + shell helpers (vendor-coverage 2026-06-06 batch) --------
;; Pure, host-independent helpers that vendor code references at load / macro
;; time.  All degrade gracefully in standalone (no real filesystem or shell).

(unless (fboundp 'file-name-absolute-p)
  (defun file-name-absolute-p (filename)
    "Return non-nil if FILENAME is an absolute file name (starts with / or ~)."
    (and (stringp filename)
         (> (length filename) 0)
         (let ((c (aref filename 0)))
           (or (eq c ?/) (eq c ?~))))))

(unless (fboundp 'file-relative-name)
  (defun file-relative-name (filename &optional directory)
    "Convert FILENAME to be relative to DIRECTORY.
Standalone: returns FILENAME unchanged (no directory arithmetic)."
    (ignore directory) filename))

(unless (fboundp 'abbreviate-file-name)
  (defun abbreviate-file-name (filename)
    "Return a shortened version of FILENAME.
Standalone: returns FILENAME unchanged (no home-dir abbreviation)."
    filename))

(unless (fboundp 'shell-quote-argument)
  (defun shell-quote-argument (argument)
    "Quote ARGUMENT for passing to an inferior POSIX shell."
    (if (equal argument "")
        "''"
      (concat "'"
              (if (fboundp 'string-replace)
                  (string-replace "'" "'\\''" argument)
                argument)
              "'"))))

(unless (fboundp 'executable-find)
  (defun executable-find (command &optional remote)
    "Stub: standalone cannot search PATH, so always returns nil."
    (ignore command remote) nil))

;;;; --- search / interaction helpers (vendor-coverage 2026-06-06 batch) -----

(unless (fboundp 'match-string-no-properties)
  (defun match-string-no-properties (num &optional string)
    "Return text matched by the last search for subexpression NUM, no props."
    (when (fboundp 'match-string)
      (let ((s (match-string num string)))
        (if (and s (fboundp 'substring-no-properties))
            (substring-no-properties s)
          s)))))

(unless (fboundp 'y-or-n-p)
  (defun y-or-n-p (prompt)
    "Stub: non-interactive standalone answers no (nil)."
    (ignore prompt) nil))

(unless (fboundp 'yes-or-no-p)
  (defun yes-or-no-p (prompt)
    "Stub: non-interactive standalone answers no (nil)."
    (ignore prompt) nil))

;;;; --- file / property / terminal helpers (vendor-coverage 2026-06-06 batch2)

(unless (fboundp 'file-remote-p)
  (defun file-remote-p (file &optional identification connected)
    "Stub: the standalone reader only sees local files, so always nil."
    (ignore file identification connected) nil))

(unless (fboundp 'file-attribute-modification-time)
  (defun file-attribute-modification-time (attributes)
    "Return the modification time from a `file-attributes' list (element 5)."
    (nth 5 attributes)))

(unless (fboundp 'get-char-property)
  (defun get-char-property (position prop &optional object)
    "Stub: standalone tracks no text properties / overlays, so always nil."
    (ignore position prop object) nil))

(unless (fboundp 'next-single-property-change)
  (defun next-single-property-change (position prop &optional object limit)
    "Stub: no text properties in standalone; report no change (LIMIT or nil)."
    (ignore position prop object) limit))

(unless (fboundp 'string-width)
  (defun string-width (string &optional from to)
    "Degraded width: count characters (wide chars not doubled) in STRING."
    (length (if (or from to) (substring string (or from 0) to) string))))

(unless (fboundp 'ding)
  (defun ding (&optional arg)
    "Stub: no terminal bell in the standalone reader."
    (ignore arg) nil))

(unless (fboundp 'beep)
  (defun beep (&optional arg)
    "Stub: no terminal bell in the standalone reader."
    (ignore arg) nil))

;;;; --- character / arithmetic helpers (vendor-coverage 2026-06-07 batch3) ---
;; Re-provided in the substrate because the current standalone reader no longer
;; bakes them in; `unless fboundp' keeps them inert when the reader does.

(unless (fboundp 'characterp)
  (defun characterp (object)
    "Return non-nil if OBJECT is a valid character code point (0..#x3FFFFF)."
    (and (integerp object) (>= object 0) (<= object #x3FFFFF))))

(unless (fboundp 'expt)
  (defun expt (base exponent)
    "Return BASE raised to EXPONENT.
Standalone supports an integer EXPONENT (the common case, e.g.
\(expt 2 N)) by repeated multiplication; a negative integer EXPONENT
yields a float reciprocal.  A non-integer EXPONENT is unsupported and
degrades to 1 (no pow primitive in the standalone reader)."
    (cond
     ((and (integerp exponent) (>= exponent 0))
      (let ((acc 1) (i 0))
        (while (< i exponent) (setq acc (* acc base)) (setq i (1+ i)))
        acc))
     ((integerp exponent)
      (let ((acc 1) (i 0) (n (- exponent)))
        (while (< i n) (setq acc (* acc base)) (setq i (1+ i)))
        (/ 1.0 acc)))
     (t 1))))

;;;; --- define-inline (Doc 15 B4): runtime-compatible function-only impl ---
;;
;; `emacs-stub-bulk' (required above) pre-stubs `define-inline' to a no-op,
;; so packages that define functions with it (ht.el's ht-create / ht-get /
;; ..., and many MELPA libs) get void functions.  The real `inline.el'
;; machinery does not mesh with this runtime's backquote -- the reader reads
;; ,X / ,@X as (comma X) / (comma-at X), not the standard \\,/\\,@, so
;; inline.el's inline-quote walker leaves (comma X) in the generated code,
;; yielding `void-function comma'.
;;
;; Provide a lean, function-version-only `define-inline' that lowers the
;; inline DSL directly against the runtime backquote: an `inline-quote' FORM
;; becomes FORM with (comma X) -> X (the bound argument value), and
;; `inline-letevals' is a no-op wrapper (its vars are already evaluated
;; args).  No compiler-macro / inlining optimisation (callability over
;; speed).  Reader-gated (`rdf') so host Emacs keeps its real `define-inline'.
;; Lowering helpers are pure and defined unconditionally (harmless on host,
;; and unit-testable there); only the `define-inline' macro is reader-gated.
(defun emacs-stub--inline-uncomma (form)
  "Lower runtime backquote unquotes in FORM: (comma X) -> X, recursively."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'comma) (emacs-stub--inline-lower (cadr form)))
   ((eq (car form) 'comma-at) (emacs-stub--inline-lower (cadr form)))
   (t (mapcar #'emacs-stub--inline-uncomma form))))

(defun emacs-stub--inline-lower (form)
  "Lower one inline DSL FORM to runtime code (function / funcall path).
Handles `inline-quote', `inline-letevals', `inline-const-p',
`inline-const-val' and `inline-error', and recurses through ordinary
sub-forms so DSL operators nested inside `if' / `cond' / `let' (as used
by org-element's accessors) are lowered too -- the previous default
left them intact, yielding `void-function inline-const-val' at runtime."
  (cond
   ((not (consp form)) form)
   ((eq (car form) 'inline-quote) (emacs-stub--inline-uncomma (cadr form)))
   ((eq (car form) 'inline-letevals)
    ;; vars are already-evaluated args -> drop the binding spec, keep body
    ;; (cf. inline--dont-leteval for the symbol case = macroexp-progn body).
    (let ((body (cddr form)))
      (if (cdr body)
          (cons 'progn (mapcar #'emacs-stub--inline-lower body))
        (emacs-stub--inline-lower (car body)))))
   ;; funcall path: `inline-const-p' is always true and `inline-const-val'
   ;; is the value itself (cf. inline--alwaysconst-p / inline--alwaysconst-val
   ;; in inline.el): in the function version the args already hold values.
   ((eq (car form) 'inline-const-p) t)
   ((eq (car form) 'inline-const-val) (emacs-stub--inline-lower (cadr form)))
   ((eq (car form) 'inline-error)
    (cons 'error (mapcar #'emacs-stub--inline-lower (cdr form))))
   ;; never descend into quoted data
   ((eq (car form) 'quote) form)
   (t (mapcar #'emacs-stub--inline-lower form))))

(defun emacs-stub--define-inline (name args body)
  "Build the `defun' form for a runtime `define-inline' (function version)."
  (when (stringp (car-safe body)) (setq body (cdr body)))
  (when (eq (car-safe (car-safe body)) 'declare) (setq body (cdr body)))
  (list 'defun name args
        (if (cdr body)
            (cons 'progn (mapcar #'emacs-stub--inline-lower body))
          (emacs-stub--inline-lower (car body)))))

(when (fboundp 'rdf)
  (defmacro define-inline (name args &rest body)
    "Runtime `define-inline': define NAME as a plain function (no inlining).
The inline DSL in BODY is lowered against the runtime backquote."
    (emacs-stub--define-inline name args body)))

;; Pre-provide `inline' so a later `(require 'inline)' is inert.  On the
;; standalone, `inline.el' is not on the load-path, so `require' silent-succeeds
;; (sets featurep) AND leaves the runtime `define-inline' above a no-op -- every
;; subsequent `define-inline' form then fails (observed: org-element-ast.el,
;; which does `(require 'inline)' before defining many inline accessors, crashed
;; the whole load).  Providing the feature here keeps the working macro.
(when (fboundp 'rdf)
  (provide 'inline))

;; `buffer-base-buffer' (C primitive, buffer.c) returns the base buffer of an
;; indirect buffer, or nil for a normal buffer.  It is only registered under
;; the `emacs-buffer-buffer-base-buffer' name (nelisp-emacs.el) and never
;; aliased to the standard name, so it is void at runtime -- org-element-at-point
;; and other callers then fail with `void-function buffer-base-buffer'.  The
;; standalone has no indirect buffers, so nil is always the correct answer.
(unless (fboundp 'buffer-base-buffer)
  (defun buffer-base-buffer (&optional _buffer)
    "Return the base buffer of an indirect buffer (always nil here)."
    nil))

;;;; --- Doc 16 breadth: foundational subr builtins (were void) ---------
;; `xor' (subr.el), `ntake' (Emacs 30 fns.c) and `char-uppercase-p'
;; (simple.el) were void in the standalone runtime.  They are widely
;; called by vendor packages -- bytecomp / comp / ert / pp / package-vc
;; all use `xor'.  Plain defuns gated on `unless (fboundp ...)' so host
;; Emacs stays a no-op.  Reader notes verified by direct --load: the bare
;; reader has no `/=' (use `(not (= ...))') and treats POSIX `[[:blank:]]'
;; classes literally, so explicit char sets are used where needed.

(unless (fboundp 'xor)
  (defun xor (cond1 cond2)
    "Return the boolean exclusive-or of COND1 and COND2.
If only one of the arguments is non-nil, return it; otherwise return nil."
    (cond ((not cond1) cond2)
          ((not cond2) cond1))))

(unless (fboundp 'ntake)
  (defun ntake (n list)
    "Modify LIST to keep only the first N elements, and return it.
If N is zero or negative, return nil.  If N is greater or equal to the
length of LIST, return LIST unmodified.  Destructive counterpart of `take'."
    (when (and (> n 0) list)
      (let ((cell (nthcdr (1- n) list)))
        (when (consp cell) (setcdr cell nil)))
      list)))

(unless (fboundp 'char-uppercase-p)
  (defun char-uppercase-p (char)
    "Return non-nil if CHAR is an upper-case character.
A character is upper-case when it differs from its `downcase' form,
which covers ASCII plus any cased letter in the runtime case table."
    (and (natnump char) (not (= char (downcase char))))))

;;;; --- Doc 16 breadth round 7: subr.el binding macros (were void) ------
;; `ignore-error' (subr.el), `while-let' + `and-let*' (subr-x bindings)
;; were void.  Standard backquote works in runtime macros, so these mirror
;; the Emacs definitions.  `and-let*' needs `internal--build-bindings'
;; (also void here -- the runtime's when-let*/if-let* are implemented
;; without it), so the binding-builder is shimmed too.  All gated on
;; `unless (fboundp ...)' so host Emacs keeps its own.
;; `with-memoization' is added in round 8 below, once the extra `setf'
;; places (notably `(gethash ...)') were registered in `cl-lib.el'.

(unless (fboundp 'internal--build-bindings)
  (defun internal--build-binding (binding prev-var)
    "Normalize a `when-let*'/`and-let*' BINDING, chaining PREV-VAR with `and'."
    (setq binding
          (cond ((symbolp binding) (list binding binding))
                ((null (cdr binding)) (list (gensym "s") (car binding)))
                (t binding)))
    (list (car binding) (list 'and prev-var (cadr binding))))

  (defun internal--build-bindings (bindings)
    "Normalize BINDINGS into short-circuiting `let*' bindings."
    (let ((prev-var t))
      (mapcar (lambda (binding)
                (let ((b (internal--build-binding binding prev-var)))
                  (setq prev-var (car b))
                  b))
              bindings))))

(unless (fboundp 'ignore-error)
  (defmacro ignore-error (condition &rest body)
    "Execute BODY; if the error CONDITION occurs, return nil.
CONDITION is a (list of) error symbol(s) and is not evaluated."
    (declare (debug t) (indent 1))
    `(condition-case nil (progn ,@body) (,condition nil))))

(unless (fboundp 'while-let)
  (defmacro while-let (spec &rest body)
    "Bind variables per SPEC and evaluate BODY while all bindings are non-nil.
SPEC has the same shape as in `if-let*'."
    (declare (indent 1) (debug if-let))
    (let ((done (gensym "done")))
      `(catch ',done
         (while t
           (if-let* ,spec
               (progn ,@body)
             (throw ',done nil)))))))

(unless (fboundp 'and-let*)
  (defmacro and-let* (varlist &rest body)
    "Bind variables per VARLIST and conditionally evaluate BODY.
Like `when-let*', but when BODY is empty and all bindings are non-nil the
result is the value of the last binding."
    (declare (indent 1) (debug if-let*))
    (let (res)
      (if varlist
          `(let* ,(setq varlist (internal--build-bindings varlist))
             (when ,(setq res (caar (last varlist)))
               ,@(or body `(,res))))
        `(let* () ,@(or body '(t)))))))

;;;; --- Doc 16 breadth round 8: with-memoization (setf-based) -----------
;; The Emacs `with-memoization' uses `gv-letplace'; the standalone reader
;; lacks full `gv', so this shim expands to `setf' instead (which round 8
;; taught `gethash'/`get'/... places via `cl-simple-setter').  Trade-off:
;; PLACE's subforms are evaluated more than once, so callers should use
;; simple subforms (e.g. `(gethash KEY TABLE)' with variable KEY/TABLE).

(unless (fboundp 'with-memoization)
  (defmacro with-memoization (place &rest code)
    "Return the value of CODE, caching it in PLACE.
If PLACE is already non-nil, return it without evaluating CODE."
    (declare (indent 1) (debug (gv-place body)))
    (let ((val (make-symbol "val")))
      `(or ,place
           (let ((,val (progn ,@code)))
             (setf ,place ,val)
             ,val)))))

;;;; --- Doc 16 breadth round 12: subr.el / macroexp list helpers --------
;; delete-consecutive-dups / rassq-delete-all (subr.el) and macroexp-quote
;; (macroexp.el) were void.  Mirror the Emacs definitions; gated on
;; `unless (fboundp ...)'.  (`dlet' and `with-output-to-string' are not
;; shimmed here: the runtime's `let' is lexical so `dlet's dynamic binding
;; does not take effect, and `princ' ignores a buffer `standard-output' so
;; output capture is unavailable.)

(unless (fboundp 'delete-consecutive-dups)
  (defun delete-consecutive-dups (list &optional circular)
    "Destructively remove `equal' consecutive duplicates from LIST.
With CIRCULAR, the first and last elements are treated as consecutive."
    (let ((tail list) last)
      (while (cdr tail)
        (if (equal (car tail) (cadr tail))
            (setcdr tail (cddr tail))
          (setq last tail
                tail (cdr tail))))
      (when (and circular last (equal (car tail) (car list)))
        (setcdr last nil))
      list)))

(unless (fboundp 'rassq-delete-all)
  (defun rassq-delete-all (value alist)
    "Delete from ALIST all elements whose cdr is `eq' to VALUE.
Return the modified alist; non-cons elements are ignored."
    (while (and (consp (car alist)) (eq (cdr (car alist)) value))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr)) (eq (cdr (car tail-cdr)) value))
            (setcdr tail (cdr tail-cdr))
          (setq tail tail-cdr))))
    alist))

(unless (fboundp 'macroexp-quote)
  (defun macroexp-quote (v)
    "Return an expression E such that `(eval E)' is V.
E is V itself when V is self-quoting, otherwise (quote V)."
    (if (and (not (consp v))
             (or (keywordp v) (not (symbolp v)) (memq v '(nil t))))
        v
      (list 'quote v))))

;;;; --- Doc 16 breadth round 15: copy-hash-table (was void) -------------
;; `copy-hash-table' was void, which broke `map-copy' on hash tables.  The
;; runtime does not expose `hash-table-test', so the copy uses the default
;; test -- correct for the common `eql'/`eq'-keyed tables.

(unless (fboundp 'copy-hash-table)
  (defun copy-hash-table (table)
    "Return a shallow copy of hash TABLE.
The copy uses the default hash test, since the runtime does not expose
`hash-table-test'."
    (let ((new (make-hash-table)))
      (maphash (lambda (k v) (puthash k v new)) table)
      new)))

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
    "Stub: register NAME as a simple generalized variable setter."
    (ignore fix)
    (list 'put (list 'quote name)
          (list 'quote 'cl-simple-setter)
          (list 'quote setter))))

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

;;;; --- pcase placeholder (avoid loading vendor pcase.el which uses old `\,' symbol escape) ---

(unless (fboundp 'pcase)
  ;; Phase 4 batch 2 — pcase with backquote / pred / and / or patterns.
  ;; Implements the pattern subset cl-macs / cl-loop / cl-some etc.
  ;; expand into.  Pure elisp on top of bootstrap eval primitives.
  ;;
  ;; Pattern syntax supported:
  ;;   `_'                — catch-all
  ;;   INTEGER / STRING   — `equal' test
  ;;   `(quote X)' / `'X' — `eq' test
  ;;   SYMBOL (bare)      — bind to value, always match
  ;;   `(pred FN)'        — call (FN value), match if non-nil
  ;;   `(and P1 P2 ...)'  — match if every P matches (binds ALL)
  ;;   `(or P1 P2 ...)'   — match if any P matches (no bindings)
  ;;   `(guard EXPR)'     — match if EXPR true
  ;;   `(let PAT EXPR)'   — bind PAT to result of EXPR (always match)
  ;;   `(backquote PAT)'  — destructure PAT.  Inside PAT:
  ;;     - `(comma SYM)'  → bind SYM to value-at-position
  ;;     - literal cons   → recursive shape match
  ;;     - literal atom   → equality test
  ;;
  ;; Backquote-pattern is the critical one — cl-macs uses it heavily
  ;; for destructuring.  E.g. `\`(,a ,b)' matches a 2-elem cons; binds
  ;; a=(car val), b=(cadr val).

  (defun emacs-stub--pcase-test (pattern value-form)
    "Build (TEST-FORM . BINDINGS) for matching PATTERN against VALUE-FORM.
VALUE-FORM is an elisp expression that evaluates to the value being
tested.  TEST-FORM is an elisp expression that evaluates to non-nil
when the pattern matches.  BINDINGS is a list of (SYMBOL FORM) pairs
to let-bind in the case body when matched."
    (cond
     ;; `_' wildcard.
     ((eq pattern '_) (cons t nil))
     ;; Bare symbol: bind to value, always match.
     ((symbolp pattern)
      (cons t (list (list pattern value-form))))
     ;; Number / string literal.
     ((or (integerp pattern) (stringp pattern))
      (cons (list 'equal value-form pattern) nil))
     ;; Cons cell — examine head for pattern type.
     ((consp pattern)
      (let ((head (car pattern))
            (rest (cdr pattern)))
        (cond
         ;; (quote X)
         ((eq head 'quote)
          (cons (list 'eq value-form (list 'quote (car rest))) nil))
         ;; (pred FN)
         ((eq head 'pred)
          (let ((fn (car rest)))
            (cons (list 'funcall (list 'function fn) value-form) nil)))
         ;; (guard EXPR)
         ((eq head 'guard)
          (cons (car rest) nil))
         ;; (let PAT EXPR)
         ((eq head 'let)
          (let* ((sub-pat (car rest))
                 (sub-expr (car (cdr rest)))
                 (built (emacs-stub--pcase-test sub-pat sub-expr)))
            (cons (car built) (cdr built))))
         ;; (and P1 P2 ...)
         ((eq head 'and)
          (emacs-stub--pcase-and rest value-form))
         ;; (or P1 P2 ...)
         ((eq head 'or)
          (emacs-stub--pcase-or rest value-form))
         ;; (backquote ...) - destructure cons / atom shape
         ((eq head 'backquote)
          (emacs-stub--pcase-backquote (car rest) value-form))
         ;; Unknown — treat as opaque catch-all (= permissive).
         (t (cons t nil)))))
     ;; Other atom (symbol via symbolp above; vector etc.)
     (t (cons (list 'equal value-form (list 'quote pattern)) nil))))

  (defun emacs-stub--pcase-and (patterns value-form)
    "Build (TEST . BINDINGS) for an `and' pattern (= all PATTERNS match)."
    (let ((tests nil)
          (bindings nil)
          (cur patterns))
      (while cur
        (let* ((built (emacs-stub--pcase-test (car cur) value-form))
               (t1 (car built))
               (b1 (cdr built)))
          (setq tests (cons t1 tests))
          (setq bindings (append bindings b1)))
        (setq cur (cdr cur)))
      (cons (cons 'and (let ((rev nil))
                         (while tests (setq rev (cons (car tests) rev)) (setq tests (cdr tests)))
                         rev))
            bindings)))

  (defun emacs-stub--pcase-or (patterns value-form)
    "Build (TEST . BINDINGS) for an `or' pattern.  No bindings (= ambiguous)."
    (let ((tests nil)
          (cur patterns))
      (while cur
        (let* ((built (emacs-stub--pcase-test (car cur) value-form))
               (t1 (car built)))
          (setq tests (cons t1 tests)))
        (setq cur (cdr cur)))
      (cons (cons 'or (let ((rev nil))
                        (while tests (setq rev (cons (car tests) rev)) (setq tests (cdr tests)))
                        rev))
            nil)))

  (defun emacs-stub--pcase-backquote (pat value-form)
    "Build (TEST . BINDINGS) for a backquote-pattern.
Walks PAT recursively; `(comma SYM)' binds SYM to corresponding
position; literal cons recurses with `car'/`cdr' index forms; atom
does `equal' check."
    (cond
     ;; (comma SYM) — bind SYM to value-form, always match.
     ((and (consp pat) (eq (car pat) 'comma))
      (let ((sym (car (cdr pat))))
        (cond
         ((eq sym '_) (cons t nil))
         ((symbolp sym) (cons t (list (list sym value-form))))
         (t (emacs-stub--pcase-test sym value-form)))))
     ;; (comma-at SYM) — bind SYM to remaining list (= value-form is tail).
     ((and (consp pat) (eq (car pat) 'comma-at))
      (let ((sym (car (cdr pat))))
        (cons t (list (list sym value-form)))))
     ;; Cons cell — recursively destructure car / cdr.
     ((consp pat)
      (let* ((head-build (emacs-stub--pcase-backquote
                          (car pat) (list 'car value-form)))
             (tail-build (emacs-stub--pcase-backquote
                          (cdr pat) (list 'cdr value-form))))
        (cons (list 'and
                    (list 'consp value-form)
                    (car head-build)
                    (car tail-build))
              (append (cdr head-build) (cdr tail-build)))))
     ;; nil at end of list — match nil tail.
     ((null pat)
      (cons (list 'null value-form) nil))
     ;; Other atom — equality test.
     (t
      (cons (list 'equal value-form (list 'quote pat)) nil))))

  (defmacro pcase (expr &rest cases)
    "Phase 4 batch 2 pcase: dispatch EXPR through CASES.
See `emacs-stub--pcase-test' for supported pattern shapes."
    (let ((value-sym (make-symbol "--pcase-value--"))
          (cond-clauses nil))
      (dolist (case cases)
        (let* ((pat (car case))
               (body (cdr case))
               (built (emacs-stub--pcase-test pat value-sym))
               (test (car built))
               (bindings (cdr built)))
          (push (list test
                      (if bindings
                          (cons 'let (cons bindings body))
                        (cons 'progn body)))
                cond-clauses)))
      (let ((forward nil))
        (while cond-clauses
          (setq forward (cons (car cond-clauses) forward))
          (setq cond-clauses (cdr cond-clauses)))
        (list 'let (list (list value-sym expr))
              (cons 'cond forward))))))

(defun emacs-stub--pcase-let-binding (binding)
  "Return (TEMP TEST BINDINGS) for a single pcase-let BINDING."
  (let* ((pattern (car binding))
         (expr (car (cdr binding)))
         (value-sym (make-symbol "--pcase-let-value--"))
         (built (emacs-stub--pcase-test pattern value-sym)))
    (list (list value-sym expr) (car built) (cdr built))))

(unless (fboundp 'pcase-let)
  (defmacro pcase-let (bindings &rest body)
    "Minimal `pcase-let' supporting the local pcase pattern subset."
    (let ((forms body)
          (rev-bindings nil))
      (dolist (binding bindings)
        (push binding rev-bindings))
      (dolist (binding rev-bindings)
        (let* ((built (emacs-stub--pcase-let-binding binding))
               (temp-binding (car built))
               (test (car (cdr built)))
               (pattern-bindings (car (cdr (cdr built)))))
          (setq forms
                (list (list 'let (list temp-binding)
                            (if pattern-bindings
                                (list 'when test
                                      (cons 'let (cons pattern-bindings forms)))
                              (cons 'when (cons test forms))))))))
      (if bindings (car forms) (cons 'progn body)))))

(unless (fboundp 'pcase-let*)
  (defmacro pcase-let* (bindings &rest body)
    "Minimal `pcase-let*' supporting sequential pcase bindings."
    (if bindings
        (list 'pcase-let (list (car bindings))
              (cons 'pcase-let* (cons (cdr bindings) body)))
      (cons 'progn body))))

(unless (fboundp 'pcase-dolist)
  (defmacro pcase-dolist (spec &rest body)
    "Minimal `pcase-dolist' supporting the local pcase pattern subset."
    (let* ((pattern (car spec))
           (list-form (car (cdr spec)))
           (result-form (car (cdr (cdr spec))))
           (value-sym (make-symbol "--pcase-dolist-value--"))
           (built (emacs-stub--pcase-test pattern value-sym))
           (test (car built))
           (pattern-bindings (cdr built)))
      (list 'dolist (list value-sym list-form result-form)
            (if pattern-bindings
                (list 'when test
                      (cons 'let (cons pattern-bindings body)))
              (cons 'when (cons test body)))))))

(unless (featurep 'pcase) (provide 'pcase))

;;;; --- cl-* macros / fns (Phase 4 batch 3 — minimal cl-lib subset) ---
;;
;; Bypass loading vendor cl-macs.el (= which fails on deep pcase patterns).
;; Provide just the cl-* surface anvil-memory uses, mapped to plain elisp.

(defun emacs-stub--split-cl-arglist (arglist)
  "Split ARGLIST into (POSITIONAL OPTIONALS RESTSYM KEYS).
KEYS = list of (KEYWORD-NAME PARAM-SYM DEFAULT-FORM) triples."
  (let ((positional nil)
        (optionals nil)
        (restsym nil)
        (keys nil)
        (mode 'positional)
        (cur arglist))
    (while cur
      (let ((tok (car cur)))
        (cond
         ((eq tok '&optional) (setq mode 'optional))
         ((eq tok '&rest)     (setq mode 'rest))
         ((eq tok '&key)      (setq mode 'key))
         ((eq tok '&aux)      (setq mode 'aux))
         (t
          (cond
           ((eq mode 'positional) (setq positional (cons tok positional)))
           ((eq mode 'optional)
            (setq optionals (cons tok optionals)))
           ((eq mode 'rest)
            (setq restsym tok))
           ((eq mode 'key)
            (let* ((sym (if (consp tok) (car tok) tok))
                   (default (if (consp tok) (car (cdr tok)) nil))
                   (kwname (intern
                            (concat ":"
                                    (symbol-name sym)))))
              (setq keys (cons (list kwname sym default) keys))))
           ;; &aux: drop (= local lets, rarely critical for stubs)
           ((eq mode 'aux) nil)))))
      (setq cur (cdr cur)))
    (let ((rev-positional nil) (rev-optionals nil) (rev-keys nil)
          (p positional) (o optionals) (k keys))
      (while p (setq rev-positional (cons (car p) rev-positional)) (setq p (cdr p)))
      (while o (setq rev-optionals (cons (car o) rev-optionals)) (setq o (cdr o)))
      (while k (setq rev-keys (cons (car k) rev-keys)) (setq k (cdr k)))
      (list rev-positional rev-optionals restsym rev-keys))))

(defun emacs-stub--cl-key-bindings (keys restsym)
  "Build let-bindings for KEYS by scanning RESTSYM (= the &rest var).
Each binding is (PARAM (or (cadr (memq KW RESTSYM)) DEFAULT))."
  (let ((out nil)
        (cur keys))
    (while cur
      (let* ((entry (car cur))
             (kw (car entry))
             (sym (car (cdr entry)))
             (def (car (cdr (cdr entry)))))
        (setq out (cons (list sym
                              (list 'or
                                    (list 'car
                                          (list 'cdr
                                                (list 'memq (list 'quote kw) restsym)))
                                    def))
                        out)))
      (setq cur (cdr cur)))
    (let ((rev nil) (c out))
      (while c (setq rev (cons (car c) rev)) (setq c (cdr c)))
      rev)))

(unless (fboundp 'cl-defun)
  ;; cl-defun supporting &optional, &rest, &key (= adequate for
  ;; anvil-memory / anvil-state arglists).
  ;;
  ;; Strategy: expand (cl-defun NAME (POS &optional O &key K1 K2) BODY) to
  ;; (defun NAME (POS &optional O &rest --cl-keys)
  ;;   (let ((K1 (or (cadr (memq :K1 --cl-keys)) DEFAULT))
  ;;         (K2 (or (cadr (memq :K2 --cl-keys)) DEFAULT)))
  ;;     BODY))
  ;; If &rest is present in the original arglist, reuse that name instead
  ;; of synthesizing --cl-keys.
  (defvar emacs-stub--cl-defun-call-count 0
    "Bumped each time the cl-defun macro stub expands a form.")
  ;; Two registration paths needed:
  ;;   1. build-tool/eval recognizes the (macro lambda ...) function cell
  ;;      → use plain `defmacro' (writes to env.set_function)
  ;;   2. nelisp-eval-form (the FULL self-host evaluator) consults
  ;;      `nelisp--macros' hashtable, NOT the function cell → also
  ;;      puthash into nelisp--macros so the takeover path expands too
  ;;
  ;; Path (2) registration happens at the bottom of this `unless'
  ;; clause via `(when (boundp 'nelisp--macros) ...)' guard.
  (defmacro cl-defun (name arglist &rest body)
    "Stub: cl-defun with &optional / &rest / &key support."
    (setq emacs-stub--cl-defun-call-count
          (+ 1 emacs-stub--cl-defun-call-count))
    (let* ((parts (emacs-stub--split-cl-arglist arglist))
           (positional (car parts))
           (optionals (car (cdr parts)))
           (restsym (car (cdr (cdr parts))))
           (keys (car (cdr (cdr (cdr parts))))))
      (cond
       ;; No &key — emit plain defun with original layout (preserve &rest).
       ((null keys)
        (let ((out positional))
          (when optionals
            (let ((tail (cons '&optional nil))
                  (o optionals))
              (while o (setq tail (append tail (list (car o)))) (setq o (cdr o)))
              (let ((all out) (t2 tail))
                (while t2 (setq all (append all (list (car t2)))) (setq t2 (cdr t2)))
                (setq out all))))
          (when restsym
            (setq out (append out (list '&rest restsym))))
          (cons 'defun (cons name (cons out body)))))
       (t
        ;; &key present — synthesize &rest --cl-keys, scan it for kw values.
        (let* ((rest-name (or restsym '--cl-keys))
               (real-arglist positional))
          (when optionals
            (let ((tail (cons '&optional nil))
                  (o optionals))
              (while o (setq tail (append tail (list (car o)))) (setq o (cdr o)))
              (setq real-arglist (append real-arglist tail))))
          (setq real-arglist (append real-arglist (list '&rest rest-name)))
          (let* ((bindings (emacs-stub--cl-key-bindings keys rest-name))
                 (real-body (list (cons 'let* (cons bindings body)))))
            (cons 'defun (cons name (cons real-arglist real-body))))))))))

(unless (fboundp 'cl-incf)
  (defmacro cl-incf (place &optional delta)
    "Stub: (setq PLACE (+ PLACE (or DELTA 1)))."
    (list 'setq place (list '+ place (or delta 1)))))

(unless (fboundp 'cl-decf)
  (defmacro cl-decf (place &optional delta)
    (list 'setq place (list '- place (or delta 1)))))

(unless (fboundp 'cl-some)
  (defun cl-some (predicate sequence &rest more)
    "Stub: return first non-nil PREDICATE result over SEQUENCE.
Ignores MORE (= multi-list version)."
    (ignore more)
    (let ((cur sequence)
          (result nil))
      (while (and cur (not result))
        (setq result (funcall predicate (car cur)))
        (setq cur (cdr cur)))
      result)))

(unless (fboundp 'cl-every)
  (defun cl-every (predicate sequence &rest more)
    (ignore more)
    (let ((cur sequence)
          (ok t))
      (while (and cur ok)
        (unless (funcall predicate (car cur)) (setq ok nil))
        (setq cur (cdr cur)))
      ok)))

(unless (fboundp 'cl-position)
  (defun cl-position (item sequence &rest _keys)
    "Stub: return index of ITEM in SEQUENCE (= eql), or nil."
    (let ((cur sequence) (idx 0) (found nil))
      (while (and cur (not found))
        (when (or (eq (car cur) item) (equal (car cur) item))
          (setq found idx))
        (setq cur (cdr cur)) (setq idx (+ idx 1)))
      found)))

(unless (fboundp 'cl-find)
  (defun cl-find (item sequence &rest _keys)
    (let ((cur sequence) (found nil))
      (while (and cur (not found))
        (when (or (eq (car cur) item) (equal (car cur) item))
          (setq found (car cur)))
        (setq cur (cdr cur)))
      found)))

(unless (fboundp 'cl-remove-if-not)
  (defun cl-remove-if-not (predicate sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (when (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-remove-if)
  (defun cl-delete-if (predicate sequence &rest _keys)
    "Stub: alias for cl-remove-if (in-place delete not supported)."
    (let ((acc nil) (cur sequence))
      (while cur
        (unless (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (let ((rev nil))
        (while acc (setq rev (cons (car acc) rev)) (setq acc (cdr acc)))
        rev)))
  (defun cl-delete-if-not (predicate sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (when (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (let ((rev nil))
        (while acc (setq rev (cons (car acc) rev)) (setq acc (cdr acc)))
        rev)))
  (defun cl-remove-if (predicate sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (unless (funcall predicate (car cur))
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-delete-if)
  (defalias 'cl-delete-if 'cl-remove-if))

(unless (fboundp 'cl-delete-duplicates)
  (defun cl-delete-duplicates (sequence &rest _keys)
    (let ((acc nil) (cur sequence))
      (while cur
        (unless (member (car cur) acc)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-union)
  (defun cl-union (list1 list2 &rest _keys)
    (let ((acc list1) (cur list2))
      (while cur
        (unless (member (car cur) acc)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      acc)))

(unless (fboundp 'cl-intersection)
  (defun cl-intersection (list1 list2 &rest _keys)
    (let ((acc nil) (cur list1))
      (while cur
        (when (member (car cur) list2)
          (setq acc (cons (car cur) acc)))
        (setq cur (cdr cur)))
      (nreverse acc))))

(unless (fboundp 'cl-sort)
  (defun cl-sort (sequence predicate &rest _keys)
    (sort sequence predicate)))

(unless (fboundp 'cl-loop)
  ;; cl-loop is incredibly complex; provide a minimal version that
  ;; handles the patterns anvil-memory uses (= for X in LIST do/collect).
  (defmacro cl-loop (&rest clauses)
    "Stub: minimal cl-loop supporting `for VAR in LIST do/collect/sum/count/...'.
For patterns this stub does not recognise, returns nil."
    (emacs-stub--cl-loop-build clauses)))

(unless (fboundp 'emacs-stub--cl-loop-build)
  (defun emacs-stub--cl-loop-build (clauses)
    "Build expansion for cl-loop CLAUSES.  Recognises `for VAR in LIST'
    + `do FORM' / `collect FORM' / `sum FORM' / `count FORM' / `with VAR = VAL'.
    Returns a `let'/`while' form, or nil for unrecognised shapes."
    (let ((var nil) (list-form nil) (do-forms nil) (collect-form nil)
          (sum-form nil) (count-form nil) (with-bindings nil)
          (cur clauses) (recognised t))
      (while (and cur recognised)
        (let ((kw (car cur)))
          (cond
           ((eq kw 'for)
            (setq var (car (cdr cur)))
            (when (eq (car (cdr (cdr cur))) 'in)
              (setq list-form (car (cdr (cdr (cdr cur)))))
              (setq cur (cdr (cdr (cdr (cdr cur)))))))
           ((eq kw 'do)
            (setq do-forms (cons (car (cdr cur)) do-forms))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'collect)
            (setq collect-form (car (cdr cur)))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'sum)
            (setq sum-form (car (cdr cur)))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'count)
            (setq count-form (car (cdr cur)))
            (setq cur (cdr (cdr cur))))
           ((eq kw 'with)
            (let ((wname (car (cdr cur))))
              (when (eq (car (cdr (cdr cur))) '=)
                (setq with-bindings
                      (append with-bindings
                              (list (list wname (car (cdr (cdr (cdr cur))))))))
                (setq cur (cdr (cdr (cdr (cdr cur))))))))
           (t (setq recognised nil)))))
      (cond
       ((not recognised) nil)
       (collect-form
        (let ((acc-sym (make-symbol "--loop-acc--")))
          (list 'let (cons (list acc-sym nil) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'setq acc-sym (list 'cons collect-form acc-sym)))
                (list 'nreverse acc-sym))))
       (sum-form
        (let ((acc-sym (make-symbol "--loop-sum--")))
          (list 'let (cons (list acc-sym 0) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'setq acc-sym (list '+ acc-sym sum-form)))
                acc-sym)))
       (count-form
        (let ((acc-sym (make-symbol "--loop-count--")))
          (list 'let (cons (list acc-sym 0) with-bindings)
                (list 'dolist (list var list-form)
                      (list 'when count-form
                            (list 'setq acc-sym (list '+ acc-sym 1))))
                acc-sym)))
       (do-forms
        (let ((rev nil))
          (while do-forms (setq rev (cons (car do-forms) rev)) (setq do-forms (cdr do-forms)))
          (list 'let with-bindings
                (cons 'dolist (cons (list var list-form) rev)))))
       (t (list 'let with-bindings nil))))))

;; Provide cl-macs / cl-seq as features so vendor (require ...) chains succeed
;; without actually loading the heavyweight files.
(unless (fboundp 'cl-defgeneric)
  (defmacro cl-defgeneric (name arglist &rest body)
    "Stub: defgeneric → defun (= no real generic dispatch)."
    (cons 'defun (cons name (cons arglist body)))))

(unless (fboundp 'cl-defmethod)
  (defmacro cl-defmethod (name arglist &rest body)
    "Stub: defmethod → defun (= last-defined wins, no specializer dispatch).
When NAME is a setf-method list `(setf X)', intern the printed form
`\"(setf X)\"' as a symbol so `defun' has a usable target.  Strips
specializer cons-cells from arglist (e.g. `(SEQUENCE array)' → `SEQUENCE')."
    (let ((real-name
           (cond
            ((symbolp name) name)
            ((and (consp name) (eq (car name) 'setf))
             (intern (format "(setf %s)" (car (cdr name)))))
            (t (intern (format "%S" name))))))
      (cons 'defun
            (cons real-name
                  (cons (mapcar (lambda (a) (if (consp a) (car a) a)) arglist)
                        body))))))

(unless (fboundp 'cl-defstruct)
  (defmacro cl-defstruct (name &rest slots)
    "Stub: defstruct → minimal alist-backed accessors."
    (let ((sname (if (consp name) (car name) name))
          (slot-names (mapcar (lambda (s) (if (consp s) (car s) s)) slots)))
      (let ((forms nil))
        ;; make-NAME constructor → returns alist of slots.
        (push (list 'defun (intern (concat "make-" (symbol-name sname)))
                    '(&rest args)
                    '(let ((alist nil)
                           (cur args))
                       (while cur
                         (setq alist (cons (cons (car cur) (car (cdr cur))) alist))
                         (setq cur (cdr (cdr cur))))
                       (cons (quote ,sname) alist)))
              forms)
        ;; NAME-p predicate.
        (push (list 'defun (intern (concat (symbol-name sname) "-p"))
                    '(obj)
                    (list 'and '(consp obj) (list 'eq '(car obj) (list 'quote sname))))
              forms)
        ;; NAME-SLOT accessor for each slot.
        (dolist (slot slot-names)
          (let ((kw (intern (concat ":" (symbol-name slot)))))
            (push (list 'defun (intern (concat (symbol-name sname) "-" (symbol-name slot)))
                        '(obj)
                        (list 'cdr (list 'assoc kw '(cdr obj))))
                  forms)))
        (cons 'progn (nreverse forms))))))

(put 'cl-defstruct 'emacs-stub-placeholder t)

(unless (fboundp 'cl-case)
  (defmacro cl-case (expr &rest cases)
    "Stub: cl-case → equivalent to cond with eql tests."
    (let ((value-sym (make-symbol "--cl-case--"))
          (clauses nil))
      (dolist (c cases)
        (let ((key (car c)) (body (cdr c)))
          (cond
           ((or (eq key 't) (eq key 'otherwise))
            (push (cons t body) clauses))
           ((listp key)
            (push (cons (list 'memql value-sym (list 'quote key)) body) clauses))
           (t (push (cons (list 'eql value-sym (list 'quote key)) body) clauses)))))
      (let ((rev nil))
        (while clauses (setq rev (cons (car clauses) rev)) (setq clauses (cdr clauses)))
        (list 'let (list (list value-sym expr))
              (cons 'cond rev))))))

(unless (fboundp 'cl-pushnew)
  (defmacro cl-pushnew (item place &rest _keys)
    (list 'unless (list 'member item place)
          (list 'setq place (list 'cons item place)))))

(unless (fboundp 'cl-letf)
  (defmacro cl-letf (bindings &rest body)
    "Stub: cl-letf → simple let* (= no place mutation tracking)."
    (cons 'let* (cons bindings body))))

(unless (fboundp 'cl-letf*)
  (defalias 'cl-letf* 'cl-letf))

(unless (fboundp 'cl-flet)
  (defmacro cl-flet (bindings &rest body)
    "Stub: cl-flet → cl-letf with function-cell binding (= simplified)."
    (cons 'let (cons bindings body))))

(unless (fboundp 'cl-labels)
  (defalias 'cl-labels 'cl-flet))

(unless (fboundp 'cl-block)
  (defmacro cl-block (_name &rest body)
    "Stub: cl-block → progn (= no return-from support)."
    (cons 'progn body)))

(unless (fboundp 'cl-return-from)
  (defmacro cl-return-from (_name &optional _val)
    "Stub: cl-return-from → no-op."
    nil))

(unless (fboundp 'cl-return)
  (defalias 'cl-return 'cl-return-from))

(unless (fboundp 'cl-getf)
  (defalias 'cl-getf 'plist-get))

(unless (fboundp 'cl-first)
  (defalias 'cl-first 'car))
(unless (fboundp 'cl-second)
  (defun cl-second (l) (car (cdr l))))
(unless (fboundp 'cl-third)
  (defun cl-third (l) (car (cdr (cdr l)))))

(unless (featurep 'cl-macs) (provide 'cl-macs))
(unless (featurep 'cl-seq) (provide 'cl-seq))
(unless (featurep 'cl-extra) (provide 'cl-extra))
(unless (featurep 'cl-generic) (provide 'cl-generic))
;; Phase B5 (= 2026-05-09): phantom-provide cl-lib so anvil-server's
;; `(require 'cl-lib)' short-circuits without descending into vendor
;; cl-lib.el (= ~80s load on standalone NeLisp).  The Cl primitives
;; anvil-* uses are already covered by NeLisp natives (`cl-defstruct',
;; `cl-incf', `cl-decf', `setf' from Phase B4) plus the `cl-loop' /
;; `cl-some' / `cl-pushnew' / etc. stubs lower in this file.
(unless (featurep 'cl-lib) (provide 'cl-lib))
(unless (featurep 'cl-loaddefs) (provide 'cl-loaddefs))

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



(unless (and (fboundp 'truncate)
             ;; If truncate is the no-op bulk stub, override with real impl.
             (not (get 'truncate 'emacs-stub-bulk)))
  (defun truncate (number &optional divisor)
    "Phase 6 polyfill: integer truncation toward zero.
NUMBER may be int or float; DIVISOR optional (= NUMBER / DIVISOR)."
    (cond
     ((null number) 0)
     (divisor
      (truncate (/ number divisor)))
     ((integerp number) number)
     ((< number 0)
      (- (truncate (- number))))
     ((>= number 1)
      ;; Avoid float literals and `while' in this early bootstrap body:
      ;; standalone-reader currently segfaults while installing that shape.
      (+ 1 (truncate (- number 1))))
     (t 0)))
  (put 'truncate 'emacs-stub-bulk nil))

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

;;;; --- Custom metadata helpers (= preloaded in real Emacs) ---

(unless (fboundp 'custom-add-option)
  (defun custom-add-option (symbol option)
    "Polyfill: add OPTION to SYMBOL's `custom-options' metadata."
    (let ((options (get symbol 'custom-options)))
      (unless (member option options)
        (put symbol 'custom-options (cons option options))))))

(unless (fboundp 'custom-add-frequent-value)
  (defalias 'custom-add-frequent-value 'custom-add-option))

(unless (fboundp 'custom-variable-p)
  (defun custom-variable-p (variable)
    "Polyfill: return non-nil when VARIABLE has Custom metadata."
    (and (symbolp variable)
         (or (get variable 'standard-value)
             (get variable 'custom-autoload)))))

(unless (fboundp 'defgroup)
  (defmacro defgroup (name members doc &rest args)
    "Standalone load-time fallback for Custom group declarations."
    `(progn
       (put ',name 'custom-group ',members)
       (put ',name 'group-documentation ,doc)
       (put ',name 'custom-args ',args)
       ',name)))

(unless (fboundp 'defcustom)
  (defmacro defcustom (symbol standard doc &rest args)
    "Standalone load-time fallback for Custom variable declarations."
    `(progn
       (defvar ,symbol ,standard ,doc)
       (put ',symbol 'standard-value (list ',standard))
       (put ',symbol 'custom-args ',args)
       ',symbol)))

(unless (fboundp 'convert-standard-filename)
  (defun convert-standard-filename (filename)
    "Standalone fallback for GNU-style standard filename conversion.
NeLisp currently targets POSIX paths, so no platform-specific rewriting
is required."
    filename))

(unless (fboundp 'string-to-list)
  (defun string-to-list (string)
    "Return a list of character codes in STRING."
    (unless (stringp string)
      (signal 'wrong-type-argument (list 'stringp string)))
    (let ((i (1- (length string)))
          chars)
      (while (>= i 0)
        (setq chars (cons (aref string i) chars))
        (setq i (1- i)))
      chars)))

;; Phase B5 globals — anvil-server.el / vendor cl-* reach for these as
;; `defcustom' defaults / load-path participants.  Empty defaults are
;; safe because anvil callers fall back through (or VAR DEFAULT).
(unless (boundp 'emacs-major-version)
  (defvar emacs-major-version 29))
(unless (boundp 'emacs-minor-version)
  (defvar emacs-minor-version 1))
(unless (boundp 'emacs-build-system)
  (defvar emacs-build-system
    (if (fboundp 'system-name) (system-name) "standalone")))
(unless (boundp 'emacs-build-time)
  (defvar emacs-build-time nil))
(unless (boundp 'emacs-build-number)
  (defvar emacs-build-number 1))
(unless (boundp 'system-configuration)
  (defvar system-configuration "nelisp-standalone"))
(unless (boundp 'source-directory)
  (defvar source-directory ""))
(unless (boundp 'motif-version-string)
  (defvar motif-version-string nil))
(unless (boundp 'gtk-version-string)
  (defvar gtk-version-string nil))
(unless (boundp 'ns-version-string)
  (defvar ns-version-string nil))
(unless (boundp 'cairo-version-string)
  (defvar cairo-version-string nil))
(unless (boundp 'emacs-repository-version)
  (defvar emacs-repository-version nil))
(unless (boundp 'emacs-repository-branch)
  (defvar emacs-repository-branch nil))
(unless (boundp 'emacs-bzr-version)
  (defvar emacs-bzr-version nil))
(unless (boundp 'user-emacs-directory)
  (defvar user-emacs-directory ""))
(unless (boundp 'user-init-file)
  (defvar user-init-file nil))
(unless (boundp 'data-directory)
  (defvar data-directory ""))
(unless (boundp 'invocation-directory)
  (defvar invocation-directory ""))
(unless (boundp 'invocation-name)
  (defvar invocation-name "nelisp"))

(unless (fboundp 'android-read-build-system)
  (defun android-read-build-system ()
    "Standalone compatibility shim: Android build system is unknown."
    nil))

(unless (fboundp 'android-read-build-time)
  (defun android-read-build-time ()
    "Standalone compatibility shim: Android build time is unknown."
    nil))

(unless (fboundp 'emacs-version)
  (defun emacs-version (&optional here)
    "Return or insert a lightweight Emacs-compatible version string."
    (let ((version-string
           (format "GNU Emacs %s (build %s, %s)"
                   (if (boundp 'emacs-version) emacs-version "29.1")
                   (if (boundp 'emacs-build-number) emacs-build-number 1)
                   (if (boundp 'system-configuration)
                       system-configuration
                     "nelisp-standalone"))))
      (if here
          (insert version-string)
        version-string))))

(unless (fboundp 'version)
  (defalias 'version 'emacs-version))

(unless (fboundp 'emacs-repository-version-git)
  (defun emacs-repository-version-git (&optional _dir)
    "Standalone compatibility shim: repository revision is unknown."
    nil))

(unless (fboundp 'emacs-repository-version-android)
  (defun emacs-repository-version-android ()
    "Standalone compatibility shim: Android repository revision is unknown."
    nil))

(unless (fboundp 'emacs-repository-get-version)
  (defun emacs-repository-get-version (&optional _dir _external)
    "Standalone compatibility shim: repository revision is unknown."
    nil))

(unless (fboundp 'emacs-bzr-get-version)
  (defalias 'emacs-bzr-get-version 'emacs-repository-get-version))

(unless (fboundp 'emacs-repository-branch-android)
  (defun emacs-repository-branch-android ()
    "Standalone compatibility shim: Android repository branch is unknown."
    nil))

(unless (fboundp 'emacs-repository-branch-git)
  (defun emacs-repository-branch-git (&optional _dir)
    "Standalone compatibility shim: repository branch is unknown."
    nil))

(unless (fboundp 'emacs-repository-get-branch)
  (defun emacs-repository-get-branch (&optional _dir)
    "Standalone compatibility shim: repository branch is unknown."
    nil))

(unless (boundp 'three-step-help)
  (defvar three-step-help nil))
(unless (boundp 'help-for-help-use-variable-pitch)
  (defvar help-for-help-use-variable-pitch t))

(unless (fboundp 'help--help-screen)
  (defun help--help-screen (help-line _help-text _helped-map _buffer-name)
    "Standalone compatibility shim for `make-help-screen' dispatchers."
    (let ((line (if (and (fboundp 'substitute-command-keys)
                         (stringp help-line))
                    (substitute-command-keys help-line)
                  help-line)))
      (when (and line (fboundp 'message))
        (message "%s" line)))
    nil))

(unless (fboundp 'make-help-screen)
  (defmacro make-help-screen (fname help-line help-text helped-map
                                    &optional buffer-name)
    "Construct a lightweight standalone help command named FNAME."
    (list 'defun fname nil
          "Help command."
          (list 'interactive)
          (list 'help--help-screen
                help-line
                help-text
                helped-map
                buffer-name))))

(unless (featurep 'help-macro)
  (provide 'help-macro))

;; Phase B5 — coding-string identity stubs.  Standalone NeLisp strings
;; are UTF-8 already; the bulk-stub no-op (returns nil) breaks JSON-RPC
;; parsing when callers wrap incoming strings with `decode-coding-string'.
;; Forward to identity so the round-trip is a no-op.
(when (emacs-stub--install-function-p 'decode-coding-string)
  (defun decode-coding-string (string &optional _coding-system _nocopy &rest _)
    "Identity stub — return STRING unchanged.  NeLisp strings are UTF-8."
    string))
(when (emacs-stub--install-function-p 'encode-coding-string)
  (defun encode-coding-string (string &optional _coding-system _nocopy &rest _)
    "Identity stub — return STRING unchanged."
    string))
