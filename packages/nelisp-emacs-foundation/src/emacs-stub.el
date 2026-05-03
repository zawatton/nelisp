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

(unless (fboundp 'narrow-to-region)
  (defun narrow-to-region (start end) (ignore start end) nil))

(unless (fboundp 'widen)
  (defun widen () nil))

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

(unless (fboundp 'pcase-let)
  (defmacro pcase-let (bindings &rest body)
    "Stub: equivalent to plain `let'."
    (cons 'let (cons bindings body))))

(unless (fboundp 'pcase-let*)
  (defmacro pcase-let* (bindings &rest body)
    "Stub: equivalent to plain `let*'."
    (cons 'let* (cons bindings body))))

(unless (fboundp 'pcase-dolist)
  (defmacro pcase-dolist (spec &rest body)
    "Stub: equivalent to plain `dolist'."
    (cons 'dolist (cons spec body))))

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


;;;; --- time + crypto polyfills (Phase 6 write path) ---------------------
;;
;; Wraps the build-tool builtins `nl-current-unix-time' / `nl-secure-hash'
;; (= bi_nl_current_unix_time / bi_nl_secure_hash in
;; build-tool/eval/builtins.rs).  Real `current-time' returns a HIGH/LOW/
;; MICRO list — anvil callsites only pull (truncate (float-time)) so we
;; expose that path directly without bothering with the legacy list shape.

(defun float-time (&optional time-value)
  "Return seconds since the Unix epoch.
TIME-VALUE is accepted for API compatibility but only a nil value
is supported (= read current time)."
  (ignore time-value)
  (if (fboundp 'nl-current-unix-time)
      (nl-current-unix-time)
    0))

(defun current-time ()
  "Return current time as (HIGH LOW USEC PSEC) — Phase 6 simplified
shape that returns (T 0 0 0) where T is the Unix epoch as a single
integer.  anvil-memory only ever feeds this back into `truncate' /
`float-time' so the legacy 3-cell shape is unnecessary here."
  (list (float-time) 0 0 0))

(unless (and (fboundp 'truncate)
             ;; If truncate is the no-op bulk stub, override with real impl.
             (let ((t1 (truncate 3.7)))
               (and (integerp t1) (= t1 3))))
  (defun truncate (number &optional divisor)
    "Phase 6 polyfill: integer truncation toward zero.
NUMBER may be int or float; DIVISOR optional (= NUMBER / DIVISOR)."
    (cond
     ((null number) 0)
     (divisor
      (truncate (/ number divisor)))
     ((integerp number) number)
     ((floatp number)
      (let ((n (if (>= number 0)
                   (- number 0.0)
                 (- 0.0 number)))
            (sign (if (>= number 0) 1 -1)))
        ;; floor-by-subtraction (no `floor' builtin available).  Adequate
        ;; for the timestamp range we care about (< 2^53 seconds).
        (let ((i 0))
          (while (>= n 1.0)
            (setq n (- n 1.0))
            (setq i (+ i 1)))
          (* sign i))))
     (t 0))))

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
