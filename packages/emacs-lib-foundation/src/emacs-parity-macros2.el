;;; emacs-parity-macros2.el --- macro-expansion parity overrides for the audit replay -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Real-init audit parity fixes (b1k21 frontier), batch 2.  Companion to
;; `emacs-parity-clloop.el' (cl-loop codegen) and `emacs-parity-evil.el'
;; (cl-destructuring-bind dotted tail).  Each override here targets a
;; `nelisp-bare-abort' family observed in the first full real-init audit
;; (`/tmp/err-evil').  A `nelisp-bare-abort' is an UNCATCHABLE hard abort of
;; the native fast source evaluator: any Lisp error raised while it
;; macroexpands (or evaluates deep inside) a top-level form escapes as a
;; hard abort rather than a signalled error.  So the cure is always to make
;; the offending expander/expansion NOT error.
;;
;; All overrides are GUARDED to the standalone NeLisp runtime -- these are
;; global clobbers (backquote, with-eval-after-load, iter-defun, ...) that
;; MUST NOT touch host Emacs (Doc: "Host Emacs should remain safe to load.
;; Avoid global overrides except in guarded compatibility layers.").
;;
;; ROOTS FIXED (see each section):
;;
;;   A. backquote vector gap        -> cl-macrolet / any `,'-template with a
;;                                     vector (menus `["x" cmd]', `-let [a b]',
;;                                     keymap vectors, pcase vectors).
;;   B. `defstruct' (old cl.el)     -> void-function; ctable (ctbl:*) and
;;                                     queue.el define their structs with the
;;                                     obsolete `defstruct' name.
;;   C. define-minor-mode family    -> closure-heavy vendor easy-mmode expander
;;                                     aborts at expansion, and the closures it
;;                                     emits abort when the mode is CALLED.
;;   D. with-eval-after-load        -> vendor subr.el emits `(eval-after-load F
;;                                     (lambda () ...))'; funcalling that lambda
;;                                     (feature already loaded) aborts on the
;;                                     substrate's closure application.
;;   E. iter-defun / iter-lambda    -> generator.el's CPS transform needs
;;                                     `macroexpand-all' to honor its ENV arg
;;                                     (Doc 22 A12, a CORE gap); a narrow eager
;;                                     non-CPS shim covers the FINITE generators
;;                                     that init actually defines.

;;; Code:

(defun emacs-parity-macros2--standalone-p ()
  "Return non-nil under the standalone NeLisp runtime (never on host Emacs)."
  (or (not (boundp 'emacs-version))
      (not (stringp (and (boundp 'emacs-version) emacs-version)))
      (fboundp 'nelisp--eval-source-string)
      (fboundp 'nelisp--write-stdout-bytes)
      (fboundp 'nelisp-process-start)))

(defun emacs-parity-macros2--register-macro (sym)
  "Force SYM's current function cell into `nelisp--macros' for the self-host
evaluator, which consults the hashtable rather than the function cell."
  (when (and (boundp 'nelisp--macros)
             (hash-table-p nelisp--macros)
             (fboundp sym))
    (puthash sym (symbol-function sym) nelisp--macros)))

(when (emacs-parity-macros2--standalone-p)

  ;;;; === A. Vector-aware backquote ==================================
  ;;
  ;; ROOT (cl-macrolet abort, /tmp/err-evil:453,460; also a broad family):
  ;; the frozen prelude `nelisp--bq-expand'
  ;; (vendor/nelisp/scripts/nelisp-stdlib-prelude.el) signals
  ;;   (error "nelisp-bq: vector quasi not supported")
  ;; whenever a backquote template contains a VECTOR.  Real code hits this
  ;; constantly: easy-menu items `["Add Project" treemacs-add-project]'
  ;; (treemacs header buttons), `-let [continue t]' destructuring vectors
  ;; (treemacs custom-node finders), keymap event vectors, pcase `\`[..]'.
  ;; A local cl-macrolet macro whose body is such a template aborts at
  ;; macroexpansion -> the whole enclosing top-level form bare-aborts.
  ;;
  ;; FIX: redefine the backquote expander subsystem so a vector template
  ;; expands element-wise and is rebuilt with `vconcat' (unquote `,X' and
  ;; splice `,@X' inside a vector work through the list expansion).  The
  ;; three helpers are copied verbatim from the prelude with only the new
  ;; vector arm added, so the shim is load-order independent (mirrors the
  ;; `emacs-cl-macros--loop-*' verbatim copies in emacs-parity-clloop.el).

  (unless (fboundp 'nelisp--bq-tag-p)
    (defun nelisp--bq-tag-p (form tag punct)
      "Non-nil if FORM is a cons whose car is the backquote marker TAG or
the real Emacs-style symbol named the literal punctuation string PUNCT."
      (and (consp form)
           (let ((head (car form)))
             (or (eq head tag) (eq head (intern punct)))))))

  (defun nelisp--bq-expand (form)
    "Return the expansion of FORM under `backquote' (vector-aware)."
    (cond
     ((vectorp form)
      ;; NEW: `[...]' template -> (vconcat LIST-EXPANSION).  vconcat accepts
      ;; the list/cons/append form produced for the vector's elements, so
      ;; interior `,X' and `,@X' compose exactly as in list position.
      (list 'vconcat (nelisp--bq-expand-list (append form nil))))
     ((not (consp form))
      (list 'quote form))
     ((nelisp--bq-tag-p form 'comma ",") (cadr form))
     ((nelisp--bq-tag-p form 'comma-at ",@")
      (signal 'error (list "nelisp-bq: top-level ,@ not allowed")))
     ((nelisp--bq-tag-p form 'backquote "`")
      (list 'quote form))
     (t (nelisp--bq-expand-list form))))

  (defun nelisp--bq-expand-list (form)
    "Walk list FORM, producing the expansion.
Recognises both (... ,X ...) interior unquote and (... . ,X) dotted
unquote / (... . ,@X) dotted splice patterns."
    (let ((parts nil)
          (cur form)
          (tail-expr nil)
          (done nil)
          (has-splice nil))
      (while (and (not done) (consp cur))
        (let ((head (car cur)))
          (cond
           ((or (eq head 'comma) (eq head (intern ",")))
            (setq tail-expr (cadr cur))
            (setq done t))
           ((or (eq head 'comma-at) (eq head (intern ",@")))
            (setq tail-expr (cadr cur))
            (setq has-splice t)
            (setq done t))
           (t
            (let ((elem head))
              (cond
               ((and (consp elem)
                     (or (eq (car elem) 'comma-at) (eq (car elem) (intern ",@"))))
                (setq has-splice t)
                (push (cons 'splice (cadr elem)) parts))
               ((and (consp elem)
                     (or (eq (car elem) 'comma) (eq (car elem) (intern ","))))
                (push (cons 'list (cadr elem)) parts))
               (t
                (push (cons 'list (nelisp--bq-expand elem)) parts))))
            (setq cur (cdr cur))))))
      (when (and (not done) (not (null cur)) (not (consp cur)))
        (setq tail-expr (list 'quote cur)))
      (nelisp--bq-build (nreverse parts) tail-expr has-splice)))

  (defun nelisp--bq-build (parts tail has-splice)
    "Build the final form from PARTS list, TAIL expression, HAS-SPLICE flag."
    (cond
     ((and (null parts) (null tail))
      (list 'quote nil))
     ((null parts) tail)
     ((and (not has-splice) (null tail))
      (cons 'list (mapcar 'cdr parts)))
     ((not has-splice)
      (let ((acc tail) (rp (reverse parts)))
        (while rp
          (setq acc (list 'cons (cdr (car rp)) acc))
          (setq rp (cdr rp)))
        acc))
     (t
      (let ((args nil) (p parts))
        (while p
          (let ((kind (car (car p))) (val (cdr (car p))))
            (cond
             ((eq kind 'list) (push (list 'list val) args))
             ((eq kind 'splice) (push val args))))
          (setq p (cdr p)))
        (setq args (nreverse args))
        (when tail (setq args (append args (list tail))))
        (cons 'append args)))))

  ;; Re-route the backquote macros through the corrected expander.  `defmacro'
  ;; re-runs the runtime's macro registration; both the reader's convenience
  ;; name `backquote' and real Emacs's literal-punctuation name `\`' are bound.
  (defmacro backquote (form)
    "Expand FORM as a quasiquoted template (NeLisp minimal subset, vector-aware)."
    (nelisp--bq-expand form))
  (defmacro \` (form)
    "Alias for `backquote' under real Emacs's own back-quote symbol name."
    (nelisp--bq-expand form))
  (emacs-parity-macros2--register-macro 'backquote)
  (emacs-parity-macros2--register-macro (intern "`"))

  ;;;; === B. Obsolete cl.el `defstruct' alias =========================
  ;;
  ;; ROOT (/tmp/err-evil:11 void-function defstruct; 476-480 ctbl demos
  ;; bare-abort; 12 void-function queue-create): the emacs-ctable (`ctbl:*')
  ;; and queue.el packages still use the OBSOLETE `defstruct' name (cl.el),
  ;; not `cl-defstruct'.  `defstruct' is unbound, so ctbl:cmodel / ctbl:model
  ;; / ctbl:param and `queue' are never defined; `make-ctbl:cmodel',
  ;; `make-ctbl:model', `copy-ctbl:param', the `ctbl:param-*' accessors and
  ;; `queue-create' are then void, and the demo `let*' forms bare-abort deep
  ;; inside the fast evaluator (a void call in a non-head position aborts
  ;; rather than signalling).
  ;;
  ;; The runtime `cl-defstruct' (prelude) is correct and fully expands these:
  ;; verified in host Emacs that `(cl-defstruct ctbl:cmodel title sorter align)'
  ;; emits `make-ctbl:cmodel', per-slot accessors, `copy-ctbl:cmodel', the
  ;; predicate, and registers `nelisp-cl-macros--accessor-info' (so `setf' on
  ;; `(ctbl:param-fixed-header p)' works).  The sole gap is the missing alias.
  ;;
  ;; FIX: alias the obsolete cl.el spellings onto their cl-lib owners (only
  ;; when the owner is present and the old name is still free), mirroring the
  ;; guarded block in `src/cl-lib.el' so the two agree.  This is the load of
  ;; last resort: install even if cl-lib.el's own block did not run.
  (dolist (pair '((defstruct . cl-defstruct)
                  (defun* . cl-defun)
                  (defsubst* . cl-defsubst)
                  (defmacro* . cl-defmacro)
                  (function* . cl-function)
                  (case . cl-case)
                  (ecase . cl-ecase)
                  (typecase . cl-typecase)
                  (etypecase . cl-etypecase)
                  (loop . cl-loop)
                  (destructuring-bind . cl-destructuring-bind)
                  (multiple-value-bind . cl-multiple-value-bind)
                  (block . cl-block)
                  (return . cl-return)
                  (return-from . cl-return-from)
                  (incf . cl-incf)
                  (decf . cl-decf)
                  (pushnew . cl-pushnew)
                  (rotatef . cl-rotatef)
                  (shiftf . cl-shiftf)))
    (when (and (not (fboundp (car pair)))
               (fboundp (cdr pair)))
      (defalias (car pair) (cdr pair))
      (emacs-parity-macros2--register-macro (car pair))))
  (unless (featurep 'cl) (provide 'cl))

  ;;;; === C. define-minor-mode / -globalized-minor-mode ================
  ;;
  ;; ROOT (/tmp/err-evil:443 define-minor-mode font-lock-mode; :482
  ;; ibuffer-auto-mode; and the calls :7 (show-paren-mode t), :8
  ;; (global-so-long-mode), :9 (global-hl-line-mode t), plus void
  ;; global-visual-line-mode / blink-cursor-mode): the vendored easy-mmode
  ;; `define-minor-mode' expander leans on lexical CLOSURES, whose application
  ;; is not yet robust on the substrate.  Defining a mode aborts at
  ;; expansion, and the mode function it emits (a closure) aborts when CALLED.
  ;;
  ;; FIX: reuse the closure-FREE fallback in `emacs-easy-mmode.el' (plain
  ;; `defvar'/`defun'/`put' expansion -- verified closure-free in host) and
  ;; UNCONDITIONALLY re-install the three defining macros so they win over any
  ;; vendor easy-mmode that loaded first.  `(require 'emacs-easy-mmode)' also
  ;; `(provide 'easy-mmode)'s, blocking a later real easy-mmode load.
  ;;
  ;; LOAD-ORDER GATE: for the bare CALLS (show-paren-mode t, ...) to stop
  ;; aborting, the mode-DEFINING file (paren.el, hl-line.el, so-long.el,
  ;; simple.el, frame.el) must be evaluated AFTER this shim installs the
  ;; fallback.  The coordinator must wire this shim before user init /
  ;; package activation (as with emacs-parity-clloop/-evil).
  (require 'emacs-easy-mmode nil t)
  (when (fboundp 'emacs-easy-mmode--plist-get)
    (defmacro define-minor-mode (mode doc &rest body)
      "Closure-free `define-minor-mode' (audit parity; see emacs-easy-mmode.el)."
      (let ((init (emacs-easy-mmode--plist-get body :init-value nil))
            (global (emacs-easy-mmode--plist-get body :global nil))
            (lighter (emacs-easy-mmode--plist-get body :lighter nil))
            (keymap (emacs-easy-mmode--plist-get body :keymap nil))
            (variable-spec (emacs-easy-mmode--variable-spec
                            mode
                            (emacs-easy-mmode--plist-get body :variable nil)))
            (hook (emacs-easy-mmode--hook-symbol mode))
            (forms (emacs-easy-mmode--keyword-tail body)))
        (append
         (list 'progn
               (list 'defvar (car variable-spec) init doc)
               (list 'defvar hook nil)
               (unless global
                 (list 'make-variable-buffer-local
                       (list 'quote (car variable-spec))))
               (emacs-easy-mmode--minor-mode-alist-form mode lighter)
               (emacs-easy-mmode--minor-mode-map-form mode keymap)
               (cons 'defun
                     (cons mode
                           (cons '(&optional arg)
                                 (cons doc
                                       (cons '(interactive "P")
                                             (append
                                              (list
                                               (emacs-easy-mmode--mode-setq-form-for-variable
                                                mode
                                                (car variable-spec)
                                                (cadr variable-spec)))
                                              forms
                                              (list
                                               (list 'run-hooks
                                                     (list 'quote hook))
                                               (car variable-spec)))))))))
         (list
          (list 'put (list 'quote mode) ''interactive-form ''(interactive "P")))
         (when global
           (list
            (list 'when mode
                  (list 'add-to-list ''global-minor-modes (list 'quote mode)))))
         (list (list 'quote mode)))))

    (defmacro define-globalized-minor-mode (global-mode mode turn-on &rest body)
      "Closure-free `define-globalized-minor-mode' (audit parity)."
      (let ((init (emacs-easy-mmode--plist-get body :init-value nil))
            (lighter (emacs-easy-mmode--plist-get body :lighter nil))
            (hook (emacs-easy-mmode--hook-symbol global-mode))
            (forms (emacs-easy-mmode--keyword-tail body)))
        (list 'progn
              (list 'defvar global-mode init)
              (list 'defvar hook nil)
              (emacs-easy-mmode--minor-mode-alist-form global-mode lighter)
              (cons 'defun
                    (cons global-mode
                          (cons '(&optional arg)
                                (cons (format "Toggle global `%s'." mode)
                                      (cons '(interactive "P")
                                            (append
                                             (list
                                              (emacs-easy-mmode--mode-setq-form
                                               global-mode)
                                              (list 'if global-mode
                                                    (list 'add-to-list
                                                          ''global-minor-modes
                                                          (list 'quote global-mode))
                                                    (list 'setq
                                                          'global-minor-modes
                                                          (list 'delq
                                                                (list 'quote global-mode)
                                                                'global-minor-modes)))
                                              (list 'when global-mode
                                                    (list 'funcall
                                                          (list 'quote turn-on))))
                                             forms
                                             (list
                                              (list 'run-hooks (list 'quote hook))
                                              global-mode)))))))
              (list 'put (list 'quote global-mode)
                    ''interactive-form ''(interactive "P"))
              (list 'quote global-mode))))

    (defalias 'define-global-minor-mode 'define-globalized-minor-mode)
    (emacs-parity-macros2--register-macro 'define-minor-mode)
    (emacs-parity-macros2--register-macro 'define-globalized-minor-mode)
    (emacs-parity-macros2--register-macro 'define-global-minor-mode))

  ;;;; === D. Closure-free with-eval-after-load ========================
  ;;
  ;; ROOT (/tmp/err-evil:465,487,490,775,1136): vendor subr.el's
  ;; `with-eval-after-load' expands to
  ;;   `(eval-after-load ,file (lambda () ,@body))
  ;; and when FILE is already loaded, `eval-after-load' FUNCALLS that lambda
  ;; closure immediately -> closure application aborts (same root as C).
  ;;
  ;; FIX: expand to a QUOTED form instead of a lambda:
  ;;   (eval-after-load FILE '(progn BODY...))
  ;; `eval-after-load' evaluates a non-function form with `eval' (both the
  ;; runtime polyfill and real subr.el do), so no closure is created.  Bodies
  ;; that call globalized minor modes (solaire-global-mode, company-*-mode)
  ;; then also need section C active to be closure-free.
  (defmacro with-eval-after-load (file &rest body)
    "Closure-free `with-eval-after-load' (audit parity): defer a QUOTED form."
    (list 'eval-after-load file (list 'quote (cons 'progn body))))
  (emacs-parity-macros2--register-macro 'with-eval-after-load)

  ;;;; === E. Eager (non-CPS) iter-defun / iter-lambda =================
  ;;
  ;; ROOT (/tmp/err-evil:767 (iter-defun avl-tree-iter ...); :13
  ;; (queue--when-generators (iter-defun queue-iter ...))): generator.el's
  ;; real `iter-defun' is a continuation-passing transform that passes a
  ;; special macro-environment to `macroexpand-all' to intercept `iter-yield'.
  ;; The substrate's `macroexpand-all'/`-1' currently IGNORE their ENV arg
  ;; (Doc 22 A12) -- a CORE reader gap in vendor/nelisp that NO disk shim can
  ;; close for general (lazy/infinite/bidirectional) generators.  The CPS
  ;; transform bare-aborts even at DEFINITION.
  ;;
  ;; FIX (narrow, honest): the generators init actually defines
  ;; (avl-tree-iter, queue-iter) are FINITE and have the eager shape
  ;;   (let (...) (while COND (iter-yield X)))
  ;; where the yield return value is unused.  Provide an EAGER, non-CPS
  ;; `iter-defun' that, at CALL time, runs the body and collects every yield
  ;; into a list, returning a cursor iterator.  It never calls
  ;; `macroexpand-all' (A12-independent), emits no closures (C-independent),
  ;; and is hang-safe via a hard yield cap (a pathological infinite generator
  ;; degrades to a caught error, never a stall).  This is a PARITY
  ;; APPROXIMATION: lazy/infinite/bidirectional generators are NOT supported
  ;; and still require the Doc 22 A12 CORE fix.
  (defvar nelisp-iter--yield-cap 200000
    "Hard cap on eager `iter-yield' count; exceeding it signals rather than hangs.")

  (defun nelisp-iter--make (list)
    "Wrap LIST in a mutable cursor iterator (a `nelisp-iter'-tagged cons box)."
    (cons 'nelisp-iter list))

  (defun nelisp-iter--drain (it)
    "Return the remaining element list of iterator IT (or IT if not one)."
    (if (and (consp it) (eq (car it) 'nelisp-iter)) (cdr it) it))

  (defun nelisp-iter--walk (form accsym nsym)
    "Rewrite (iter-yield X) / (iter-yield-from IT) inside FORM to accumulate
into ACCSYM (reversed), counting via NSYM against `nelisp-iter--yield-cap'.
Quote / function(#') templates are left untouched."
    (cond
     ((not (consp form)) form)
     ((eq (car form) 'quote) form)
     ((eq (car form) 'function) form)
     ((eq (car form) 'iter-yield)
      (list 'progn
            (list 'setq nsym (list '1+ nsym))
            (list 'if (list '> nsym 'nelisp-iter--yield-cap)
                  (list 'error "nelisp eager iter cap exceeded")
                  (list 'setq accsym
                        (list 'cons (nelisp-iter--walk (cadr form) accsym nsym)
                              accsym)))))
     ((eq (car form) 'iter-yield-from)
      (list 'setq accsym
            (list 'append
                  (list 'reverse
                        (list 'nelisp-iter--drain
                              (nelisp-iter--walk (cadr form) accsym nsym)))
                  accsym)))
     (t (cons (nelisp-iter--walk (car form) accsym nsym)
              (mapcar (lambda (s) (nelisp-iter--walk s accsym nsym)) (cdr form))))))

  (defun iter-next (iterator &optional _yield-result)
    "Return the next value from eager ITERATOR, or signal `iter-end-of-sequence'."
    (let ((rem (cdr iterator)))
      (if rem
          (progn (setcdr iterator (cdr rem)) (car rem))
        (signal 'iter-end-of-sequence nil))))

  (defun iter-close (_iterator)
    "No-op close for eager iterators."
    nil)

  (defmacro iter-defun (name args &rest body)
    "Eager (non-CPS) `iter-defun' for FINITE generators (audit parity)."
    (let ((acc (make-symbol "--iter-acc--"))
          (n (make-symbol "--iter-n--"))
          (doc (when (stringp (car body)) (prog1 (car body) (setq body (cdr body))))))
      (list 'defun name args (or doc "")
            (list 'let (list (list acc nil) (list n 0))
                  (cons 'progn
                        (mapcar (lambda (s) (nelisp-iter--walk s acc n)) body))
                  (list 'nelisp-iter--make (list 'nreverse acc))))))

  (defmacro iter-lambda (args &rest body)
    "Eager (non-CPS) `iter-lambda' for FINITE generators (audit parity)."
    (let ((acc (make-symbol "--iter-acc--"))
          (n (make-symbol "--iter-n--")))
      (list 'lambda args
            (list 'let (list (list acc nil) (list n 0))
                  (cons 'progn
                        (mapcar (lambda (s) (nelisp-iter--walk s acc n)) body))
                  (list 'nelisp-iter--make (list 'nreverse acc))))))

  (unless (get 'iter-end-of-sequence 'error-conditions)
    (put 'iter-end-of-sequence 'error-conditions '(iter-end-of-sequence error))
    (put 'iter-end-of-sequence 'error-message "iteration terminated"))
  (emacs-parity-macros2--register-macro 'iter-defun)
  (emacs-parity-macros2--register-macro 'iter-lambda)
  ;; generator.el is otherwise require-able; our eager macros must win, so
  ;; re-provide only if not already, and leave the vendored file's other
  ;; entry points (iter-yield is intercepted structurally above) in place.
  (unless (featurep 'generator) (provide 'generator)))

(provide 'emacs-parity-macros2)

;;; emacs-parity-macros2.el ends here
