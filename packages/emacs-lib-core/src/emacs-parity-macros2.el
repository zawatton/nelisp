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
;; global clobbers (with-eval-after-load, define-minor-mode, ...) that
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

  ;;;; === A. Backquote ===============================================
  ;;
  ;; The prelude loads the vendored GNU backquote.el, which handles vector
  ;; templates and nested levels; this file used to rebind `\`' and
  ;; `backquote' to a NeLisp expander that also took the plain symbols
  ;; `comma' and `comma-at' for unquotes (so `(comma "re" 1)' entries of
  ;; compile.el's `compilation-error-regexp-alist-alist' lost their head).

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
  (emacs-parity-macros2--register-macro 'with-eval-after-load))

(provide 'emacs-parity-macros2)

;;; emacs-parity-macros2.el ends here
