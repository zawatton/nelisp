;;; emacs-mode.el --- Major-mode framework (Track H)  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Doc 51 Track H (2026-05-03) — Layer 2.
;;
;; Major-mode framework MVP: `major-mode' / `mode-name' state vars,
;; `fundamental-mode' / `text-mode' / `emacs-lisp-mode' base modes,
;; `define-derived-mode' macro, `run-mode-hooks' /
;; `kill-all-local-variables' / `auto-mode-alist' / `set-auto-mode'.
;;
;; Out of scope (= deferred to later γ phases):
;;   - font-lock-mode integration / syntactic fontification
;;   - real buffer-local variable killing (= our `kill-all-local-
;;     variables' is a placeholder since the substrate has no
;;     buffer-local concept)
;;   - mode-line indicator updates (= mode-line-format / friends are
;;     touched by Doc 43 redisplay, not here)
;;   - syntax-table per-mode binding
;;
;; The substrate keeps `major-mode' / `mode-name' as plain defvars
;; (= simulating "buffer-local" via the convention that they're
;; re-set every time the user calls a mode function in a buffer).
;; A future phase can attach an alist mapping
;; `nelisp-ec-buffer' record → mode symbol if cross-buffer mode
;; awareness becomes important.

;;; Code:

(require 'cl-lib)

(define-error 'emacs-mode-error "Major-mode error")

;;;; --- core state -----------------------------------------------------

(defvar emacs-mode--current-major-mode 'fundamental-mode
  "Substrate-internal mirror of `major-mode'.")

(defvar emacs-mode--current-mode-name "Fundamental"
  "Substrate-internal mirror of `mode-name'.")

(defvar emacs-mode--registered nil
  "Alist (MODE-SYMBOL . PROPERTIES).
Each PROPERTIES is a plist storing parent / name / doc /
hook-var so that `define-derived-mode' and tests can introspect.")

(defvar emacs-mode--auto-mode-alist nil
  "Substrate-internal mirror of `auto-mode-alist' — list of
(REGEXP . MODE-SYMBOL) entries used by `set-auto-mode'.")

;;;; --- accessors ------------------------------------------------------

(defun emacs-mode-major-mode ()
  "Return the current major-mode symbol."
  emacs-mode--current-major-mode)

(defun emacs-mode-mode-name ()
  "Return the current mode-name string."
  emacs-mode--current-mode-name)

(defun emacs-mode-set-major-mode (mode &optional display-name)
  "Set the active major mode to MODE (= a symbol).
DISPLAY-NAME (= optional) overrides `mode-name'.  Returns MODE."
  (unless (symbolp mode)
    (signal 'wrong-type-argument (list 'symbolp mode)))
  (setq emacs-mode--current-major-mode mode)
  (when display-name
    (setq emacs-mode--current-mode-name display-name))
  ;; Mirror to the unprefixed defvars when the bridge has loaded
  ;; them (= standalone path).
  (when (boundp 'major-mode) (setq major-mode mode))
  (when (and display-name (boundp 'mode-name))
    (setq mode-name display-name))
  mode)

(defun emacs-mode-reset ()
  "Reset substrate state to fundamental.  Test helper."
  (setq emacs-mode--current-major-mode 'fundamental-mode
        emacs-mode--current-mode-name "Fundamental"
        emacs-mode--registered nil
        emacs-mode--auto-mode-alist nil)
  (when (boundp 'major-mode) (setq major-mode 'fundamental-mode))
  (when (boundp 'mode-name) (setq mode-name "Fundamental")))

;;;; --- run-mode-hooks ------------------------------------------------

(defun emacs-mode-run-mode-hooks (&rest hooks)
  "Run HOOKS via `run-hooks' (= each HOOK is a symbol whose value is a
function or list of functions).  Returns nil.

Like the upstream definition, additionally appends
`change-major-mode-after-body-hook' / `after-change-major-mode-hook'
when those are bound — both are nil-defaulted at the bridge layer
so they remain inert when the user has not configured them."
  (when (fboundp 'run-hooks)
    (apply #'run-hooks hooks)
    (when (boundp 'change-major-mode-after-body-hook)
      (run-hooks 'change-major-mode-after-body-hook))
    (when (boundp 'after-change-major-mode-hook)
      (run-hooks 'after-change-major-mode-hook)))
  nil)

;;;; --- kill-all-local-variables placeholder --------------------------

(defun emacs-mode-kill-all-local-variables (&optional _kill-permanent)
  "Phase H placeholder: clears mode-related state only.

Real Emacs flushes the buffer's full buffer-local table; our
substrate has no per-buffer local store, so this drops only the
mode-tracking vars.  Returns nil."
  (setq emacs-mode--current-major-mode 'fundamental-mode
        emacs-mode--current-mode-name  "Fundamental")
  (when (boundp 'major-mode) (setq major-mode 'fundamental-mode))
  (when (boundp 'mode-name) (setq mode-name "Fundamental"))
  nil)

;;;; --- base modes ----------------------------------------------------

(defvar emacs-mode-fundamental-mode-hook nil
  "Hook run when entering `fundamental-mode'.")

(defun emacs-mode-fundamental-mode ()
  "Switch to `fundamental-mode' (= the no-op default mode)."
  (interactive)
  (emacs-mode-kill-all-local-variables)
  (emacs-mode-set-major-mode 'fundamental-mode "Fundamental")
  (emacs-mode-run-mode-hooks 'emacs-mode-fundamental-mode-hook
                             (when (boundp 'fundamental-mode-hook)
                               'fundamental-mode-hook))
  nil)

(defvar emacs-mode-text-mode-hook nil
  "Hook run when entering `text-mode'.")

(defun emacs-mode-text-mode ()
  "Switch to `text-mode'."
  (interactive)
  (emacs-mode-fundamental-mode)
  (emacs-mode-set-major-mode 'text-mode "Text")
  (emacs-mode-run-mode-hooks 'emacs-mode-text-mode-hook
                             (when (boundp 'text-mode-hook)
                               'text-mode-hook))
  nil)

(defvar emacs-mode-emacs-lisp-mode-hook nil
  "Hook run when entering `emacs-lisp-mode'.")

(defun emacs-mode-emacs-lisp-mode ()
  "Switch to `emacs-lisp-mode'."
  (interactive)
  (emacs-mode-fundamental-mode)
  (emacs-mode-set-major-mode 'emacs-lisp-mode "Emacs-Lisp")
  (emacs-mode-run-mode-hooks 'emacs-mode-emacs-lisp-mode-hook
                             (when (boundp 'emacs-lisp-mode-hook)
                               'emacs-lisp-mode-hook))
  nil)

;;;; --- define-derived-mode -------------------------------------------

(defmacro emacs-mode-define-derived-mode
    (child parent name &optional doc &rest body)
  "Track H `define-derived-mode', GNU-semantics MVP.

CHILD = symbol naming the new mode.
PARENT = symbol naming the parent mode (= called as a function before
the body runs).  Pass nil, or `fundamental-mode' (normalized to nil
the same way GNU's `define-derived-mode' does — neither has a
MODE-map/-syntax-table of its own to chain from), to derive from no
particular parent.
NAME = display string (= written to `mode-name').
DOC = docstring (= optional).
BODY = optional leading KEYWORD VALUE pairs (`:group', `:syntax-table',
`:abbrev-table', `:after-hook', `:interactive' — the same keywords
GNU's macro accepts), followed by forms run AFTER the parent + before
the hook fires.

Beyond registering the derived mode function and a `CHILD-hook'
defvar, this also defines `CHILD-map' (a sparse keymap whose parent is
chained from the parent mode's active local map the first time CHILD
runs), `CHILD-syntax-table' (inheriting from the parent's active
syntax table the first time CHILD runs, unless `:syntax-table' names
an existing table or nil), and `CHILD-abbrev-table' (when the abbrev
primitives are available) — matching real Emacs's `derived.el',
including preserving an existing value across a reload (`unless
(boundp ...)'). The keymap-parent / syntax-table-parent / abbrev-table
:parents linkage steps are only emitted as `fboundp'-guarded runtime
checks, so the same macro degrades gracefully (skips the linkage, but
still creates the variable) before those substrate pieces are loaded,
instead of signalling.

Built with explicit `list'/`append' calls instead of a backquote
template on purpose (Doc 33 §8 item 221): the standalone NeLisp
reader's macro system does not correctly invoke a user-defined macro
whose expansion-producing body is a backquote template — the call
silently installs nothing, with no visible error — while the
identical expansion built from plain `list'/`append'/`quote' calls
works.  This macro loads on the standalone bootstrap path (it is the
Track H bridge for `define-derived-mode'), so it must stay
backquote-free even though host Emacs's own macro system has no such
limitation.  See the minimal repro under tmp-diag/ (gitignored) for
the isolation that pinned this down to backquote specifically, not
`declare' clauses, docstring size, or nesting depth."
  ;; GNU normalizes an explicit `fundamental-mode' parent to nil.
  (when (eq parent 'fundamental-mode) (setq parent nil))
  (let* (;; When DOC was omitted, it may really be the first BODY form (a
         ;; keyword or an ordinary form) — shift it back onto BODY, same as
         ;; GNU's own `(push docstring body)' fallback.
         (real-doc (if (stringp doc) doc nil))
         (real-body (if (stringp doc) body (cons doc body)))
         (group nil)
         (declare-syntax t)
         (declare-abbrev t)
         (interactive-flag t)
         (after-hook-form nil)
         (map-var (intern (format "%s-map" child)))
         (syntax-var (intern (format "%s-syntax-table" child)))
         (abbrev-var (intern (format "%s-abbrev-table" child))))
    ;; Consume leading KEYWORD VALUE pairs (mirrors GNU's `(pcase (pop body)
    ;; ...)' keyword-args loop: an unrecognized keyword still consumes one
    ;; value and is otherwise ignored).
    (while (keywordp (car real-body))
      (let ((kw (car real-body)))
        (setq real-body (cdr real-body))
        (cond
         ((eq kw :group) (setq group (car real-body)))
         ((eq kw :syntax-table)
          (setq syntax-var (car real-body) declare-syntax nil))
         ((eq kw :abbrev-table)
          (setq abbrev-var (car real-body) declare-abbrev nil))
         ((eq kw :after-hook) (setq after-hook-form (car real-body)))
         ((eq kw :interactive) (setq interactive-flag (car real-body))))
        (setq real-body (cdr real-body))))
    (let* ((parent-call (cond
                         ((null parent) '(emacs-mode-fundamental-mode))
                         (t (list parent))))
           (hook-var (intern (format "%s-hook" child)))
           (e-hook-var (intern (format "emacs-mode-%s-hook" child)))
           (hook-doc (format "Hook run when entering `%s'." child))
           (e-hook-doc (format "Substrate-internal hook run when entering `%s'."
                                child))
           (fn-doc (or real-doc (format "Major mode %s." child)))
           (register-form
            (list 'push
                  (list 'cons (list 'quote child)
                        (list 'list :parent (list 'quote parent)
                              :name name
                              :doc real-doc
                              :hook (list 'quote hook-var)))
                  'emacs-mode--registered))
           ;; Record the parent so `derived-mode-p' can walk the major-mode
           ;; hierarchy (e.g. org-mode -> outline-mode -> text-mode).  Faithful
           ;; to real `define-derived-mode', which sets this symbol property.
           (parent-property-forms
            (when parent
              (list (list 'put (list 'quote child)
                          ''derived-mode-parent (list 'quote parent)))))
           (group-forms
            (when group
              (list (list 'put (list 'quote child)
                          ''custom-mode-group group))))
           ;; `CHILD-map' always gets declared (GNU has no keyword to opt
           ;; out of it); `fboundp'-guard the constructor since
           ;; `make-sparse-keymap' may not be installed yet this early in a
           ;; from-scratch standalone boot.
           (map-defvar-form
            (list 'unless (list 'boundp (list 'quote map-var))
                  (list 'defvar map-var
                        (list 'if (list 'fboundp (list 'quote 'make-sparse-keymap))
                              '(make-sparse-keymap)
                              nil)
                        (format "Keymap for `%s'." child))))
           (syntax-defvar-form
            (when declare-syntax
              (list 'unless (list 'boundp (list 'quote syntax-var))
                    (list 'defvar syntax-var
                          (list 'if (list 'fboundp (list 'quote 'make-syntax-table))
                                '(make-syntax-table)
                                nil)
                          (format "Syntax table for `%s'." child)))))
           (abbrev-defvar-form
            (when declare-abbrev
              (list 'unless (list 'boundp (list 'quote abbrev-var))
                    (list 'progn
                          (list 'defvar abbrev-var nil
                                (format "Abbrev table for `%s'." child))
                          (list 'when (list 'fboundp (list 'quote 'define-abbrev-table))
                                (list 'define-abbrev-table
                                      (list 'quote abbrev-var) nil))))))
           ;; Runtime (inside the mode function), only when PARENT is
           ;; non-nil: chain CHILD's map/syntax-table onto whatever the
           ;; parent left active, the first time CHILD runs (an existing,
           ;; user-set parent is left alone — same `unless' guard GNU uses).
           (mode-class-fixup-form
            (list 'when (list 'get (list 'quote parent) ''mode-class)
                  (list 'put (list 'quote child) ''mode-class
                        (list 'get (list 'quote parent) ''mode-class))))
           (map-parent-fixup-form
            (list 'when (list 'fboundp (list 'quote 'set-keymap-parent))
                  (list 'unless
                        (list 'and (list 'fboundp (list 'quote 'keymap-parent))
                              (list 'keymap-parent map-var))
                        (list 'when (list 'fboundp (list 'quote 'current-local-map))
                              (list 'set-keymap-parent map-var
                                    (list 'current-local-map))))))
           (syntax-parent-fixup-form
            (when declare-syntax
              (list 'when (list 'and (list 'fboundp (list 'quote 'char-table-parent))
                                (list 'fboundp (list 'quote 'set-char-table-parent))
                                (list 'fboundp (list 'quote 'standard-syntax-table))
                                (list 'fboundp (list 'quote 'syntax-table)))
                    (list 'let (list (list 'emacs-mode--dd-parent-syntax
                                            (list 'char-table-parent syntax-var)))
                          (list 'unless
                                (list 'and 'emacs-mode--dd-parent-syntax
                                      (list 'not
                                            (list 'eq 'emacs-mode--dd-parent-syntax
                                                  (list 'standard-syntax-table))))
                                (list 'set-char-table-parent syntax-var
                                      (list 'syntax-table)))))))
           (abbrev-parent-fixup-form
            (when declare-abbrev
              (list 'when (list 'and (list 'fboundp (list 'quote 'abbrev-table-get))
                                (list 'fboundp (list 'quote 'abbrev-table-put))
                                (list 'boundp (list 'quote 'local-abbrev-table)))
                    (list 'unless
                          (list 'or (list 'abbrev-table-get abbrev-var :parents)
                                (list 'eq abbrev-var 'local-abbrev-table))
                          (list 'abbrev-table-put abbrev-var :parents
                                (list 'list 'local-abbrev-table))))))
           (parent-fixup-block
            (cons 'progn
                  (delq nil (list mode-class-fixup-form
                                  map-parent-fixup-form
                                  syntax-parent-fixup-form
                                  abbrev-parent-fixup-form))))
           (use-local-map-form
            (list 'when (list 'fboundp (list 'quote 'use-local-map))
                  (list 'use-local-map map-var)))
           (set-syntax-table-form
            (when syntax-var
              (list 'when (list 'fboundp (list 'quote 'set-syntax-table))
                    (list 'set-syntax-table syntax-var))))
           (set-local-abbrev-table-form
            (when abbrev-var
              (list 'when (list 'boundp (list 'quote 'local-abbrev-table))
                    (list 'setq 'local-abbrev-table abbrev-var))))
           (parent-complete (make-symbol "parent-complete"))
           ;; Keep the body/hooks outside cleanup.  The parent call still needs
           ;; this small wrapper on the standalone cold-image path, where nested
           ;; derived-mode parent calls can otherwise drop following forms.
           (parent-form
            (list 'let (list (list parent-complete nil))
                  (list 'unwind-protect
                        (list 'progn parent-call
                              (list 'setq parent-complete t))
                        nil)
                  (list 'unless parent-complete
                        (list 'signal ''emacs-mode-error
                              (list 'list
                                    "Parent mode did not complete"
                                    (list 'quote parent))))))
           (defun-form
            (append (list 'defun child '() fn-doc)
                    (if interactive-flag (list '(interactive)) nil)
                    (list parent-form)
                    (if parent (list parent-fixup-block) nil)
                    (list use-local-map-form)
                    (if set-syntax-table-form (list set-syntax-table-form) nil)
                    (if set-local-abbrev-table-form
                        (list set-local-abbrev-table-form)
                      nil)
                    (list (list 'emacs-mode-set-major-mode
                                (list 'quote child) name))
                    real-body
                    ;; Some vendor mode bodies still exercise incomplete
                    ;; buffer-local substrate paths.  Reassert the child mode
                    ;; before hooks so hook observers see the derived mode that
                    ;; `define-derived-mode' promised.  Keep this to direct
                    ;; assignments: after a large vendor body the standalone
                    ;; cold-image path has exposed crashes through an additional
                    ;; full setter call.
                    (list (list 'setq 'emacs-mode--current-major-mode
                                (list 'quote child))
                          (list 'when (list 'boundp ''major-mode)
                                (list 'setq 'major-mode
                                      (list 'quote child))))
                    (list (list 'emacs-mode-run-mode-hooks
                                (list 'quote e-hook-var)
                                (list 'quote hook-var)))
                    (if after-hook-form (list after-hook-form) nil)
                    (list nil))))
      (append
       (list 'progn
             (list 'defvar hook-var nil hook-doc)
             (list 'defvar e-hook-var nil e-hook-doc)
             register-form
             map-defvar-form)
       (if syntax-defvar-form (list syntax-defvar-form) nil)
       (if abbrev-defvar-form (list abbrev-defvar-form) nil)
       parent-property-forms
       group-forms
       (list defun-form (list 'quote child))))))

;;;; --- auto-mode-alist + set-auto-mode -------------------------------

(defun emacs-mode-auto-mode-alist ()
  "Return the substrate's auto-mode-alist."
  emacs-mode--auto-mode-alist)

(defun emacs-mode-set-auto-mode-alist (alist)
  "Replace the substrate's auto-mode-alist with ALIST.
ALIST = list of (REGEXP . MODE-SYMBOL).  Returns ALIST."
  (setq emacs-mode--auto-mode-alist alist)
  (when (boundp 'auto-mode-alist)
    (setq auto-mode-alist alist))
  alist)

(defun emacs-mode-set-auto-mode (&optional filename)
  "Pick a major mode for FILENAME by walking `auto-mode-alist'.
Returns the mode symbol that was activated, or nil if no match.
With no FILENAME, uses the current buffer's `buffer-file-name'
(= via the Track D bridge)."
  (let* ((path (or filename
                   (and (boundp 'buffer-file-name) buffer-file-name)
                   (and (fboundp 'emacs-fileio-buffer-file-name)
                        (emacs-fileio-buffer-file-name))))
         (alist (or (and (boundp 'auto-mode-alist) auto-mode-alist)
                    emacs-mode--auto-mode-alist))
         (matched nil))
    (when (and path alist)
      (catch 'done
        (dolist (cell alist)
          (let ((re (car cell))
                (mode (cdr cell)))
            (when (and (stringp re) (string-match re path))
              (setq matched mode)
              (when (fboundp mode) (funcall mode))
              (throw 'done t))))))
    matched))

(provide 'emacs-mode)

;;; emacs-mode.el ends here
