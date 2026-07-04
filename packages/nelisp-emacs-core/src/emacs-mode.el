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
  "Track H MVP `define-derived-mode'.

CHILD = symbol naming the new mode.
PARENT = symbol naming the parent mode (= called as a function before
the body runs).  Pass nil to derive from `fundamental-mode'.
NAME = display string (= written to `mode-name').
DOC = docstring (= optional).
BODY = forms run AFTER the parent + before the hook fires.

The derived mode function is registered under CHILD's function-cell
and a `CHILD-hook' defvar is created.

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
  (let* ((parent-call (cond
                       ((null parent) '(emacs-mode-fundamental-mode))
                       (t (list parent))))
         (hook-var (intern (format "%s-hook" child)))
         (e-hook-var (intern (format "emacs-mode-%s-hook" child)))
         ;; When DOC was omitted, BODY shifts left by one slot.
         (real-doc (if (stringp doc) doc nil))
         (real-body (if (stringp doc) body (cons doc body)))
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
          (append (list 'defun child '()
                        fn-doc
                        '(interactive)
                        parent-form
                        (list 'emacs-mode-set-major-mode
                              (list 'quote child) name))
                  real-body
                  (list (list 'emacs-mode-run-mode-hooks
                              (list 'quote e-hook-var)
                              (list 'quote hook-var))
                        nil))))
    (append
     (list 'progn
           (list 'defvar hook-var nil hook-doc)
           (list 'defvar e-hook-var nil e-hook-doc)
           register-form)
     parent-property-forms
     (list defun-form (list 'quote child)))))

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
