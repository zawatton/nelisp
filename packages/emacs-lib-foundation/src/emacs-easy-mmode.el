;;; emacs-easy-mmode.el --- minimal easy-mmode fallback for NeLisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Vendored easy-mmode.el is ordinary Emacs Lisp, but its macro
;; expander uses lexical closures heavily.  Until NeLisp's closure
;; application is robust enough for that path, this Layer 2 fallback
;; provides the small macro surface needed by core vendor files such as
;; files.el.  Host Emacs keeps its native easy-mmode implementation.

;;; Code:

(defvar emacs-easy-mmode--standalone-p
  (or (not (boundp 'emacs-version))
      (and (fboundp 'nelisp--eval-source-string)
           (or (not (fboundp 'define-minor-mode))
               (get 'define-minor-mode 'emacs-stub-bulk))))
  "Non-nil when the standalone fallback should replace easy-mmode stubs.

The `(not (fboundp \\='define-minor-mode))' arm matters for load order
(Doc 33 §8 item 221): the bootstrap bundle loads `emacs-stub-bulk' LAST
(Doc 22 A19), so when this file loads, `define-minor-mode' carries no
`emacs-stub-bulk' property yet -- it is simply undefined.  Without this
arm the fallback never installed on the standalone reader, the stub-bulk
no-op macro won and every vendor `define-minor-mode' call (for example
`paragraph-indent-minor-mode' in text-mode.el) silently defined
nothing.")

(when emacs-easy-mmode--standalone-p
  (defun emacs-easy-mmode--keyword-tail (body)
    "Return BODY with leading keyword/value pairs removed."
    (while (and (consp body)
                (symbolp (car body))
                (let ((name (symbol-name (car body))))
                  (and (> (length name) 0)
                       (eq (aref name 0) ?:))))
      (setq body (cdr (cdr body))))
    body)

  (defun emacs-easy-mmode--plist-get (plist prop default)
    "Return PROP from PLIST, or DEFAULT when absent."
    (let ((cur plist)
          (found nil)
          (value default))
      (while (and cur (not found))
        (if (eq (car cur) prop)
            (setq value (car (cdr cur))
                  found t)
          (setq cur (cdr (cdr cur)))))
      value))

  (defun emacs-easy-mmode--hook-symbol (mode)
    "Return MODE's conventional hook symbol."
    (intern (concat (symbol-name mode) "-hook")))

  (defun emacs-easy-mmode--minor-mode-alist-form (mode lighter)
    "Return a load-time form registering MODE and LIGHTER."
    (when lighter
      (list 'unless
            (list 'assq (list 'quote mode) 'minor-mode-alist)
            (list 'setq 'minor-mode-alist
                  (list 'cons
                        (list 'quote (list mode lighter))
                        'minor-mode-alist)))))

  (defun emacs-easy-mmode--minor-mode-map-form (mode keymap)
    "Return a load-time form registering MODE's KEYMAP."
    (when keymap
      (list 'unless
            (list 'assq (list 'quote mode) 'minor-mode-map-alist)
            (list 'setq 'minor-mode-map-alist
                  (list 'cons
                        (list 'cons (list 'quote mode) keymap)
                        'minor-mode-map-alist)))))

  (defun emacs-easy-mmode--mode-setq-form (mode)
    "Return the prefix-arg toggle assignment for MODE."
    (emacs-easy-mmode--mode-setq-form-for-variable mode mode nil))

  (defun emacs-easy-mmode--variable-spec (mode spec)
    "Return (VARIABLE SETTER) for MODE's `:variable' SPEC."
    (cond
     ((null spec) (list mode nil))
     ((eq spec t) (list mode nil))
     ((symbolp spec) (list spec nil))
     ((and (consp spec) (symbolp (car spec)))
      (list (car spec)
            (if (symbolp (cdr spec))
                (list 'quote (cdr spec))
              (cdr spec))))
     (t (list mode nil))))

  (defun emacs-easy-mmode--mode-setq-form-for-variable (mode variable setter)
    "Return the prefix-arg toggle assignment for MODE's VARIABLE.
When SETTER is non-nil, call it with the computed toggle value and
return VARIABLE's resulting value.  This covers `define-minor-mode'
forms such as `emacs-lock-mode' whose setter maps plain t to a
mode-specific symbol."
    (let ((value-form
           (list 'cond
                 (list (list 'eq 'arg ''toggle)
                       (list 'not variable))
                 (list (list 'null 'arg)
                       (list 'not variable))
                 (list (list 'and
                             (list 'numberp 'arg)
                             (list '< 'arg 1))
                       nil)
                 (list t t))))
      (if setter
          (list 'let (list (list '--easy-mmode-value-- value-form))
                (list 'funcall setter '--easy-mmode-value--)
                variable)
        (list 'setq variable value-form))))

  (defmacro define-minor-mode (mode doc &rest body)
    "Fallback `define-minor-mode' for standalone NeLisp.
The implementation preserves the common load-time contract: define the
mode variable, define the mode command, evaluate BODY when toggled, and
return MODE.  Custom metadata is deferred to the full vendor
implementation, but the vendor-visible load-time surfaces for
`:global', `:lighter', and `:keymap' are materialized."
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
        (list 'put
              (list 'quote mode)
              ''interactive-form
              ''(interactive "P")))
       (when global
         (list
          (list 'when mode
                (list 'add-to-list ''global-minor-modes
                      (list 'quote mode)))))
       (list (list 'quote mode)))))

  (defmacro define-globalized-minor-mode (global-mode mode turn-on &rest body)
    "Fallback `define-globalized-minor-mode' for standalone NeLisp."
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
                                                        (list 'quote
                                                              global-mode))
                                                  (list 'setq
                                                        'global-minor-modes
                                                        (list 'delq
                                                              (list 'quote
                                                                    global-mode)
                                                              'global-minor-modes)))
                                            (list 'when global-mode
                                                  (list 'funcall
                                                        (list 'quote turn-on))))
                                           forms
                                           (list
                                            (list 'run-hooks
                                                  (list 'quote hook))
                                            global-mode)))))))
            (list 'put
                  (list 'quote global-mode)
                  ''interactive-form
                  ''(interactive "P"))
            (list 'quote global-mode))))

  (defalias 'define-global-minor-mode 'define-globalized-minor-mode)

  (provide 'easy-mmode))

(provide 'emacs-easy-mmode)

;;; emacs-easy-mmode.el ends here
