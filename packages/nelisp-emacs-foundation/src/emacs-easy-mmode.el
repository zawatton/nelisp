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

(unless (boundp 'emacs-version)
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

  (defmacro define-minor-mode (mode doc &rest body)
    "Fallback `define-minor-mode' for standalone NeLisp.
The implementation preserves the common load-time contract: define the
mode variable, define the mode command, evaluate BODY when toggled, and
return MODE.  Custom metadata, lighter, and keymap integration are
deferred to the full vendor implementation."
    (let ((init (emacs-easy-mmode--plist-get body :init-value nil))
          (forms (emacs-easy-mmode--keyword-tail body)))
      (list 'progn
            (list 'defvar mode init doc)
            (cons 'defun
                  (cons mode
                        (cons '(&optional arg)
                              (cons doc
                                    (cons '(interactive "P")
                                          (append
                                           (list
                                            (list 'setq mode
                                                  (list 'cond
                                                        (list (list 'eq 'arg ''toggle)
                                                              (list 'not mode))
                                                        (list (list 'null 'arg)
                                                              (list 'not mode))
                                                        (list (list 'and
                                                                    (list 'numberp 'arg)
                                                                    (list '< 'arg 1))
                                                              nil)
                                                        (list t t))))
                                           forms
                                           (list mode)))))))
            (list 'quote mode))))

  (defmacro define-globalized-minor-mode (global-mode mode turn-on &rest body)
    "Fallback `define-globalized-minor-mode' for standalone NeLisp."
    (let ((forms (emacs-easy-mmode--keyword-tail body)))
      (list 'progn
            (list 'defvar global-mode nil)
            (cons 'defun
                  (cons global-mode
                        (cons '(&optional arg)
                              (cons (format "Toggle global `%s'." mode)
                                    (cons '(interactive "P")
                                          (append
                                           (list
                                            (list 'setq global-mode
                                                  (list 'if
                                                        (list 'and
                                                              (list 'numberp 'arg)
                                                              (list '< 'arg 1))
                                                        nil
                                                        (list 'not global-mode)))
                                            (list 'when global-mode
                                                  (list 'funcall
                                                        (list 'quote turn-on))))
                                           forms
                                           (list global-mode)))))))
            (list 'quote global-mode))))

  (defalias 'define-global-minor-mode 'define-globalized-minor-mode)

  (provide 'easy-mmode))

(provide 'emacs-easy-mmode)

;;; emacs-easy-mmode.el ends here
