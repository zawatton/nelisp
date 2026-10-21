;;; nelisp-load-macroexpand.el --- GNU load macroexpansion loop -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Adapt GNU Emacs 31.1 eval.c:1283-1354 and :2494-2503. Keep this Lisp
;; implementation in the boot provider, compiled through its existing selector.
(defvar nelisp--native-declaration-macros nil)

(defun macroexpand (form &optional environment)
  "Expand FORM's outer macros using GNU's direct lookup loop."
  (let ((done nil))
    (while (and (consp form) (not done))
      (let* ((def (car form)) (sym def) (cell nil)
             (resolve t) (expander nil) (expand nil))
        (while (and resolve (symbolp def))
          (setq sym def cell (assq sym environment))
          (if cell
              (setq resolve nil)
            ;; The runtime's unbound symbol-function needs this guard.
            (setq def (and (fboundp sym) (symbol-function sym)))
            ;; Native builtins use their own symbol as the function cell.
            (when (or (null def) (eq def sym)) (setq resolve nil))))
        (if cell
            (setq expander (cdr cell) expand (not (null (cdr cell))))
          ;; GNU's autoload helper immediately returns ordinary definitions.
          (when (and (consp def) (eq (car def) 'autoload))
            (setq def (autoload-do-load def sym 'macro)))
          (when (and (consp def) (eq (car def) 'macro)
                     (not (memq def nelisp--native-declaration-macros)))
            (setq expander (cdr def) expand t)))
        (if (not expand)
            (setq done t)
          (let ((next (apply expander (cdr form))))
            (if (eq next form) (setq done t) (setq form next))))))
    form))

(provide 'nelisp-load-macroexpand)
;;; nelisp-load-macroexpand.el ends here
