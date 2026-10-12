;;; emacs-buffer-builtins-hooks-test.el --- GNU parity of buffer-local hooks  -*- lexical-binding: t; -*-

;;; Commentary:

;; The standalone install replaces `add-hook', `remove-hook' and the
;; `run-hook*' family with the prefixed ports in emacs-buffer-builtins.
;; Host Emacs keeps its own, so each scenario runs twice -- once through
;; GNU's functions, once through the ports -- and the observable results
;; (default value, local value, run order, return values) must match.

;;; Code:

(require 'ert)
(require 'emacs-buffer-builtins)

(defvar emacs-buffer-builtins-hooks-test--log nil)
(defvar emacs-buffer-builtins-hooks-test-hook nil)

(defun emacs-buffer-builtins-hooks-test--g (&rest _)
  (push 'global emacs-buffer-builtins-hooks-test--log) 'g)
(defun emacs-buffer-builtins-hooks-test--l (&rest _)
  (push 'local emacs-buffer-builtins-hooks-test--log) 'l)
(defun emacs-buffer-builtins-hooks-test--n (&rest _)
  (push 'nil-returner emacs-buffer-builtins-hooks-test--log) nil)

(defun emacs-buffer-builtins-hooks-test--scenario (add remove run run-args succ fail wrapped)
  "Run a fixed hook scenario with the given hook functions; return observations."
  (let ((hook 'emacs-buffer-builtins-hooks-test-hook)
        (out nil))
    (setq-default emacs-buffer-builtins-hooks-test-hook nil)
    (put hook 'hook--depth-alist nil)
    (funcall add hook #'emacs-buffer-builtins-hooks-test--g)
    (funcall add hook #'emacs-buffer-builtins-hooks-test--n 50)
    (with-temp-buffer
      (funcall add hook #'emacs-buffer-builtins-hooks-test--l nil t)
      (push (list 'local (symbol-value hook) (local-variable-p hook)) out)
      (push (list 'default (default-value hook)) out)
      (setq emacs-buffer-builtins-hooks-test--log nil)
      (funcall run hook)
      (push (list 'run (reverse emacs-buffer-builtins-hooks-test--log)) out)
      (setq emacs-buffer-builtins-hooks-test--log nil)
      (funcall run-args hook 1 2)
      (push (list 'run-args (reverse emacs-buffer-builtins-hooks-test--log)) out)
      (push (list 'success (funcall succ hook 1)) out)
      (push (list 'failure (funcall fail hook 1)) out)
      (push (list 'wrapped (funcall wrapped hook (lambda (fn &rest _) (and (eq fn #'emacs-buffer-builtins-hooks-test--g) 'wrapped-g)))) out)
      (funcall remove hook #'emacs-buffer-builtins-hooks-test--l t)
      (push (list 'after-local-remove (symbol-value hook) (local-variable-p hook)) out))
    (with-temp-buffer
      (setq emacs-buffer-builtins-hooks-test--log nil)
      (funcall run hook)
      (push (list 'other-buffer (reverse emacs-buffer-builtins-hooks-test--log)) out))
    (funcall remove hook #'emacs-buffer-builtins-hooks-test--n)
    (push (list 'after-global-remove (default-value hook)) out)
    (nreverse out)))

(ert-deftest emacs-buffer-builtins-hooks-test/ports-match-gnu ()
  "Buffer-local add/remove and the runners behave exactly like GNU's."
  (let ((gnu (emacs-buffer-builtins-hooks-test--scenario
              #'add-hook #'remove-hook #'run-hooks #'run-hook-with-args
              #'run-hook-with-args-until-success #'run-hook-with-args-until-failure
              #'run-hook-wrapped))
        (port (emacs-buffer-builtins-hooks-test--scenario
               #'emacs-buffer-builtins--add-hook #'emacs-buffer-builtins--remove-hook
               #'emacs-buffer-builtins--run-hooks
               #'emacs-buffer-builtins--run-hook-with-args
               #'emacs-buffer-builtins--run-hook-with-args-until-success
               #'emacs-buffer-builtins--run-hook-with-args-until-failure
               #'emacs-buffer-builtins--run-hook-wrapped)))
    (should (equal (assq 'run gnu) '(run (local global nil-returner))))
    (should (equal port gnu))))

(provide 'emacs-buffer-builtins-hooks-test)

;;; emacs-buffer-builtins-hooks-test.el ends here
