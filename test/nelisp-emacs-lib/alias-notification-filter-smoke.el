;;; alias-notification-filter-smoke.el --- Alias registry lifecycle parity -*- lexical-binding: t; -*-
;; Register, retarget and remove aliases while independent watchers stay live.
(defun i3-alias-row (name fn)
  (princ (format "I3-ALIAS|%s|%S\n" name
                 (condition-case err (funcall fn) (error err)))))
(i3-alias-row 'retarget
  (lambda ()
    (defvar i3-alias-base-a 1)
    (defvar i3-alias-base-b 2)
    (defvaralias 'i3-alias-a 'i3-alias-base-a)
    (defvaralias 'i3-alias-a 'i3-alias-base-b)
    (setq i3-alias-base-a 3 i3-alias-base-b 4)
    (let ((before i3-alias-a))
      (setq i3-alias-a 5)
      (list before i3-alias-base-a i3-alias-base-b i3-alias-a))))
(i3-alias-row 'chain-retarget
  (lambda ()
    (defvar i3-chain-base-a 10)
    (defvar i3-chain-base-b 20)
    (defvaralias 'i3-chain-a 'i3-chain-base-a)
    (defvaralias 'i3-chain-b 'i3-chain-a)
    (defvaralias 'i3-chain-a 'i3-chain-base-b)
    (setq i3-chain-base-b 30)
    (let ((inside (let ((i3-chain-b 40))
                    (list i3-chain-a i3-chain-b i3-chain-base-b))))
      (list inside i3-chain-base-a i3-chain-base-b i3-chain-a i3-chain-b))))
(i3-alias-row 'removed-alias-independent-watcher
  (lambda ()
    (defvar i3-removed-base 7)
    (defvaralias 'i3-removed-alias 'i3-removed-base)
    (internal-delete-indirect-variable 'i3-removed-alias)
    (defvar i3-independent 1)
    (defvar i3-independent-calls nil)
    (add-variable-watcher 'i3-independent
      (lambda (symbol value operation _where)
        (push (list symbol value operation i3-independent) i3-independent-calls)))
    (setq i3-removed-base 8 i3-independent 2)
    (list (boundp 'i3-removed-alias) i3-removed-base
          (reverse i3-independent-calls))))
(i3-alias-row 'nil-base
  (lambda ()
    (defvaralias 'i3-nil-alias 'nil)
    (list (symbol-value 'i3-nil-alias)
          (condition-case err
              (progn (setq i3-nil-alias 2) 'accepted)
            (error (car err))))))
(princ "I3-ALIAS-DONE\n")
