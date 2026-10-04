(command-modes
 (list (command-modes nil)
       (command-modes 17)
       (command-modes 'ccore-probe-unbound-command)
       (command-modes (lambda () nil)))
 (progn
   (defun ccore-probe-plain-command () (interactive) nil)
   (command-modes 'ccore-probe-plain-command))
 (progn
   (defun ccore-probe-tagged-command ()
     (interactive nil emacs-lisp-mode text-mode)
     nil)
   (command-modes 'ccore-probe-tagged-command))
 (command-modes (symbol-function 'ccore-probe-tagged-command))
 (command-modes '(lambda () (interactive nil text-mode) nil))
 (command-modes (lambda () (interactive nil emacs-lisp-mode) nil))
 (progn
   (defalias 'ccore-probe-alias-command 'ccore-probe-tagged-command)
   (command-modes 'ccore-probe-alias-command))
 (progn
   (function-put 'ccore-probe-alias-command 'command-modes '(alias-mode))
   (list (command-modes 'ccore-probe-alias-command)
         (command-modes 'ccore-probe-tagged-command)))
 (progn
   (function-put 'ccore-probe-alias-command 'command-modes nil)
   (command-modes 'ccore-probe-alias-command))
 (condition-case err (command-modes)
   (error err))
 (condition-case err (command-modes 'ccore-probe-tagged-command 'extra)
   (error err)))
