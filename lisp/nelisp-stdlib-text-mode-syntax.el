;;; nelisp-stdlib-text-mode-syntax.el --- Genuine GNU31 text syntax initializer -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(defvar nelisp-stdlib-text-mode-syntax--dialect-owner (symbol-function 'nelisp-bytecode-compiler-input-dialect))
(defvar nelisp-stdlib-text-mode-syntax--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(eval make-syntax-table modify-syntax-entry copy-syntax-table
            syntax-table set-syntax-table char-syntax standard-syntax-table)))
(defconst nelisp-stdlib-text-mode-syntax--forms
  '
((defvar text-mode-syntax-table (let ((st (make-syntax-table))) (modify-syntax-entry 34 ".   " st) (modify-syntax-entry 92 ".   " st) (modify-syntax-entry 39 "w p" st) (modify-syntax-entry 1523 "w   " st) (modify-syntax-entry 1524 "w   " st) (modify-syntax-entry 183 "w   " st) (modify-syntax-entry 8231 "w   " st) (modify-syntax-entry 65306 "w   " st) st) "Syntax table used while in `text-mode'.")))
(defvar nelisp-stdlib-text-mode-syntax--sealed-forms
  (copy-tree nelisp-stdlib-text-mode-syntax--forms))
(defun nelisp-stdlib-text-mode-syntax-install ()
  "Evaluate the exact GNU31 text-mode syntax initializer using real syntax owners.
Preserve GNU hosts and existing variable values.  This supplies one genuine
initializer, not the full text-mode library or unsupported syntax flag parity."
  (unless (and (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
                   nelisp-stdlib-text-mode-syntax--dialect-owner)
               (equal nelisp-stdlib-text-mode-syntax--forms
                      nelisp-stdlib-text-mode-syntax--sealed-forms))
    (error "Text syntax source owner changed"))
  (dolist (owner nelisp-stdlib-text-mode-syntax--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Text syntax runtime owner changed: %S" (car owner))))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "Text syntax requires verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (dolist (form nelisp-stdlib-text-mode-syntax--forms) (eval form t))
      'installed)))
(provide 'nelisp-stdlib-text-mode-syntax)
