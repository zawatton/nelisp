;;; nelisp-stdlib-print-circle.el --- GNU printer startup declaration -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input-dialect)
(defvar nelisp-stdlib-print-circle--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect eval boundp symbol-function)))
;;;###autoload
(defun nelisp-stdlib-print-circle-install ()
  "Declare the genuine GNU nil default without changing an existing value.
This enables the GNU cl-print consumer; core graph printing is not supplied."
  (dolist (owner nelisp-stdlib-print-circle--owners)
    (unless (eq (cdr owner) (symbol-function (car owner)))
      (error "Print-circle startup owner changed: %S" (car owner))))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "Print-circle startup requires verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        (if (boundp 'print-circle) 'preserved
          (error "GNU print-circle owner unavailable"))
      ;; GNU31 src/print.c:2966–2976, SHA256
      ;; a4d3613dd97297cb4a9b682f0fbe5a86d3ae1ac9a1a07bddaf6da970b28f0a85.
      ;; DEFVAR_LISP declares a special variable; Vprint_circle = Qnil.
      ;; Omitting a docstring preserves an existing genuine host/user plist.
      (let ((existing (boundp 'print-circle)))
        (eval '(defvar print-circle nil) nil)
        (if existing 'preserved 'installed)))))
(provide 'nelisp-stdlib-print-circle)
