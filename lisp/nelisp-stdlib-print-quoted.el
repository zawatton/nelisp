;;; nelisp-stdlib-print-quoted.el --- GNU quoted-printing startup declaration -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(defvar nelisp-stdlib-print-quoted--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect eval boundp symbol-function)))
;;;###autoload
(defun nelisp-stdlib-print-quoted-install ()
  "Declare GNU's true quoted-printing default and preserve existing values.
The genuine cl-print consumer owns its abbreviation semantics."
  (dolist (owner nelisp-stdlib-print-quoted--owners)
    (unless (eq (cdr owner) (symbol-function (car owner)))
      (error "Print-quoted startup owner changed: %S" (car owner))))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "Print-quoted startup requires verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        (if (boundp 'print-quoted) 'preserved
          (error "GNU print-quoted owner unavailable"))
      ;; GNU31 src/print.c:2952–2955, SHA256
      ;; a4d3613dd97297cb4a9b682f0fbe5a86d3ae1ac9a1a07bddaf6da970b28f0a85.
      ;; DEFVAR_BOOL declares a special variable; print_quoted = true.
      (let ((existing (boundp 'print-quoted)))
        (eval '(defvar print-quoted t) nil)
        (if existing 'preserved 'installed)))))
(provide 'nelisp-stdlib-print-quoted)
