;;; nelisp-stdlib-derived-parent.el --- Genuine GNU mode registration owners -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-derived-parent--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes expand-file-name eval get put mapc
            make-sparse-keymap fset set)))
(defconst nelisp-stdlib-derived-parent--names
  '(define-prefix-command derived-mode-set-parent derived-mode--flush))
(defvar nelisp-stdlib-derived-parent--installed-owners nil)
;;;###autoload
(defun nelisp-stdlib-derived-parent-install (&optional source-root)
  "Install GNU prefix commands, parent registration and cache invalidation.
Authenticate configured SOURCE-ROOT, preserving GNU host function owners.
This does not define a major mode, publish a feature, or replace mode queries."
  (dolist (owner nelisp-stdlib-derived-parent--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Derived parent dependency changed: %S" (car owner))))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Derived parent requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (dolist (name nelisp-stdlib-derived-parent--names)
        (when (fboundp name)
          (unless (eq (symbol-function name)
                      (cdr (assq name nelisp-stdlib-derived-parent--installed-owners)))
            (error "Foreign standalone derived parent owner: %S" name))))
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Derived parent source is unconfigured"))
      (let* ((path (expand-file-name "lisp/subr.el" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 338196)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 338196))
                            "a51d2fb5d52749133cc95904d24aaceb6887a591b0f6c07ee255c1e876d20b96"))
          (error "Derived parent GNU source pin differs"))
        (if nelisp-stdlib-derived-parent--installed-owners 'preserved
          (require 'nelisp-vendor-source)
          (eval (read (concat "(progn
"
                              (nelisp-vendor-source-forms
                               "vendor/staged-emacs-lisp/subr.el"
                               nelisp-stdlib-derived-parent--names path)
                              "
)"))
                t)
          (setq nelisp-stdlib-derived-parent--installed-owners
                (mapcar (lambda (name) (cons name (symbol-function name)))
                        nelisp-stdlib-derived-parent--names))
          'installed)))))
(provide 'nelisp-stdlib-derived-parent)
