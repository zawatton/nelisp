;;; nelisp-stdlib-derived-parent.el --- Genuine GNU mode registration owners -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-derived-parent--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes expand-file-name eval get put mapc
            make-sparse-keymap fset set)))
(defconst nelisp-stdlib-derived-parent--forms
  '((defun define-prefix-command (command &optional mapvar name)
      "Define COMMAND as a prefix command.  COMMAND should be a symbol.
A new sparse keymap is stored as COMMAND's function definition and its
value.
This prepares COMMAND for use as a prefix key's binding.
If a second optional argument MAPVAR is given, it should be a symbol.
The map is then stored as MAPVAR's value instead of as COMMAND's
value; but COMMAND is still defined as a function.
The third optional argument NAME, if given, supplies a menu name
string for the map.  This is required to use the keymap as a menu.
This function returns COMMAND."
      (let ((map (make-sparse-keymap name)))
        (fset command map)
        (set (or mapvar command) map)
        command))
    (defun derived-mode-set-parent (mode parent)
      "Declare PARENT to be the parent of MODE."
      (put mode 'derived-mode-parent parent)
      (derived-mode--flush mode))
    (defun derived-mode--flush (mode)
      (put mode 'derived-mode--all-parents nil)
      (let ((followers (get mode 'derived-mode--followers)))
        (when followers
          (put mode 'derived-mode--followers nil)
          (mapc #'derived-mode--flush followers))))))
(defvar nelisp-stdlib-derived-parent--sealed-forms
  (copy-tree nelisp-stdlib-derived-parent--forms))
(defvar nelisp-stdlib-derived-parent--installed-owners nil)
;;;###autoload
(defun nelisp-stdlib-derived-parent-install (&optional source-root)
  "Install GNU prefix commands, parent registration and cache invalidation.
Authenticate configured SOURCE-ROOT, preserving GNU host function owners.
This does not define a major mode, publish a feature, or replace mode queries."
  (dolist (owner nelisp-stdlib-derived-parent--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Derived parent dependency changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-derived-parent--forms
                 nelisp-stdlib-derived-parent--sealed-forms)
    (error "Derived parent source payload changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Derived parent requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (dolist (name '(define-prefix-command derived-mode-set-parent derived-mode--flush))
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
          (error "Derived parent GNU source pin differs")))
      (if nelisp-stdlib-derived-parent--installed-owners 'preserved
        (dolist (form nelisp-stdlib-derived-parent--forms) (eval form t))
        (setq nelisp-stdlib-derived-parent--installed-owners
              (mapcar (lambda (name) (cons name (symbol-function name)))
                      '(define-prefix-command derived-mode-set-parent derived-mode--flush)))
        'installed))))
(provide 'nelisp-stdlib-derived-parent)
