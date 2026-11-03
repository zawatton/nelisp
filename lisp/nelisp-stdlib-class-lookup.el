;;; nelisp-stdlib-class-lookup.el --- Genuine GNU class lookup expansion -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-class-lookup--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            get secure-hash file-attributes expand-file-name)))
(defconst nelisp-stdlib-class-lookup--form
  '(defun cl-find-class (type)
     "Return CL class of TYPE.

Call `cl--find-class' to get TYPE's propname `cl--class'"
     (get type 'cl--class)))
(defvar nelisp-stdlib-class-lookup--sealed-form
  (copy-tree nelisp-stdlib-class-lookup--form))
(defvar nelisp-stdlib-class-lookup--installed-owner nil)
;;;###autoload
(defun nelisp-stdlib-class-lookup-install (&optional source-root)
  "Install the genuine GNU-expanded class lookup at verified SOURCE-ROOT.
The one cl--find-class macro call was expanded by its pinned GNU cl-macs
owner.  This reads real class metadata; it creates no synthetic classes and
does not load cl-extra's unrelated help/UI initializers.  Preserve GNU hosts."
  (dolist (owner nelisp-stdlib-class-lookup--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Class lookup owner changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-class-lookup--form nelisp-stdlib-class-lookup--sealed-form)
    (error "Class lookup source payload changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Class lookup requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (when (if nelisp-stdlib-class-lookup--installed-owner
                (not (eq (symbol-function 'cl-find-class)
                         nelisp-stdlib-class-lookup--installed-owner))
              (fboundp 'cl-find-class))
        (error "Foreign standalone class lookup is prebound"))
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Class lookup source root is unconfigured"))
      (dolist (entry '(("lisp/emacs-lisp/cl-extra.el" 38864 "4aca78d4d9c161ba688bf0253f0c4445415d5c0fcb9a8f573cc6ab1270583d42")
                       ("lisp/emacs-lisp/cl-macs.el" 165989 "f58f3f9b88755549a7713b59045059affda99c2e22251e22e5c40fbe017f2394")))
        (let* ((path (expand-file-name (car entry) source-root))
               (attributes (file-attributes path)))
          (unless (and attributes (null (car attributes)) (= (nth 7 attributes) (nth 1 entry))
                       (equal (secure-hash 'sha256
                                           (nelisp-native-raw-file-read path 0 (nth 1 entry)))
                              (nth 2 entry)))
            (error "Class lookup GNU source pin differs: %s" (car entry)))))
      (unless nelisp-stdlib-class-lookup--installed-owner
        (eval nelisp-stdlib-class-lookup--form t)
        (setq nelisp-stdlib-class-lookup--installed-owner (symbol-function 'cl-find-class)))
      'installed)))
(provide 'nelisp-stdlib-class-lookup)
