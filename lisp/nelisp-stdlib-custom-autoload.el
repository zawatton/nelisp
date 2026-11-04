;;; nelisp-stdlib-custom-autoload.el --- Genuine GNU custom dependency registration -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-custom-autoload--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes expand-file-name get put member)))
(defconst nelisp-stdlib-custom-autoload--forms
  '((defun custom-add-load (symbol load)
      "To the custom option SYMBOL add the dependency LOAD.
LOAD should be either a library file name, or a feature name."
      (let ((loads (get symbol 'custom-loads)))
        (unless (member load loads)
          (put symbol 'custom-loads (cons load loads)))))
    (defun custom-autoload (symbol load &optional noset)
      "Mark SYMBOL as autoloaded custom variable and add dependency LOAD.
If NOSET is non-nil, don't bother autoloading LOAD when setting the variable."
      (put symbol 'custom-autoload (if noset 'noset t))
      (custom-add-load symbol load))))
(defvar nelisp-stdlib-custom-autoload--sealed-forms
  (copy-tree nelisp-stdlib-custom-autoload--forms))
(defvar nelisp-stdlib-custom-autoload--installed-owners nil)
;;;###autoload
(defun nelisp-stdlib-custom-autoload-install (&optional source-root)
  "Install exact GNU custom registration from authenticated SOURCE-ROOT.
Preserve GNU host providers.  Standalone owns only these two declarations;
it does not publish custom/widget features or initialize customization UI."
  (dolist (owner nelisp-stdlib-custom-autoload--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Custom registration owner changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-custom-autoload--forms
                 nelisp-stdlib-custom-autoload--sealed-forms)
    (error "Custom registration source payload changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Custom registration requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (dolist (name '(custom-add-load custom-autoload))
        (when (if nelisp-stdlib-custom-autoload--installed-owners
                  (not (eq (symbol-function name)
                           (cdr (assq name nelisp-stdlib-custom-autoload--installed-owners))))
                (fboundp name))
          (error "Foreign standalone custom registration is prebound: %S" name)))
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Custom registration source root is unconfigured"))
      (let* ((path (expand-file-name "lisp/custom.el" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 77104)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 77104))
                            "363b7f408c88c1788aa53f35a15ef46c07ad86fb20a9d110f381b0cd0e85a5c0"))
          (error "Custom registration GNU source pin differs")))
      (unless nelisp-stdlib-custom-autoload--installed-owners
        (dolist (form nelisp-stdlib-custom-autoload--forms) (eval form t))
        (setq nelisp-stdlib-custom-autoload--installed-owners
              (mapcar (lambda (name) (cons name (symbol-function name)))
                      '(custom-add-load custom-autoload))))
      'installed)))
(provide 'nelisp-stdlib-custom-autoload)
