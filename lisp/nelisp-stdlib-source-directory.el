;;; nelisp-stdlib-source-directory.el --- Explicit genuine compatibility source root -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-source-directory--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            file-attributes file-truename file-directory-p
            file-accessible-directory-p expand-file-name file-name-as-directory secure-hash)))
(defconst nelisp-stdlib-source-directory--manifest
  '(("src/lread.c" 176587 "487a69760e78fdfc415e6051f2e6ecb4cd7b07ce41b0847d5d0d9a88a8b95e9d")
    ("lisp/emacs-lisp/find-func.el" 36696 "cd9110aee5e3f95ecfd6503daca3efe82c171bea262d3c74000c9a95e0cf57bf")))
(defvar nelisp-stdlib-source-directory--sealed-manifest
  (copy-tree nelisp-stdlib-source-directory--manifest))
;;;###autoload
(defun nelisp-stdlib-source-directory-install (&optional root)
  "Publish explicitly configured genuine GNU compatibility sources at ROOT.
On GNU hosts preserve their actual build source-directory.  On standalone,
ROOT identifies the selected compatibility corpus, not NeLisp's build tree,
GNU process identity, graphical capabilities, or compiler ABI eligibility.
The C initializer's PATH_DUMPLOADSEARCH build path is not synthesized."
  (dolist (owner nelisp-stdlib-source-directory--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Compatibility source owner changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-source-directory--manifest
                 nelisp-stdlib-source-directory--sealed-manifest)
    (error "Compatibility source manifest changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Compatibility sources require verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless (and (stringp root) (> (length root) 0))
        (error "Compatibility source root is unconfigured"))
      (setq root (file-name-as-directory (file-truename root)))
      (unless (and (file-directory-p root)
                   (file-accessible-directory-p (expand-file-name "src" root))
                   (file-accessible-directory-p (expand-file-name "lisp" root)))
        (error "Compatibility source installation is incomplete"))
      (dolist (entry nelisp-stdlib-source-directory--manifest)
        (let* ((path (expand-file-name (car entry) root))
               (attributes (file-attributes path)))
          (unless (and attributes (null (car attributes))
                       (= (nth 7 attributes) (nth 1 entry))
                       (equal (secure-hash 'sha256
                                           (nelisp-native-raw-file-read path 0 (nth 1 entry)))
                              (nth 2 entry)))
            (error "Compatibility source pin differs: %s" (car entry)))))
      (when (and (boundp 'source-directory)
                 (not (equal (symbol-value 'source-directory) "/"))
                 (not (equal (symbol-value 'source-directory) root)))
        (error "Existing compatibility source root differs"))
      (set 'source-directory root)
      'installed)))
(provide 'nelisp-stdlib-source-directory)
