;;; nelisp-stdlib-data-directory.el --- Explicit authenticated compatibility resources -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-data-directory--dialect-owner
  (symbol-function 'nelisp-bytecode-compiler-input-dialect))
(defvar nelisp-stdlib-data-directory--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-native-raw-file-read file-attributes file-truename
            expand-file-name file-name-as-directory secure-hash)))
(defconst nelisp-stdlib-data-directory--manifest
  '((source "image.el.gz" 18033 "54daf504e4e6e221b152bccf4c300ab164389225f818fd8e4bd6190c8094ec0b")
    (asset "images/up.svg" 4864 "94717ba4cad999d50ad9e8b0972bd6f49433e1c89008b59bf34f87c4c2dfd5c4")
    (asset "images/new.xpm" 3331 "fb3386656272a8451810179978c738125e651c8199573380ce8f521a47e19a1b")))
(defvar nelisp-stdlib-data-directory--sealed-manifest
  (copy-tree nelisp-stdlib-data-directory--manifest))
(defun nelisp-stdlib-data-directory--check-file (path size digest)
  "Check a real regular file's exact bytes through the sealed native reader."
  (let* ((attributes (file-attributes path)) (mode (nth 8 attributes)))
    (unless (and attributes (null (car attributes))
                 (stringp mode) (> (length mode) 0) (= (aref mode 0) ?-))
      (error "Required compatibility resource missing: %s" path))
    (unless (and (= (nth 7 attributes) size)
                 (equal (secure-hash 'sha256 (nelisp-native-raw-file-read path 0 size)) digest))
      (error "Compatibility resource source pin differs: %s" path))))
(defun nelisp-stdlib-data-directory-install (&optional source-root resource-root)
  "Publish the actual SOURCE-ROOT/RESOURCE-ROOT compatibility bundle directory.
RESOURCE-ROOT must be the configured source bundle's sibling etc directory.
Its genuine GNU31 image source and selected actual assets must match exact pins.
Preserve GNU hosts.  Refuse unconfigured standalone resources; this is unrelated
to GNU process identity, native ABI eligibility or graphical image support."
  (unless (and (eq (symbol-function 'nelisp-bytecode-compiler-input-dialect)
                   nelisp-stdlib-data-directory--dialect-owner)
               (equal nelisp-stdlib-data-directory--manifest
                      nelisp-stdlib-data-directory--sealed-manifest))
    (error "Compatibility resource evidence owner changed"))
  (dolist (owner nelisp-stdlib-data-directory--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Compatibility resource runtime owner changed: %S" (car owner))))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (and (eq (plist-get evidence :status) 'pinned)
                 (equal (plist-get evidence :dialect) "GNU Emacs 31.1"))
      (error "Compatibility resources require verified GNU31 dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless (and (stringp source-root) (stringp resource-root))
        (error "Compatibility resource root is unconfigured"))
      (let* ((source (file-name-as-directory (expand-file-name source-root)))
             (root (file-name-as-directory (file-truename resource-root)))
             (expected (file-name-as-directory
                        (file-truename (expand-file-name "../etc" source)))))
        (unless (equal root expected) (error "Compatibility resource root differs from source bundle"))
        (dolist (record nelisp-stdlib-data-directory--manifest)
          (nelisp-stdlib-data-directory--check-file
           (expand-file-name (nth 1 record) (if (eq (car record) 'source) source root))
           (nth 2 record) (nth 3 record)))
        (when (and (boundp 'data-directory) (not (equal data-directory root)))
          (error "Existing runtime data directory owner differs"))
        (set 'data-directory root)
        'installed))))
(provide 'nelisp-stdlib-data-directory)
