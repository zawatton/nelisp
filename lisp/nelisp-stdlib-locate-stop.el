;;; nelisp-stdlib-locate-stop.el --- Genuine GNU root-search default -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-locate-stop--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes expand-file-name eval)))
(defconst nelisp-stdlib-locate-stop--form
  '(defvar locate-dominating-stop-dir-regexp
     "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'"
     "Regexp of directory names that stop the search in `locate-dominating-file'.
Any directory whose name matches this regexp will be treated like
a kind of root directory by `locate-dominating-file', which will stop its
search when it bumps into it.
The default regexp prevents fruitless and time-consuming attempts to find
special files in directories in which file names are interpreted as host names,
or mount points potentially requiring authentication as a different user."))
(defvar nelisp-stdlib-locate-stop--sealed-form
  (copy-tree nelisp-stdlib-locate-stop--form))
;;;###autoload
(defun nelisp-stdlib-locate-stop-install (&optional source-root)
  "Initialize GNU's exact root-search regexp from verified SOURCE-ROOT.
Preserve GNU host metadata and existing standalone user settings.  This
declaration does not replace the filesystem traversal implementation."
  (dolist (owner nelisp-stdlib-locate-stop--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Locate stop initializer owner changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-locate-stop--form nelisp-stdlib-locate-stop--sealed-form)
    (error "Locate stop initializer source payload changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Locate stop initializer requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        'preserved
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Locate stop initializer source is unconfigured"))
      (let* ((path (expand-file-name "lisp/files.el" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 405858)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 405858))
                            "f40bf4d7c2a77e045ea244890b870d8463b7879d0880701ce09aa895769e2ed9"))
          (error "Locate stop initializer GNU source pin differs")))
      (if (boundp 'locate-dominating-stop-dir-regexp) 'preserved
        (eval nelisp-stdlib-locate-stop--form t)
        'installed))))
(provide 'nelisp-stdlib-locate-stop)
