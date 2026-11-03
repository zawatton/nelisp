;;; nelisp-stdlib-etags-program.el --- Authenticated GNU program-name default -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-etags-program--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes expand-file-name nelisp--target-os-code)))
;;;###autoload
(defun nelisp-stdlib-etags-program-install (&optional source-root)
  "Publish GNU's non-Android program name from authenticated SOURCE-ROOT.
This supplies a startup variable, without claiming the executable exists.
Preserve genuine GNU host metadata and existing standalone user values."
  (dolist (owner nelisp-stdlib-etags-program--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Etags program initializer owner changed: %S" (car owner))))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Etags program initializer requires verified dialect"))
    (if (not (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified))
        (if (boundp 'etags-program-name) 'preserved
          (error "GNU program-name owner unavailable"))
      (unless (and (subrp (symbol-function 'nelisp--target-os-code))
                   (= (nelisp--target-os-code) 0))
        (error "Etags program default unsupported on this runtime target"))
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Etags program initializer source is unconfigured"))
      (let* ((path (expand-file-name "src/callproc.c" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes))
                     (= (nth 7 attributes) 69878)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 69878))
                            "8822204190eaf45fecca1b9a920a05f1c3fb811dc672b2aada5e5739ab1eb5b4"))
          (error "Etags program initializer GNU source pin differs")))
      (if (boundp 'etags-program-name) 'preserved
        ;; src/callproc.c: non-Android Vetags_program_name build_string.
        (eval '(defvar etags-program-name "etags") t)
        'installed))))
(provide 'nelisp-stdlib-etags-program)
