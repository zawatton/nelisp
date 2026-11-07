;;; nelisp-stdlib-completion-ignored.el --- Genuine GNU completion defaults -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
(require 'nelisp-bytecode-compiler-input)
(require 'nelisp-native-raw-file)
(defvar nelisp-stdlib-completion-ignored--owners
  (mapcar (lambda (name) (cons name (symbol-function name)))
          '(nelisp-bytecode-compiler-input-dialect nelisp-native-raw-file-read
            secure-hash file-attributes)))
(defconst nelisp-stdlib-completion-ignored--form
  '
  (setq completion-ignored-extensions
        (append
         (cond ((memq system-type '(ms-dos windows-nt))
                '(".o" "~" ".bin" ".bak" ".obj" ".map" ".ico" ".pif" ".lnk"
                  ".a" ".ln" ".blg" ".bbl" ".dll" ".drv" ".vxd" ".386"))
	       (t
                '(".o" "~" ".bin" ".lbin" ".so"
                  ".a" ".ln" ".blg" ".bbl")))
         '(".elc" ".lof"
	   ".glo" ".idx" ".lot"
	   ;; VCS metadata directories
	   ".svn/" ".hg/" ".git/" ".bzr/" "CVS/" "_darcs/" "_MTN/"
	   ;; TeX-related
	   ".fmt" ".tfm"
	   ;; Java compiled
	   ".class"
	   ;; CLISP
	   ".fas" ".lib" ".mem"
	   ;; CMUCL
	   ".x86f" ".sparcf"
	   ;; OpenMCL / Clozure CL
	   ".dfsl" ".pfsl" ".d64fsl" ".p64fsl" ".lx64fsl" ".lx32fsl"
	   ".dx64fsl" ".dx32fsl" ".fx64fsl" ".fx32fsl" ".sx64fsl"
	   ".sx32fsl" ".wx64fsl" ".wx32fsl"
           ;; Other CL implementations (Allegro, LispWorks)
           ".fasl" ".ufsl" ".fsl" ".dxl"
	   ;; Libtool
	   ".lo" ".la"
	   ;; Gettext
	   ".gmo" ".mo"
	   ;; Texinfo-related
	   ;; This used to contain .log, but that's commonly used for log
	   ;; files you do want to see, not just TeX stuff.  -- fx
	   ".toc" ".aux"
	   ".cp" ".fn" ".ky" ".pg" ".tp" ".vr"
	   ".cps" ".fns" ".kys" ".pgs" ".tps" ".vrs"
	   ;; Python byte-compiled
           ".pyc" ".pyo")))
  )
(defvar nelisp-stdlib-completion-ignored--sealed-form
  (copy-tree nelisp-stdlib-completion-ignored--form))
;;;###autoload
(defun nelisp-stdlib-completion-ignored-install (&optional source-root)
  "Initialize the exact GNU bindings default, preserving existing user values.
Authenticate the standalone runtime and SOURCE-ROOT before any publication."
  (dolist (owner nelisp-stdlib-completion-ignored--owners)
    (unless (eq (symbol-function (car owner)) (cdr owner))
      (error "Completion initializer owner changed: %S" (car owner))))
  (unless (equal nelisp-stdlib-completion-ignored--form
                 nelisp-stdlib-completion-ignored--sealed-form)
    (error "Completion initializer payload changed"))
  (let ((evidence (nelisp-bytecode-compiler-input-dialect)))
    (unless (eq (plist-get evidence :status) 'pinned)
      (error "Completion initializer requires verified dialect"))
    (if (boundp 'completion-ignored-extensions)
        'preserved
      (unless (eq (plist-get evidence :runtime-evidence) 'standalone-build-verified)
        (error "Completion initializer requires verified standalone runtime"))
      (unless (and (stringp source-root) (> (length source-root) 0))
        (error "Completion initializer source is unconfigured"))
      (let* ((path (expand-file-name "bindings.el.gz" source-root))
             (attributes (file-attributes path)))
        (unless (and attributes (null (car attributes)) (= (nth 7 attributes) 18055)
                     (equal (secure-hash 'sha256
                                         (nelisp-native-raw-file-read path 0 18055))
                            "4e9df3887bdcaf22e71d81b0ba0662a2ebe586e5dee984fd0959243b39e1cbc1"))
          (error "Completion initializer GNU source pin differs")))
      (eval nelisp-stdlib-completion-ignored--form t)
      'installed)))
(provide 'nelisp-stdlib-completion-ignored)
