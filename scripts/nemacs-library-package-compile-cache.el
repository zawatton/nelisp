;;; nemacs-library-package-compile-cache.el --- reuse fresh C-core bytecode -*- lexical-binding: t; -*-

;;; Commentary:

;; Archive installation smokes run after the repository compile gate.  Reuse
;; the included C-core bytecode only when it is present and at least as new as
;; its installed source; otherwise retain package.el's normal compilation.

;;; Code:

(defun nemacs-library-package-compile-cache--byte-compile-file
    (original file &rest arguments)
  "Reuse fresh installed C-core bytecode, else call ORIGINAL on FILE."
  (let ((compiled (concat (expand-file-name file) "c")))
    (if (and (string-match-p "\\`emacs-cc-.*\\.el\\'"
                             (file-name-nondirectory file))
             (file-readable-p compiled)
             (not (file-newer-than-file-p file compiled)))
        compiled
      (apply original file arguments))))

(defun nemacs-library-package-compile-cache--package-compile
    (original descriptor)
  "Skip package compilation when DESCRIPTOR contains a complete fresh cache."
  (let* ((directory (package-desc-dir descriptor))
         (sources (and (stringp directory)
                       (directory-files-recursively directory "\\.el\\'")))
         (sources
          (cl-remove-if
           (lambda (file)
             (let ((name (file-name-nondirectory file)))
               (or (string-suffix-p "-autoloads.el" name)
                   (string-suffix-p "-pkg.el" name))))
           sources)))
    (if (and sources
             (cl-every
              (lambda (file)
                (let ((compiled (concat file "c")))
                  (and (file-readable-p compiled)
                       (not (file-newer-than-file-p file compiled)))))
              sources))
        nil
      (funcall original descriptor))))

(require 'bytecomp)
(require 'cl-lib)
(require 'package)
(unless (advice-member-p
         #'nemacs-library-package-compile-cache--byte-compile-file
         'byte-compile-file)
  (advice-add 'byte-compile-file :around
              #'nemacs-library-package-compile-cache--byte-compile-file))
(unless (advice-member-p
         #'nemacs-library-package-compile-cache--package-compile
         'package--compile)
  (advice-add 'package--compile :around
              #'nemacs-library-package-compile-cache--package-compile))

(provide 'nemacs-library-package-compile-cache)
;;; nemacs-library-package-compile-cache.el ends here
