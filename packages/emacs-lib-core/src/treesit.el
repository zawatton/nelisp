;;; treesit.el --- Tier 3 tree-sitter facade loader  -*- lexical-binding: t; -*-

;;; Code:

(defun treesit--nelisp-standalone-runtime-p ()
  "Return non-nil on the standalone NeLisp reader."
  (or (not (boundp 'emacs-version))
      (fboundp 'nl-write-file)
      (fboundp 'nl-syscall-write-file)
      (fboundp 'nelisp--eval-source-string)))

(defun treesit--nelisp-host-load-standard ()
  "Load host Emacs's standard `treesit' library.
This directory shadows it on `load-path', and host libraries such as
`sh-script' (reached from GNU Org's `org-src') need the real one."
  (let ((shim-dir (expand-file-name
                   (file-name-as-directory
                    (file-name-directory
                     (or (and (boundp 'load-file-name) load-file-name)
                         (and (boundp 'buffer-file-name) buffer-file-name)
                         default-directory)))))
        filtered)
    (dolist (dir load-path)
      (unless (equal (expand-file-name (file-name-as-directory dir))
                     shim-dir)
        (push dir filtered)))
    (let ((load-path (nreverse filtered)))
      (load "treesit" nil t))))

;; On the host the real library comes first; the facade's definitions are
;; all `fboundp'-guarded, so loading it afterwards only fills gaps.  A host
;; without the tree-sitter primitives cannot load the real library, and
;; then the facade alone stands in for it.
(unless (treesit--nelisp-standalone-runtime-p)
  (condition-case nil
      (treesit--nelisp-host-load-standard)
    (error nil)))
(require 'emacs-tier3-facades)
(provide 'treesit)

;;; treesit.el ends here
