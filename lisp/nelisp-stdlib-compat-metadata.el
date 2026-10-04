;;; nelisp-stdlib-compat-metadata.el --- Verified Lisp API target metadata -*- lexical-binding: t; -*-

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; In standalone NeLisp, `emacs-version' describes the selected Emacs Lisp
;; API compatibility target.  It does not identify a GNU Emacs process or
;; attest native ABI support, complete API coverage, or package acceptance.
;; `nelisp-version' identifies the runtime.  Startup must publish its genuine
;; repository entrypoint declaration before using this installer.  This
;; module never supplies dialect or runtime identity, or a `nelisp' feature.

;;; Code:

(defvar nelisp-version)
(declare-function nelisp-bytecode-compiler-input-dialect
                  "nelisp-bytecode-compiler-input")

;;;###autoload
(defun nelisp-stdlib-compat-metadata-install (&optional target)
  "Install the verified standalone Lisp API TARGET without replacing a host.
TARGET defaults to the compiler's verified dialect.  Existing host metadata
and function cells are preserved.  Unverified or mismatched targets fail
before publishing any metadata.  Runtime identity must already be present."
  (if (boundp 'emacs-version)
      'preserved
    (unless (fboundp 'nelisp-bytecode-compiler-input-dialect)
      (error "Compatibility metadata requires compiler dialect evidence"))
    (let* ((evidence (nelisp-bytecode-compiler-input-dialect))
           (selected (and (eq (plist-get evidence :status) 'pinned)
                          (eq (plist-get evidence :runtime-evidence)
                              'standalone-build-verified)
                          (equal (plist-get evidence :dialect) "GNU Emacs 31.1")
                          "31.1")))
      (unless (and selected (or (null target) (equal target selected)))
        (error "Compatibility metadata requires a matching verified target"))
      (unless (and (boundp 'nelisp-version)
                   (stringp nelisp-version) (> (length nelisp-version) 0))
        (error "Compatibility metadata requires genuine runtime identity"))
      (eval (list 'defconst 'nelisp-emacs-lisp-compatibility-target selected
                  "Selected Lisp API target; not complete compatibility or ABI evidence.")
            nil)
      (eval (list 'defvar 'emacs-version selected
                  "Selected Emacs Lisp API target in NeLisp; see nelisp-version.")
            nil)
      (eval '(defvar emacs-major-version 31) nil)
      (eval '(defvar emacs-minor-version 1) nil)
      (list :target selected :implementation 'nelisp
            :implementation-version nelisp-version))))

(provide 'nelisp-stdlib-compat-metadata)
;;; nelisp-stdlib-compat-metadata.el ends here
