;;; nemacs-org-image-preload.el --- bake vendor GNU Org into a runtime image -*- lexical-binding: t; -*-

;;; Commentary:

;; Loaded by `make bake-org-runtime-image' / `make bake-org-runtime-dump' on top
;; of the base runtime image.  It loads the real vendor GNU Org (source,
;; unpatched) from the image load-path, so a baked image starts with `org'
;; ready instead of paying the interpreted load on every `(require 'org)'.
;; Kept out of nemacs-runtime-image-preload.el so the base image replay is
;; unchanged.

;;; Code:

(defvar nemacs-org-extension-features
  '(org org-agenda ox ox-ascii ox-html ob-core ob-emacs-lisp org-capture)
  "Features loaded by `nemacs-runtime-image-preload-org-extension'.")

(defvar nemacs-org-extension-failures nil
  "Alist of (FEATURE . ERROR) for features that failed during the Org bake.")

(defun nemacs-runtime-image-preload-org-extension (repo-root)
  "Extend a base runtime image with the real vendor Org feature set.
REPO-ROOT is the repository root.  A failure of a secondary feature is
recorded in `nemacs-org-extension-failures' and printed; only `org' itself
is fatal."
  (nemacs-runtime-image-setup-paths repo-root)
  (setq nemacs-org-extension-failures nil)
  (dolist (feature nemacs-org-extension-features)
    (condition-case err
        (require feature)
      (error
       (push (cons feature err) nemacs-org-extension-failures)
       (when (fboundp 'nelisp--write-stdout-bytes)
         (nelisp--write-stdout-bytes
          (format "ORG-BAKE feature %S failed: %S\n" feature err))))))
  ;; Warm the lazily loaded `org-modules' libraries (ol-*) so the first
  ;; `org-mode' in a started image does not pay for them.
  (condition-case err
      (with-temp-buffer (org-mode))
    (error
     (push (cons 'org-mode err) nemacs-org-extension-failures)
     (when (fboundp 'nelisp--write-stdout-bytes)
       (nelisp--write-stdout-bytes
        (format "ORG-BAKE warm org-mode failed: %S\n" err)))))
  ;; The first `find-file' of a fresh process lazily loads a long chain (about
  ;; 100 s interpreted, measured on the base image); pay it here once.
  (let ((file (make-temp-file "nemacs-org-warm-" nil ".org")))
    (unwind-protect
        (condition-case err
            (progn
              (with-temp-file file (insert "* TODO warm\nSCHEDULED: <2026-01-01 Thu>\n"))
              (let ((buf (find-file-noselect file)))
                (with-current-buffer buf (org-mode))
                (kill-buffer buf)))
          (error
           (push (cons 'find-file err) nemacs-org-extension-failures)
           (when (fboundp 'nelisp--write-stdout-bytes)
             (nelisp--write-stdout-bytes
              (format "ORG-BAKE warm find-file failed: %S\n" err)))))
      (ignore-errors (delete-file file))))
  (unless (featurep 'org)
    (error "org bake: feature org did not load: %S" nemacs-org-extension-failures))
  t)

(provide 'nemacs-org-image-preload)

;;; nemacs-org-image-preload.el ends here
