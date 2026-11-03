;;; nemacs-magit-bundle-diagnose-probe.el --- diagnose Magit bundle load -*- lexical-binding: t; -*-

;; Evaluated by `make magit-bundle-diagnose' against the base runtime image.
;; This sits below `magit-status-diagnose': if the generated Magit bundle
;; itself stops loading, this prints the last bundle part reached.

(defun nemacs-magit-bundle-diagnose--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(let* ((repo (file-name-as-directory
              (or (and (boundp 'nemacs-magit-probe-repo-root)
                       nemacs-magit-probe-repo-root)
                  (getenv "NEMACS_MAGIT_REPO_ROOT")
                  "")))
       (bridge-source (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo))
       (bundle-dir (expand-file-name "build" repo))
       (part-count (and (boundp 'nemacs-magit-bundle-part-count)
                        nemacs-magit-bundle-part-count))
       (parts nil))
  (nemacs-magit-bundle-diagnose--print
   "MAGIT-BUNDLE bridge-source path=%S readable=%S loader=%S"
   bridge-source
   (file-readable-p bridge-source)
   (fboundp 'nemacs-runtime-image-preload--load-source-file))
  (if (fboundp 'nemacs-runtime-image-preload--load-source-file)
      (nemacs-runtime-image-preload--load-source-file
       bridge-source)
    (load bridge-source nil 'no-message t t))
  (setq nelisp-emacs-magit-bridge-repo-root repo)
  (nemacs-magit-bundle-diagnose--print
   "MAGIT-BUNDLE bridge-source fbound=%S load=%S"
   (fboundp 'nelisp-emacs-magit-bridge--ensure-preconditions)
   (fboundp 'nelisp-emacs-magit-bridge-load))
  (nemacs-magit-bundle-diagnose--print "MAGIT-BUNDLE preconditions BEGIN")
  (nelisp-emacs-magit-bridge--ensure-preconditions)
  (nemacs-magit-bundle-diagnose--print "MAGIT-BUNDLE preconditions PASS")
  (unless (and (integerp part-count)
               (> part-count 0))
    (error "MAGIT-BUNDLE missing part count: %S" part-count))
  (let ((part 1))
    (while (<= part part-count)
      (push (expand-file-name
             (format "nelisp-emacs-magit-bridge-bundle-part%d.el" part)
             bundle-dir)
            parts)
      (setq part (1+ part))))
  (setq parts (nreverse parts))
  (nemacs-magit-bundle-diagnose--print
   "MAGIT-BUNDLE parts=%d" (length parts))
  (dolist (file parts)
    (let ((part-name
           (file-name-base file)))
      (nemacs-magit-bundle-diagnose--print
       "MAGIT-BUNDLE %s BEGIN %S" part-name file)
      (load file nil 'no-message t t)
      (nemacs-magit-bundle-diagnose--print "MAGIT-BUNDLE %s PASS" part-name)))
  (nemacs-magit-bundle-diagnose--print "MAGIT-BUNDLE-SUMMARY PASS"))

;;; nemacs-magit-bundle-diagnose-probe.el ends here
