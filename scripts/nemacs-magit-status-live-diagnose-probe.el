;;; nemacs-magit-status-live-diagnose-probe.el --- live-load Magit status diagnosis -*- lexical-binding: t; -*-

;; Evaluated by `make magit-status-live-diagnose' against the base runtime
;; image.  This keeps baked magit-runtime replay failures separate from the
;; actual status-buffer diagnostic stages in
;; `scripts/nemacs-magit-status-diagnose-probe.el'.

(defvar nemacs-magit-status-live-diagnose--ok t)

(defun nemacs-magit-status-live-diagnose--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defun nemacs-magit-status-live-diagnose--backtrace-lines ()
  (let ((text (with-output-to-string (backtrace))))
    (split-string text "\n" t)))

(defun nemacs-magit-status-live-diagnose--run (name thunk)
  (nemacs-magit-status-live-diagnose--print "MAGIT-LIVE %s BEGIN" name)
  (condition-case err
      (let ((value (funcall thunk)))
        (nemacs-magit-status-live-diagnose--print
         "MAGIT-LIVE %s PASS %S" name value)
        value)
    (error
     (setq nemacs-magit-status-live-diagnose--ok nil)
     (nemacs-magit-status-live-diagnose--print
      "MAGIT-LIVE %s ERROR %S" name err)
     (dolist (line (nemacs-magit-status-live-diagnose--backtrace-lines))
       (nemacs-magit-status-live-diagnose--print
        "MAGIT-LIVE %s BT %s" name line))
     nil)))

(let* ((repo (file-name-as-directory (getenv "NEMACS_MAGIT_REPO_ROOT")))
       (status-probe (expand-file-name
                      "scripts/nemacs-magit-status-diagnose-probe.el"
                      repo)))
  (nemacs-magit-status-live-diagnose--print "MAGIT-LIVE repo PASS %S" repo)
  (nemacs-magit-status-live-diagnose--run
   "bridge-source-load"
   (lambda ()
     (load (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo)
           nil 'no-message t t)))
  (when nemacs-magit-status-live-diagnose--ok
    (nemacs-magit-status-live-diagnose--run
     "bridge-load"
     (lambda ()
       (setq nelisp-emacs-magit-bridge-repo-root repo)
       (nelisp-emacs-magit-bridge-load))))
  (if nemacs-magit-status-live-diagnose--ok
      (nemacs-magit-status-live-diagnose--run
       "status-probe-eval"
       (lambda ()
         (load status-probe nil 'no-message t t)
         (and (boundp 'nemacs-magit-status-diagnose--done)
              nemacs-magit-status-diagnose--done)))
    (nemacs-magit-status-live-diagnose--print "MAGIT-DIAG-SUMMARY FAIL")))

;;; nemacs-magit-status-live-diagnose-probe.el ends here
