;;; nemacs-org-step-probe.el --- stepwise Org bridge probe -*- lexical-binding: t; -*-

(defun nemacs-org-step-probe--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defvar nemacs-org-step-probe--repo-root nil
  "Explicit repository root for the stepwise Org probe.")

(let ((repo nemacs-org-step-probe--repo-root)
      (inbox nil))
  (unless (and (stringp repo) (> (length repo) 0))
    (error "nemacs-org-step-probe--repo-root is required"))
  (nemacs-org-step-probe--print "ORG-STEP S0 repo=%S" repo)
  (load (expand-file-name "src/nelisp-emacs-org-bridge.el" repo)
        nil 'no-message t t)
  (setq nelisp-emacs-org-bridge-repo-root repo)
  (nemacs-org-step-probe--print "ORG-STEP S1 bridge-load=%S" (fboundp 'nelisp-emacs-org-bridge-load))
  (nelisp-emacs-org-bridge-load)
  (nemacs-org-step-probe--print "ORG-STEP S2 bridge-ok=%S" nelisp-emacs-org-bridge-loaded)
  (setq inbox (make-temp-file "nemacs-org-step-" nil ".org"))
  (nemacs-org-step-probe--print "ORG-STEP S3 temp=%S" inbox)
  (with-temp-file inbox
    (insert "* INBOX\n** TODO ship it\nbody\n"))
  (nemacs-org-step-probe--print "ORG-STEP S4 wrote=t")
  (find-file inbox)
  (nemacs-org-step-probe--print "ORG-STEP S5 find-file mode=%S point=%S" major-mode (point))
  (org-mode)
  (nemacs-org-step-probe--print "ORG-STEP S6 org-mode mode=%S point=%S" major-mode (point))
  (goto-char (point-min))
  (search-forward "ship it")
  (nemacs-org-step-probe--print "ORG-STEP S7 search point=%S" (point))
  (save-buffer)
  (nemacs-org-step-probe--print "ORG-STEP S8 save=t")
  (when inbox
    (ignore-errors (delete-file inbox))))

;;; nemacs-org-step-probe.el ends here
