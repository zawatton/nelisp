;;; nemacs-org-mode-smoke-probe.el --- vendor-first Org mode smoke -*- lexical-binding: t; -*-

(defun nemacs-org-mode-smoke--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defun nemacs-org-mode-smoke--backtrace-lines ()
  "Return current backtrace as a list of lines."
  (let ((text (with-output-to-string (backtrace))))
    (split-string text "\n" t)))

(defvar nemacs-org-mode-smoke--repo-root nil
  "Explicit repository root for standalone Org smoke.")

(let ((ok t)
      (repo (or nemacs-org-mode-smoke--repo-root
                (and (boundp 'default-directory) default-directory)
                "."))
      (inbox nil))
  (condition-case err
      (progn
        (unless (fboundp 'nelisp-emacs-org-bridge-load)
          (load (expand-file-name "src/nelisp-emacs-org-bridge.el" repo)
                nil 'no-message t t))
        (unless (fboundp 'nelisp-emacs-org-bridge-load)
          (error "org bridge entrypoint missing after load: %S"
                 'nelisp-emacs-org-bridge-load))
        (set 'nelisp-emacs-org-bridge-repo-root repo)
        (nemacs-org-mode-smoke--print
         "ORG-MODE-SMOKE bridge repo=%S resolved=%S bundle=%S"
         repo
         (nelisp-emacs-org-bridge--repo-root)
         (nelisp-emacs-org-bridge--bundle-file))
        (nelisp-emacs-org-bridge-load)
        (setq inbox (make-temp-file "nemacs-org-mode-smoke-" nil ".org"))
        (with-temp-file inbox
          (insert "* INBOX\n** TODO ship it\nbody\n"))
        (find-file inbox)
        (org-mode)
        (nemacs-org-mode-smoke--print
         "ORG-MODE-SMOKE major-mode=%S fboundp-org-mode=%S feature-org=%S"
         major-mode (fboundp 'org-mode) (featurep 'org))
        (unless (eq major-mode 'org-mode)
          (setq ok nil))
        (goto-char (point-min))
        (search-forward "ship it")
        (beginning-of-line)
        (org-back-to-heading)
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (nemacs-org-mode-smoke--print
           "ORG-MODE-SMOKE point=%S heading=%S"
           (point) line)
          (unless (= (point) 9)
            (setq ok nil)))
        (save-buffer)
        (nemacs-org-mode-smoke--print "ORG-MODE-SMOKE save-buffer=t"))
    (error
     (setq ok nil)
     (nemacs-org-mode-smoke--print "ORG-MODE-SMOKE ERROR %S" err)
     (dolist (line (nemacs-org-mode-smoke--backtrace-lines))
       (nemacs-org-mode-smoke--print "ORG-MODE-SMOKE BT %s" line))))
  (when inbox
    (ignore-errors (delete-file inbox)))
  (nemacs-org-mode-smoke--print
   "ORG-MODE-SMOKE %s" (if ok "PASS" "FAIL")))

;;; nemacs-org-mode-smoke-probe.el ends here
