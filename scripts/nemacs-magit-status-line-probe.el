;;; nemacs-magit-status-line-probe.el --- inspect line/section mapping -*- lexical-binding: t; -*-

(let* ((repo (file-name-as-directory (getenv "NEMACS_MAGIT_REPO_ROOT")))
       (fixture (file-name-as-directory (getenv "NEMACS_MAGIT_FIXTURE_DIR"))))
  (load (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo)
        nil 'no-message t t)
  (setq nelisp-emacs-magit-bridge-repo-root repo)
  (nelisp-emacs-magit-bridge-load)
  (let ((default-directory fixture))
    (magit-status-setup-buffer fixture))
  (with-current-buffer (magit-get-mode-buffer 'magit-status-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((line 1))
        (while (< (point) (point-max))
          (let ((section (magit-section-at (point)))
                (text (buffer-substring-no-properties
                       (line-beginning-position)
                       (line-end-position))))
            (nelisp--write-stdout-bytes
             (format "LINE %S point=%S type=%S text=%S\n"
                     line (point)
                     (and section (oref section type))
                     text)))
          (forward-line 1)
          (setq line (1+ line)))))
    (goto-char (point-min))
    (let ((sec0 (magit-current-section)))
      (magit-section-forward)
      (nelisp--write-stdout-bytes
       (format "FORWARD point=%S type0=%S type1=%S eq=%S\n"
               (point)
               (oref sec0 type)
               (oref (magit-current-section) type)
               (eq sec0 (magit-current-section)))))))

;;; nemacs-magit-status-line-probe.el ends here
