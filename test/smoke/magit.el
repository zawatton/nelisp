;;; magit.el --- Git status, index, commit and log -*- lexical-binding: t; -*-

(defun smoke-magit-git (directory &rest args)
  (with-temp-buffer
    (let ((default-directory directory))
      (let ((code (apply #'call-process "git" nil t nil args)))
        (unless (equal code 0)
          (error "git %S returned %S: %s" args code (buffer-string)))
        (buffer-substring-no-properties (point-min) (point-max))))))

(defun smoke-magit ()
  (when (smoke-step "magit-load"
          (setq magit-auto-revert-mode nil)
          (require 'magit-autorevert)
          (require 'magit)
          (require 'git-commit)
          (require 'magit-commit)
          (require 'magit-status)
          'ready)
    (let ((repo (file-name-as-directory (expand-file-name "repo")))
          (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
      (smoke-step "magit-status"
        (magit-status repo)
        (list major-mode
              (lookup-key (current-local-map) (kbd "s"))
              (smoke-matching-lines "^[[:space:]]*\\(Unstaged changes\\|modified[[:space:]]+tracked.txt\\)")))
      (smoke-step "magit-stage"
        (let ((default-directory repo))
          (magit-stage-file (list "tracked.txt"))
          (smoke-magit-git repo "diff" "--cached" "--name-only")))
      (smoke-step "magit-commit"
        (let* ((default-directory repo)
               ;; -m supplies the message, so no Emacs server socket is needed.
               (with-editor-emacsclient-executable nil)
               (process (magit-commit-create '("-m" "smoke third"))))
          (list (smoke-await-process process)
                (smoke-magit-git repo "log" "-1" "--format=%s"))))
      (smoke-step "magit-log"
        (let ((default-directory repo))
          (with-current-buffer (magit-log-setup-buffer '("HEAD") '("-n3") nil)
            (list major-mode
                  (smoke-matching-lines "smoke \\(first\\|second\\|third\\)")
                  (smoke-magit-git repo "status" "--porcelain"))))))))

