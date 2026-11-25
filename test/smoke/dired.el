;;; dired.el --- Directory marking, rename and deletion -*- lexical-binding: t; -*-

(defvar dired-no-confirm)
(defvar dired-deletion-confirmer)
(defvar dired-keep-marker-rename)
(defvar delete-by-moving-to-trash)

(defun smoke-dired ()
  (let ((directory (expand-file-name "listing"))
        (dired-no-confirm '(move delete))
        (dired-deletion-confirmer #'always)
        (delete-by-moving-to-trash nil))
    (when (smoke-step "dired-open"
            (require 'dired)
            (require 'dired-aux)
            (dired directory)
            (list major-mode (key-binding (kbd "m")) (key-binding (kbd "R"))
                  (key-binding (kbd "D"))))
      (smoke-step "dired-mark"
        (unless (dired-goto-file (expand-file-name "old.txt" directory))
          (error "Dired did not list old.txt"))
        (dired-mark 1)
        (mapcar #'file-name-nondirectory (dired-get-marked-files)))
      (smoke-step "dired-rename"
        ;; Supply the minibuffer's target through Dired's file operation API.
        (let ((dired-keep-marker-rename ?*))
          (dired-create-files #'dired-rename-file "Rename"
                              (dired-get-marked-files)
                              (lambda (_) (expand-file-name "new.txt" directory))
                              dired-keep-marker-rename))
        (list (file-exists-p (expand-file-name "old.txt" directory))
              (smoke-read (expand-file-name "new.txt" directory))))
      (smoke-step "dired-delete"
        (unless (dired-goto-file (expand-file-name "new.txt" directory))
          (error "Dired did not list new.txt"))
        (dired-do-delete)
        (directory-files directory nil "\\(?:new\\|old\\|keep\\)\\.txt\\'")))))
