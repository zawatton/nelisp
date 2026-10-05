;;; user-init-library-fixes.el --- S13 synthetic parity probes -*- lexical-binding: t; -*-
;; No user's source, dictionary, credentials, or journal contents here.
;; Host loads shared owners; the cold image already owns their public shims.
(defvar user-init-library-fixes--reference
  (equal (getenv "USER_INIT_FIXES_GNU_REFERENCE") "1"))
(unless user-init-library-fixes--reference
  (require 'emacs-list)
  (require 'emacs-fileio-builtins)
  (require 'emacs-shell-command))
(defun user-init-library-fixes--glob (pattern &optional full regexp)
  (if user-init-library-fixes--reference
      (file-expand-wildcards pattern full regexp)
    (emacs-fileio-expand-wildcards pattern full regexp)))
(defvar user-init-library-fixes--history nil)
(defvar user-init-library-fixes--custom)
(defun user-init-library-fixes--defaults ()
  (let ((sym (make-symbol "s13-default")))
    (set-default sym 42)
    (defcustom user-init-library-fixes--custom nil "S13 setter probe."
      :type 'boolean :set (lambda (symbol value) (set-default symbol value)))
    (list (boundp sym) (symbol-value sym)
          (boundp 'user-init-library-fixes--custom)
          (symbol-value 'user-init-library-fixes--custom))))
(defun user-init-library-fixes--history ()
  (let ((fn (if user-init-library-fixes--reference
                #'add-to-history #'emacs-list-add-to-history))
        (history-length 3) (history-delete-duplicates t)
        (user-init-library-fixes--history '("duplicate" "old" "old")))
    (funcall fn 'user-init-library-fixes--history "old")
    (let ((dedup (copy-sequence user-init-library-fixes--history)))
      (funcall fn 'user-init-library-fixes--history "")
      (funcall fn 'user-init-library-fixes--history "" 2 t)
      (let ((empty (copy-sequence user-init-library-fixes--history)))
        (funcall fn 'user-init-library-fixes--history "zero" 0 t)
        (list dedup empty user-init-library-fixes--history)))))
(defun user-init-library-fixes-run ()
  (let ((root (make-temp-file "s13-glob-" t)))
    (unwind-protect
        (let ((default-directory (file-name-as-directory root)))
          (dolist (dir '("pkg-b" "pkg-a" "other"))
            (make-directory (expand-file-name dir root)))
          (dolist (file '("pkg-b/b.el" "pkg-a/a.el" "pkg-a/z.el" "pkg-a/.hidden.el" "other/c.txt"))
            (write-region "s13" nil (expand-file-name file root) nil 'silent))
          (prin1
           (list
            (user-init-library-fixes--glob "pkg-*/*.el")
            (mapcar #'file-name-nondirectory (user-init-library-fixes--glob "pkg-a/[az].el" t))
            (user-init-library-fixes--glob "pkg-[!b]/?.el")
            (user-init-library-fixes--glob "missing/*")
            (user-init-library-fixes--glob "pkg-a/.*")
            (user-init-library-fixes--glob "pkg-*/")
            (user-init-library-fixes--glob "pkg-a/[az]\\.el" nil t)
            (let ((emacs-shell-command--orig-shell-command nil))
              (with-temp-buffer
                (insert "beforeafter")
                (goto-char 7)
                (let ((result (shell-command "printf middle" t)))
                  (list result (buffer-string) (point) (mark t)))))
            (shell-command-to-string "printf s13")
            (user-init-library-fixes--history)
            (user-init-library-fixes--defaults))))
      (delete-directory root t)))
  (princ "\nS13-FIXES-DONE\n"))
(user-init-library-fixes-run)
