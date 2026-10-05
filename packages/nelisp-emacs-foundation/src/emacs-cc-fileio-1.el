;;; emacs-cc-fileio-1.el --- fileio.c primitives -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'access-file)
  (defun access-file (filename string)
    "Access FILENAME, and get an error if that does not work."
    (unless (file-readable-p filename)
      (signal 'file-missing (list string "No such file or directory" filename)))
    nil))

(unless (fboundp 'clear-buffer-auto-save-failure)
  (defun clear-buffer-auto-save-failure ()
    "Clear any record of a recent auto-save failure in the current buffer."
    (when (local-variable-p 'auto-save-failure)
      (setq auto-save-failure nil))))

(unless (fboundp 'delete-directory-internal)
  (defvar emacs-cc-fileio--delete-directory-primitive
    (symbol-function 'delete-directory)
    "Original directory-removal leaf before GNU files.el replaces its facade.")
  (defun delete-directory-internal (directory)
    "Delete the directory named DIRECTORY.  Does not follow symlinks."
    (funcall emacs-cc-fileio--delete-directory-primitive directory)))

(unless (fboundp 'delete-file-internal)
  (defvar emacs-cc-fileio--delete-file-primitive
    (symbol-function 'delete-file)
    "Original file-removal leaf before GNU files.el replaces its facade.")
  (defun delete-file-internal (filename)
    "Delete file named FILENAME; internal use only."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    (funcall emacs-cc-fileio--delete-file-primitive filename)))

(unless (fboundp 'directory-name-p)
  (defun directory-name-p (name)
    "Return non-nil if NAME ends with a directory separator character."
    (unless (stringp name) (signal 'wrong-type-argument (list 'stringp name)))
    (and (> (length name) 0)
         (memq (aref name (1- (length name))) '(?/ ?\\)) t)))

(unless (fboundp 'do-auto-save)
  (defun do-auto-save (&optional no-message current-only)
    "Auto-save all buffers that need it."
    (ignore no-message current-only)
    nil))

(unless (fboundp 'file-acl)
  (defun file-acl (filename)
    "Return ACL entries of file named FILENAME, or nil if it does not exist."
    (and (stringp filename) nil)))

(unless (fboundp 'file-selinux-context)
  (defun file-selinux-context (filename)
    "Return SELinux context of file named FILENAME."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    '(nil nil nil nil)))

(unless (fboundp 'file-system-info)
  (defun file-system-info (filename)
    "Return storage information about the file system FILENAME is on."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    nil))

(unless (fboundp 'make-directory-internal)
  (defun make-directory-internal (directory)
    "Create a new directory named DIRECTORY."
    (make-directory directory)))

(unless (fboundp 'make-temp-file-internal)
  (defun make-temp-file-internal (prefix dir-flag suffix text)
    "Generate a temporary file or directory beginning with PREFIX."
    (unless (stringp prefix) (signal 'wrong-type-argument (list 'stringp prefix)))
    (let* ((base (make-temp-file prefix dir-flag))
           (name (if suffix (concat base suffix) base)))
      (when suffix
        (if dir-flag
            (progn (delete-directory base) (make-directory name))
          (rename-file base name)))
      (when (stringp text)
        (with-temp-buffer (insert text) (write-region (point-min) (point-max) name)))
      name))
  ;; The I/O owner replaces this compatibility fallback when it is loaded.
  ;; Never use this marker for a native/host primitive.
  (put 'make-temp-file-internal 'emacs-cc-fileio-fallback t))

(unless (fboundp 'next-read-file-uses-dialog-p)
  (defun next-read-file-uses-dialog-p ()
    "Return t if a call to `read-file-name' will use a dialog."
    (and (display-graphic-p)
         (bound-and-true-p use-dialog-box))))

(provide 'emacs-cc-fileio-1)
