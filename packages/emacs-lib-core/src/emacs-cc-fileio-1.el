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
    "Permit a retry of the current buffer's auto-save."
    (setq emacs-cc-fileio-1--auto-save-failure-time nil)))

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

(defvar auto-save-hook)
(defvar auto-save-include-big-deletions)
(defvar emacs-cc-fileio-2--auto-saved-tick)
(defvar-local emacs-cc-fileio-1--auto-save-failure-time nil)
(put 'emacs-cc-fileio-1--auto-save-failure-time 'permanent-local t)

(defun emacs-cc-fileio-1--auto-save-buffer (no-message)
  "Auto-save the current buffer if edits have changed it since the last save."
  (when (and (stringp buffer-auto-save-file-name)
             (not (buffer-base-buffer))
             (buffer-modified-p)
             (not (equal (buffer-modified-tick) emacs-cc-fileio-2--auto-saved-tick))
             (natnump buffer-saved-size)
             (or (not emacs-cc-fileio-1--auto-save-failure-time)
                 (>= (- (float-time) emacs-cc-fileio-1--auto-save-failure-time) 1200)))
    (if (and buffer-file-name (not no-message)
             (not (bound-and-true-p auto-save-include-big-deletions))
             (> buffer-saved-size 5000)
             (< (* 4 (buffer-size)) (* 3 buffer-saved-size)))
        (progn
          (setq buffer-saved-size -1)
          (message "Buffer %s has shrunk a lot; auto save disabled in that buffer until next real save"
                   (buffer-name)))
      (condition-case err
          (save-restriction
            (widen)
            (let ((coding-system-for-write buffer-file-coding-system)
                  (buffer-file-format buffer-auto-save-file-format))
              (with-file-modes #o600
                (write-region nil nil buffer-auto-save-file-name nil 'silent)))
            (set-buffer-auto-saved)
            (setq buffer-saved-size (buffer-size)
                  emacs-cc-fileio-1--auto-save-failure-time nil)
            t)
        (file-error
         (setq emacs-cc-fileio-1--auto-save-failure-time (float-time))
         (message "Auto-saving %s: %s" (buffer-name) (error-message-string err))
         nil)))))

(unless (fboundp 'do-auto-save)
  (defun do-auto-save (&optional no-message current-only)
    "Auto-save buffers changed since they were last saved or auto-saved.
NO-MESSAGE suppresses success messages. CURRENT-ONLY limits saving
to the current buffer."
    (run-hooks 'auto-save-hook)
    (let ((buffers (if current-only (list (current-buffer)) (buffer-list)))
          (saved nil))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (when (emacs-cc-fileio-1--auto-save-buffer no-message)
              (setq saved t)))))
      (when (and saved (not no-message))
        (message "Auto-saving...done")))
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
  (defvar emacs-cc-fileio--make-directory-primitive
    (symbol-function 'make-directory)
    "Directory-creation leaf before GNU files.el replaces its facade.")
  (defun make-directory-internal (directory)
    "Create a new directory named DIRECTORY through the captured leaf."
    ;; GNU make-directory calls this primitive.  Looking up its public name
    ;; here would recurse back through the newly loaded Lisp facade.
    (funcall emacs-cc-fileio--make-directory-primitive directory)))

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
