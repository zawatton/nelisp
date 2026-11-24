;;; emacs-cc-fileio-2.el --- fileio C-core primitives -*- lexical-binding: t; -*-

;;; Code:

(defvar emacs-cc-fileio-2--auto-saved-ticks nil
  "Buffer/tick pairs recording the last explicit auto-save mark.")

(defun emacs-cc-fileio-2--tick (&optional buffer)
  "Return BUFFER's modification tick when available."
  (and (fboundp 'buffer-modified-tick)
       (condition-case nil (buffer-modified-tick buffer) (error nil))))

(unless (fboundp 'recent-auto-save-p)
  (defun recent-auto-save-p ()
    "Return t if current buffer has been auto-saved recently."
    (let* ((buffer (current-buffer))
           (saved (assq buffer emacs-cc-fileio-2--auto-saved-ticks))
           (saved-tick (cdr saved))
           (save-tick (and (fboundp 'files--buffer-save-tick)
                           (files--buffer-save-tick buffer))))
      (if (numberp save-tick)
          (and saved (numberp saved-tick) (< save-tick saved-tick))
        (let ((current-tick (emacs-cc-fileio-2--tick buffer)))
          (and saved current-tick (equal saved-tick current-tick)))))))

(unless (fboundp 'set-binary-mode)
  (defun set-binary-mode (stream mode)
    "Switch STREAM to binary I/O mode or text I/O mode."
    (ignore mode)
    (unless (memq stream '(stdin stdout stderr))
      (signal 'error (list "unsupported stream" stream)))
    t))

(unless (fboundp 'set-buffer-auto-saved)
  (defun set-buffer-auto-saved ()
    "Mark current buffer as auto-saved with its current text."
    (let ((buffer (current-buffer)))
      (setq emacs-cc-fileio-2--auto-saved-ticks
            (cons (cons buffer (emacs-cc-fileio-2--tick buffer))
                  (assq-delete-all buffer emacs-cc-fileio-2--auto-saved-ticks)))
      nil)))

(unless (fboundp 'set-file-acl)
  (defun set-file-acl (filename acl-string)
    "Set ACL of file named FILENAME to ACL-STRING."
    (ignore filename acl-string)
    nil))

(unless (fboundp 'set-file-selinux-context)
  (defun set-file-selinux-context (filename context)
    "Set SELinux context of file named FILENAME to CONTEXT."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    (ignore context)
    nil))

(unless (fboundp 'unix-sync)
  (defun unix-sync ()
    "Tell Unix to finish all pending disk updates."
    (when (and (fboundp 'executable-find) (executable-find "sync")
               (fboundp 'call-process))
      (call-process "sync" nil nil nil))
    nil))

(unless (fboundp 'directory-files-recursively)
  ;; GNU lisp/files.el (not vendored); org-persist-gc, project and indexers call
  ;; it.  Same contract (depth first, sorted per directory, absolute names),
  ;; built on `directory-files' instead of `file-name-all-completions'.
  (defun directory-files-recursively (dir regexp
                                          &optional include-directories predicate
                                          follow-symlinks)
    "Return list of all files under directory DIR whose names match REGEXP."
    (let ((result nil)
          (files nil)
          (dir (directory-file-name dir)))
      (dolist (leaf (directory-files dir nil nil nil))
        (unless (member leaf '("." ".."))
          (let ((full-file (concat dir "/" leaf)))
            (if (file-directory-p full-file)
                (progn
                  (when (and (or (not (file-symlink-p full-file)) follow-symlinks)
                             (or (eq predicate nil) (eq predicate t)
                                 (funcall predicate full-file)))
                    (let ((sub-files
                           (if (eq predicate t)
                               (condition-case nil
                                   (directory-files-recursively
                                    full-file regexp include-directories
                                    predicate follow-symlinks)
                                 (file-error nil))
                             (directory-files-recursively
                              full-file regexp include-directories
                              predicate follow-symlinks))))
                      (setq result (nconc result sub-files))))
                  (when (and include-directories (string-match regexp leaf))
                    (setq result (nconc result (list full-file)))))
              (when (string-match regexp leaf)
                (push full-file files))))))
      (nconc result (nreverse files)))))

(provide 'emacs-cc-fileio-2)
;;; emacs-cc-fileio-2.el ends here
