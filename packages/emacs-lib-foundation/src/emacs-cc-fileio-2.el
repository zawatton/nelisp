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

(provide 'emacs-cc-fileio-2)
;;; emacs-cc-fileio-2.el ends here
