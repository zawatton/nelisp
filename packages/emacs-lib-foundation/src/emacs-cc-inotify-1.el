;;; emacs-cc-inotify-1.el --- inotify compatibility -*- lexical-binding: t; -*-

(defvar emacs-cc-inotify-1--next-descriptor 1)
(defvar emacs-cc-inotify-1--watches (make-hash-table :test 'equal))

(defun emacs-cc-inotify-1--fail (message detail &optional subject)
  (signal 'file-notify-error (delq nil (list message detail subject))))

(defun emacs-cc-inotify-1--aspects (aspect)
  (let ((allowed '(access attrib close-write close-nowrite create delete delete-self
                   modify move-self moved-from moved-to open all-events move close
                   dont-follow onlydir))
        (items (if (listp aspect) aspect (list aspect))))
    (dolist (item items)
      (unless (or (eq item t) (memq item allowed))
        (emacs-cc-inotify-1--fail "Unknown aspect" "Invalid argument" item)))
    items))

(unless (fboundp 'inotify-add-watch)
  (defun inotify-add-watch (filename aspect callback)
    "Add a watch for FILE-NAME to inotify."
    (unless (stringp filename)
      (signal 'wrong-type-argument (list 'stringp filename)))
    (let ((aspects (emacs-cc-inotify-1--aspects aspect)))
      (unless (file-exists-p filename)
        (emacs-cc-inotify-1--fail "Could not add watch for file" "No such file or directory" filename))
      (when (and (memq 'onlydir aspects) (not (file-directory-p filename)))
        (emacs-cc-inotify-1--fail "Could not add watch for file" "Not a directory" filename))
      (let ((descriptor (cons emacs-cc-inotify-1--next-descriptor 0)))
        (setq emacs-cc-inotify-1--next-descriptor (1+ emacs-cc-inotify-1--next-descriptor))
        (puthash descriptor (list filename aspects callback) emacs-cc-inotify-1--watches)
        descriptor))))

(unless (fboundp 'inotify-rm-watch)
  (defun inotify-rm-watch (watch-descriptor)
    "Remove an existing WATCH-DESCRIPTOR."
    (if (gethash watch-descriptor emacs-cc-inotify-1--watches)
        (progn (remhash watch-descriptor emacs-cc-inotify-1--watches) t)
      (emacs-cc-inotify-1--fail "Invalid descriptor " "Invalid argument"))))

(unless (fboundp 'inotify-valid-p)
  (defun inotify-valid-p (watch-descriptor)
    "Check a watch specified by its WATCH-DESCRIPTOR."
    (and (gethash watch-descriptor emacs-cc-inotify-1--watches) t)))

(provide 'emacs-cc-inotify-1)
