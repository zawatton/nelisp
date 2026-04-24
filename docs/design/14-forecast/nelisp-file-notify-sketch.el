;;; nelisp-file-notify-sketch.el --- Phase 5-C.3 forecast sketch -*- lexical-binding: t; -*-
;;
;;; Code:

(require 'cl-lib)
(require 'filenotify)

(defvar nelisp-file-notify--descriptors nil
  "Alist (DESC . CALLBACK-WRAPPER).")

(defun nelisp-file-notify-add-watch (path flags callback)
  "Wrap host `file-notify-add-watch' with NeLisp event normalisation.
Returns a descriptor compatible with `-rm-watch'."
  (let* ((wrapper
          (lambda (ev)
            ;; ev = (DESC ACTION FILE [FILE1])
            (let* ((desc (nth 0 ev))
                   (action (nth 1 ev))
                   (file (nth 2 ev))
                   (file1 (nth 3 ev))
                   (normalised
                    (list :descriptor desc
                          :action action
                          :file file
                          :file1 file1)))
              (funcall callback normalised))))
         (desc (file-notify-add-watch path flags wrapper)))
    (push (cons desc wrapper) nelisp-file-notify--descriptors)
    desc))

(defun nelisp-file-notify-rm-watch (desc)
  (file-notify-rm-watch desc)
  (setq nelisp-file-notify--descriptors
        (assq-delete-all desc nelisp-file-notify--descriptors)))

(defun nelisp-file-notify-valid-p (desc)
  (file-notify-valid-p desc))

(defun nelisp-file-notify-list ()
  (mapcar #'car nelisp-file-notify--descriptors))

;; File attribute helpers used by the broader module.

(defun nelisp-file-attributes (path)
  (file-attributes path))

(defun nelisp-file-exists-p (path)
  (file-exists-p path))

(defun nelisp-file-directory-p (path)
  (file-directory-p path))

(defun nelisp-delete-file (path)
  (delete-file path))

(defun nelisp-rename-file (src dst &optional ok-if-exists)
  (rename-file src dst ok-if-exists))

(provide 'nelisp-file-notify-sketch)
;;; nelisp-file-notify-sketch.el ends here
