;;; par-simple-kill-ring.el --- GNU kill-ring behavior -*- lexical-binding: t; -*-

(defmacro par-simple-kill-ring-row (label &rest body)
  `(progn
     (prin1 (cons ',label (condition-case err (progn ,@body)
                           (error (list 'error (car err))))))
     (terpri)))

(par-simple-kill-ring-row owners
  (mapcar (lambda (name)
            (let ((file (symbol-file name 'defun)))
              (and file (equal (file-name-base file) "simple") t)))
          '(current-kill kill-append kill-ring-save yank)))

(par-simple-kill-ring-row paste-hook
  (let* ((kill-ring (list "cached"))
         (kill-ring-yank-pointer kill-ring)
         (interprogram-paste-function 'par-missing-paste-function))
    (list (current-kill 0))))

(par-simple-kill-ring-row append-undo
  (with-temp-buffer
    (let ((kill-ring (list "old"))
          (kill-append-merge-undo t)
          (interprogram-cut-function nil)
          (interprogram-paste-function nil))
      (setq buffer-undo-list (list (cons 1 2) nil (cons 3 4)))
      (kill-append " plus" nil)
      (list kill-ring (member nil buffer-undo-list)))))

(par-simple-kill-ring-row copy-and-yank
  (with-temp-buffer
    (let ((kill-ring nil)
          (interprogram-cut-function nil)
          (interprogram-paste-function nil)
          (yank-excluded-properties t))
      (insert (propertize "abc" 'face 'bold))
      (kill-ring-save 1 3)
      (goto-char (point-max))
      (yank)
      (list (buffer-string) (point) (mark t) (get-text-property 4 'face)))))
