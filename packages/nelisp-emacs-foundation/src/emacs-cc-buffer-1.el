;;; emacs-cc-buffer-1.el --- GNU buffer.c primitives -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-buffer-1--buffer (object)
  "Return OBJECT as a live buffer, or signal GNU's buffer validation error."
  (unless (bufferp object)
    (signal 'wrong-type-argument (list 'bufferp object)))
  (unless (buffer-live-p object)
    (signal 'error (list "Selecting deleted buffer" object)))
  object)

(unless (fboundp 'barf-if-buffer-read-only)
  (defun barf-if-buffer-read-only (&optional position)
    "Signal a `buffer-read-only' error if the current buffer is read-only."
    (let ((pos (or position (point))))
      (when (and buffer-read-only
                 (not (get-char-property pos 'inhibit-read-only)))
        (signal 'buffer-read-only (list (current-buffer))))
      nil)))

(unless (fboundp 'buffer-last-name)
  (defun buffer-last-name (&optional buffer)
    "Return the last name of BUFFER, or its current name."
    (buffer-name (emacs-cc-buffer-1--buffer (or buffer (current-buffer))))))

(unless (fboundp 'buffer-swap-text)
  (defun buffer-swap-text (buffer)
    "Swap the text between current buffer and BUFFER."
    (let ((other (emacs-cc-buffer-1--buffer buffer))
          (here (current-buffer)))
      (unless (eq other here)
        (let ((a (with-current-buffer here (buffer-substring (point-min) (point-max))))
              (b (with-current-buffer other (buffer-substring (point-min) (point-max)))))
          (with-current-buffer here (let ((inhibit-read-only t)) (erase-buffer) (insert b)))
          (with-current-buffer other (let ((inhibit-read-only t)) (erase-buffer) (insert a)))))
      t)))

(unless (fboundp 'bury-buffer-internal)
  (defun bury-buffer-internal (buffer)
    "Move BUFFER to the end of the buffer list."
    (bury-buffer (emacs-cc-buffer-1--buffer buffer))))

(unless (fboundp 'delete-all-overlays)
  (defun delete-all-overlays (&optional buffer)
    "Delete all overlays of BUFFER, defaulting to the current buffer."
    (let ((target (or buffer (current-buffer))))
      (unless (bufferp target) (signal 'wrong-type-argument (list 'bufferp target)))
      (when (buffer-live-p target)
        (if (fboundp 'emacs-buffer-delete-all-overlays)
            (emacs-buffer-delete-all-overlays target)
          (with-current-buffer target (mapc #'delete-overlay (overlays-in (point-min) (point-max))))))
      nil)))

(unless (fboundp 'find-buffer)
  (defun find-buffer (variable value)
    "Return a live buffer whose buffer-local VARIABLE equals VALUE."
    (unless (symbolp variable) (signal 'wrong-type-argument (list 'symbolp variable)))
    (catch 'found
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer)
                   (local-variable-p variable buffer)
                   (equal (buffer-local-value variable buffer) value))
          (throw 'found buffer)))
      nil)))

(unless (fboundp 'get-truename-buffer)
  (defun get-truename-buffer (filename)
    "Return a buffer visiting FILENAME by its file truename."
    (unless (stringp filename) (signal 'wrong-type-argument (list 'stringp filename)))
    (let ((true (file-truename filename)) found)
      (dolist (buffer (buffer-list))
        (when (and (buffer-live-p buffer) (buffer-file-name buffer)
                   (equal (condition-case nil (file-truename (buffer-file-name buffer)) (error nil)) true))
          (setq found buffer)))
      found)))

(unless (fboundp 'internal--set-buffer-modified-tick)
  (defun internal--set-buffer-modified-tick (tick &optional buffer)
    "Set BUFFER's modification tick to TICK when supported."
    (let ((target (or buffer (current-buffer))))
      (unless (integerp tick) (signal 'wrong-type-argument (list 'integerp tick)))
      (emacs-cc-buffer-1--buffer target)
      (when (and (fboundp 'emacs-buffer--ensure-ext)
                 (fboundp 'emacs-buffer--ext-modified-tick))
        (setf (emacs-buffer--ext-modified-tick (emacs-buffer--ensure-ext target)) tick))
      tick)))

(unless (fboundp 'other-buffer)
  (defun other-buffer (&optional buffer visible-ok frame)
    "Return the most recently selected buffer other than BUFFER."
    (let* ((exclude (and (bufferp buffer) (buffer-live-p buffer) buffer))
           (buffers (if (and frame (frame-live-p frame))
                        (buffer-list frame) (buffer-list)))
           (chosen (or (cl-find-if (lambda (b) (and (not (eq b exclude))
                                                   (not (string-prefix-p " " (buffer-name b)))
                                                   (or visible-ok (not (get-buffer-window b t))))) buffers)
                       (cl-find-if (lambda (b) (and (not (eq b exclude))
                                                   (not (string-prefix-p " " (buffer-name b)))))
                                   (buffer-list)))))
      (or chosen (get-buffer-create "*scratch*")))))

(unless (fboundp 'set-buffer-major-mode)
  (defun set-buffer-major-mode (buffer)
    "Set an appropriate major mode for BUFFER."
    (let ((target (emacs-cc-buffer-1--buffer buffer)))
      (with-current-buffer target
        (funcall (if (equal (buffer-name target) "*scratch*")
                     initial-major-mode (default-value 'major-mode))))
      nil)))

(provide 'emacs-cc-buffer-1)
;;; emacs-cc-buffer-1.el ends here
