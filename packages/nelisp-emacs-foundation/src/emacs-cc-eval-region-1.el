;;; emacs-cc-eval-region-1.el --- Evaluate a bounded buffer stream -*- lexical-binding: t; -*-

(defvar emacs-eval-region--source nil)
(defvar emacs-eval-region--end nil)

(unless (fboundp 'eval-region)
  (defalias 'emacs-eval-region--original-read (symbol-function 'read))

  (defun emacs-eval-region--read (&optional stream)
    (if (and emacs-eval-region--source
             (eq stream emacs-eval-region--source))
        (let* ((origin (point))
               (limit (marker-position emacs-eval-region--end))
               (text (buffer-substring-no-properties origin limit))
               (nelisp--rd-error-index nil))
          (condition-case err
              (let ((result (read-from-string text)))
                (goto-char (+ origin (cdr result)))
                (car result))
            (end-of-file
             (goto-char limit)
             (signal (car err) (cdr err)))
            (invalid-read-syntax
             (let ((kind (nelisp--read-buffer-scan text 0 (length text))))
               (cond ((consp kind) (goto-char (+ origin 1 (cdr kind))))
                     ((integerp nelisp--rd-error-index)
                      (goto-char (+ origin 1 nelisp--rd-error-index)))))
             (signal (car err) (cdr err)))))
      (emacs-eval-region--original-read stream)))

  (defun emacs-eval-region--next (cursor reader custom-reader)
    (set-buffer emacs-eval-region--source)
    (goto-char (marker-position cursor))
    (let ((limit (marker-position emacs-eval-region--end)))
      (while (progn
               (skip-chars-forward " \t\r\n" limit)
               (and (< (point) limit) (eq (char-after) ?\;)))
        (skip-chars-forward "^\n" limit))
      (unless (>= (point) limit)
        (let ((form
               (if custom-reader
                   (let ((original-read (symbol-function 'read)))
                     (unwind-protect
                         (progn
                           (fset 'read #'emacs-eval-region--read)
                           (funcall reader emacs-eval-region--source))
                       (fset 'read original-read)))
                 (funcall reader emacs-eval-region--source))))
          (set-marker cursor (point))
          (cons t form)))))

  (defun eval-region (start end &optional printflag read-function)
    "Evaluate forms between START and END, optionally printing their values."
    (let ((private-point (not (null start))))
      (setq start (if (null start) (point)
                    (if (markerp start) (marker-position start) start)))
      (setq end (if (null end) (point-max)
                  (if (markerp end) (marker-position end) end)))
      (unless (integerp start)
        (signal 'wrong-type-argument (list 'integer-or-marker-p start)))
      (unless (integerp end)
        (signal 'wrong-type-argument (list 'integer-or-marker-p end)))
      (unless (and (<= (point-min) end) (<= end (point-max)))
        (signal 'args-out-of-range (list start end)))
      (let ((emacs-eval-region--source (current-buffer))
            (emacs-eval-region--end (copy-marker end))
            (cursor (copy-marker (max (point-min) (min start (point-max)))))
            (reader (or read-function #'emacs-eval-region--read))
            (lexical lexical-binding)
            (finished nil))
        (unwind-protect
            (while (not finished)
              ;; An explicit START gives the reader private point movement.
              ;; A nil START leaves read movement visible, including errors.
              (let ((item
                     (if private-point
                         (save-excursion
                           (emacs-eval-region--next cursor reader read-function))
                       (save-current-buffer
                         (emacs-eval-region--next cursor reader read-function)))))
                (if (null item)
                    (setq finished t)
                  (let ((value (eval (cdr item) lexical)))
                    (when printflag (print value printflag))))))
          (set-marker cursor nil)
          (set-marker emacs-eval-region--end nil)))
      nil)))

(provide 'emacs-cc-eval-region-1)
