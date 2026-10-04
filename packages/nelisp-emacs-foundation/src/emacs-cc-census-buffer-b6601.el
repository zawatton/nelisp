;;; emacs-cc-census-buffer-b6601.el --- Labeled restrictions and buffer notifications  -*- lexical-binding: t; -*-

;;; Code:

(unless (fboundp 'combine-after-change-execute)
  (defun combine-after-change-execute (&rest arguments)
    "Deliver deferred after-change notifications when combining has ended."
    (when arguments
      (signal 'wrong-number-of-arguments
              (list 'combine-after-change-execute (length arguments))))
    (emacs-buffer-builtins--flush-after-change
     emacs-buffer-builtins--after-change-buffer)))

(defun emacs-cc-census-buffer-b6601--position (position)
  "Return POSITION as an integer, validating an integer or marker argument."
  (cond ((integerp position) position)
        ((markerp position)
         (or (marker-position position)
             (signal 'error '("Marker does not point anywhere"))))
        (t (signal 'wrong-type-argument (list 'integer-or-marker-p position)))))

(unless (fboundp 'internal--labeled-narrow-to-region)
  (defun internal--labeled-narrow-to-region (&rest arguments)
    "Narrow to START and END and push a restriction identified by LABEL."
    (unless (= (length arguments) 3)
      (signal 'wrong-number-of-arguments
              (list 'internal--labeled-narrow-to-region (length arguments))))
    (let ((start (nth 0 arguments)) (end (nth 1 arguments))
          (label (nth 2 arguments)))
      (setq start (emacs-cc-census-buffer-b6601--position start)
            end (emacs-cc-census-buffer-b6601--position end))
      (when (or (< (min start end) 1)
		(> (max start end) (1+ (nelisp-buffer-size (current-buffer)))))
	(signal 'args-out-of-range (list start end)))
      (let* ((buffer (current-buffer))
             (ext (emacs-buffer--ensure-ext buffer))
             (stack (emacs-buffer--labeled-restrictions buffer)))
	;; A nil label suspends the containing labeled limits, just as GNU does.
	(when (null label)
          (emacs-buffer--set-labeled-restrictions buffer nil))
	(unwind-protect (narrow-to-region start end)
          (emacs-buffer--set-labeled-restrictions buffer stack))
	(emacs-buffer--set-labeled-restrictions buffer
              (cons (list label (copy-marker (point-min) nil)
                          (copy-marker (point-max) t) 1) stack)))
      nil)))

(unless (fboundp 'internal--labeled-widen)
  (defun internal--labeled-widen (&rest arguments)
    "Pop the current restriction if its label is LABEL, then widen."
    (unless (= (length arguments) 1)
      (signal 'wrong-number-of-arguments
              (list 'internal--labeled-widen (length arguments))))
    (let ((label (car arguments)))
      (let* ((buffer (current-buffer))
             (ext (gethash buffer emacs-buffer--state))
             (stack (and ext (emacs-buffer--labeled-restrictions buffer))))
	(when (and stack (eq label (caar stack)))
          (emacs-buffer-builtins--retain-label-stack (cdr stack))
          (emacs-buffer--set-labeled-restrictions buffer (cdr stack))
          (emacs-buffer-builtins--release-label-stack stack))
	(widen)))))

(unless (fboundp 'set-buffer-local-toplevel-value)
  (defun set-buffer-local-toplevel-value (&rest arguments)
    "Set SYMBOL's toplevel local binding in BUFFER to VALUE and return nil."
    (unless (and (>= (length arguments) 2) (<= (length arguments) 3))
      (signal 'wrong-number-of-arguments
              (list 'set-buffer-local-toplevel-value (length arguments))))
    (emacs-buffer-set-buffer-local-toplevel-value
     (nth 0 arguments) (nth 1 arguments) (nth 2 arguments))))

(defvar emacs-cc-census-buffer-b6601--suspend-backend nil
  "Existing suspension backend, retained for supported suspension requests.")

(unless (fboundp 'suspend-emacs)
  (setq emacs-cc-census-buffer-b6601--suspend-backend
        (symbol-function 'suspend-emacs))
  (defun suspend-emacs (&rest arguments)
    "Suspend through the existing backend, with optional STUFFSTRING."
    (when (> (length arguments) 1)
      (signal 'wrong-number-of-arguments (list 'suspend-emacs (length arguments))))
    (let ((stuffstring (car arguments)))
      (when (and stuffstring (not (stringp stuffstring)))
	(signal 'wrong-type-argument (list 'stringp stuffstring)))
      (funcall emacs-cc-census-buffer-b6601--suspend-backend stuffstring))))

(provide 'emacs-cc-census-buffer-b6601)
;;; emacs-cc-census-buffer-b6601.el ends here
