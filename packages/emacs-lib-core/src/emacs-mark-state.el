;;; emacs-mark-state.el --- Buffer-owned mark state -*- lexical-binding: t; -*-

(defvar transient-mark-mode (not (and (boundp 'noninteractive) noninteractive))
  "Whether commands use and deactivate an active region.
GNU simple.el expects this C-core variable before defining its global mode.
Preserve any host or consumer setting that already exists.")

(defun emacs-mark-state-initialize-interactive ()
  "Apply GNU's interactive mark default before consumer customization.
Heap images are prepared in batch mode.  Interactive consumers explicitly
apply this shared default at startup; buffer-local overrides remain intact."
  (setq-default transient-mark-mode t))

(defvar emacs-cc-mark--markers (make-hash-table :test 'eq))
(defvar select-active-regions t
  "Whether activating a region also owns the primary selection.")
(defvar deactivate-mark nil
  "Whether the command loop should deactivate the active mark.")
(defvar saved-region-selection nil
  "Primary selection saved while the active region owns it.")

;; Replace only the disposable nil bulk fallbacks. GNU definitions remain.
(dolist (name '(mark mark-marker set-mark))
  (when (get name 'emacs-stub-bulk)
    (fmakunbound name)
    (put name 'emacs-stub-bulk nil)))

(unless (fboundp 'mark-marker)
  (defun mark-marker ()
    "Return the current buffer's mark marker, initially detached."
    (let* ((buffer (current-buffer)) (marker (gethash buffer emacs-cc-mark--markers)))
      (unless marker
        (setq marker (make-marker))
        (puthash buffer marker emacs-cc-mark--markers))
      marker)))

(unless (fboundp 'mark)
  (defun mark (&optional force)
    "Return the current buffer's mark position, or nil if unset."
    (when (and (not force) (boundp 'transient-mark-mode) transient-mark-mode
               (not mark-active) (not mark-even-if-inactive))
      (error "The mark is not active now"))
    (marker-position (mark-marker))))

(unless (fboundp 'set-mark)
  (defun set-mark (position)
    "Move the mark to POSITION; a nil position detaches it."
    (if (null position)
        (set-marker (mark-marker) nil)
      (set-marker (mark-marker) position (current-buffer))
      (setq mark-active t)
      nil)))

(provide 'emacs-mark-state)
;;; emacs-mark-state.el ends here
