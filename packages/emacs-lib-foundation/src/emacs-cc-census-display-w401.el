;;; emacs-cc-census-display-w401.el --- Headless display primitives  -*- lexical-binding: t; -*-

;;; Code:

(defun emacs-cc-census-display-w401--arity (name arguments maximum)
  "Signal an arity error when NAME receives more than MAXIMUM ARGUMENTS."
  (when (> (length arguments) maximum)
    (signal 'wrong-number-of-arguments (list name (length arguments)))))

(unless (fboundp 'posn-at-point)
  (defun posn-at-point (&rest arguments)
    "Return glyph position information for POS in WINDOW.
POS and WINDOW are optional and default to WINDOW's point and the selected
window.  A headless batch session has no visible glyphs, so return nil after
validating the window and position."
    (emacs-cc-census-display-w401--arity 'posn-at-point arguments 2)
    (let ((window (or (cadr arguments) (selected-window)))
          (position (car arguments)))
      ;; GNU decodes the window before checking the position, including
      ;; when both arguments are invalid.
      (unless (window-live-p window)
        (signal 'wrong-type-argument (list 'window-live-p window)))
      (unless (null position)
        (unless (or (integerp position) (markerp position))
          (signal 'wrong-type-argument
                  (list 'integer-or-marker-p position)))
        (when (and (markerp position) (null (marker-position position)))
          (signal 'error '("Marker does not point anywhere"))))
      ;; No range check: even out-of-buffer integer positions simply have
      ;; no visible glyph in GNU's batch session.
      nil)))

(unless (fboundp 'redisplay)
  (defun redisplay (&rest arguments)
    "Perform redisplay, returning t unless executing a keyboard macro.
The optional FORCE argument is accepted for historical reasons and ignored."
    (emacs-cc-census-display-w401--arity 'redisplay arguments 1)
    (unless (and (boundp 'executing-kbd-macro) executing-kbd-macro)
      (unless noninteractive
        (emacs-redisplay-trigger-redisplay))
      t)))

(unless (fboundp 'redraw-display)
  (defun redraw-display (&rest arguments)
    "Clear and redisplay all visible frames, returning nil."
    (interactive)
    (emacs-cc-census-display-w401--arity 'redraw-display arguments 0)
    (unless noninteractive
      (emacs-redisplay-redraw-display))
    nil))

;; The remaining assigned primitives keep their existing bindings:
;;
;; backtrace--frames-from-thread is provided by the runtime activation ABI.
;; Its Lisp veneer uses GC-rooted evaluator records, including cold-image runs.
;;
;; set-window-dedicated-p, set-window-display-table and set-window-hscroll
;; need working readers and storage in the shared window model.  Their
;; readers in emacs-stub-bulk.el return nil regardless of the window.
;; scroll-left and scroll-right need that same horizontal-scroll storage.
;;
;; set-frame-selected-window needs per-frame selection storage and a reader
;; that consults it.  frame-selected-window in emacs-window-builtins.el
;; ignores the frame after validation and returns the global selected window.
;; Installing private state here would leave those public readers unchanged.

(provide 'emacs-cc-census-display-w401)
;;; emacs-cc-census-display-w401.el ends here
