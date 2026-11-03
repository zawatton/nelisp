;;; emacs-cc-pgtkselect-1.el --- pgtk selection primitives -*- lexical-binding: t; -*-

(unless (fboundp 'pgtk-disown-selection-internal)
  (defun pgtk-disown-selection-internal (selection &optional time-object terminal)
    "If we own the selection SELECTION, disown it."
    (ignore selection time-object terminal)
    nil))

(unless (fboundp 'pgtk-drop-finish)
  (defun pgtk-drop-finish (success timestamp delete)
    "Finish the drag-n-drop event that happened at TIMESTAMP."
    (ignore success delete)
    (unless (or (integerp timestamp)
                (and (consp timestamp) (integerp (car timestamp))
                     (integerp (cdr timestamp)))
                (and (floatp timestamp) (= timestamp (truncate timestamp))))
      (error "Not an in-range integer, integral float, or cons of integers"))
    nil))

(unless (fboundp 'pgtk-get-selection-internal)
  (defun pgtk-get-selection-internal (selection-symbol target-type &optional time-stamp terminal)
    "Return text selected from some X window."
    (ignore selection-symbol target-type time-stamp terminal)
    (error "GDK selection unavailable for this frame")))

(unless (fboundp 'pgtk-own-selection-internal)
  (defun pgtk-own-selection-internal (selection value &optional frame)
    "Assert a selection of type SELECTION and value VALUE."
    (ignore selection value frame)
    (error "GDK selection unavailable for this frame")))

(unless (fboundp 'pgtk-register-dnd-targets)
  (defun pgtk-register-dnd-targets (frame targets)
    "Register TARGETS on FRAME."
    (ignore frame targets)
    (error "Window system frame should be used")))

(unless (fboundp 'pgtk-selection-exists-p)
  (defun pgtk-selection-exists-p (&optional selection terminal)
    "Whether there is an owner for the given selection."
    (ignore selection terminal)
    nil))

(unless (fboundp 'pgtk-selection-owner-p)
  (defun pgtk-selection-owner-p (&optional selection terminal)
    "Whether the current Emacs process owns the given selection."
    (ignore selection terminal)
    nil))

(unless (fboundp 'pgtk-update-drop-status)
  (defun pgtk-update-drop-status (action timestamp)
    "Update the status of the current drag-and-drop operation."
    (ignore action)
    (unless (or (integerp timestamp)
                (and (consp timestamp) (integerp (car timestamp))
                     (integerp (cdr timestamp)))
                (and (floatp timestamp) (= timestamp (truncate timestamp))))
      (error "Not an in-range integer, integral float, or cons of integers"))
    nil))

(provide 'emacs-cc-pgtkselect-1)
