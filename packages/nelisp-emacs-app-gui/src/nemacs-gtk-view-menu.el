;;; nemacs-gtk-view-menu.el --- Dynamic View menu buffer submenu helper  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 zawatton + Claude

;; This file is part of nelisp-emacs.

;;; Commentary:

;; Helper for nelisp-emacs-gtk Phase 2.A menu-bar finish work.
;; Design reference:
;; /home/madblack-21/Cowork/Notes/dev/nelisp-emacs-gtk/docs/design/01-phase2a-menu-bar-finish.org
;; §3.3 "Task C: buffer-list を View menu に動的展開".
;;
;; This module intentionally does not compose or install the full menu
;; bar.  Task A owns the top-level menu-bar integration and can call
;; `nemacs-gtk-view-menu-build' when wiring the View menu.

;;; Code:

(require 'emacs-buffer-ui)

(defconst nemacs-gtk-view-menu--switch-prefix "switch-to-buffer:"
  "Action-name prefix used by the dynamic buffer submenu.")

(defun nemacs-gtk-view-menu--buffer-list ()
  "Return the current live buffer list."
  (buffer-list))

(defun nemacs-gtk-view-menu--buffer-name (buffer)
  "Return BUFFER's display name."
  (buffer-name buffer))

(defun nemacs-gtk-view-menu--apply-buffer-action (name)
  "Apply the dynamic buffer action for NAME."
  (switch-to-buffer name))

(defun nemacs-gtk-view-menu--buffer-submenu ()
  "Return flat submenu leaves for the current live buffer list."
  (let ((items nil))
    (dolist (buf (nemacs-gtk-view-menu--buffer-list))
      (let ((name (nemacs-gtk-view-menu--buffer-name buf)))
        (when (and (stringp name)
                   (> (length name) 0))
          (push (cons name
                      (concat nemacs-gtk-view-menu--switch-prefix name))
                items))))
    (nreverse items)))

;;;###autoload
(defun nemacs-gtk-view-menu-build ()
  "Return the View menu spec with a dynamic `Switch to Buffer' submenu."
  (list "View"
        (cons "Switch to Buffer"
              (nemacs-gtk-view-menu--buffer-submenu))))

;;;###autoload
(defun nemacs-gtk-view-menu-handle-action (action)
  "Handle dynamic View menu ACTION.
Recognises `switch-to-buffer:NAME', switches to NAME, and returns t.
Return nil for unrelated ACTION strings so other dispatchers can
continue handling them."
  (when (and (stringp action)
             (>= (length action) (length nemacs-gtk-view-menu--switch-prefix))
             (string= (substring action 0 (length nemacs-gtk-view-menu--switch-prefix))
                      nemacs-gtk-view-menu--switch-prefix))
    (nemacs-gtk-view-menu--apply-buffer-action
     (substring action (length nemacs-gtk-view-menu--switch-prefix)))
    t))

;;;###autoload
(defun nemacs-gtk-view-menu-rebuild-trigger ()
  "Hook helper for refreshing the GTK menu bar before display.
This module does not install a full menu-bar spec by itself because
Task A owns the top-level composer.  Callers should attach this helper
once a full-spec rebuild entry point exists."
  nil)

(provide 'nemacs-gtk-view-menu)

;;; nemacs-gtk-view-menu.el ends here
