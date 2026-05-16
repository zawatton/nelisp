;;; nemacs-gtk-frontend-menu-test.el --- ERT for GTK menu wiring -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(setq load-prefer-newer t)
(require 'nemacs-gtk-frontend)

(ert-deftest nemacs-gtk-menu-spec-includes-find-file-leaf ()
  (let* ((file-menu (assoc "File" nemacs-gtk--menu-spec))
         (leaves (cdr file-menu)))
    (should (equal '("Open File..." . "find-file")
                   (assoc "Open File..." leaves)))))

(ert-deftest nemacs-gtk-menu-accels-defconst-shape ()
  (should (listp nemacs-gtk--menu-accels))
  (dolist (entry nemacs-gtk--menu-accels)
    (should (consp entry))
    (should (stringp (car entry)))
    (should (stringp (cdr entry)))))

(ert-deftest nemacs-gtk-handle-menu-action-find-file-dispatches ()
  (let (called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (command &optional _record-flag _keys)
                 (setq called command)
                 :ok)))
      (should (eq :ok (nemacs-gtk--handle-menu-action "find-file")))
      (should (eq #'find-file called)))))

(ert-deftest nemacs-gtk-handle-menu-action-undo-dispatches ()
  (let (called)
    (cl-letf (((symbol-function 'call-interactively)
               (lambda (command &optional _record-flag _keys)
                 (setq called command)
                 :ok)))
      (should (eq :ok (nemacs-gtk--handle-menu-action "undo")))
      (should (eq #'undo called)))))

(provide 'nemacs-gtk-frontend-menu-test)

;;; nemacs-gtk-frontend-menu-test.el ends here
