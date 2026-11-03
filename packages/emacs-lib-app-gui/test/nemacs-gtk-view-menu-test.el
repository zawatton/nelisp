;;; nemacs-gtk-view-menu-test.el --- ERT for nemacs-gtk-view-menu.el  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emacs-mode)
(require 'nemacs-gtk-view-menu)

(defmacro nemacs-gtk-view-menu-test--with-fresh-world (&rest body)
  "Run BODY with clean buffer and window state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (emacs-buffer--state (make-hash-table :test 'eq))
         (emacs-buffer--variable-buffer-local nil)
         (emacs-buffer--default-values (make-hash-table :test 'eq))
         (emacs-window--id-counter 0)
         (emacs-window--root nil)
         (emacs-window--selected nil))
     (emacs-mode-reset)
     ,@body))

(defmacro nemacs-gtk-view-menu-test--with-nelisp-bindings (&rest body)
  "Run BODY with menu helpers bound to the Nelisp buffer UI."
  (declare (indent 0) (debug (body)))
  `(cl-letf (((symbol-function 'nemacs-gtk-view-menu--buffer-list)
              #'emacs-buffer-buffer-list)
             ((symbol-function 'nemacs-gtk-view-menu--buffer-name)
              #'nelisp-ec-buffer-name)
             ((symbol-function 'nemacs-gtk-view-menu--apply-buffer-action)
              #'emacs-buffer-ui-switch-to-buffer))
     ,@body))

(defun nemacs-gtk-view-menu-test--submenu-leaves ()
  "Return the `Switch to Buffer' leaves from `nemacs-gtk-view-menu-build'."
  (cdr (cadr (nemacs-gtk-view-menu-build))))

(ert-deftest nemacs-gtk-view-menu-build-includes-current-buffers ()
  (nemacs-gtk-view-menu-test--with-fresh-world
    (nemacs-gtk-view-menu-test--with-nelisp-bindings
      (nelisp-ec-generate-new-buffer "alpha")
      (nelisp-ec-generate-new-buffer "beta")
      (let ((leaves (nemacs-gtk-view-menu-test--submenu-leaves)))
        (should (member '("alpha" . "switch-to-buffer:alpha") leaves))
        (should (member '("beta" . "switch-to-buffer:beta") leaves))))))

(ert-deftest nemacs-gtk-view-menu-build-encodes-buffer-name-as-action ()
  (nemacs-gtk-view-menu-test--with-fresh-world
    (nemacs-gtk-view-menu-test--with-nelisp-bindings
      (nelisp-ec-generate-new-buffer "name with spaces")
      (let ((leaf (assoc "name with spaces"
                         (nemacs-gtk-view-menu-test--submenu-leaves))))
        (should (equal '("name with spaces" . "switch-to-buffer:name with spaces")
                       leaf))))))

(ert-deftest nemacs-gtk-view-menu-handle-action-switches-to-named-buffer ()
  (nemacs-gtk-view-menu-test--with-fresh-world
    (nemacs-gtk-view-menu-test--with-nelisp-bindings
      (let ((alpha (nelisp-ec-generate-new-buffer "alpha"))
            (beta (nelisp-ec-generate-new-buffer "beta")))
        (emacs-window-set-window-buffer (emacs-window-selected-window) alpha)
        (nelisp-ec-set-buffer alpha)
        (should (eq t (nemacs-gtk-view-menu-handle-action "switch-to-buffer:beta")))
        (should (eq beta (emacs-window-window-buffer)))
        (should (eq beta (nelisp-ec-current-buffer)))))))

(ert-deftest nemacs-gtk-view-menu-handle-action-returns-nil-on-unrelated-string ()
  (nemacs-gtk-view-menu-test--with-fresh-world
    (nemacs-gtk-view-menu-test--with-nelisp-bindings
      (let ((alpha (nelisp-ec-generate-new-buffer "alpha")))
        (emacs-window-set-window-buffer (emacs-window-selected-window) alpha)
        (nelisp-ec-set-buffer alpha)
        (should-not (nemacs-gtk-view-menu-handle-action "save"))
        (should (eq alpha (emacs-window-window-buffer)))
        (should (eq alpha (nelisp-ec-current-buffer)))))))

(provide 'nemacs-gtk-view-menu-test)

;;; nemacs-gtk-view-menu-test.el ends here
