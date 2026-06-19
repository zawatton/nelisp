;;; emacs-toolbar-test.el --- ERT for emacs-toolbar  -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'emacs-toolbar)

(defmacro emacs-toolbar-test--with-fresh-world (&rest body)
  "Run BODY with isolated toolbar state."
  (declare (indent 0) (debug t))
  `(let ((emacs-toolbar-gui-backend nil)
         (emacs-toolbar-gui-spec emacs-toolbar-gui-default-spec)
         (emacs-toolbar-gui-cell-width-default 9))
     ,@body))

(ert-deftest emacs-toolbar-hit-test-uses-cell-width ()
  (emacs-toolbar-test--with-fresh-world
    (emacs-toolbar-gui-register-backend :cell-width (lambda () 6))
    (should (equal "New" (emacs-toolbar-gui-label-at-x 7)))
    (should (equal "C-x C-f" (emacs-toolbar-gui-keys-at-x 7)))
    (should (equal "Open" (emacs-toolbar-gui-label-at-x 39)))
    (should (equal "" (emacs-toolbar-gui-label-at-x 9999)))))

(ert-deftest emacs-toolbar-click-opens-dropdown ()
  (emacs-toolbar-test--with-fresh-world
    (let (written-menu)
      (emacs-toolbar-gui-register-backend
       :cell-width (lambda () 6)
       :write-menu (lambda (menu) (setq written-menu menu)))
      (let ((result (emacs-toolbar-gui-handle-click "7,0")))
        (should (equal "" (plist-get result :keys)))
        (should (eq 'ignore (plist-get result :command)))
        (should (equal "ignore" (plist-get result :effective-command)))
        (should (string-match-p "Find File\tC-x C-f" written-menu))))))

(ert-deftest emacs-toolbar-click-dropdown-row-yields-keys ()
  (emacs-toolbar-test--with-fresh-world
    (let ((current-menu (emacs-toolbar-gui-menu-for-label "Save"))
          written-menu)
      (emacs-toolbar-gui-register-backend
       :read-menu (lambda () current-menu)
       :write-menu (lambda (menu) (setq written-menu menu)))
      (let ((result (emacs-toolbar-gui-handle-click "10,35")))
        (should (equal "C-x C-w" (plist-get result :keys)))
        (should (null (plist-get result :command)))
        (should (equal "" written-menu))))))

(ert-deftest emacs-toolbar-click-empty-dropdown-row-ignores ()
  (emacs-toolbar-test--with-fresh-world
    (let ((current-menu (emacs-toolbar-gui-menu-for-label "Undo"))
          written-menu)
      (emacs-toolbar-gui-register-backend
       :read-menu (lambda () current-menu)
       :write-menu (lambda (menu) (setq written-menu menu)))
      (let ((result (emacs-toolbar-gui-handle-click "10,80")))
        (should (equal "" (plist-get result :keys)))
        (should (eq 'ignore (plist-get result :command)))
        (should (equal "ignore" (plist-get result :effective-command)))
        (should (equal "" written-menu))))))

(ert-deftest emacs-toolbar-write-state-uses-backend ()
  (emacs-toolbar-test--with-fresh-world
    (let (written)
      (emacs-toolbar-gui-register-backend
       :write-state (lambda (spec) (setq written spec)))
      (emacs-toolbar-gui-write-state)
      (should (equal emacs-toolbar-gui-default-spec written)))))

(provide 'emacs-toolbar-test)

;;; emacs-toolbar-test.el ends here
