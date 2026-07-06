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

;;;; Icon registry (glyph resolution / ASCII fallback / GUI image path)

(ert-deftest emacs-toolbar-icon-glyph-resolves-unicode-when-forced ()
  (let ((emacs-toolbar-icon-force-mode 'unicode))
    (should (equal "✚" (emacs-toolbar-icon-glyph "new")))
    (should (equal "▣" (emacs-toolbar-icon-glyph "save")))
    (should (= 1 (string-width (emacs-toolbar-icon-glyph "search"))))))

(ert-deftest emacs-toolbar-icon-glyph-resolves-ascii-when-forced ()
  (let ((emacs-toolbar-icon-force-mode 'ascii))
    (should (equal "[N]" (emacs-toolbar-icon-glyph "new")))
    (should (equal "[S]" (emacs-toolbar-icon-glyph "save")))
    (should (equal "[/]" (emacs-toolbar-icon-glyph "search")))))

(ert-deftest emacs-toolbar-icon-glyph-unknown-name-falls-back-silently ()
  (let ((emacs-toolbar-icon-force-mode 'unicode))
    (should (equal "" (emacs-toolbar-icon-glyph "not-a-real-icon"))))
  (let ((emacs-toolbar-icon-force-mode 'ascii))
    (should (equal "" (emacs-toolbar-icon-glyph "not-a-real-icon"))))
  (should (equal "" (emacs-toolbar-icon-glyph nil))))

(ert-deftest emacs-toolbar-icon-registry-glyphs-are-single-column ()
  (let ((emacs-toolbar-icon-force-mode 'unicode))
    (dolist (name '("new" "open" "diropen" "close" "save" "undo"
                     "cut" "copy" "paste" "search"))
      (should (= 1 (string-width (emacs-toolbar-icon-glyph name)))))))

(ert-deftest emacs-toolbar-icon-environment-unicode-detection ()
  (let ((process-environment (cons "LC_ALL=C" process-environment)))
    (should-not (emacs-toolbar-icon-environment-unicode-p)))
  (let ((process-environment (cons "LC_ALL=C.UTF-8" process-environment)))
    (should (emacs-toolbar-icon-environment-unicode-p)))
  (let ((process-environment (cons "LC_ALL=en_US.utf8" process-environment)))
    (should (emacs-toolbar-icon-environment-unicode-p))))

(ert-deftest emacs-toolbar-icon-file-resolves-vendored-asset ()
  (let* ((root (locate-dominating-file
                (or (locate-library "emacs-toolbar") default-directory)
                "vendor"))
         (dir (and root (expand-file-name "vendor/emacs-etc/images/" root))))
    (skip-unless (and dir (file-directory-p dir)))
    (dolist (name '("new" "open" "diropen" "close" "save" "undo"
                     "cut" "copy" "paste" "search"))
      (let ((path (emacs-toolbar-icon-file name dir)))
        (should (stringp path))
        (should (file-exists-p path))
        (should (string-match-p (concat (regexp-quote name) "\\.\\(xpm\\|pbm\\)\\'")
                                 path))))))

(ert-deftest emacs-toolbar-icon-file-nil-for-unknown-name ()
  (should-not (emacs-toolbar-icon-file "not-a-real-icon" "/nonexistent-dir-for-test/")))

(ert-deftest emacs-toolbar-icon-file-nil-when-directory-absent ()
  (should-not (emacs-toolbar-icon-file "save" "/nonexistent-dir-for-test/")))

(provide 'emacs-toolbar-test)

;;; emacs-toolbar-test.el ends here
