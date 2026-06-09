;;; emacs-org-outline-test.el --- ERT for emacs-org-outline  -*- lexical-binding: t; -*-

;;; Commentary:

;; M3.1 outline tests for `emacs-org-outline.el'.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-fileio)
(require 'emacs-org-outline)

(defvar emacs-org-outline-test--tmp-counter 0)

(defun emacs-org-outline-test--tmp-path (suffix)
  "Return a unique temporary path ending with SUFFIX."
  (setq emacs-org-outline-test--tmp-counter
        (1+ emacs-org-outline-test--tmp-counter))
  (format "/tmp/emacs-org-outline-test-%d-%d-%s"
          (emacs-pid)
          emacs-org-outline-test--tmp-counter
          suffix))

(defmacro emacs-org-outline-test--with-fresh-world (&rest body)
  "Run BODY with clean mode, file, and buffer state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-fileio--buffer-files nil)
         (emacs-fileio--buffer-default-directories nil)
         (emacs-fileio--buffer-major-modes nil)
         (emacs-fileio--buffer-mode-names nil)
         (auto-mode-alist nil)
         (default-directory "/tmp/")
         (major-mode 'fundamental-mode)
         (mode-name "Fundamental")
         (buffer-invisibility-spec nil))
     (unwind-protect
         (progn
           (when (fboundp 'emacs-mode-reset)
             (emacs-mode-reset))
           (org-outline--install-auto-mode)
           ,@body)
       (when (fboundp 'emacs-mode-reset)
         (emacs-mode-reset)))))

(defmacro emacs-org-outline-test--with-org-buffer (content &rest body)
  "Create a fresh Org buffer seeded with CONTENT, then run BODY."
  (declare (indent 1) (debug (form body)))
  `(emacs-org-outline-test--with-fresh-world
     (let ((buf (generate-new-buffer "*org-outline-test*")))
       (unwind-protect
           (with-current-buffer buf
             (insert ,content)
             (goto-char (point-min))
             (org-mode)
             ,@body)
         (when (buffer-live-p buf)
           (kill-buffer buf))))))

(defmacro emacs-org-outline-test--with-temp-file (var suffix content &rest body)
  "Bind VAR to a temp file ending in SUFFIX seeded with CONTENT."
  (declare (indent 3) (debug (symbolp form form body)))
  `(let ((,var (emacs-org-outline-test--tmp-path ,suffix)))
     (unwind-protect
         (progn
           (with-temp-file ,var
             (insert ,content))
           ,@body)
       (when (file-exists-p ,var)
         (delete-file ,var)))))

(defun emacs-org-outline-test--line-start (needle)
  "Return the line-start position of the first line containing NEEDLE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle nil t)
    (line-beginning-position)))

(defun emacs-org-outline-test--invisible-at-line (needle)
  "Return non-nil when the line containing NEEDLE is hidden."
  (org-outline--invisible-p (emacs-org-outline-test--line-start needle)))

(ert-deftest org-list-bootstrap-callables-are-present ()
  (should (featurep 'org-list))
  (should (stringp (org-item-re)))
  (should (string-prefix-p "^" (org-item-beginning-re)))
  (should (fboundp 'org-list-get-item-begin))
  (should (fboundp 'org-list-get-first-item))
  (should (fboundp 'org-list-checkbox-radio-mode))
  (should (fboundp 'org-list-to-lisp))
  (should (fboundp 'org-toggle-checkbox)))

(ert-deftest org-footnote-bootstrap-vars-are-present ()
  (should (featurep 'org-footnote))
  (should (stringp org-footnote-re))
  (should (stringp org-footnote-definition-re))
  (should (equal org-footnote-forbidden-blocks
                 '("comment" "example" "export" "src"))))

(ert-deftest org-entities-bootstrap-callables-are-present ()
  (should (featurep 'org-entities))
  (should (org-entities--user-safe-p nil))
  (should (org-entities--user-safe-p
           '(("demo" "\\demo{}" nil "&demo;" "demo" "demo" "demo"))))
  (should-not (org-entities--user-safe-p '(("bad name" "" nil "" "" "" ""))))
  (should (null (org-entity-get "missing")))
  (should (fboundp 'org-entities-create-table))
  (should (fboundp 'org-entities-help)))

(ert-deftest org-element-ast-bootstrap-properties-work ()
  (let ((node (list 'demo '(:key "old"))))
    (should (equal (org-element-property :key node) "old"))
    (should (eq (org-element-put-property node :key "new") node))
    (should (equal (org-element-property :key node) "new"))
    (should (equal (org-element-put-property-2 :extra "v" node) node))
    (should (equal (org-element-property-raw :extra node) "v"))
    (should (null (org-element-parent node)))))

(ert-deftest org-macro-bootstrap-callables-are-present ()
  (should (featurep 'org-macro))
  (should (boundp 'org-macro-templates))
  (should (boundp 'org-macro--counter-table))
  (should (equal (org-macro--makeargs "$2 $1") '(&optional $1 $2 &rest _)))
  (should (equal (org-macro--set-templates
                  '(("name" . "old") ("name" . "new") ("empty")))
                 '(("name" . "new") ("empty" . ""))))
  (org-macro--counter-initialize)
  (should (= 1 (org-macro--counter-increment " item ")))
  (should (= 1 (org-macro--counter-increment "item" "-")))
  (should (= 5 (org-macro--counter-increment "item" "5")))
  (should (equal (org-macro-extract-arguments
                  (org-macro-escape-arguments "a" "b"))
                 '("a" "b")))
  (should (equal (org-macro-expand '(:key "demo" :args ("x" "y"))
                                   '(("demo" . "$2/$1")))
                 "y/x"))
  (org-macro-initialize-templates '(("demo" . "$1")))
  (should (assoc "demo" org-macro-templates)))

(ert-deftest ob-eval-bootstrap-callables-are-present ()
  (should (featurep 'ob-eval))
  (should (boundp 'org-babel-error-buffer-name))
  (dolist (symbol '(org-babel-eval-error-notify
                    org-babel-eval
                    org-babel-eval-read-file
                    org-babel--shell-command-on-region
                    org-babel--write-temp-buffer-input-file
                    org-babel-eval-wipe-error-buffer
                    org-babel--get-shell-file-name))
    (should (fboundp symbol)))
  (should (stringp (org-babel--get-shell-file-name)))
  (emacs-org-outline-test--with-temp-file path "babel.txt" "payload"
    (should (equal (org-babel-eval-read-file path) "payload"))))

(ert-deftest org-faces-bootstrap-tag-face-callable-is-present ()
  (should (boundp 'org-level-faces))
  (should (boundp 'org-todo-keyword-faces))
  (should (boundp 'org-tag-faces))
  (should (boundp 'org-tags-special-faces-re))
  (should (fboundp 'org-set-tag-faces))
  (let ((old-tag-faces org-tag-faces)
        (old-tags-special-faces-re org-tags-special-faces-re))
    (unwind-protect
        (progn
          (org-set-tag-faces 'org-tag-faces
                             '(("work" . bold) ("home" . italic)))
          (should (equal org-tag-faces
                         '(("work" . bold) ("home" . italic))))
          (should (string-match-p org-tags-special-faces-re ":work:"))
          (org-set-tag-faces 'org-tag-faces nil)
          (should (null org-tag-faces))
          (should (null org-tags-special-faces-re)))
      (setq org-tag-faces old-tag-faces)
      (setq org-tags-special-faces-re old-tags-special-faces-re))))

(ert-deftest org-mode-dispatches-on-org-extension ()
  (emacs-org-outline-test--with-fresh-world
    (should (equal 'org-mode
                   (cdr (assoc "\\.org\\'" auto-mode-alist))))
    (emacs-org-outline-test--with-temp-file path "notes.org" "* H\n"
      (let ((buf (find-file path)))
        (should (eq 'org-mode major-mode))
        (should (eq buf (current-buffer)))))))

(ert-deftest org-cycle-folds-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild body\n* Sibling\nsibling body\n"
    (goto-char (point-min))
    (org-cycle)
    (should (emacs-org-outline-test--invisible-at-line "body"))
    (should (emacs-org-outline-test--invisible-at-line "** Child"))
    (should-not (emacs-org-outline-test--invisible-at-line "* Sibling"))))

(ert-deftest org-cycle-unfolds-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild body\n*** Grandchild\ng body\n* Sibling\n"
    (goto-char (point-min))
    (org-cycle)
    (org-cycle)
    (should (emacs-org-outline-test--invisible-at-line "body"))
    (should-not (emacs-org-outline-test--invisible-at-line "** Child"))
    (should (emacs-org-outline-test--invisible-at-line "child body"))
    (org-cycle)
    (should-not (emacs-org-outline-test--invisible-at-line "body"))
    (should-not (emacs-org-outline-test--invisible-at-line "** Child"))
    (should-not (emacs-org-outline-test--invisible-at-line "child body"))
    (should-not (emacs-org-outline-test--invisible-at-line "*** Grandchild"))))

(ert-deftest org-shifttab-cycles-global-visibility ()
  (emacs-org-outline-test--with-org-buffer
      "* Top\nbody\n** Child\nchild body\n* Next\nnext body\n"
    (org-shifttab)
    (should-not (emacs-org-outline-test--invisible-at-line "* Top"))
    (should (emacs-org-outline-test--invisible-at-line "** Child"))
    (should (emacs-org-outline-test--invisible-at-line "body"))
    (org-shifttab)
    (should-not (emacs-org-outline-test--invisible-at-line "** Child"))
    (should (emacs-org-outline-test--invisible-at-line "body"))
    (should (emacs-org-outline-test--invisible-at-line "child body"))
    (org-shifttab)
    (should-not (emacs-org-outline-test--invisible-at-line "body"))
    (should-not (emacs-org-outline-test--invisible-at-line "** Child"))
    (should-not (emacs-org-outline-test--invisible-at-line "child body"))))

(ert-deftest org-insert-heading-at-same-level ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n* Sibling\n"
    (goto-char (point-min))
    (org-insert-heading)
    (should (equal "* Parent\n* \nbody\n* Sibling\n"
                   (buffer-string)))))

(ert-deftest org-promote-decreases-level ()
  (emacs-org-outline-test--with-org-buffer
      "** Child\nbody\n"
    (goto-char (point-min))
    (org-promote)
    (should (equal "* Child\nbody\n" (buffer-string)))))

(ert-deftest org-demote-increases-level ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n"
    (goto-char (point-min))
    (org-demote)
    (should (equal "** Parent\nbody\n" (buffer-string)))))

(ert-deftest org-promote-on-toplevel-errors ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n"
    (goto-char (point-min))
    (should-error (org-promote) :type 'user-error)))

(provide 'emacs-org-outline-test)

;;; emacs-org-outline-test.el ends here
