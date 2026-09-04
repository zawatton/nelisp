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
         (org-stored-links nil)
         (org-clock-marker nil)
         (org-clock-start-time nil)
         (org-clock-start-line-marker nil)
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

(defun emacs-org-outline-test--line-containing (needle)
  "Return the full line containing NEEDLE."
  (save-excursion
    (goto-char (point-min))
    (search-forward needle nil t)
    (buffer-substring-no-properties
     (line-beginning-position)
     (line-end-position))))

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

(ert-deftest org-element-parse-buffer-headline-substrate ()
  (unless (featurep 'org-element)
    (ert-skip "vendored org-element is not loaded; Org parser work is postponed"))
  (emacs-org-outline-test--with-org-buffer
      "* TODO Project :work:home:\nSCHEDULED: <2026-07-01 Wed> DEADLINE: <2026-07-03 Fri>\nBody\n** NEXT Child :review:\n"
    (let* ((tree (org-element-parse-buffer))
           (headlines (org-element-map tree 'headline #'identity))
           (root (car headlines)))
      (should (eq 'org-data (org-element-type tree)))
      (should (= 2 (length headlines)))
      (should (= 1 (org-element-property :level root)))
      (should (org-element-property :raw-value root)))))

(ert-deftest org-element-map-first-match-headline ()
  (unless (featurep 'org-element)
    (ert-skip "vendored org-element is not loaded; Org parser work is postponed"))
  (emacs-org-outline-test--with-org-buffer
      "* TODO A :a:\n* NEXT B :b:\n"
    (let ((tree (org-element-parse-buffer)))
      (should (equal "B"
                     (org-element-map
                         tree 'headline
                       (lambda (headline)
                         (and (equal "NEXT"
                                     (org-element-property
                                      :todo-keyword headline))
                              (org-element-property :raw-value headline)))
                       nil t))))))

(ert-deftest org-element-parse-buffer-body-substrate ()
  (unless (featurep 'org-element)
    (ert-skip "vendored org-element is not loaded; Org parser work is postponed"))
  (emacs-org-outline-test--with-org-buffer
      "#+title: Inbox\nTop paragraph <2026-07-04 Sat>\n\n* TODO Project :work:\nSCHEDULED: <2026-07-01 Wed>\n:PROPERTIES:\n:Effort: 1:30\n:Review: [2026-06-30 Tue]\n:END:\nBody line one\nBody line two <2026-07-02 Thu 09:15>\n** NEXT Child\nChild body\n"
    (let* ((tree (org-element-parse-buffer))
           (headlines (org-element-map tree 'headline #'identity))
           (sections (org-element-map tree 'section #'identity))
           (paragraphs (org-element-map tree 'paragraph #'identity))
           (drawers (org-element-map tree 'property-drawer #'identity))
           (properties (org-element-map tree 'node-property #'identity))
           (timestamps (org-element-map tree 'timestamp #'identity)))
      (should (= 2 (length headlines)))
      (should (>= (length sections) 2))
      (should (>= (length paragraphs) 3))
      (should (>= (length drawers) 1))
      (should (>= (length properties) 2))
      (should (>= (length timestamps) 1)))))

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
                    org-babel-execute-src-block
                    org-babel--get-shell-file-name))
    (should (fboundp symbol)))
  (should (stringp (org-babel--get-shell-file-name)))
  (emacs-org-outline-test--with-temp-file path "babel.txt" "payload"
    (should (equal (org-babel-eval-read-file path) "payload"))))

(ert-deftest org-babel-execute-src-block-runs-emacs-lisp-and-inserts-result ()
  (emacs-org-outline-test--with-org-buffer
      "* Demo\n#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n"
    (goto-char (point-min))
    (search-forward "(+ 1 2)")
    (should (equal 3 (org-babel-execute-src-block)))
    (should (equal "* Demo\n#+begin_src emacs-lisp\n(+ 1 2)\n#+end_src\n#+RESULTS:\n: 3\n"
                   (buffer-string)))))

(ert-deftest org-babel-execute-src-block-runs-last-emacs-lisp-form ()
  (emacs-org-outline-test--with-org-buffer
      "#+begin_src elisp\n(setq org-babel-test-value 4)\n(* org-babel-test-value 5)\n#+end_src\n"
    (goto-char (point-min))
    (should (equal 20 (org-babel-execute-src-block)))))

(ert-deftest org-babel-execute-src-block-errors-outside-src-block ()
  (emacs-org-outline-test--with-org-buffer
      "* Demo\nbody\n"
    (goto-char (point-min))
    (should-error (org-babel-execute-src-block) :type 'user-error)))

(ert-deftest org-require-loads-lightweight-entry ()
  (require 'org)
  (should (featurep 'org))
  (dolist (feature '(emacs-org-outline
                     emacs-org-todo
                     emacs-org-table))
    (should (featurep feature)))
  (dolist (symbol '(org-mode
                    org-cycle
                    org-at-heading-p
                    org-back-to-heading
                    org-get-heading
                    org-next-visible-heading
                    outline-next-visible-heading
                    org-previous-visible-heading
                    org-forward-heading-same-level
                    org-backward-heading-same-level
                    outline-up-heading
                    org-move-subtree-up
                    org-move-subtree-down
                    org-metaup
                    org-metadown
                    org-promote-subtree
                    org-demote-subtree
                    org-narrow-to-subtree
                    org-entry-get
                    org-entry-put
                    org-entry-delete
                    org-set-property
                    org-insert-link
                    org-open-at-point
                    org-get-tags
                    org-set-tags-command
                    org-export-dispatch
                    org-columns
                    org-todo
                    org-priority
                    org-table-align))
    (should (fboundp symbol))))

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

(ert-deftest org-narrow-to-subtree-narrows-parent-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild body\n* Sibling\nsibling body\n"
    (goto-char (point-min))
    (org-narrow-to-subtree)
    (should (equal "* Parent\nbody\n** Child\nchild body\n"
                   (buffer-string)))
    (widen)
    (should (equal "* Parent\nbody\n** Child\nchild body\n* Sibling\nsibling body\n"
                   (buffer-string)))))

(ert-deftest org-narrow-to-subtree-from-body-narrows-child-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild body\n* Sibling\nsibling body\n"
    (goto-char (emacs-org-outline-test--line-start "child body"))
    (org-narrow-to-subtree)
    (should (equal "** Child\nchild body\n"
                   (buffer-string)))
    (widen)
    (should (equal "* Parent\nbody\n** Child\nchild body\n* Sibling\nsibling body\n"
                   (buffer-string)))))

(ert-deftest org-narrow-to-subtree-before-first-heading-errors ()
  (emacs-org-outline-test--with-org-buffer
      "intro\n* Parent\nbody\n"
    (goto-char (point-min))
    (let ((origin (point))
          (before (buffer-string)))
      (should-error (org-narrow-to-subtree) :type 'user-error)
      (should (= (point) origin))
      (should (equal before (buffer-string))))))

(ert-deftest org-narrow-to-subtree-keymap-binding-is-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-x n s"))
              #'org-narrow-to-subtree)))

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

(ert-deftest org-subtree-demote-relevels-whole-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild\n*** Grandchild\ngrand\n* Sibling\n"
    (goto-char (point-min))
    (org-demote-subtree)
    (should (equal "** Parent\nbody\n*** Child\nchild\n**** Grandchild\ngrand\n* Sibling\n"
                   (buffer-string)))
    (should (looking-at "Parent"))))

(ert-deftest org-subtree-promote-relevels-whole-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "** Parent\nbody\n*** Child\nchild\n**** Grandchild\ngrand\n* Sibling\n"
    (goto-char (point-min))
    (org-promote-subtree)
    (should (equal "* Parent\nbody\n** Child\nchild\n*** Grandchild\ngrand\n* Sibling\n"
                   (buffer-string)))
    (should (looking-at "Parent"))))

(ert-deftest org-subtree-promote-on-toplevel-errors-without-editing ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild\n"
    (goto-char (point-min))
    (let ((origin (point))
          (before (buffer-string)))
      (should-error (org-promote-subtree) :type 'user-error)
      (should (= (point) origin))
      (should (equal before (buffer-string))))))

(ert-deftest org-subtree-promote-demote-keymap-bindings-are-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c <"))
              #'org-promote-subtree))
  (should (eq (lookup-key org-mode-map (kbd "C-c >"))
              #'org-demote-subtree)))

(ert-deftest org-property-drawer-put-get-and-update-work ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n"
    (goto-char (point-min))
    (should (equal (org-entry-put nil "id" "abc-1") "abc-1"))
    (should (equal (org-entry-get nil "ID") "abc-1"))
    (should (equal "* Parent\n:PROPERTIES:\n:ID: abc-1\n:END:\nbody\n"
                   (buffer-string)))
    (org-entry-put nil ":ID:" "abc-2")
    (should (equal (org-entry-get nil "id") "abc-2"))
    (should (equal "* Parent\n:PROPERTIES:\n:ID: abc-2\n:END:\nbody\n"
                   (buffer-string)))))

(ert-deftest org-property-drawer-delete-removes-empty-drawer ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n:PROPERTIES:\n:ID: abc\n:END:\nbody\n"
    (goto-char (point-min))
    (org-entry-delete nil "ID")
    (should-not (org-entry-get nil "ID"))
    (should (equal "* Parent\nbody\n" (buffer-string)))))

(ert-deftest org-property-drawer-current-entry-and-child-are-isolated ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\nchild body\n"
    (goto-char (emacs-org-outline-test--line-start "body"))
    (org-entry-put nil "ID" "parent")
    (goto-char (emacs-org-outline-test--line-start "child body"))
    (org-entry-put nil "ID" "child")
    (should (equal (org-entry-get nil "ID") "child"))
    (goto-char (point-min))
    (should (equal (org-entry-get nil "ID") "parent"))
    (should (equal "* Parent\n:PROPERTIES:\n:ID: parent\n:END:\nbody\n** Child\n:PROPERTIES:\n:ID: child\n:END:\nchild body\n"
                   (buffer-string)))))

(ert-deftest org-property-drawer-pom-and-literal-nil-work ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n:PROPERTIES:\n:STATE: nil\n:END:\nbody\n"
    (let ((pos (point-min)))
      (goto-char (point-max))
      (should-not (org-entry-get pos "STATE"))
      (should (equal (org-entry-get pos "STATE" nil t) "nil"))
      (org-entry-put (copy-marker pos) "STATE" "done")
      (should (equal (org-entry-get pos "STATE") "done")))))

(ert-deftest org-property-drawer-keymap-binding-is-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c C-x p"))
              #'org-set-property)))

(ert-deftest org-set-effort-writes-effort-property ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Parent\nbody\n"
    (goto-char (emacs-org-outline-test--line-start "body"))
    (should (equal "1:30" (org-set-effort "1:30")))
    (should (equal "1:30" (org-entry-get nil "EFFORT")))
    (should (equal "* TODO Parent\n:PROPERTIES:\n:EFFORT: 1:30\n:END:\nbody\n"
                   (buffer-string)))))

(ert-deftest org-set-effort-updates-existing-effort-property ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Parent\n:PROPERTIES:\n:EFFORT: 0:30\n:END:\nbody\n"
    (goto-char (point-min))
    (should (equal "2:00" (org-set-effort "2:00")))
    (should (equal "* TODO Parent\n:PROPERTIES:\n:EFFORT: 2:00\n:END:\nbody\n"
                   (buffer-string)))))

(ert-deftest org-link-insert-plain-and-described-links ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n"
    (goto-char (point-max))
    (should (equal (org-insert-link "file:notes.org") "[[file:notes.org]]"))
    (insert "\n")
    (should (equal (org-insert-link "https://example.invalid" "site")
                   "[[https://example.invalid][site]]"))
    (should (equal "* Parent\n[[file:notes.org]]\n[[https://example.invalid][site]]"
                   (buffer-string)))))

(ert-deftest org-store-link-stores-file-line-with-heading-description ()
  (emacs-org-outline-test--with-fresh-world
    (emacs-org-outline-test--with-temp-file path "store.org" "* Parent\nbody\n"
      (let ((buf (find-file path)))
        (with-current-buffer buf
          (goto-char (point-min))
          (should (equal (format "file:%s::1" (expand-file-name path))
                         (org-store-link nil nil)))
          (should-not org-stored-links)
          (should (equal (format "file:%s::1" (expand-file-name path))
                         (org-store-link nil t)))
          (should (equal (list (list (format "file:%s::1"
                                             (expand-file-name path))
                                     "Parent"))
                         org-stored-links)))))))

(ert-deftest org-store-link-uses-existing-link-at-point ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nSee [[https://example.invalid][site]] now\n"
    (goto-char (emacs-org-outline-test--line-start "See"))
    (search-forward "site")
    (should (equal "https://example.invalid"
                   (org-store-link nil t)))
    (should (equal '(("https://example.invalid" "site"))
                   org-stored-links))))

(ert-deftest org-sort-sorts-direct-child-subtrees-by-heading ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n** Zebra\nz\n*** Nested\nn\n** Alpha\na\n** Bravo\nb\n* Other\n"
    (goto-char (point-min))
    (org-sort nil ?a)
    (should (equal "* Parent\n** Alpha\na\n** Bravo\nb\n** Zebra\nz\n*** Nested\nn\n* Other\n"
                   (buffer-string)))
    (should (looking-at-p "\\* Parent"))))

(ert-deftest org-sort-descending-preserves-child-subtrees ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n** 10\nbody\n** 2\n*** Two child\nnested\n** 30\nbody\n"
    (goto-char (point-min))
    (org-sort nil ?N)
    (should (equal "* Parent\n** 30\nbody\n** 10\nbody\n** 2\n*** Two child\nnested\n"
                   (buffer-string)))))

(ert-deftest org-sort-keymap-binding-is-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c ^"))
              #'org-sort)))

(ert-deftest org-link-open-url-delegates-to-browse-url ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nSee [[https://example.invalid][site]] now\n"
    (let (opened)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url &rest _args)
                   (setq opened url)
                   :opened)))
        (goto-char (emacs-org-outline-test--line-start "See"))
        (search-forward "site")
        (should (eq (org-open-at-point) :opened))
        (should (equal opened "https://example.invalid"))))))

(ert-deftest org-link-open-file-delegates-to-find-file ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nOpen [[file:notes.org][notes]]\n"
    (let (opened)
      (cl-letf (((symbol-function 'find-file)
                 (lambda (file &rest _args)
                   (setq opened file)
                   :file-opened)))
        (goto-char (emacs-org-outline-test--line-start "Open"))
        (search-forward "notes")
        (should (eq (org-open-at-point) :file-opened))
        (should (equal opened "notes.org"))))))

(ert-deftest org-link-open-id-returns-plan-without-id-opener ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nLink [[id:abc-123][target]]\n"
    (cl-letf (((symbol-function 'org-id-open) nil)
              ((symbol-function 'org-roam-id-open) nil))
      (goto-char (emacs-org-outline-test--line-start "Link"))
      (search-forward "target")
      (should (equal (org-open-at-point)
                     '(:type id :target "abc-123"))))))

(ert-deftest org-link-open-errors-when-no-link-at-point ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nNo link here\n"
    (goto-char (emacs-org-outline-test--line-start "No link"))
    (should-error (org-open-at-point) :type 'user-error)))

(ert-deftest org-link-keymap-bindings-are-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c C-o"))
              #'org-open-at-point))
  (should (eq (lookup-key org-mode-map (kbd "C-c C-l"))
              #'org-insert-link)))

(ert-deftest org-tags-get-and-set-heading-tags ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent :work:home:\nbody\n"
    (goto-char (point-min))
    (should (equal (org-get-tags) '("work" "home")))
    (should (equal (org-set-tags-command '("next" "review"))
                   '("next" "review")))
    (should (equal (org-get-tags) '("next" "review")))
    (should (equal "* Parent :next:review:\nbody\n"
                   (buffer-string)))))

(ert-deftest org-tags-set-from-string-and-delete-tags ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent :old:\nbody\n"
    (goto-char (point-min))
    (org-set-tags-command "alpha beta,gamma")
    (should (equal (org-get-tags) '("alpha" "beta" "gamma")))
    (should (equal "* Parent :alpha:beta:gamma:\nbody\n"
                   (buffer-string)))
    (org-set-tags-command "")
    (should-not (org-get-tags))
    (should (equal "* Parent\nbody\n" (buffer-string)))))

(ert-deftest org-tags-preserve-todo-keyword-and-heading-text ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Parent heading :old:\nbody\n"
    (goto-char (point-min))
    (org-set-tags-command '("work"))
    (should (equal "* TODO Parent heading :work:\nbody\n"
                   (buffer-string)))))

(ert-deftest org-tags-keymap-binding-is-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c C-q"))
              #'org-set-tags-command)))

(ert-deftest org-toggle-archive-tag-adds-and-removes-archive-tag ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Parent :work:\nbody\n"
    (goto-char (point-min))
    (should (equal '("work" "ARCHIVE")
                   (org-toggle-archive-tag)))
    (should (equal "* TODO Parent :work:ARCHIVE:\nbody\n"
                   (buffer-string)))
    (should (equal '("work")
                   (org-toggle-archive-tag)))
    (should (equal "* TODO Parent :work:\nbody\n"
                   (buffer-string)))))

(ert-deftest org-toggle-archive-tag-from-body-toggles-owning-heading ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child :ARCHIVE:\nchild body\n"
    (goto-char (emacs-org-outline-test--line-start "child body"))
    (should-not (org-toggle-archive-tag))
    (should (looking-at "\\*\\* Child"))
    (should (equal "* Parent\nbody\n** Child\nchild body\n"
                   (buffer-string)))))

(ert-deftest org-map-entries-collects-headings-and-preserves-point ()
  (emacs-org-outline-test--with-org-buffer
      "* NEXT Parent :work:\nbody\n** WAIT Child :work:next:\n* DONE Other :done:\n"
    (goto-char (emacs-org-outline-test--line-start "body"))
    (let ((origin (point)))
      (should (equal
               '("NEXT Parent" "WAIT Child" "DONE Other")
               (org-map-entries
                (lambda ()
                  (org-get-heading t nil t)))))
      (should (= (point) origin)))))

(ert-deftest org-map-entries-matches-tags-todo-and-tree-scope ()
  (emacs-org-outline-test--with-org-buffer
      "* NEXT Parent :work:\nbody\n** WAIT Child :work:next:\n* DONE Other :done:\n"
    (should (equal
             '("NEXT Parent" "WAIT Child")
             (org-map-entries
              (lambda ()
                (org-get-heading t nil t))
              "+work-done")))
    (should (equal
             '("WAIT Child")
             (org-map-entries
              (lambda ()
                (org-get-heading t nil t))
              "TODO=\"WAIT\"")))
    (goto-char (point-min))
    (should (equal
             '("NEXT Parent" "WAIT Child")
             (org-map-entries
              (lambda ()
                (org-get-heading t nil t))
              nil
              'tree)))))

(ert-deftest org-columns-renders-heading-tags-and-effort-buffer ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Parent :work:\n:PROPERTIES:\n:EFFORT: 1:30\n:END:\nbody\n** Child :next:\nchild\n"
    (let ((origin (point)))
      (unwind-protect
          (let ((columns-buffer (org-columns)))
            (should (buffer-live-p columns-buffer))
            (should (= (point) origin))
            (should (equal "Level\tItem\tTags\tEffort\n1\tTODO Parent\twork\t1:30\n2\tChild\tnext\t\n"
                           (with-current-buffer columns-buffer
                             (buffer-string)))))
        (when (get-buffer org-columns-buffer-name)
          (kill-buffer org-columns-buffer-name))))))

(ert-deftest org-columns-reuses-columns-buffer ()
  (emacs-org-outline-test--with-org-buffer
      "* First\n"
    (unwind-protect
        (let ((first (org-columns)))
          (erase-buffer)
          (insert "* Second :tag:\n")
          (goto-char (point-min))
          (should (eq first (org-columns)))
          (should (equal "Level\tItem\tTags\tEffort\n1\tSecond\ttag\t\n"
                         (with-current-buffer first
                           (buffer-string)))))
      (when (get-buffer org-columns-buffer-name)
        (kill-buffer org-columns-buffer-name)))))

(ert-deftest org-export-dispatch-renders-plain-text-buffer ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Parent :work:\nbody\n** Child\n#+RESULTS:\n: 3\n"
    (unwind-protect
        (let ((export-buffer (org-export-dispatch)))
          (should (buffer-live-p export-buffer))
          (should (equal "TODO Parent :work:\nbody\nChild\nResults:\n: 3\n"
                         (with-current-buffer export-buffer
                           (buffer-string)))))
      (when (get-buffer org-export-buffer-name)
        (kill-buffer org-export-buffer-name)))))

(ert-deftest org-export-dispatch-reuses-export-buffer ()
  (emacs-org-outline-test--with-org-buffer
      "* First\n"
    (unwind-protect
        (let ((first (org-export-dispatch)))
          (erase-buffer)
          (insert "* Second\n")
          (should (eq first (org-export-dispatch)))
          (should (equal "Second\n"
                         (with-current-buffer first
                           (buffer-string)))))
      (when (get-buffer org-export-buffer-name)
        (kill-buffer org-export-buffer-name)))))

(ert-deftest org-archive-subtree-moves-current-subtree-to-archive-heading ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Keep\nbody\n* DONE Old\nold body\n** Note\nnote\n"
    (goto-char (emacs-org-outline-test--line-start "Old"))
    (should (equal "Archived subtree: DONE Old" (org-archive-subtree)))
    (should (equal
             "* TODO Keep\nbody\n* Archive\n* DONE Old\nold body\n** Note\nnote\n"
             (buffer-string)))
    (should (looking-at "\\* DONE Old"))))

(ert-deftest org-archive-subtree-appends-to-existing-archive-heading ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Keep\n* Archive\n** DONE Earlier\n* DONE Later\n"
    (goto-char (emacs-org-outline-test--line-start "Later"))
    (org-archive-subtree)
    (should (equal
             "* TODO Keep\n* Archive\n** DONE Earlier\n* DONE Later\n"
             (buffer-string)))))

(ert-deftest org-archive-subtree-refuses-archive-heading ()
  (emacs-org-outline-test--with-org-buffer
      "* Archive\n** DONE Earlier\n"
    (goto-char (point-min))
    (let ((before (buffer-string)))
      (should-error (org-archive-subtree) :type 'user-error)
      (should (equal before (buffer-string))))))

(ert-deftest org-clock-in-out-records-clock-line-and-clears-state ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Timed\nbody\n"
    (let ((start (encode-time 0 15 8 27 6 2026))
          (end (encode-time 0 45 9 27 6 2026)))
      (should (equal "Clock starts at 08:15 - TODO Timed"
                     (org-clock-in nil start)))
      (should org-clock-marker)
      (should (string-match-p
               "^CLOCK: \\[2026-06-27 [A-Za-z]\\{3\\} 08:15\\]$"
               (emacs-org-outline-test--line-containing "CLOCK:")))
      (should (string-match-p
               "\\`Clocking: TODO Timed ([0-9]+:[0-9][0-9])\\'"
               (org-clock-get-clock-string)))
      (should (equal "Clock stopped after 1:30 - TODO Timed"
                     (org-clock-out nil nil end)))
      (should-not org-clock-marker)
      (should (equal ""
                     (org-clock-get-clock-string)))
      (should (string-match-p
               "^CLOCK: \\[2026-06-27 [A-Za-z]\\{3\\} 08:15\\]--\\[2026-06-27 [A-Za-z]\\{3\\} 09:45\\] => 1:30$"
               (emacs-org-outline-test--line-containing "CLOCK:"))))))

(ert-deftest org-clock-out-error-and-fail-quietly-work ()
  (emacs-org-outline-test--with-org-buffer
      "* TODO Timed\n"
    (should-error (org-clock-out) :type 'user-error)
    (should-not (org-clock-out nil t))))

(ert-deftest org-refile-moves-subtree-under-rfloc-target ()
  (emacs-org-outline-test--with-org-buffer
      "* Inbox\n** TODO Move\nbody\n* Project\n"
    (let ((target (emacs-org-outline-test--line-start "Project")))
      (goto-char (emacs-org-outline-test--line-start "Move"))
      (should (equal "Refiled subtree: TODO Move"
                     (org-refile nil nil (list "Project" nil nil target))))
      (should (equal
               "* Inbox\n* Project\n** TODO Move\nbody\n"
               (buffer-string)))
      (should (looking-at "\\*\\* TODO Move")))))

(ert-deftest org-refile-finds-target-by-heading-name ()
  (emacs-org-outline-test--with-org-buffer
      "* Inbox\n** WAIT Move\n* Project\n"
    (goto-char (emacs-org-outline-test--line-start "Move"))
    (org-refile nil nil "Project")
    (should (equal
             "* Inbox\n* Project\n** WAIT Move\n"
             (buffer-string)))))

(ert-deftest org-refile-refuses-target-inside-source-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n** Child\n* Other\n"
    (goto-char (point-min))
    (let ((before (buffer-string))
          (target (emacs-org-outline-test--line-start "Child")))
      (should-error
       (org-refile nil nil (list "Child" nil nil target))
       :type 'user-error)
      (should (equal before (buffer-string))))))

(ert-deftest org-refile-keymap-binding-is-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c C-w"))
              #'org-refile)))

(ert-deftest org-heading-navigation-moves-between-visible-headings ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Hidden child\nchild body\n* Sibling\n"
    (goto-char (point-min))
    (org-cycle)
    (org-next-visible-heading)
    (should (looking-at "\\* Sibling"))
    (org-previous-visible-heading)
    (should (looking-at "\\* Parent"))))

(ert-deftest org-heading-query-compatibility-functions-work ()
  (require 'emacs-org-todo)
  (emacs-org-outline-test--with-org-buffer
      "* INBOX [#A] Parent heading :work:\nbody\n** Child\n"
    (goto-char (emacs-org-outline-test--line-start "body"))
    (should-not (org-at-heading-p))
    (should (= (org-back-to-heading) (point-min)))
    (should (org-at-heading-p))
    (should (equal (org-get-heading) "INBOX [#A] Parent heading :work:"))
    (should (equal (org-get-heading t) "INBOX [#A] Parent heading"))
    (should (equal (org-get-heading t t) "[#A] Parent heading"))
    (should (equal (org-get-heading t t t) "Parent heading"))))

(ert-deftest org-heading-navigation-outline-alias-works ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n** Child\n* Sibling\n"
    (goto-char (point-min))
    (outline-next-visible-heading 2)
    (should (looking-at "\\* Sibling"))))

(ert-deftest org-heading-navigation-same-level-and-parent-work ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\n** Child A\nbody\n** Child B\n*** Grandchild\nbody\n* Sibling\n"
    (goto-char (emacs-org-outline-test--line-start "** Child A"))
    (org-forward-heading-same-level)
    (should (looking-at "\\*\\* Child B"))
    (org-backward-heading-same-level)
    (should (looking-at "\\*\\* Child A"))
    (goto-char (emacs-org-outline-test--line-start "body"))
    (outline-up-heading)
    (should (looking-at "\\* Parent"))))

(ert-deftest org-heading-navigation-restores-point-on-failure ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n"
    (goto-char (point-min))
    (let ((origin (point)))
      (should-error (org-previous-visible-heading) :type 'user-error)
      (should (= (point) origin)))
    (should-error (org-forward-heading-same-level) :type 'user-error)
    (should (looking-at "\\* Parent"))))

(ert-deftest org-heading-navigation-keymap-bindings-are-present ()
  (should (eq (lookup-key org-mode-map (kbd "C-c C-n"))
              #'org-next-visible-heading))
  (should (eq (lookup-key org-mode-map (kbd "C-c C-p"))
              #'org-previous-visible-heading))
  (should (eq (lookup-key org-mode-map (kbd "C-c C-f"))
              #'org-forward-heading-same-level))
  (should (eq (lookup-key org-mode-map (kbd "C-c C-b"))
              #'org-backward-heading-same-level))
  (should (eq (lookup-key org-mode-map (kbd "C-c C-u"))
              #'outline-up-heading)))

(ert-deftest org-subtree-move-down-reorders-whole-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* A\na\n** A child\nac\n* B\nb\n* C\nc\n"
    (goto-char (point-min))
    (org-move-subtree-down)
    (should (equal "* B\nb\n* A\na\n** A child\nac\n* C\nc\n"
                   (buffer-string)))
    (should (looking-at "A"))))

(ert-deftest org-subtree-move-up-reorders-whole-subtree ()
  (emacs-org-outline-test--with-org-buffer
      "* A\na\n* B\nb\n** B child\nbc\n* C\nc\n"
    (goto-char (emacs-org-outline-test--line-start "* B"))
    (org-move-subtree-up)
    (should (equal "* B\nb\n** B child\nbc\n* A\na\n* C\nc\n"
                   (buffer-string)))
    (should (looking-at "B"))))

(ert-deftest org-subtree-move-does-not-cross-parent ()
  (emacs-org-outline-test--with-org-buffer
      "* P1\n** A\n* P2\n** B\n"
    (goto-char (emacs-org-outline-test--line-start "** B"))
    (let ((origin (point)))
      (should-error (org-move-subtree-up) :type 'user-error)
      (should (= (point) origin)))
    (should (equal "* P1\n** A\n* P2\n** B\n"
                   (buffer-string)))))

(ert-deftest org-subtree-move-wrappers-and-keymap-bindings-work ()
  (emacs-org-outline-test--with-org-buffer
      "* A\n* B\n* C\n"
    (should (eq (lookup-key org-mode-map (kbd "M-<up>"))
                #'org-move-subtree-up))
    (should (eq (lookup-key org-mode-map (kbd "M-<down>"))
                #'org-move-subtree-down))
    (goto-char (emacs-org-outline-test--line-start "* B"))
    (org-metadown)
    (should (equal "* A\n* C\n* B\n" (buffer-string)))
    (org-metaup)
    (should (equal "* A\n* B\n* C\n" (buffer-string)))))

(ert-deftest org-promote-on-toplevel-errors ()
  (emacs-org-outline-test--with-org-buffer
      "* Parent\nbody\n"
    (goto-char (point-min))
    (should-error (org-promote) :type 'user-error)))

(provide 'emacs-org-outline-test)

;;; emacs-org-outline-test.el ends here
