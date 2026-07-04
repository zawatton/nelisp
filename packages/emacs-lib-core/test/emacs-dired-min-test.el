;;; emacs-dired-min-test.el --- ERT for emacs-dired-min -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-dired-min)
(require 'emacs-dired-min-gui)

(defmacro emacs-dired-min-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp + dired substrate state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-dired-min--state (make-hash-table :test 'eq :weakness nil))
         (emacs-dired-min-gui-backend nil)
         (emacs-dired-min-gui-directory "")
         (emacs-dired-min-gui-target "")
         (emacs-dired-min-gui-current-file-name "")
         (emacs-dired-min-gui-status "ok")
         (emacs-dired-min-gui-buffer-name "")
         (emacs-mode--current-major-mode 'fundamental-mode)
         (emacs-mode--current-mode-name "Fundamental")
         (emacs-keymap-local-map nil))
     (emacs-mode-reset)
     ,@body))

(defun emacs-dired-min-test--write-file (path content)
  "Write CONTENT to PATH."
  (with-temp-file path
    (insert content)))

(defun emacs-dired-min-test--make-tree ()
  "Create and return a temp directory tree for dired tests."
  (let* ((root (make-temp-file "emacs-dired-min-" t))
         (file-a (expand-file-name "alpha.txt" root))
         (file-b (expand-file-name "beta.txt" root))
         (subdir (expand-file-name "subdir" root))
         (nested (expand-file-name "nested.txt" subdir)))
    (make-directory subdir)
    (emacs-dired-min-test--write-file file-a "alpha")
    (emacs-dired-min-test--write-file file-b "beta")
    (emacs-dired-min-test--write-file nested "nested")
    (list :root root
          :file-a file-a
          :file-b file-b
          :subdir subdir
          :nested nested)))

(defun emacs-dired-min-test--cleanup-tree (tree)
  "Delete TREE created by `emacs-dired-min-test--make-tree'."
  (when tree
    (delete-directory (plist-get tree :root) t)))

(defun emacs-dired-min-test--buffer-string ()
  "Return the current nelisp buffer contents."
  (nelisp-ec-buffer-string))

(defun emacs-dired-min-test--buffer-lines ()
  "Return the current nelisp buffer as a list of lines."
  (split-string (emacs-dired-min-test--buffer-string) "\n" t))

(ert-deftest dired-mode-map-is-built-lazily-and-stable ()
  (let ((dired-mode-map nil))
    (should-not dired-mode-map)
    (let ((map (emacs-dired-min--ensure-mode-map)))
      (should (eq map dired-mode-map))
      (should (keymapp map))
      (should (eq #'dired-find-file (lookup-key map (kbd "RET"))))
      (should (eq #'emacs-dired-min-quit-window (lookup-key map (kbd "q")))))))

(defun emacs-dired-min-test--goto-entry (name)
  "Move point to the dired entry named NAME in the current buffer."
  (let* ((state (gethash (nelisp-ec-current-buffer) emacs-dired-min--state))
         (entries (plist-get state :entries))
         (line-starts (plist-get state :line-starts))
         (index (cl-position-if
                 (lambda (entry)
                   (equal name (plist-get entry :name)))
                 entries)))
    (should index)
    (nelisp-ec-goto-char (nth index line-starts))))

(ert-deftest dired-gui-directory-listing-renders-simple-text ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (let* ((root (plist-get tree :root))
                 (listing (emacs-dired-min-gui-directory-listing root))
                 (entries (plist-get listing :entries))
                 (text (plist-get listing :text)))
            (should (equal (expand-file-name root)
                           (plist-get listing :directory)))
            (should (= 5 (plist-get listing :count)))
            (should (equal '("." ".." "alpha.txt" "beta.txt" "subdir")
                           (mapcar (lambda (entry) (plist-get entry :name))
                                   entries)))
            (should (string-match-p
                     (concat "Directory: " (regexp-quote (expand-file-name root)))
                     text))
            (should (string-match-p "  -  alpha\\.txt" text))
            (should (string-match-p "  d  subdir" text)))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-gui-simple-listing-renders-bridge-text ()
  (should (equal '(:directory "/tmp/demo"
                   :entries ("alpha.txt" "sub")
                   :count 2
                   :text "Directory /tmp/demo\n  alpha.txt\n  sub\n")
                 (emacs-dired-min-gui-simple-listing
                  "/tmp/demo/"
                  '("." ".." "alpha.txt" "sub")))))

(ert-deftest dired-gui-render-directory-buffer-uses-callbacks ()
  (let (directory buffer-name emitted displayed)
    (should
     (equal
      "*Dired*"
      (emacs-dired-min-gui-render-directory-buffer
       ""
       :default-directory (lambda () "/tmp/demo")
       :directory-files (lambda (dir)
                          (setq directory dir)
                          '("." ".." "alpha.txt"))
       :emit-text (lambda (text)
                    (setq emitted text))
       :display-buffer (lambda (name text)
                         (setq displayed (list name text)))
       :set-directory (lambda (dir)
                        (setq directory dir))
       :set-buffer-name (lambda (name)
                          (setq buffer-name name))
       :buffer-name "*Dired*")))
    (should (equal "/tmp/demo" directory))
    (should (equal "*Dired*" buffer-name))
    (should (string-match-p "alpha.txt" emitted))
    (should (equal (list "*Dired*" emitted) displayed))))

(ert-deftest dired-lists-files-and-dirs ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (let ((lines (emacs-dired-min-test--buffer-lines)))
              (should (cl-find-if (lambda (line)
                                    (string-prefix-p "  .\t" line))
                                  lines))
              (should (cl-find-if (lambda (line)
                                    (string-prefix-p "  ..\t" line))
                                  lines))
              (should (member "  alpha.txt\t5\t-rw-rw-r--" lines))
              (should (member "  beta.txt\t4\t-rw-rw-r--" lines))
              (should (cl-find-if (lambda (line)
                                    (string-prefix-p "  subdir\t" line))
                                  lines))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-lines-carry-file-metadata-properties ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (let ((entry (emacs-buffer-get-text-property
                          (nelisp-ec-point) 'dired-file
                          (nelisp-ec-current-buffer))))
              (should (equal "alpha.txt" (plist-get entry :name)))
              (should (equal (plist-get tree :file-a)
                             (plist-get entry :path)))
              (should (plist-get entry :attributes)))
            (emacs-dired-min-test--goto-entry "subdir")
            (let ((entry (emacs-buffer-get-text-property
                          (nelisp-ec-point) 'dired-file
                          (nelisp-ec-current-buffer))))
              (should (equal "subdir" (plist-get entry :name)))
              (should (nelisp-ec-file-directory-p
                       (plist-get entry :path))))
            (let ((spans (emacs-buffer-text-property-view
                          1 (nelisp-ec-point-max) '(dired-file)
                          (nelisp-ec-current-buffer))))
              (should (= 5 (length spans)))
              (should (cl-every
                       (lambda (span)
                         (plist-get (plist-get (nth 2 span) 'dired-file)
                                    :path))
                       spans))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-host-interactive-mirror-renders-listing ()
  "Host -nw mirrors the minimal Dired listing into a visible host buffer."
  (emacs-dired-min-test--with-fresh-world
    (let* ((tree (emacs-dired-min-test--make-tree))
           (root (plist-get tree :root))
           (name (emacs-dired-min--dired-buffer-name
                  (emacs-dired-min--normalize-directory root)))
           (shown nil))
      (unwind-protect
          (let ((noninteractive nil))
            (cl-letf (((symbol-function 'selected-window)
                       (lambda () :selected-window))
                      ((symbol-function 'set-window-buffer)
                       (lambda (_window buffer &optional _keep-margins)
                         (setq shown buffer))))
              (dired root)
              (let ((host-buffer (get-buffer name)))
                (should host-buffer)
                (should (eq shown host-buffer))
                (with-current-buffer host-buffer
                  (should (eq major-mode 'dired-mode))
                  (should (equal mode-name "Dired"))
                  (should (save-excursion
                            (goto-char (point-min))
                            (search-forward "alpha.txt" nil t)))
                  (should (save-excursion
                            (goto-char (point-min))
                            (search-forward "subdir" nil t)))))))
        (when-let ((host-buffer (get-buffer name)))
          (kill-buffer host-buffer))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-RET-opens-file-at-point ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree))
          (opened nil))
      (unwind-protect
          (cl-letf (((symbol-function 'find-file)
                     (lambda (path)
                       (setq opened path)
                       :opened)))
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (should (eq :opened (dired-find-file)))
            (should (equal (plist-get tree :file-a) opened)))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-RET-on-subdir-enters-it ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "subdir")
            (let ((buffer (dired-find-file)))
              (should (eq buffer (nelisp-ec-current-buffer)))
              (should (equal (emacs-dired-min--normalize-directory
                              (plist-get tree :subdir))
                             (plist-get (gethash buffer emacs-dired-min--state)
                                        :directory)))
              (should (member "  nested.txt\t6\t-rw-rw-r--"
                              (emacs-dired-min-test--buffer-lines)))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-up-directory-goes-to-parent ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :subdir))
            (let ((buffer (dired-up-directory)))
              (should (eq buffer (nelisp-ec-current-buffer)))
              (should (equal (emacs-dired-min--normalize-directory
                              (plist-get tree :root))
                             (plist-get (gethash buffer emacs-dired-min--state)
                                        :directory)))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-revert-buffer-rescans ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree))
          (new-file nil))
      (unwind-protect
          (progn
            (setq new-file (expand-file-name "gamma.txt" (plist-get tree :root)))
            (dired (plist-get tree :root))
            (should-not (member "  gamma.txt\t6\t-rw-rw-r--"
                                (emacs-dired-min-test--buffer-lines)))
            (emacs-dired-min-test--write-file new-file "gamma!")
            (emacs-dired-min-revert-buffer)
            (should (member "  gamma.txt\t6\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-quit-window-restores-previous-buffer ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree))
          (previous (nelisp-ec-generate-new-buffer " *previous*")))
      (unwind-protect
          (progn
            (should (fboundp 'emacs-dired-min-quit-window))
            (nelisp-ec-set-buffer previous)
            (dired (plist-get tree :root))
            (let ((dired-buffer (nelisp-ec-current-buffer)))
              (should (gethash dired-buffer emacs-dired-min--state))
              (emacs-dired-min-quit-window)
              (should (eq previous (nelisp-ec-current-buffer)))
              (should-not (gethash dired-buffer emacs-dired-min--state))))
        (when (buffer-live-p previous)
          (kill-buffer previous))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-next-and-previous-line-move-point ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry ".")
            (let ((start (nelisp-ec-point)))
              (dired-next-line)
              (should (> (nelisp-ec-point) start))
              (let ((after-next (nelisp-ec-point)))
                (dired-previous-line)
                (should (= start (nelisp-ec-point)))
                (should (> after-next (nelisp-ec-point))))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-find-file-signals-when-find-file-missing ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (cl-letf (((symbol-function 'find-file) nil))
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "beta.txt")
            (should-error (dired-find-file) :type 'user-error))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-mark-marks-file-and-advances ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (let ((start (nelisp-ec-point)))
              (dired-mark)
              (should (member "* alpha.txt\t5\t-rw-rw-r--"
                              (emacs-dired-min-test--buffer-lines)))
              (should (> (nelisp-ec-point) start))
              (emacs-dired-min-test--goto-entry "alpha.txt")
              (should (equal "alpha.txt"
                             (plist-get
                              (emacs-buffer-get-text-property
                               (nelisp-ec-point) 'dired-file
                               (nelisp-ec-current-buffer))
                              :name)))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-unmark-clears-the-mark ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (dired-mark)
            (should (member "* alpha.txt\t5\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines)))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (dired-unmark)
            (should (member "  alpha.txt\t5\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-flag-and-flagged-delete-removes-file ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (progn
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "beta.txt")
            (dired-flag-file-deletion)
            (should (member "D beta.txt\t4\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines)))
            (should (= 1 (dired-do-flagged-delete)))
            (should-not (cl-find-if
                         (lambda (line) (string-match-p "beta\\.txt" line))
                         (emacs-dired-min-test--buffer-lines)))
            (should-not (nelisp-ec-file-attributes (plist-get tree :file-b)))
            ;; an unflagged file is untouched
            (should (nelisp-ec-file-attributes (plist-get tree :file-a))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-gui-backend-opens-directory-with-display-action ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil))
      (emacs-dired-min-gui-register-backend
       :list-directory
       (lambda (directory)
         (push (list :list directory) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-dired-min-gui-set-context :directory "/tmp/demo")
      (should (equal "*Directory*" (emacs-dired-min-gui-dired "other")))
      (should (equal "*Directory*" emacs-dired-min-gui-buffer-name))
      (should (equal '((:display "other")
                       (:list "/tmp/demo"))
                     calls)))))

(ert-deftest dired-gui-backend-jump-derives-directory-from-current-file ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil))
      (emacs-dired-min-gui-register-backend
       :list-directory
       (lambda (directory)
         (push (list :list directory) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-dired-min-gui-set-context :directory "/old"
                                       :current-file-name "/tmp/a/b.txt")
      (should (equal "*Directory*" (emacs-dired-min-gui-dired-jump "same")))
      (should (equal "/old" emacs-dired-min-gui-directory))
      (should (equal '((:display "same")
                       (:list "/tmp/a/"))
                     calls)))))

(ert-deftest dired-gui-refresh-context-from-backend ()
  (emacs-dired-min-test--with-fresh-world
    (emacs-dired-min-gui-register-backend
     :current-directory (lambda () "/tmp/demo")
     :current-target (lambda () "/tmp/target.txt")
     :current-file-name (lambda () "/tmp/current.txt")
     :current-status (lambda () "ok")
     :buffer-name (lambda () "*Directory*"))
    (should (equal '(:directory "/tmp/demo"
                     :target "/tmp/target.txt"
                     :current-file-name "/tmp/current.txt"
                     :status "ok"
                     :buffer-name "*Directory*")
                   (emacs-dired-min-gui-refresh-context-from-backend)))
    (should (equal "/tmp/demo" emacs-dired-min-gui-directory))
    (should (equal "/tmp/target.txt" emacs-dired-min-gui-target))
    (should (equal "/tmp/current.txt"
                   emacs-dired-min-gui-current-file-name))
    (should (equal "ok" emacs-dired-min-gui-status))
    (should (equal "*Directory*" emacs-dired-min-gui-buffer-name))))

(ert-deftest dired-gui-current-context-command-variants ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil))
      (emacs-dired-min-gui-register-backend
       :current-directory (lambda () "/tmp/demo")
       :current-target (lambda () "")
       :current-file-name (lambda () "/tmp/a/b.txt")
       :current-status (lambda () "ok")
       :list-directory
       (lambda (directory)
         (push (list :list directory) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-dired-current-context-command
                      "frame")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-dired-jump-current-context-command
                      "other")))
      (should (equal '((:display "other")
                       (:list "/tmp/a/")
                       (:display "frame")
                       (:list "/tmp/demo"))
                     calls)))))

(ert-deftest dired-gui-current-context-command-dispatcher ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil)
          (directory "/tmp/demo")
          (current-file "/tmp/a/b.txt"))
      (emacs-dired-min-gui-register-backend
       :current-directory (lambda () directory)
       :current-target (lambda () "")
       :current-file-name (lambda () current-file)
       :current-status (lambda () "ok")
       :list-directory
       (lambda (dir)
         (push (list :list dir) calls)
         "*Directory*")
       :mark
       (lambda (mark)
         (push (list :mark mark) calls)
         "*Directory*")
       :flagged-delete
       (lambda ()
         (push :delete calls)
         "*Directory*")
       :rename
       (lambda (target)
         (push (list :rename target) calls)
         "*Directory*")
       :copy
       (lambda (target)
         (push (list :copy target) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired "frame")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-jump "other")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-mark)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-unmark)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-flag-file-deletion)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-do-flagged-delete)))
      (setq directory "renamed.txt")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-do-rename)))
      (setq directory "copy.txt")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'dired-do-copy)))
      (should (equal nil
                     (emacs-dired-min-gui-current-context-command
                      'unknown-dired-command)))
      (should (equal '((:copy "copy.txt")
                       (:rename "renamed.txt")
                       :delete
                       (:mark "D")
                       (:mark " ")
                       (:mark "*")
                       (:display "other")
                       (:list "/tmp/a/")
                       (:display "frame")
                       (:list "/tmp/demo"))
                     calls)))))

(ert-deftest dired-gui-project-current-context-commands ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil)
          (directory "nested")
          (project-directory "/tmp/project/sub"))
      (emacs-dired-min-gui-register-backend
       :current-directory (lambda () directory)
       :current-target (lambda () "")
       :current-file-name (lambda () "/tmp/project/sub/current.txt")
       :current-status (lambda () "ok")
       :project-directory (lambda () project-directory)
       :list-directory
       (lambda (dir)
         (push (list :list dir) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'project-find-dir "same")))
      (should (equal "nested" emacs-dired-min-gui-directory))
      (setq directory "")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'project-dired "other")))
      (should (equal "" emacs-dired-min-gui-directory))
      (setq directory "/tmp/absolute")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-current-context-command
                      'project-dired "frame")))
      (should (equal '((:display "frame")
                       (:list "/tmp/absolute")
                       (:display "other")
                       (:list "/tmp/project/sub")
                       (:display "same")
                       (:list "/tmp/project/sub/nested"))
                     calls)))))

(ert-deftest dired-gui-backend-mark-and-file-ops-delegate ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil))
      (emacs-dired-min-gui-register-backend
       :mark
       (lambda (mark)
         (push (list :mark mark) calls)
         "*Directory*")
       :flagged-delete
       (lambda ()
         (push :delete calls)
         "*Directory*")
       :rename
       (lambda (target)
         (push (list :rename target) calls)
         "*Directory*")
       :copy
       (lambda (target)
         (push (list :copy target) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*"))
      (should (equal "*Directory*" (emacs-dired-min-gui-mark "D")))
      (should (equal "*Directory*" (emacs-dired-min-gui-do-flagged-delete)))
      (should (equal "*Directory*" (emacs-dired-min-gui-do-rename "new.txt")))
      (should (equal "*Directory*" (emacs-dired-min-gui-do-copy "copy.txt")))
      (should (equal '((:copy "copy.txt")
                       (:rename "new.txt")
                       :delete
                       (:mark "D"))
                     calls)))))

(ert-deftest dired-gui-apply-mark-core-uses-low-level-backend ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "*Directory*")
          (entry "alpha.txt"))
      (emacs-dired-min-gui-register-backend
       :directory-buffer-p
       (lambda ()
         t)
       :name-at-point
       (lambda ()
         entry)
       :remove-mark
       (lambda (name)
         (push (list :remove name) calls))
       :set-mark
       (lambda (name mark)
         (push (list :set name mark) calls))
       :write-marks-state
       (lambda ()
         (push :write calls))
       :rerender
       (lambda ()
         (push :rerender calls))
       :next-line
       (lambda ()
         (push :next calls))
       :buffer-name
       (lambda ()
         buffer-name)
       :set-status
       (lambda (status)
         (push (list :status status) calls)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-mark "*")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-mark " ")))
      (setq entry "")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-mark "D")))
      (should (equal '(:next
                       :next
                       :rerender
                       :write
                       (:remove "alpha.txt")
                       :next
                       :rerender
                       :write
                       (:set "alpha.txt" "*"))
                     calls)))))

(ert-deftest dired-gui-file-op-cores-use-low-level-backend ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil)
          (buffer-name "*Directory*")
          (entry "alpha.txt")
          (existing '("/tmp/alpha.txt" "/tmp/beta.txt"))
          (directories '("/tmp/subdir")))
      (emacs-dired-min-gui-register-backend
       :directory-buffer-p
       (lambda ()
         t)
       :marks-text
       (lambda ()
         "alpha.txt\tD\nsubdir\tD\nbeta.txt\t*\n")
       :name-at-point
       (lambda ()
         entry)
       :expand-name
       (lambda (name)
         (concat "/tmp/" name))
       :directory-p
       (lambda (path)
         (member path directories))
       :delete-file
       (lambda (path)
         (push (list :delete path) calls)
         t)
       :rename-file
       (lambda (source dest)
         (push (list :rename source dest) calls)
         t)
       :file-exists-p
       (lambda (path)
         (member path existing))
       :read-file
       (lambda (path)
         (push (list :read path) calls)
         "contents")
       :write-file
       (lambda (path text)
         (push (list :write-file path text) calls)
         t)
       :remove-mark
       (lambda (name)
         (push (list :remove name) calls))
       :write-marks-state
       (lambda ()
         (push :write-marks calls))
       :rerender
       (lambda ()
         (push :rerender calls))
       :set-modeline
       (lambda (text)
         (push (list :modeline text) calls))
       :set-status
       (lambda (status)
         (push (list :status status) calls))
       :buffer-name
       (lambda ()
         buffer-name))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-do-flagged-delete)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-do-rename "renamed.txt")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-do-copy "copy.txt")))
      (should (equal '(:rerender
                       (:write-file "/tmp/copy.txt" "contents")
                       (:read "/tmp/alpha.txt")
                       :rerender
                       :write-marks
                       (:remove "alpha.txt")
                       (:rename "/tmp/alpha.txt" "/tmp/renamed.txt")
                       (:modeline "Deleted 1 files")
                       :rerender
                       :write-marks
                       (:remove "alpha.txt")
                       (:delete "/tmp/alpha.txt"))
                     calls)))))

(ert-deftest dired-gui-command-variants ()
  (emacs-dired-min-test--with-fresh-world
    (let ((calls nil))
      (emacs-dired-min-gui-register-backend
       :list-directory
       (lambda (directory)
         (push (list :list directory) calls)
         "*Directory*")
       :mark
       (lambda (mark)
         (push (list :mark mark) calls)
         "*Directory*")
       :flagged-delete
       (lambda ()
         (push :delete calls)
         "*Directory*")
       :rename
       (lambda (target)
         (push (list :rename target) calls)
         "*Directory*")
       :copy
       (lambda (target)
         (push (list :copy target) calls)
         "*Directory*")
       :buffer-name
       (lambda ()
         "*Directory*")
       :apply-display-prefix
       (lambda (action)
         (push (list :display action) calls)))
      (emacs-dired-min-gui-set-context :directory "/tmp/demo"
                                       :current-file-name "/tmp/a/b.txt")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-dired-command "frame")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-dired-jump-command "other")))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-mark-command)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-unmark-command)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-flag-file-deletion-command)))
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-do-flagged-delete-command)))
      (emacs-dired-min-gui-set-context :directory "renamed.txt")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-do-rename-command)))
      (emacs-dired-min-gui-set-context :directory "copy.txt")
      (should (equal "*Directory*"
                     (emacs-dired-min-gui-do-copy-command)))
      (should (equal '((:copy "copy.txt")
                       (:rename "renamed.txt")
                       :delete
                       (:mark "D")
                       (:mark " ")
                       (:mark "*")
                       (:display "other")
                       (:list "/tmp/a/")
                       (:display "frame")
                       (:list "/tmp/demo"))
                     calls)))))

(ert-deftest dired-gui-run-directory-command-uses-frontend-hooks ()
  (emacs-dired-min-test--with-fresh-world
    (let ((installed nil)
          (prompts nil)
          (context nil)
          (command nil)
          (action nil))
      (cl-letf (((symbol-function 'emacs-dired-min-gui-set-context)
                 (lambda (&rest plist)
                   (setq context plist)
                   plist))
                ((symbol-function 'emacs-dired-min-gui-current-context-command)
                 (lambda (cmd where)
                   (setq command cmd
                         action where)
                   "*Dired*")))
        (should
         (equal
          "*Dired*"
          (emacs-dired-min-gui-run-directory-command
           :install-function (lambda () (setq installed t))
           :read-string (lambda (prompt)
                          (push prompt prompts)
                          "")
           :default-directory (lambda () "/tmp/default")
           :buffer-name "*Dired*")))
        (should installed)
        (should (equal '("Dired (directory): ") prompts))
        (should (equal "/tmp/default" (plist-get context :directory)))
        (should (equal "ok" (plist-get context :status)))
        (should (equal "*Dired*" (plist-get context :buffer-name)))
        (should (eq 'dired command))
        (should (equal "same" action))))))

(ert-deftest emacs-dired-min-gui-writeback-spec ()
  (emacs-dired-min-test--with-fresh-world
    (should (equal '(:buffer t :file t :buffer-name t :window t
                     :point t :mark t :window-start t)
                   (emacs-dired-min-gui-writeback-spec 'dired)))
    (should (equal '(:buffer t :file t :buffer-name t :window t
                     :point t :mark t :window-start t)
                   (emacs-dired-min-gui-writeback-spec
                    "project-find-dir")))
    (should (equal '(:buffer t :file t :buffer-name t :window t
                     :frame t :point t :mark t :window-start t)
                   (emacs-dired-min-gui-writeback-spec
                    'dired-other-frame)))
    (should (equal '(:buffer t :file t :buffer-name t :window t
                     :tab t :point t :mark t :window-start t)
                   (emacs-dired-min-gui-writeback-spec
                    'dired-other-tab)))
    (should (equal '(:buffer t :buffer-name t
                     :point t :mark t :window-start t)
                   (emacs-dired-min-gui-writeback-spec
                    'dired-do-rename)))
    (should (equal '(:buffer t :buffer-name t :modeline t
                     :point t :mark t :window-start t)
                   (emacs-dired-min-gui-writeback-spec
                    'dired-do-flagged-delete)))
    (should-not (emacs-dired-min-gui-writeback-spec 'find-file))))

(ert-deftest emacs-dired-min-gui-writeback-spec-flag ()
  (let ((spec (emacs-dired-min-gui-writeback-spec
               'dired-do-flagged-delete)))
    (should (emacs-dired-min-gui-writeback-spec-flag spec :buffer))
    (should (emacs-dired-min-gui-writeback-spec-flag spec :modeline))
    (should-not (emacs-dired-min-gui-writeback-spec-flag spec :file))
    (should-not (emacs-dired-min-gui-writeback-spec-flag nil :buffer))))

(ert-deftest emacs-dired-min-gui-writeback-state ()
  (emacs-dired-min-test--with-fresh-world
    (let (calls)
      (emacs-dired-min-gui-register-backend
       :write-buffer-state (lambda () (push :buffer calls))
       :write-file-state (lambda () (push :file calls))
       :write-buffer-name-state (lambda () (push :buffer-name calls))
       :write-window-state (lambda () (push :window calls))
       :write-frame-state (lambda () (push :frame calls))
       :write-tab-state (lambda () (push :tab calls))
       :write-modeline-state (lambda () (push :modeline calls))
       :write-point-state (lambda () (push :point calls))
       :write-mark-state (lambda () (push :mark calls))
       :write-window-start-state (lambda () (push :window-start calls))
       :mark-written-state (lambda () (push :written calls)))
      (should (emacs-dired-min-gui-writeback-state 'dired-other-tab))
      (should (equal '(:buffer :file :buffer-name :window :tab
                       :point :mark :window-start :written)
                     (nreverse calls)))
      (setq calls nil)
      (should (emacs-dired-min-gui-writeback-state
               'dired-do-flagged-delete))
      (should (equal '(:buffer :buffer-name :modeline
                       :point :mark :window-start :written)
                     (nreverse calls)))
      (setq calls nil)
      (should-not (emacs-dired-min-gui-writeback-state 'find-file))
      (should-not calls))))

(ert-deftest dired-do-rename-renames-file-at-point ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) "renamed.txt")))
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (dired-do-rename)
            (should (member "  renamed.txt\t5\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines)))
            (should-not (cl-find-if
                         (lambda (line) (string-match-p "alpha\\.txt" line))
                         (emacs-dired-min-test--buffer-lines)))
            (should (nelisp-ec-file-attributes
                     (nelisp-ec-expand-file-name
                      "renamed.txt" (plist-get tree :root))))
            (should-not (nelisp-ec-file-attributes (plist-get tree :file-a))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(ert-deftest dired-do-copy-copies-file-at-point ()
  (emacs-dired-min-test--with-fresh-world
    (let ((tree (emacs-dired-min-test--make-tree)))
      (unwind-protect
          (cl-letf (((symbol-function 'read-file-name)
                     (lambda (&rest _) "alpha-copy.txt")))
            (dired (plist-get tree :root))
            (emacs-dired-min-test--goto-entry "alpha.txt")
            (dired-do-copy)
            ;; both the original and the copy are present
            (should (member "  alpha.txt\t5\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines)))
            (should (member "  alpha-copy.txt\t5\t-rw-rw-r--"
                            (emacs-dired-min-test--buffer-lines)))
            (should (nelisp-ec-file-attributes (plist-get tree :file-a))))
        (emacs-dired-min-test--cleanup-tree tree)))))

(provide 'emacs-dired-min-test)

;;; emacs-dired-min-test.el ends here
