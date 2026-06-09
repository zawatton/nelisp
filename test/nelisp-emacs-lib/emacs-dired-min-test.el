;;; emacs-dired-min-test.el --- ERT for emacs-dired-min -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emacs-dired-min)

(defmacro emacs-dired-min-test--with-fresh-world (&rest body)
  "Run BODY with clean nelisp + dired substrate state."
  (declare (indent 0) (debug (body)))
  `(let ((nelisp-ec--buffers nil)
         (nelisp-ec--current-buffer nil)
         (nelisp-ec--match-data nil)
         (emacs-dired-min--state (make-hash-table :test 'eq :weakness nil))
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
              (should (> (nelisp-ec-point) start))))
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

(provide 'emacs-dired-min-test)

;;; emacs-dired-min-test.el ends here
