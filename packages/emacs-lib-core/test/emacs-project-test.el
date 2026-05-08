;;; emacs-project-test.el --- Tests for emacs-project  -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'emacs-project)

(defmacro emacs-project-test--with-temp-project (files &rest body)
  "Create a temp directory with FILES, then run BODY there.
FILES is a list of relative file paths to populate with placeholder text."
  (declare (indent 1))
  `(let ((temporary-file-directory "/tmp/"))
     (let* ((root (make-temp-file "emacs-project-test-" t))
            (default-directory root)
            (project-list-file (make-temp-file "emacs-project-list-"))
            project--list)
     (unwind-protect
         (progn
           (make-directory (expand-file-name ".git" root) t)
           (dolist (rel ',files)
             (let ((path (expand-file-name rel root)))
               (make-directory (file-name-directory path) t)
               (with-temp-file path
                 (insert rel "\n"))))
           ,@body)
       (delete-directory root t)
       (delete-file project-list-file)))))

(ert-deftest project-current-detects-git-root ()
  (emacs-project-test--with-temp-project ("src/main.el")
    (let ((nested (expand-file-name "src/" root)))
      (should (equal (project-root (project-current nil nested))
                     (file-name-as-directory root))))))

(ert-deftest project-current-returns-nil-outside-vc ()
  (let ((temporary-file-directory "/tmp/"))
    (let* ((root (make-temp-file "emacs-project-novc-" t))
           (project-list-file (make-temp-file "emacs-project-list-"))
           project--list)
      (unwind-protect
          (cl-letf (((symbol-function 'project--vc-root-p)
                     (lambda (_dir) nil)))
            (should-not (project-current nil root)))
        (delete-directory root t)
        (delete-file project-list-file)))))

(ert-deftest project-find-file-lists-tracked-files ()
  (emacs-project-test--with-temp-project ("src/main.el" "README.md")
    (let (seen)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt collection &rest _)
                   (setq seen collection)
                   "src/main.el"))
                ((symbol-function 'find-file)
                 (lambda (_filename) t)))
        (project-find-file)
        (should (member "src/main.el" seen))
        (should (member "README.md" seen))))))

(ert-deftest project-find-file-opens-selected ()
  (emacs-project-test--with-temp-project ("src/main.el")
    (let (opened)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt _collection &rest _)
                   "src/main.el"))
                ((symbol-function 'find-file)
                 (lambda (filename)
                   (setq opened filename)
                   filename)))
        (project-find-file)
        (should (equal opened
                       (expand-file-name "src/main.el" root)))))))

(ert-deftest project-switch-project-changes-default-directory ()
  (let ((temporary-file-directory "/tmp/"))
    (let* ((project-list-file (make-temp-file "emacs-project-list-"))
           (root (make-temp-file "emacs-project-switch-" t))
           (default-directory "/tmp/")
           project--list
           opened)
      (unwind-protect
          (progn
            (make-directory (expand-file-name ".git" root) t)
            (with-temp-file (expand-file-name "note.txt" root)
              (insert "note\n"))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (prompt collection &rest _)
                         (if (string-match-p "Switch to project" prompt)
                             root
                           (car collection))))
                      ((symbol-function 'find-file)
                       (lambda (filename)
                         (setq opened filename)
                         filename)))
              (project--append-known-project root)
              (project-switch-project root)
              (should (equal default-directory
                             (file-name-as-directory root)))
              (should (equal opened
                             (expand-file-name "note.txt" root)))))
        (delete-directory root t)
        (delete-file project-list-file)))))

(ert-deftest project-current-detects-hg-root ()
  (let ((temporary-file-directory "/tmp/"))
    (let* ((root (make-temp-file "emacs-project-hg-" t))
           (project-list-file (make-temp-file "emacs-project-list-"))
           project--list)
      (unwind-protect
        (progn
          (make-directory (expand-file-name ".hg" root) t)
          (should (equal (project-root (project-current nil root))
                         (file-name-as-directory root))))
        (delete-directory root t)
        (delete-file project-list-file)))))

(provide 'emacs-project-test)

;;; emacs-project-test.el ends here
