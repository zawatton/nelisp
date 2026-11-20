;;; emacs-files-gnu-cd-test.el --- focused cd parity smoke -*- lexical-binding: t; -*-

(defun emacs-files-gnu-cd-test--run ()
  "Check GNU `cd' directory and buffer semantics with direct markers."
  (let ((checks 0)
        (failed nil)
        (root (make-temp-file "emacs-files-gnu-cd-" t))
        (original default-directory)
        (original-pwd (getenv "PWD"))
        (buffer-a (generate-new-buffer " *cd-test-a*"))
        (buffer-b (generate-new-buffer " *cd-test-b*")))
    (unwind-protect
        (condition-case err
            (progn
              (unless (fboundp 'cd)
                (error "cd is not available"))
              (let* ((one (expand-file-name "one" root))
                     (two (expand-file-name "two" root))
                     (regular (expand-file-name "plain-file" root)))
                (make-directory one)
                (make-directory two)
                (with-temp-file regular (insert "file"))
                (when (boundp 'cd-path) (setq cd-path nil))
                (setenv "CDPATH" nil)
                (cd one)
                (unless (and (equal default-directory (file-name-as-directory one))
                             (equal list-buffers-directory
                                    (file-name-as-directory one)))
                  (error "absolute directory change failed"))
                (setq checks (1+ checks))
                (condition-case err
                    (cd "../two")
                  (error
                   (error "relative cd failed: dir=%S target=%S cd-path=%S expanded=%S cwd-expanded=%S target-dir=%S expanded-dir=%S found-cwd=%S found-root=%S condition=%S"
                          default-directory two cd-path
                          (expand-file-name "../two" "./")
                          (expand-file-name "./" default-directory)
                          (file-directory-p two)
                          (file-directory-p (expand-file-name "../two" "./"))
                          (locate-file "../two" cd-path nil
                                       (lambda (file)
                                         (and (file-directory-p file) 'dir-ok)))
                          (locate-file "../two"
                                       (list (file-name-as-directory default-directory)) nil
                                       (lambda (file)
                                         (and (file-directory-p file) 'dir-ok)))
                          (locate-file "two" (list (file-name-as-directory root)) nil
                                       (lambda (file)
                                         (and (file-directory-p file) 'dir-ok)))
                          err)))
                (unless (equal default-directory (file-name-as-directory two))
                  (error "relative directory change failed"))
                (setq checks (1+ checks))
                (unless (equal (getenv "PWD") original-pwd)
                  (error "cd changed PWD"))
                (setq checks (1+ checks))
                (let ((before default-directory))
                  (unless (condition-case nil
                              (progn (cd (expand-file-name "missing" root)) nil)
                            (error t))
                    (error "missing directory did not signal"))
                  (unless (equal default-directory before)
                    (error "missing directory changed default-directory"))
                  (setq checks (+ checks 2)))
                (let ((before default-directory))
                  (unless (condition-case nil
                              (progn (cd regular) nil)
                            (error t))
                    (error "regular file did not signal"))
                  (unless (equal default-directory before)
                    (error "regular file changed default-directory"))
                  (setq checks (+ checks 2)))
                (with-current-buffer buffer-a
                  (setq default-directory (file-name-as-directory one))
                  (cd "."))
                (with-current-buffer buffer-b
                  (setq default-directory (file-name-as-directory two)))
                (unless (and (with-current-buffer buffer-a
                              (equal default-directory (file-name-as-directory one)))
                             (with-current-buffer buffer-b
                               (equal default-directory (file-name-as-directory two))))
                  (error "buffer default directories were not preserved"))
                (setq checks (1+ checks))))
          (error (setq failed err)))
      (when (fboundp 'cd)
        (ignore-errors (cd original)))
      (when (buffer-live-p buffer-a) (kill-buffer buffer-a))
      (when (buffer-live-p buffer-b) (kill-buffer buffer-b))
      (ignore-errors (delete-directory root t)))
    (if failed
        (progn
          (princ (format "CD-FOCUSED-SMOKE FAIL cases=%d error=%S\n"
                         checks failed))
          2)
      (princ (format "CD-FOCUSED-SMOKE PASS cases=%d\n" checks))
      nil)))

(emacs-files-gnu-cd-test--run)

;;; emacs-files-gnu-cd-test.el ends here
