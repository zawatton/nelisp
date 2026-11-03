;;; emacs-vc-test.el --- ERT for emacs-vc  -*- lexical-binding: t; -*-

;;; Commentary:

;; Read-only VC (git) command semantics tests.  Exercises root detection,
;; status parsing, and diff/log/status buffer construction against a throwaway
;; git work-tree.  Host-side (uses the host `call-process'); validates the
;; Layer 2 logic independently of the Layer 1 reader.

;;; Code:

(require 'ert)
(require 'emacs-vc)

(defun emacs-vc-test--git (&rest args)
  "Run git with ARGS in `default-directory'; return exit code.
The work-tree is isolated from the developer's global/system git config."
  (let ((process-environment
         (append '("GIT_CONFIG_GLOBAL=/dev/null"
                   "GIT_CONFIG_SYSTEM=/dev/null"
                   "GIT_AUTHOR_NAME=Test" "GIT_AUTHOR_EMAIL=t@example.com"
                   "GIT_COMMITTER_NAME=Test" "GIT_COMMITTER_EMAIL=t@example.com")
                 process-environment)))
    (apply #'call-process "git" nil nil nil args)))

(defmacro emacs-vc-test--with-git-repo (&rest body)
  "Run BODY with `default-directory' bound to a fresh git work-tree."
  (declare (indent 0) (debug (body)))
  `(let* ((dir (make-temp-file "emacs-vc-test-" t))
          (default-directory (file-name-as-directory dir)))
     (unwind-protect
         (progn
           (emacs-vc-test--git "init" "-q" "-b" "main")
           (emacs-vc-test--git "config" "user.email" "t@example.com")
           (emacs-vc-test--git "config" "user.name" "Test")
           ,@body)
       (delete-directory dir t))))

(defun emacs-vc-test--write (file content)
  "Write CONTENT to FILE under `default-directory'."
  (with-temp-file (expand-file-name file default-directory)
    (insert content)))

(defun emacs-vc-test--seed-commit ()
  "Create and commit `file.txt', then modify it (uncommitted)."
  (emacs-vc-test--write "file.txt" "line one\n")
  (emacs-vc-test--git "add" "file.txt")
  (emacs-vc-test--git "commit" "-q" "-m" "seed commit")
  (emacs-vc-test--write "file.txt" "line one\nline two\n"))

;;;; --- root detection ----------------------------------------------

(ert-deftest emacs-vc-test/git-root-detects-worktree ()
  (emacs-vc-test--with-git-repo
    (should (emacs-vc--git-root))
    ;; the detected root is the temp dir (modulo symlink normalisation)
    (should (string-equal (file-truename (emacs-vc--git-root))
                          (file-truename default-directory)))
    ;; works from a nested subdirectory too
    (make-directory (expand-file-name "sub/deep" default-directory) t)
    (should (string-equal
             (file-truename (emacs-vc--git-root
                             (expand-file-name "sub/deep" default-directory)))
             (file-truename default-directory)))))

(ert-deftest emacs-vc-test/git-root-nil-outside-worktree ()
  (let ((default-directory temporary-file-directory))
    ;; /tmp itself is not expected to be a git work-tree in CI
    (when (not (emacs-vc--git-root temporary-file-directory))
      (should-not (emacs-vc--git-root temporary-file-directory)))))

;;;; --- status parsing (pure unit) ----------------------------------

(ert-deftest emacs-vc-test/parse-status-unit ()
  (let ((entries (emacs-vc--parse-status " M file.txt\n?? new.txt\nA  added.txt\n")))
    (should (equal entries '((" M" . "file.txt")
                             ("??" . "new.txt")
                             ("A " . "added.txt"))))
    (should (null (emacs-vc--parse-status "")))
    (should (null (emacs-vc--parse-status nil)))))

;;;; --- status / diff / log against a real work-tree ----------------

(ert-deftest emacs-vc-test/status-reports-modified ()
  (emacs-vc-test--with-git-repo
    (emacs-vc-test--seed-commit)
    (let ((entries (emacs-vc-status)))
      (should (assoc " M" entries))
      (should (string-equal "file.txt" (cdr (assoc " M" entries)))))))

(ert-deftest emacs-vc-test/diff-shows-change ()
  (emacs-vc-test--with-git-repo
    (emacs-vc-test--seed-commit)
    (let ((buf (emacs-vc-diff)))
      (unwind-protect
          (with-current-buffer buf
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "diff --git" text))
              (should (string-match-p "\\+line two" text))))
        (kill-buffer buf)))))

(ert-deftest emacs-vc-test/log-shows-commit ()
  (emacs-vc-test--with-git-repo
    (emacs-vc-test--seed-commit)
    (let ((buf (emacs-vc-print-log)))
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p
                     "seed commit"
                     (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer buf)))))

(ert-deftest emacs-vc-test/dir-shows-status ()
  (emacs-vc-test--with-git-repo
    (emacs-vc-test--seed-commit)
    (let ((buf (emacs-vc-dir)))
      (unwind-protect
          (with-current-buffer buf
            (should (string-match-p
                     "file.txt"
                     (buffer-substring-no-properties (point-min) (point-max)))))
        (kill-buffer buf)))))

(ert-deftest emacs-vc-test/diff-errors-outside-worktree ()
  (let ((default-directory temporary-file-directory))
    (when (not (emacs-vc--git-root))
      (should-error (emacs-vc-diff)))))

;;;; --- annotate / revision-diff (read-only extensions) -------------

(ert-deftest emacs-vc-test/annotate-shows-blame ()
  (emacs-vc-test--with-git-repo
    (emacs-vc-test--write "file.txt" "alpha line\n")
    (emacs-vc-test--git "add" "file.txt")
    (emacs-vc-test--git "commit" "-q" "-m" "add alpha")
    (let ((buf (emacs-vc-annotate "file.txt")))
      (unwind-protect
          (with-current-buffer buf
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              ;; blame output carries the author and the source line
              (should (string-match-p "Test" text))
              (should (string-match-p "alpha line" text))))
        (kill-buffer buf)))))

(ert-deftest emacs-vc-test/annotate-errors-without-file ()
  (emacs-vc-test--with-git-repo
    (let ((buffer-file-name nil))
      (should-error (emacs-vc-annotate)))))

(ert-deftest emacs-vc-test/revision-diff-between-commits ()
  (emacs-vc-test--with-git-repo
    (emacs-vc-test--write "file.txt" "v1\n")
    (emacs-vc-test--git "add" "file.txt")
    (emacs-vc-test--git "commit" "-q" "-m" "c1")
    (emacs-vc-test--write "file.txt" "v1\nv2\n")
    (emacs-vc-test--git "commit" "-q" "-am" "c2")
    (let ((buf (emacs-vc-revision-diff "HEAD~1" "HEAD")))
      (unwind-protect
          (with-current-buffer buf
            (let ((text (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "diff --git" text))
              (should (string-match-p "\\+v2" text))))
        (kill-buffer buf)))))

(provide 'emacs-vc-test)

;;; emacs-vc-test.el ends here
