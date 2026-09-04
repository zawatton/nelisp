;;; nemacs-magit-stage-workflow-probe.el --- Magit stage workflow smoke -*- lexical-binding: t; -*-

(defun nemacs-magit-stage-workflow--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defun nemacs-magit-stage-workflow--backtrace-lines ()
  (if (fboundp 'backtrace)
      (let* ((text (with-output-to-string (backtrace)))
             (lines (string-lines text t)))
        (if (and (null lines)
                 (not (string= text "")))
            (list text)
          lines))
    nil))

(defun nemacs-magit-stage-workflow--buffer-tail (buffer &optional max-lines)
  (when (bufferp buffer)
    (with-current-buffer buffer
      (let* ((max-lines (or max-lines 20))
             (lines (string-lines (buffer-substring-no-properties
                                   (point-min) (point-max))
                                  t))
             (tail (nthcdr (max 0 (- (length lines) max-lines)) lines)))
        (string-join tail "\n")))))

(defun nemacs-magit-stage-workflow--repo-root ()
  (file-name-as-directory
   (or (and (boundp 'nemacs-magit-probe-repo-root)
            nemacs-magit-probe-repo-root)
       (getenv "NEMACS_MAGIT_REPO_ROOT")
       (and (boundp 'default-directory) default-directory)
       "")))

(defun nemacs-magit-stage-workflow--fixture-dir (repo)
  (file-name-as-directory
   (or (and (boundp 'nemacs-magit-probe-fixture-dir)
            nemacs-magit-probe-fixture-dir)
       (getenv "NEMACS_MAGIT_FIXTURE_DIR")
       (expand-file-name "build/nemacs-magit-fixture" repo))))

(let ((ok nil))
  (nemacs-magit-stage-workflow--print "MAGIT-STAGE-WORKFLOW BEGIN")
  (condition-case err
      (let* ((repo (nemacs-magit-stage-workflow--repo-root))
             (fixture (nemacs-magit-stage-workflow--fixture-dir repo))
             (target "file-1.txt"))
        (nemacs-magit-stage-workflow--print
         "MAGIT-STAGE-WORKFLOW repo=%S fixture=%S target=%S"
         repo fixture target)
        (nemacs-magit-stage-workflow--print
         "MAGIT-STAGE-WORKFLOW STEP load-bridge")
        (if (fboundp 'nemacs-runtime-image-preload--load-source-file)
            (nemacs-runtime-image-preload--load-source-file
             (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo))
          (load (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo)
                nil 'no-message t t))
        (setq nelisp-emacs-magit-bridge-repo-root repo)
        (nemacs-magit-stage-workflow--print
         "MAGIT-STAGE-WORKFLOW STEP bridge-load")
        (nelisp-emacs-magit-bridge-load)
        (nemacs-magit-stage-workflow--print
         "MAGIT-STAGE-WORKFLOW bridge-check symbol-function=%S"
         (condition-case err
             (list 'ok (symbol-function #'split-string))
           (error (list 'error err))))
        (nemacs-magit-stage-workflow--print
         "MAGIT-STAGE-WORKFLOW STEP status-setup")
        (let ((default-directory fixture))
          (magit-status-setup-buffer fixture))
        (nemacs-magit-stage-workflow--print
         "MAGIT-STAGE-WORKFLOW STEP status-buffer")
        (let ((buf (magit-get-mode-buffer 'magit-status-mode)))
          (unless (bufferp buf)
            (error "missing magit-status buffer"))
          (with-current-buffer buf
            (nemacs-magit-stage-workflow--print
             "MAGIT-STAGE-WORKFLOW STEP search-target")
            (goto-char (point-min))
            (unless (search-forward target nil t)
              (error "missing target line: %s" target))
            (beginning-of-line)
            (let ((before-unstaged (magit-git-lines "diff" "--name-only"))
                  (before-staged (magit-git-lines "diff" "--cached" "--name-only"))
                  (diff-type (magit-diff-type))
                  (diff-scope (magit-diff-scope))
                  (diff-files magit-buffer-diff-files))
              (nemacs-magit-stage-workflow--print
               "MAGIT-STAGE-WORKFLOW before point=%S section=%S diff-type=%S diff-scope=%S diff-files=%S before-unstaged=%S before-staged=%S"
               (point)
               (let ((section (magit-current-section)))
                 (and section (oref section type)))
               diff-type
               diff-scope
               diff-files
               before-unstaged
               before-staged)
              (magit-stage)
              (let ((after-unstaged (magit-git-lines "diff" "--name-only"))
                    (after-staged (magit-git-lines "diff" "--cached" "--name-only"))
                    (status-short (magit-git-lines "status" "--short")))
                (nemacs-magit-stage-workflow--print
                 "MAGIT-STAGE-WORKFLOW after after-unstaged=%S after-staged=%S status=%S"
                 after-unstaged after-staged)
                (setq ok
                      (and (member target before-unstaged)
                           (not (member target before-staged))
                           (not (member target after-unstaged))
                           (member target after-staged)))
                (unless ok
                  (let ((process-buffer (ignore-errors (magit-process-buffer))))
                    (nemacs-magit-stage-workflow--print
                     "MAGIT-STAGE-WORKFLOW process-buffer=%S"
                     process-buffer)
                    (when process-buffer
                      (nemacs-magit-stage-workflow--print
                       "MAGIT-STAGE-WORKFLOW process-buffer-tail<<%s>>"
                       (or (nemacs-magit-stage-workflow--buffer-tail process-buffer)
                           ""))))))))))
    (error
     (nemacs-magit-stage-workflow--print
      "MAGIT-STAGE-WORKFLOW ERROR %S" err)
     (dolist (line (nemacs-magit-stage-workflow--backtrace-lines))
       (nemacs-magit-stage-workflow--print
        "MAGIT-STAGE-WORKFLOW BT %s" line))))
  (if ok
      (nemacs-magit-stage-workflow--print "MAGIT-STAGE-WORKFLOW PASS")
    (unless ok
      (nemacs-magit-stage-workflow--print "MAGIT-STAGE-WORKFLOW FAIL"))))

;;; nemacs-magit-stage-workflow-probe.el ends here
