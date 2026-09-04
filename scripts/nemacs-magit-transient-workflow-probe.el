;;; nemacs-magit-transient-workflow-probe.el --- Magit transient workflow smoke -*- lexical-binding: t; -*-

(defun nemacs-magit-transient-workflow--print (format-string &rest args)
  (nelisp--write-stdout-bytes (apply #'format (concat format-string "\n") args)))

(defun nemacs-magit-transient-workflow--backtrace-lines ()
  (if (fboundp 'backtrace)
      (let* ((text (with-output-to-string (backtrace)))
             (lines (string-lines text t)))
        (if (and (null lines)
                 (not (string= text "")))
            (list text)
          lines))
    nil))

(defun nemacs-magit-transient-workflow--repo-root ()
  (file-name-as-directory
   (or (and (boundp 'nemacs-magit-probe-repo-root)
            nemacs-magit-probe-repo-root)
       (getenv "NEMACS_MAGIT_REPO_ROOT")
       (and (boundp 'default-directory) default-directory)
       "")))

(defun nemacs-magit-transient-workflow--fixture-dir (repo)
  (file-name-as-directory
   (or (and (boundp 'nemacs-magit-probe-fixture-dir)
            nemacs-magit-probe-fixture-dir)
       (getenv "NEMACS_MAGIT_FIXTURE_DIR")
       (expand-file-name "build/nemacs-magit-fixture" repo))))

(let ((ok nil))
  (nemacs-magit-transient-workflow--print "MAGIT-TRANSIENT-WORKFLOW BEGIN")
  (condition-case err
      (let* ((repo (nemacs-magit-transient-workflow--repo-root))
             (fixture (nemacs-magit-transient-workflow--fixture-dir repo)))
        (nemacs-magit-transient-workflow--print
         "MAGIT-TRANSIENT-WORKFLOW repo=%S fixture=%S"
         repo fixture)
        (if (fboundp 'nemacs-runtime-image-preload--load-source-file)
            (nemacs-runtime-image-preload--load-source-file
             (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo))
          (load (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo)
                nil 'no-message t t))
        (setq nelisp-emacs-magit-bridge-repo-root repo)
        (nemacs-magit-transient-workflow--print
         "MAGIT-TRANSIENT-WORKFLOW STEP bridge-load")
        (nelisp-emacs-magit-bridge-load)
        (nemacs-magit-transient-workflow--print
         "MAGIT-TRANSIENT-WORKFLOW features branch=%S sequence=%S commit=%S magit=%S"
         (featurep 'magit-branch)
         (featurep 'magit-sequence)
         (featurep 'magit-commit)
         (featurep 'magit))
        (nemacs-magit-transient-workflow--print
         "MAGIT-TRANSIENT-WORKFLOW fboundp dispatch=%S cherry-pick=%S branch=%S commit=%S"
         (fboundp 'magit-dispatch)
         (fboundp 'magit-cherry-pick)
         (fboundp 'magit-branch)
         (fboundp 'magit-commit))
        (let ((default-directory fixture))
          (nemacs-magit-transient-workflow--print
           "MAGIT-TRANSIENT-WORKFLOW STEP init-objects")
          (transient--init-objects 'magit-dispatch)
          (nemacs-magit-transient-workflow--print
           "MAGIT-TRANSIENT-WORKFLOW suffixes=%S"
           (mapcar (lambda (obj)
                     (and obj
                          (slot-boundp obj 'command)
                          (oref obj command)))
                   transient--suffixes))
          (nemacs-magit-transient-workflow--print
           "MAGIT-TRANSIENT-WORKFLOW STEP dispatch")
          (magit-dispatch)
          (setq ok
                (and (window-live-p transient--window)
                     (bufferp transient--buffer)
                     (eq (oref transient--prefix command) 'magit-dispatch)))
          (nemacs-magit-transient-workflow--print
           "MAGIT-TRANSIENT-WORKFLOW window=%S buffer=%S prefix=%S"
           transient--window
           transient--buffer
           (and transient--prefix (oref transient--prefix command)))))
    (error
     (nemacs-magit-transient-workflow--print
      "MAGIT-TRANSIENT-WORKFLOW ERROR %S" err)
     (dolist (line (nemacs-magit-transient-workflow--backtrace-lines))
       (nemacs-magit-transient-workflow--print
        "MAGIT-TRANSIENT-WORKFLOW BT %s" line))))
  (if ok
      (nemacs-magit-transient-workflow--print "MAGIT-TRANSIENT-WORKFLOW PASS")
    (nemacs-magit-transient-workflow--print "MAGIT-TRANSIENT-WORKFLOW FAIL"))
  (let ((code (if ok 0 1)))
    (condition-case nil
        (exit code)
      (void-function
       (when (fboundp 'kill-emacs)
         (kill-emacs code))))))

;;; nemacs-magit-transient-workflow-probe.el ends here
