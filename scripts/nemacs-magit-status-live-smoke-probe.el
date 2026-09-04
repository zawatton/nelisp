;;; nemacs-magit-status-live-smoke-probe.el --- live-load Magit smoke -*- lexical-binding: t; -*-

;; Evaluated by `make magit-status-smoke' against the base runtime image.
;; Keep the whole proof form in one file: nested `load' of a second smoke
;; script is currently unreliable under the live bridge path, while the same
;; top-level forms execute correctly when inlined here.

(defun nemacs-magit-status-live-smoke--print (format-string &rest args)
  (nelisp--write-stdout-bytes
   (apply #'format (concat format-string "\n") args)))

(defun nemacs-magit-status-live-smoke--backtrace-lines ()
  (if (fboundp 'backtrace)
      (let ((text (with-output-to-string (backtrace))))
        (split-string text "\n" t))
    nil))

(defun nemacs-magit-status-live-smoke--ensure-current-buffer ()
  (when (and (fboundp 'get-buffer-create)
             (fboundp 'set-buffer)
             (or (not (fboundp 'current-buffer))
                 (null (ignore-errors (current-buffer)))))
    (set-buffer (get-buffer-create "*scratch*"))))

(defun nemacs-magit-status-live-smoke--repo-root ()
  (file-name-as-directory
   (or (and (boundp 'nemacs-magit-probe-repo-root)
            nemacs-magit-probe-repo-root)
       (getenv "NEMACS_MAGIT_REPO_ROOT")
       (and (boundp 'default-directory) default-directory)
       "")))

(defun nemacs-magit-status-live-smoke--fixture-dir (repo)
  (file-name-as-directory
   (or (and (boundp 'nemacs-magit-probe-fixture-dir)
            nemacs-magit-probe-fixture-dir)
       (getenv "NEMACS_MAGIT_FIXTURE_DIR")
       (expand-file-name "build/nemacs-magit-fixture" repo))))

(let ((repo (nemacs-magit-status-live-smoke--repo-root))
      (ok-status nil)
      (ok-git nil))
  (condition-case err
      (progn
        (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S0")
        (nemacs-magit-status-live-smoke--ensure-current-buffer)
        (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S1")
        (if (fboundp 'nemacs-runtime-image-preload--load-source-file)
            (nemacs-runtime-image-preload--load-source-file
             (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo))
          (load (expand-file-name "src/nelisp-emacs-magit-bridge.el" repo)
                nil 'no-message t t))
        (setq nelisp-emacs-magit-bridge-repo-root repo)
        (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S2")
        (nelisp-emacs-magit-bridge-load)
        (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S3")
        (let* ((fixture (nemacs-magit-status-live-smoke--fixture-dir repo))
               (buf nil))
          (let ((default-directory fixture))
            (magit-status-setup-buffer fixture))
          (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S4")
          (nemacs-magit-status-live-smoke--ensure-current-buffer)
          (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S5")
          (setq buf (magit-get-mode-buffer 'magit-status-mode))
          (nemacs-magit-status-live-smoke--print
           "MAGIT-SMOKE STEP S6 buf=%S bufferp=%S"
           buf (ignore-errors (bufferp buf)))
          (when (bufferp buf)
            (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S7")
            (with-current-buffer buf
              (let ((ok-mode nil)
                    (ok-size nil)
                    (ok-prop nil)
                    (ok-children nil)
                    (ok-forward nil))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7a current=%S"
                 (ignore-errors (current-buffer)))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7a.1 point-min-fn=%S point-fn=%S goto-char-fn=%S"
                 (ignore-errors (symbol-function 'point-min))
                 (ignore-errors (symbol-function 'point))
                 (ignore-errors (symbol-function 'goto-char)))
                (setq ok-mode (eq major-mode 'magit-status-mode))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7b mode=%S" ok-mode)
                (setq ok-size (> (buffer-size) 0))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7c size=%S" ok-size)
                (setq ok-prop (get-text-property (point-min) 'magit-section))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7d prop=%S" ok-prop)
                (let ((children (oref magit-root-section children)))
                  (setq ok-children (and children (> (length children) 0))))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7e children=%S" ok-children)
                (let ((sec0 (magit-current-section)))
                  (magit-section-forward)
                  (setq ok-forward (not (eq sec0 (magit-current-section)))))
                (nemacs-magit-status-live-smoke--print
                 "MAGIT-SMOKE STEP S7f forward=%S" ok-forward)
                (setq ok-status
                      (and ok-mode ok-size ok-prop ok-children ok-forward))
                (let ((top (ignore-errors (magit-toplevel)))
                      (head (ignore-errors
                              (magit-git-string "rev-parse" "HEAD"))))
                  (nemacs-magit-status-live-smoke--print
                   "MAGIT-SMOKE STEP S7g top=%S" top)
                  (nemacs-magit-status-live-smoke--print
                   "MAGIT-SMOKE STEP S7h head=%S" head)
                  (setq ok-git
                        (and (stringp top)
                             (equal (file-name-as-directory top)
                                    fixture)
                             (stringp head)
                             (= (length head) 40))))))))
        (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE STEP S8")
        (nemacs-magit-status-live-smoke--print
         "MAGIT-STATUS-BUFFER %s"
         (if ok-status "PASS" "FAIL"))
        (nemacs-magit-status-live-smoke--print
         "MAGIT-GIT-EXEC %s"
         (if ok-git "PASS" "FAIL")))
    (error
     (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE ERROR")
     (nemacs-magit-status-live-smoke--print
      "MAGIT-SMOKE ERR %S" err)
     (dolist (line (nemacs-magit-status-live-smoke--backtrace-lines))
       (nemacs-magit-status-live-smoke--print "MAGIT-SMOKE BT %s" line))
     (nemacs-magit-status-live-smoke--print "MAGIT-STATUS-BUFFER FAIL")
     (nemacs-magit-status-live-smoke--print "MAGIT-GIT-EXEC FAIL"))))

;;; nemacs-magit-status-live-smoke-probe.el ends here
